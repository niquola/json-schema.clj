(ns json-schema.core
  (:require [cheshire.core :as json]
            [clojure.set :as cset]
            [clojure.set :as set]
            [clojure.string :as str]))

(declare validate)
(declare validate*)

(defn add-error [ctx err] (update-in ctx [:errors] conj err))
(defn add-warn [ctx err] (update-in ctx [:warnings] conj err))

(defn mk-bound-fn [{type-filter-fn :type-filter-fn
                    value-fn :value-fn
                    operator :operator
                    operator-fn :operator-fn}]

  (fn [key rule schema subj ctx]
    (if-not (type-filter-fn subj)
      ctx
      (let [op (or operator (operator-fn key rule schema subj ctx))
            value (value-fn subj)]
        (if (op value rule)
          ctx
          (add-error ctx {:desc key 
                          :actual value 
                          :expected (str  key " then " rule)}))))))

(defn check-multiple-of [_ divider _ subj ctx]
  (if-not (number? subj)
    ctx
    (let [res (/ subj divider)]
      (if (re-matches #"^\d+(\.0)?$" (str res))
        ctx
        (add-error ctx {:expected (str  subj "/" divider)
                        :actual (str res)})))))

(defn check-pattern [_ pat _ subj ctx]
  (if-not (string? subj)
    ctx
    (if (re-find (re-pattern pat) subj)
      ctx
      (add-error ctx {:expected (str "matches: " pat)
                      :actual subj}))))


(defn skip [key rule schema subj ctx] ctx)

(defn string-length [x] (.count (.codePoints x)))


;; todo should be atom
(def basic-types
  {"object" map?
   "array" #(or (seq? %) (and (coll? %) (not (map? %))))
   "string" string?
   "number" number?
   "integer" integer?
   "boolean" #(instance? Boolean %)
   "null" nil?
   "any" (constantly true)})

(defn check-type [_ tp _ subj ctx]
  (let [validators (if (vector? tp) tp [tp])
        validators-fns (map #(get basic-types %) validators)]
    (if (some (fn [v] (v subj)) validators-fns)
      ctx
      (add-error ctx {:expectend (str "type:" tp)
                      :actual subj}))))

(defn check-properties [_ props schema subj ctx]
  (if-not (map? subj)
    ctx
    (reduce
     (fn [ctx [prop-key prop-val]]
       (if-let [prop-sch (get props prop-key)]
         (validate* prop-sch prop-val ctx)
         ctx))
     ctx
     subj)))

(defn check-enum [_ enum schema subj ctx]
  (if (some (fn [v] (= v subj)) enum)
    ctx
    (add-error ctx {:expectend (str "one of " enum)
                    :actual subj})))

(defn check-required [_ requireds schema subj ctx]
  (if-not (map? subj)
    ctx
    (reduce (fn [ctx required-key]
              (if (contains? subj (keyword required-key))
                ctx
                (add-error ctx {:expectend (str "field " required-key " is required")
                                :actual subj})))
            ctx requireds)))

(defn- collect-missing-keys [subj requered-keys]
  (reduce (fn [missing-keys key]
            (if (contains? subj (keyword key))
              missing-keys
              (conj missing-keys key)))
          [] requered-keys))

(defn- check-dependencies-array [deps-sch subj ctx]
  (let [missing-keys (collect-missing-keys subj deps-sch)]
    (if (empty? missing-keys)
      ctx
      (add-error ctx {:expected (str "keys present " deps-sch)
                      :actual (str missing-keys " are missed")}))))

(defn- check-dependencies-object [deps-key deps-sch subj ctx]
  (if (contains? subj deps-key) 
    (validate* deps-sch subj ctx)
    ctx))

(defn check-dependencies
  [_ deps schema subj ctx]
  (if-not (map? subj)
    ctx
    (reduce (fn [ctx [deps-key deps-sch]]
              (if-not (contains? subj (keyword deps-key))
                ctx
                (if (vector? deps-sch)
                  (check-dependencies-array deps-sch subj ctx)
                  (check-dependencies-object deps-key deps-sch subj ctx))))
            ctx
            deps)))


(defn additionalProperties [_ a-prop schema subj ctx]
  (if-not (map? subj)
    ctx
    (if (= a-prop true)
        ctx
        (let [s   (set (keys subj)) 
              p   (set (keys (:properties schema)))
              pp  (map (fn [re] (re-pattern (name re)))
                       (keys (:patternProperties schema)))
              additional  (->> (set/difference s p)
                               (remove (fn [x] (some #(re-find % (name x))  pp))))]
          (cond
            (= a-prop false) (if (empty? additional)
                               ctx
                               (add-error ctx {:expected (str "one of " s)
                                               :actual (str "extra props: " additional)}))
            (map? a-prop) (reduce (fn [ctx p-key]
                                    (validate* a-prop (get subj p-key) ctx))
                                  ctx additional))))))


(defn patternProperties [_ props schema subj ctx]
  (if-not (map? subj)
    ctx
    (reduce
     (fn [ctx [pat sch]]
       (let [re-pat (re-pattern (name pat))]
         (reduce (fn [ctx [k v]]
                   (if (re-find re-pat (name k))
                     (validate* sch v ctx)
                     ctx)
                   ) ctx subj)))
     ctx props)))

(defn check-items [_ item-schema {additional-items :additionalItems :as schema} subj ctx]
  (if-not (vector? subj)
    ctx
    (if (vector? item-schema)
      (reduce (fn [ctx [idx v]]
                (let [sch (or (get item-schema idx) additional-items)]
                  (cond
                    (= false sch) (add-error ctx {:desc (str "No additional schema for item " idx) :actual subj})
                    (map? sch)    (validate* sch v ctx)
                    :else         ctx)))
              ctx (map vector (range) subj))

      (reduce (fn [ctx value]
                (validate* item-schema value ctx))
              ctx subj))))


(defn check-not [_ not-schema schema subj ctx]
  (if-not (validate not-schema subj)
    ctx
    (add-error ctx {:expected (str "not " (pr-str not-schema))
                    :details  subj
                    :actual   "valid"})))

(defn check-one-of [_ schemas _ subj ctx]
  (let [checked-cnt (reduce (fn [acc sch]
                              (if (validate sch subj) (inc acc) acc))
                            0 schemas)]
    (if (= 1 checked-cnt)
      ctx
      (add-error ctx {:expected (str "one of " schemas)
                      :actual (str checked-cnt "valid")
                      :details  subj}))))

(defn check-any-of [_ schemas _ subj ctx]
  (let [results (map #(:errors (validate* % subj ctx)) schemas)]
    (if (some #(empty? %) results)
      ctx
      (add-error ctx {:expected (str "any of " schemas)
                      :actual (pr-str results) 
                      :details  subj}))))

(defn check-all-of [_ schemas _ subj ctx]
  (reduce
   (fn [ctx sch] (validate* sch subj ctx))
   ctx schemas))

(defn check-uniq-items [_ unique? _ subj ctx]
  (if-not (vector? subj)
    ctx
    (if (= (count subj)
           (count (set subj)))
      ctx
      (add-error ctx {:expected "all unique"
                      :actual (str subj)}))))

(defn decode-json-pointer [x]
  (-> x
      (str/replace #"~0" "~")
      (str/replace #"~1" "/")
      (str/replace #"%25" "%")))

(defn ref-to-path [ref]
  (mapv (fn [x]
         (if (re-matches #"\d+" x)
           (read-string x)
           (keyword (decode-json-pointer x))))
       (rest (str/split (.substring ref 1) #"/"))))

(defn relative-ref? [url]
  (and (not (.startsWith url "#"))
       (not (re-matches #"^https?://.*" url))))

(defn resolve-base-url [base-url url]
  (when (and base-url url)
    (str (.resolve (java.net.URI. base-url) url))))

(defn resolve-ref [scope ref]
  (when (string? ref)
    (when-let [path (when (.startsWith ref "#") (ref-to-path ref))]
     (println "RESOLVE SCOPE:" path " in " scope)
     (get-in scope path))))

(comment
  (ref-to-path "#/a/b/0/c")
  (ref-to-path "#/aka/b/0/c")
  (resolve-ref {:a {:b [{:c 1}]}} "#/a/b/0/c"))

(defn cycle-refs? [scope ref]
  (loop [ref ref visited #{}]
    (let [new-ref (resolve-ref scope ref)]
      (if (:$ref new-ref)
        (if (contains? visited ref)
          true
          (recur new-ref (conj visited ref)))
        false))))

(defn remote-ref? [ref]
  (not (re-matches #"^#.*$" ref)))

(defn default-resolve-ref [ref]
  (when-let [res (slurp ref)]
    (println "Loaded: " res)
    (json/parse-string res keyword)))

(defn resolve-remote-ref [ref ctx]
  (let [uri (if (relative-ref? ref)
              (resolve-base-url (:base-url ctx) ref)
              ref)]
    (if-let [f (:resolve-ref-fn ctx)]
      (or (f uri) (default-resolve-ref uri))
      (default-resolve-ref uri))))

(defn remote-ref-fragment [ref]
  (when-let [hsh (second (str/split ref #"#" 2))]
    {:$ref (str "#" hsh)}))

(defn check-ref [_ ref _ subj ctx]
  (println "Ref:" ref)
  (if (remote-ref? ref)
    (if-let [schema (resolve-remote-ref ref ctx)]
      (if-let [internal-ref (remote-ref-fragment ref)]
        (validate* internal-ref subj (assoc ctx :current-schema schema))
        (validate* schema subj (assoc ctx :current-schema schema)))
      (add-error ctx {:details (str "could not resolve ref " ref)}))
    (if (cycle-refs? (:current-schema ctx) ref)
      (add-error ctx {:details (str "cycle refs" ref)})
      (if-let [schema (resolve-ref (:current-schema ctx) ref)]
        (do (println "Resolved to: " schema)
            (validate* schema subj ctx))
        (add-error ctx {:expected (str "ref " ref)
                        :actual (str "not resolved")})))))


(def format-re
  {"date-time" #"(\d{4})-(\d{2})-(\d{2})[tT\s](\d{2}):(\d{2}):(\d{2})(\.\d+)?(?:([zZ])|(?:(\+|\-)(\d{2}):(\d{2})))"
   "email"     #"^[\w!#$%&'*+/=?`{|}~^-]+(?:\.[\w!#$%&'*+/=?`{|}~^-]+)*@(?:[a-zA-Z0-9-]+\.)+[a-zA-Z]{2,6}$"
   "hostname"  #"^([a-zA-Z0-9]([a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])?\.)+[a-zA-Z]{2,6}$"
   "ipv4"      #"^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"
   "ipv6"      #"(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))"
   "uri"       #"^((https?|ftp|file):)?//[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]"})

(defn check-format [_ fmt _ subj ctx]
  (if-not (string? subj)
    ctx
    (if-let [re (get format-re fmt)]
      (if (re-matches re subj)
        ctx
        (add-error ctx {:expected (str subj  " matches " re)}))
      (add-error ctx {:details (str "Not known format" fmt)}))))

;; todo should be atom
(def validators
  {:modifiers #{:exclusiveMaximum
                :definitions
                :id
                :exclusiveMinimum}

   :type check-type

   :pattern check-pattern

   :not check-not

   :oneOf check-one-of

   :anyOf check-any-of

   :allOf check-all-of

   :properties check-properties

   :required check-required

   :dependencies check-dependencies

   :patternProperties patternProperties

   :additionalProperties additionalProperties

   :items check-items

   :$ref check-ref

   :uniqueItems check-uniq-items

   :maxItems (mk-bound-fn {:type-filter-fn vector?
                           :value-fn count
                           :operator <=})
   :minItems (mk-bound-fn {:type-filter-fn vector?
                           :value-fn count
                           :operator >=})

   :maxLength (mk-bound-fn {:type-filter-fn string?
                            :value-fn string-length
                            :operator <=})
   :minLength (mk-bound-fn {:type-filter-fn string?
                            :value-fn string-length 
                            :operator >=})

   :minimum (mk-bound-fn {:type-filter-fn number?
                          :value-fn identity
                          :operator-fn (fn [_ _ schema & _] (if (:exclusiveMinimum schema) > >=))})
   :maximum (mk-bound-fn {:type-filter-fn number?
                          :value-fn identity
                          :operator-fn (fn [_ _ schema & _] (if (:exclusiveMaximum schema) < <=))})

   :format check-format

   :multipleOf check-multiple-of

   :maxProperties (mk-bound-fn {:type-filter-fn map?
                                :value-fn count
                                :operator <=})

   :minProperties (mk-bound-fn {:type-filter-fn map?
                                :value-fn count
                                :operator >=})


   :enum check-enum})

(defn push-to-parent-scopes [schema parent-scopes]
  (if (and (:id schema) (not= schema (last parent-scopes)))
    (conj parent-scopes schema)
    parent-scopes))


(defn validate* [schema subj ctx]
  (let [ctx (if (:id schema)
              (update-in ctx [:base-url]
                         (fn [base-url] (resolve-base-url base-url (:id schema))))
              ctx)]
      (if (map? schema)
        (reduce
         (fn [ctx [key rule]]
           (if-let [h (get validators key)]
             (h key rule schema subj ctx)
             (if (contains? (:modifiers validators) key)
               ctx
               (add-warn ctx {:desc "Unknown schema keyword" :details key}))))
         ctx
         schema))))

(defn check [schema subj & [ctx]]
  (validate* schema subj (merge (or ctx {:base-url (:id schema)})
                                {:errors []
                                 :warnings []
                                 :current-schema schema})))

(defn validate [schema subj & [ctx]]
  (let [res (check schema subj ctx)]
    (if (empty? (:errors res))
      true
      false)))
