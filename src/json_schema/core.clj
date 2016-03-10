(ns json-schema.core
  (:require [cheshire.core :as json]
            [clojure.set :as cset]
            [clojure.set :as set]
            [json-schema.refs :as refs]
            [clojure.string :as str]))

(declare valid?)
(declare validate*)

(defn new-context [ctx schema]
  (merge (or ctx {}) {:base-uri "" :errors [] :warnings [] :docs {"" schema}}))

(defn add-error [ctx err] (update-in ctx [:errors] conj err))

(defn add-error-on [ctx pred opts]
  (if-not pred (add-error ctx opts) ctx))

(defn add-warn  [ctx err] (update-in ctx [:warnings] conj err))

(defn mk-bound-fn [{type-filter :type-filter
                    value-fn :value-fn
                    operator :operator
                    operator-fn :operator-fn}]

  {:type-filter type-filter
   :validator (fn [key rule schema subj ctx]
                (let [op (or operator (operator-fn key rule schema subj ctx))
                      value (value-fn subj)]
                  (add-error-on
                   ctx
                   (op value rule)
                   {:desc key 
                    :actual value 
                    :expected (str  key " then " rule)})))})

(defn check-multiple-of [_ divider _ subj ctx]
  (let [res (/ subj divider)]
    (add-error-on
     ctx
     (re-matches #"^\d+(\.0)?$" (str res))
     {:expected (str  subj "/" divider)
      :actual (str res)})))

(defn check-pattern [_ pat _ subj ctx]
  (add-error-on
   ctx
   (re-find (re-pattern pat) subj)
   {:expected (str "matches: " pat)
    :actual subj}))



(defn string-utf8-length [x] (.count (.codePoints x)))


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
    (add-error-on
     ctx
     (some (fn [v] (v subj)) validators-fns)
     {:expectend (str "type:" tp)
      :actual subj})))

(defn check-properties [_ props schema subj ctx]
  (reduce
   (fn [ctx [prop-key prop-val]]
     (if-let [prop-sch (get props prop-key)]
       (validate* prop-sch prop-val ctx)
       ctx))
   ctx subj))

(defn check-enum [_ enum schema subj ctx]
  (add-error-on
   ctx
   (some (fn [v] (= v subj)) enum)
   {:expectend (str "one of " enum)
    :actual subj}))

(defn check-required [_ requireds schema subj ctx]
  (reduce (fn [ctx required-key]
            (add-error-on
             ctx
             (contains? subj (keyword required-key))
             {:expectend (str "field " required-key " is required")
              :actual subj}))
          ctx requireds))

(defn- collect-missing-keys [subj requered-keys]
  (reduce (fn [missing-keys key]
            (if (contains? subj (keyword key))
              missing-keys
              (conj missing-keys key)))
          [] requered-keys))

(defn- check-dependencies-array [deps-sch subj ctx]
  (let [missing-keys (collect-missing-keys subj deps-sch)]
    (add-error-on ctx
     (empty? missing-keys)
     {:expected (str "keys present " deps-sch)
      :actual (str missing-keys " are missed")})))

(defn- check-dependencies-object [deps-key deps-sch subj ctx]
  (if (contains? subj deps-key) (validate* deps-sch subj ctx) ctx))

(defn check-dependencies
  [_ deps schema subj ctx]
  (reduce (fn [ctx [deps-key deps-sch]]
            (if-not (contains? subj (keyword deps-key))
              ctx
              (if (vector? deps-sch)
                (check-dependencies-array deps-sch subj ctx)
                (check-dependencies-object deps-key deps-sch subj ctx))))
          ctx
          deps))


(defn check-additional-properites [_ a-prop schema subj ctx]
  (if (= a-prop true)
    ctx
    (let [s   (set (keys subj)) 
          p   (set (keys (:properties schema)))
          pp  (map (fn [re] (re-pattern (name re)))
                   (keys (:patternProperties schema)))
          additional  (->> (set/difference s p)
                           (remove (fn [x] (some #(re-find % (name x))  pp))))]
      (cond
        (= a-prop false) (add-error-on ctx
                          (empty? additional)
                          {:expected (str "one of " s)
                           :actual (str "extra props: " (vec additional))})
        (map? a-prop)    (reduce (fn [ctx p-key]
                                   (validate* a-prop (get subj p-key) ctx))
                                 ctx additional)))))


(defn check-pattern-properites [_ props schema subj ctx]
  (reduce
   (fn [ctx [pat sch]]
     (let [re-pat (re-pattern (name pat))]
       (reduce (fn [ctx [k v]]
                 (if (re-find re-pat (name k))
                   (validate* sch v ctx)
                   ctx)
                 ) ctx subj)))
   ctx props))

(defn check-items [_ item-schema {additional-items :additionalItems :as schema} subj ctx]
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
            ctx subj)))

(defn check-constant [_ item-schema schema subj ctx]
  (add-error-on
   ctx (= item-schema subj)
   {:expected (str subj " equals " item-schema)
    :actual "not equal"
    :details  subj}))

(defn check-contains [_ item-schema schema subj ctx]
  (add-error-on
   ctx (some #(valid? item-schema %) subj)
   {:expected (str "contains on of matching " item-schema)
    :actual "non of"
    :details  subj}))

(defn check-not [_ not-schema schema subj ctx]
  (add-error-on
   ctx (not (valid? not-schema subj))
   {:expected (str "not " (pr-str not-schema))
    :details  subj
    :actual   "valid"}))

(defn check-one-of [_ schemas _ subj ctx]
  (let [checked-cnt (reduce (fn [acc sch]
                              (if (valid? sch subj) (inc acc) acc))
                            0 schemas)]
    (add-error-on ctx
     (= 1 checked-cnt)
     {:expected (str "one of " schemas)
      :actual (str checked-cnt "valid")
      :details  subj})))

(defn check-any-of [_ schemas _ subj ctx]
  (let [results (map #(:errors (validate* % subj ctx)) schemas)]
    (add-error-on ctx
     (some #(empty? %) results)
     {:expected (str "any of " schemas)
      :actual (pr-str results) 
      :details  subj})))

(defn check-all-of [_ schemas _ subj ctx]
  (reduce (fn [ctx sch] (validate* sch subj ctx)) ctx schemas))

(defn check-uniq-items [_ unique? _ subj ctx]
  (add-error-on
   ctx
   (= (count subj) (count (set subj)))
   {:expected "all unique"
    :actual (str subj)}))

(defn check-ref [_ ref _ subj ctx]
  (if (refs/cycle-refs? ctx ref)
    (add-error ctx {:details (str "cycle refs" ref)})
    (let [[sch doc ctx] (refs/resolve-ref ctx ref)]
      (if sch
        (validate* sch subj (assoc-in ctx [:docs ""] doc))
        (add-error ctx {:desc (str "Could not resolve " ref)})))))


(def format-regexps
  {"date-time" #"(\d{4})-(\d{2})-(\d{2})[tT\s](\d{2}):(\d{2}):(\d{2})(\.\d+)?(?:([zZ])|(?:(\+|\-)(\d{2}):(\d{2})))"
   "email"     #"^[\w!#$%&'*+/=?`{|}~^-]+(?:\.[\w!#$%&'*+/=?`{|}~^-]+)*@(?:[a-zA-Z0-9-]+\.)+[a-zA-Z]{2,6}$"
   "hostname"  #"^([a-zA-Z0-9]([a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])?\.)+[a-zA-Z]{2,6}$"
   "ipv4"      #"^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"
   "ipv6"      #"(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))"
   "uri"       #"^((https?|ftp|file):)?//[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]"})

(defn check-format [key fmt _ subj ctx]
  (if-let [re (get format-regexps fmt)]
    (add-error-on
     ctx (re-matches re subj)
     {:key key
      :expected (str subj  " matches " re)})
    (add-error ctx {:key key
                    :details (str "Not known format" fmt)})))

;; todo should be atom
(def validators
  {:modifiers #{:exclusiveMaximum
                :definitions
                :id
                :description
                :$schema
                :default
                :details
                :exclusiveMinimum}

   :type {:validator check-type}

   :not {:validator check-not}

   :oneOf {:validator check-one-of}

   :anyOf {:validator check-any-of}

   :allOf {:validator check-all-of}

   :properties {:type-filter map?
                :validator check-properties}

   :required {:type-filter map?
              :validator check-required}

   :dependencies {:type-filter map?
                  :validator check-dependencies}

   :patternProperties {:type-filter map?
                       :validator check-pattern-properites}


   :additionalProperties {:type-filter map?
                          :validator check-additional-properites}

   :maxProperties (mk-bound-fn {:type-filter map?
                                :value-fn count
                                :operator <=})

   :minProperties (mk-bound-fn {:type-filter map?
                                :value-fn count
                                :operator >=})

   :$ref {:validator check-ref}

   :items {:type-filter vector?
           :validator check-items}

   :constant {:validator check-constant}

   :contains {:type-filter vector?
              :validator check-contains}

   :uniqueItems {:type-filter vector?
                 :validator check-uniq-items}

   :maxItems (mk-bound-fn {:type-filter vector?
                           :value-fn count
                           :operator <=})
   :minItems (mk-bound-fn {:type-filter vector?
                           :value-fn count
                           :operator >=})


   :pattern {:type-filter string?
             :validator    check-pattern}


   :format {:type-filter string?
            :validator check-format}

   :maxLength (mk-bound-fn {:type-filter string?
                            :value-fn string-utf8-length
                            :operator <=})

   :minLength (mk-bound-fn {:type-filter string?
                            :value-fn string-utf8-length
                            :operator >=})


   :minimum (mk-bound-fn {:type-filter number?
                          :value-fn identity
                          :operator-fn (fn [_ _ schema & _]
                                         (if (:exclusiveMinimum schema) > >=))})
   :maximum (mk-bound-fn {:type-filter number?
                          :value-fn identity
                          :operator-fn (fn [_ _ schema & _]
                                         (if (:exclusiveMaximum schema) < <=))})

   :multipleOf {:type-filter number?
                :validator check-multiple-of}

   :enum {:validator check-enum}})


(defn warn-on-unknown-keys [key ctx]
  (if (contains? (:modifiers validators) key)
    ctx
    (add-warn ctx {:desc "Unknown schema keyword" :details key})))

(defn validate* [schema subj ctx]
  {:pre  [(map? schema)]}
  (let [ctx (refs/set-resolution-context ctx (:id schema))]
    ;;(update-base-url ctx schema)
    (reduce
     (fn [ctx [key rule]]
       (if-let [{:keys [type-filter validator]} (get validators key)]
         (if (or (not type-filter) (and type-filter (type-filter subj)))
           (validator key rule schema subj ctx)
           ctx)
         (warn-on-unknown-keys key ctx)))
     ctx
     schema)))

(defn validate [schema subj & [ctx]]
  (select-keys (validate* schema subj (new-context ctx schema))
               [:errors :warnings]))

(defn valid? [schema subj & [ctx]]
  (-> (validate schema subj ctx) :errors (empty?)))
