(ns json-schema.core
  (:require [cheshire.core :as json]
            [clojure.set :as cset]
            [clojure.set :as set]
            [json-schema.refs :as refs]
            [clojure.string :as str]))

(declare valid?)
(declare validate*)

(defn new-context [ctx schema subj]
  (let [docs (-> (:docs ctx)
                 (cond-> (nil? (get-in ctx [:docs ""])) (assoc "" schema)))]
    (merge (or ctx {})
           {:base-uri ""
            :errors []
            :path []
            :warnings []
            :subj (or (:subj ctx) subj)
            :docs docs})))

(defn- push-path [ctx k]
  (update-in ctx [:path] conj k))

(defn- pop-path [ctx]
  (update-in ctx [:path] (fn [v] (into [] (butlast v)))))

(defn safe-parse-number [x]
  (cond
    (string? x) (if (re-matches #"^\d+$" x) (read-string x) nil)
    (number? x) x
    :else nil))

(defn- resolve-$data [ctx sch]
  (if-let [ref (:$data sch)]
    (let [x (refs/resolve-relative-ref (:subj ctx) (:path ctx) ref)]
      (if (keyword? x) (name x) x))
    sch))

(defn add-error [ctx err]
  (update-in ctx [:errors] conj (assoc err :path (:path ctx))))

(defn add-error-on [ctx pred opts]
  (if-not pred (add-error ctx opts) ctx))

(defn add-warn  [ctx err] (update-in ctx [:warnings] conj err))

(defn validate-bounds [value-fn op [key rule schema subj ctx]]
  (let [value (value-fn subj)
        resolved-rule (resolve-$data ctx rule)]
    (cond
      (number? resolved-rule)
      (add-error-on
       ctx
       (op value resolved-rule)
       {:desc key
        :actual value
        :expected (str  key " then " rule)})

      (nil? resolved-rule) ctx

      :else (add-error ctx
                       {:desc   (str "max/min value should be number")
                        :actual (str "typeof " resolved-rule "=" (type resolved-rule))}))))

(defn mk-bound-fn [{type-filter :type-filter
                    value-fn :value-fn
                    operator :operator}]

  {:type-filter type-filter
   :validator (fn [& args]
                (validate-bounds value-fn operator args))})


(defn mk-minmax-validator [exclusive-key exclusive-op non-exclusive-op]
  (fn [& args]
    (let [[_ _ schema _ ctx] args]
      (if (contains? schema exclusive-key)
        (let [resolved-exclusive (resolve-$data ctx (get schema exclusive-key))]
          (cond
            (instance? Boolean resolved-exclusive) (validate-bounds identity (if resolved-exclusive  exclusive-op non-exclusive-op) args)
            (nil? resolved-exclusive) (validate-bounds identity non-exclusive-op args)
            :else (add-error
                   ctx {:desc   (str (name exclusive-key) " value should be boolean")
                        :actual (str "typeof " resolved-exclusive "=" (type resolved-exclusive))})))
        (validate-bounds identity non-exclusive-op args)))))

(defn check-multiple-of [_ divider _ subj ctx]
  (let [resolved-devider (resolve-$data ctx divider)]
    (cond
      (number? resolved-devider) (let [res (/ subj resolved-devider)]
                                   (add-error-on
                                    ctx
                                    (re-matches #"^\d+(\.0)?$" (str res))
                                    {:expected (str  subj "/" divider)
                                     :actual (str res)}))
      (nil? resolved-devider) ctx
      :else (add-error
             ctx {:expected (str  "multipleOf value must be number, but typeof " resolved-devider "=" (type resolved-devider))}))))

(defn check-pattern [_ pat _ subj ctx]
  (let [resolved-pat (resolve-$data ctx pat)]
    (cond
      (string? resolved-pat) (add-error-on
                              ctx
                              (re-find (re-pattern resolved-pat) subj)
                              {:expected (str "matches: " resolved-pat)
                               :actual subj})
      (nil? resolved-pat) ctx
      :else (add-error
             ctx {:expected (str "pattern value should be string, but typeof " resolved-pat " = " (type resolved-pat))}))))



(defn string-utf8-length [x] (.count (.codePoints x)))

(defn is-array? [x]
  (or (seq? x) (and (coll? x) (not (map? x)))))

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
     {:expected (str "type:" tp)
      :actual subj})))

(defn check-properties [_ props schema subj ctx]
  (reduce
   (fn [ctx [prop-key prop-val]]
     (if-let [prop-sch (get props prop-key)]
       (pop-path (validate* prop-sch prop-val (push-path ctx prop-key)))
       ctx))
   ctx subj))

(defn check-pattern-groups [_ props schema subj ctx]
  (reduce
   (fn [ctx [key-regexp group-schema]]
     (let [regexp (re-pattern (name key-regexp))]
       (let [new-ctx (reduce
                      (fn [ctx [prop-key prop-val]]
                        (if (re-find regexp (name prop-key)) 
                          (-> (validate* (:schema group-schema) prop-val (push-path ctx prop-key))
                              (pop-path)
                              (update :_matched-props inc))
                          ctx))
                      (assoc ctx :_matched-props 0) subj)]
         (-> new-ctx
             (add-error-on
              (or (nil? (:minimum group-schema))
                   (>= (:_matched-props new-ctx) (:minimum group-schema)))
              {:expected (str "keys matched " key-regexp " count " (:_matched-props new-ctx) "  >= " (:minimum group-schema))})
             (add-error-on
              (or (nil? (:maximum group-schema))
                   (<= (:_matched-props new-ctx) (:maximum group-schema)))
              {:expected (str "keys matched " key-regexp " count " (:_matched-props new-ctx) "  <= " (:maximum group-schema))})
             (dissoc :_matched-props)))))
   ctx props))

(defn check-enum [_ enum schema subj ctx]
  (let [resolved-enum (resolve-$data ctx enum)]
    (add-error-on
     ctx
     (or (nil? resolved-enum)
         (some (fn [v] (= (resolve-$data ctx v) subj)) resolved-enum))
     {:expected (str "one of " resolved-enum)
      :actual subj})))


(defn check-required [_ requireds schema subj ctx]
  (let [resolved-requireds (resolve-$data ctx requireds)]
    (cond
      (is-array? resolved-requireds) (reduce (fn [ctx required-key]
                                             (add-error-on
                                              ctx
                                              (contains? subj (keyword required-key))
                                              {:expected (str "field " required-key " is required")
                                               :actual subj}))
                                           ctx resolved-requireds)
      (nil? resolved-requireds) ctx
      :else (add-error
             ctx {:expected (str "required value should be array, but typeof " resolved-requireds "=" (type resolved-requireds))}))))

(defn- contains-pattern? [subj-keys regexp]
  (some #(re-find regexp %) subj-keys))

(defn check-pattern-required [_ requireds schema subj ctx]
  (let [requireds-regexps (map #(re-pattern %) requireds)
        subj-keys (map name (keys subj))]
    (reduce (fn [ctx required-key]
              (add-error-on
               ctx
               (contains-pattern? subj-keys required-key)
               {:expected (str "some fields should match " required-key " but " subj-keys)
                :actual subj-keys}))
            ctx requireds-regexps)))

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
                   (concat
                    (or (keys (:patternProperties schema)) [])
                    (or (keys (:patternGroups schema)) [])))
          additional  (->> (set/difference s p)
                           (remove (fn [x] (some #(re-find % (name x))  pp))))]
      (cond
        (= a-prop false) (add-error-on ctx
                          (empty? additional)
                          {:expected (str "one of " p)
                           :actual (str "extra props: " (vec additional))})
        (map? a-prop)    (reduce (fn [ctx p-key]
                                   (pop-path
                                    (validate* a-prop (get subj p-key) (push-path ctx p-key))))
                                 ctx additional)))))


(defn check-pattern-properites [_ props schema subj ctx]
  (reduce
   (fn [ctx [pat sch]]
     (let [re-pat (re-pattern (name pat))]
       (reduce (fn [ctx [k v]]
                 (if (re-find re-pat (name k))
                   (pop-path
                    (validate* sch v (push-path ctx k)))
                   ctx)
                 ) ctx subj)))
   ctx props))

(defn check-items [_ item-schema {additional-items :additionalItems :as schema} subj ctx]
  (if (vector? item-schema)
    (reduce (fn [ctx [idx v]]
              (let [sch (or (get item-schema idx) additional-items)
                    new-ctx (push-path ctx idx)]
                (cond
                  (= false sch) (pop-path
                                 (add-error new-ctx {:desc (str "No additional schema for item " idx)
                                                     :actual subj}))
                  (map? sch)    (pop-path
                                 (validate* sch v new-ctx))
                  :else         ctx)))
            ctx (map vector (range) subj))

    (reduce (fn [ctx [idx value]]
              (pop-path (validate* item-schema value (push-path ctx idx))))
            ctx (map vector (range) subj))))

(defn check-constant [_ item-schema schema subj ctx]
  (let [schema-value (resolve-$data ctx item-schema)]
    (add-error-on
     ctx (=  schema-value subj)
     {:expected (str subj " equals " schema-value)
      :actual "not equal"
      :details  subj})))

(defn check-contains [_ item-schema schema subj ctx]
  (add-error-on
   ctx (some (fn [[idx x]] (valid? item-schema x (push-path ctx idx))) (map vector (range) subj))
   {:expected (str "contains on of matching " item-schema)
    :actual "non of"
    :details  subj}))

(defn check-not [_ not-schema schema subj ctx]
  (add-error-on
   ctx (not (valid? not-schema subj ctx))
   {:expected (str "not " (pr-str not-schema))
    :details  subj
    :actual   "valid"}))

(defn check-switch [_ switch schema subj ctx]
  (loop [[clause & clauses] switch ctx ctx]
    (if (or (not (:if clause)) (valid? (:if clause) subj ctx))
      (let [then (:then clause)]
        (cond
          (= true then) ctx
          (= false then) (add-error
                          ctx {:expected (str "switch keyword failed on " subj " by " (:if clause))})
          (map? then)    (let [new-ctx (validate* then subj ctx)]
                           (if (and (:continue clause) (not (empty? clauses)))
                             (recur clauses new-ctx)
                             new-ctx))
          :else (add-error
                 ctx {:expected (str "unexpected switch clause " clause)})))
      (if (empty? clauses) ctx (recur clauses ctx)))))

(defn check-one-of [_ schemas _ subj ctx]
  (let [checked-cnt (reduce (fn [acc sch]
                              (if (valid? sch subj ctx) (inc acc) acc))
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
  (let [resolved-unique (resolve-$data ctx unique?)]
    (cond
      (instance? Boolean resolved-unique) (add-error-on
                                           ctx (if resolved-unique
                                                 (= (count subj) (count (set subj)))
                                                 (not (= (count subj) (count (set subj)))))
                                           {:expected "all unique"
                                            :actual (str subj)})
      (nil? resolved-unique) ctx
      :else (add-error
             ctx {:expected (str "uniqueItems value should be boolean, but typeof " resolved-unique "=" (type resolved-unique))}))))

(defn check-ref [_ ref _ subj ctx]
  (if (refs/cycle-refs? ctx ref)
    (add-error ctx {:details (str "cycle refs" ref)})
    (let [[sch doc ctx] (refs/resolve-ref ctx ref)]
      (if sch
        (validate* sch subj (assoc-in ctx [:docs ""] doc))
        (add-error ctx {:desc (str "Could not resolve " ref)})))))

(defn check-deferred [k ref x subj ctx]
  (let [item (merge {:value subj} ref {:path (:path ctx)})]
    (update-in ctx [:deferreds]
               (fn [ds]
                 (if ds
                   (conj ds item)
                   #{item})))))


(defn type-property [k prop schema subj ctx]
  (if-let [tp (get subj (keyword prop))]
    (check-ref k (str "#/definitions/" tp) schema subj ctx)
    (add-error ctx {:details (str "property " prop " should not be empty")
                    :actual subj})))


(def format-regexps
  {"date-time" #"^(\d{4})-(\d{2})-(\d{2})[tT\s](\d{2}):(\d{2}):(\d{2})(\.\d+)?(?:([zZ])|(?:(\+|\-)(\d{2}):(\d{2})))$"
   "date"      #"^(\d{4})-(\d{2})-(\d{2})$"
   "time"      #"^(\d{2}):(\d{2}):(\d{2})(\.\d+)?([zZ]|(\+|\-)(\d{2}):(\d{2}))?$"
   "email"     #"^[\w!#$%&'*+/=?`{|}~^-]+(?:\.[\w!#$%&'*+/=?`{|}~^-]+)*@(?:[a-zA-Z0-9-]+\.)+[a-zA-Z]{2,6}$"
   "hostname"  #"^([a-zA-Z0-9]([a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])?\.)+[a-zA-Z]{2,6}$"
   "ipv4"      #"^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"
   "ipv6"      #"(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))"
   "uri"       #"^((https?|ftp|file):)?//[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]"
   "unknownformat"   #"^.*$"
   "unknown"   #"^.*$"})

(defn check-format [key fmt _ subj ctx]
  (if-let [resolved-fmt (resolve-$data ctx fmt)]
    (cond
      (or (string? resolved-fmt)
          (keyword? resolved-fmt)) (if-let [re (get format-regexps (name resolved-fmt))]
                                     (add-error-on
                                      ctx (re-matches re subj)
                                      {:key key
                                       :expected (str subj  " matches " re)})
                                     (add-error ctx {:key key
                                                     :details (str "Not known format " resolved-fmt)}))

      (nil? resolved-fmt) ctx
      :else (add-error
             ctx {:expected (str "format value should be string, but typeof " resolved-fmt "=" (type resolved-fmt))}))
    ctx))


(defn- remove-timezone [x]
  (str/replace x #"([zZ]|[+-]\d{2}(:\d{2})?)$" ""))

(comment
  (remove-timezone "12:00:23.000Z")
  (remove-timezone "12:00:23.000+03:01"))

(defn- litteral-date-compare [d1 d2]
  (compare (remove-timezone d1) (remove-timezone d2)))

(def format-comparators
  {"date"  litteral-date-compare
   "time" litteral-date-compare 
   "unknown" (fn [& args] 0)
   "date-time" litteral-date-compare})


(defn check-format-minmax [exclusion-key
                          exclusive-op non-exclusive-op
                          key limit schema subj ctx]
  (let [format         (resolve-$data ctx (:format schema))
        exclusive      (resolve-$data ctx (get schema exclusion-key))
        resolved-limit (resolve-$data ctx limit)
        op (if exclusive exclusive-op non-exclusive-op)]

    (cond
      (and (or (nil? exclusive) (instance? Boolean exclusive))
           (string? resolved-limit)) (if-let [comparator-fn (get format-comparators format)]
                                       (add-error-on
                                        ctx (and resolved-limit subj (op 0 (comparator-fn resolved-limit subj)))
                                        {:expected (str "expected " subj " "
                                                        (if (= exclusion-key :exclusiveFormatMaximum) ">" "<")
                                                        (when-not exclusive "=")
                                                        " " limit)})
                                       (add-error
                                        ctx {:expected (str "Do not know how to compare format " format)}))
      (nil? resolved-limit) ctx

      (not (string? resolved-limit)) (add-error ctx {:expected (str (name key) "should be string, but typeof " resolved-limit "=" (type resolved-limit))})

      (not (or (nil? exclusive) (instance? Boolean exclusive))) (add-error ctx {:expected (str (name exclusion-key) "should be boolean, but typeof " exclusive "=" (type exclusive))})

      :else (-> ctx
                (add-error {:expected (str (name key) "should be string, but typeof " resolved-limit "=" (type resolved-limit))})
                (add-error {:expected (str (name exclusion-key) "should be boolean, but typeof " exclusive "=" (type exclusive))})))))


(defn check-format-maximum [key limit schema subj ctx]
  (check-format-minmax
   :exclusiveFormatMaximum
   < <=
   key limit schema subj ctx))

(defn check-format-minimum [key limit schema subj ctx]
  (check-format-minmax
   :exclusiveFormatMinimum
   > >=
   key limit schema subj ctx))

;; todo should be atom
(def validators
  {:modifiers #{:exclusiveMaximum
                :definitions
                :id
                :description
                :$schema
                :default
                :details
                :exclusiveFormatMinimum
                :exclusiveFormatMaximum
                :exclusiveMinimum}

   :type {:validator check-type}

   :not {:validator check-not}
   :switch {:validator check-switch}

   :oneOf {:validator check-one-of}

   :anyOf {:validator check-any-of}

   :allOf {:validator check-all-of}

   :properties {:type-filter map?
                :validator check-properties}

   :patternGroups {:type-filter map?
                   :validator check-pattern-groups}

   :required {:type-filter map?
              :validator check-required}

   :patternRequired {:type-filter map?
                     :validator check-pattern-required}

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
   :$deferred {:validator check-deferred}

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

   :formatMaximum {:type-filter string?
                   :validator check-format-maximum}

   :formatMinimum {:type-filter string?
                   :validator check-format-minimum}

   :maxLength (mk-bound-fn {:type-filter string?
                            :value-fn string-utf8-length
                            :operator <=})

   :minLength (mk-bound-fn {:type-filter string?
                            :value-fn string-utf8-length
                            :operator >=})


   :minimum {:type-filter number?
             :validator (mk-minmax-validator :exclusiveMinimum > >=)}

   :maximum {:type-filter number?
             :validator (mk-minmax-validator :exclusiveMaximum < <=)}
   
   :multipleOf {:type-filter number?
                :validator check-multiple-of}

   :enum {:validator check-enum}

   :typeProperty {:type-filter map?
                  :validator type-property}})


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
  (let [res (select-keys (validate* schema subj (new-context ctx schema subj))
                         [:errors :warnings :deferreds])]
    (if (and (empty? (:errors res)) (empty? (:deferreds res)))
      nil
      res)))

(defn valid? [schema subj & [ctx]]
  (-> (validate schema subj ctx) :errors (empty?)))
