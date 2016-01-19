(ns json-schema.core
  (:require [clojure.set :as cset]
            [clojure.string :as str]))

(declare validate)
(declare validate*)

(defn add-error [ctx err]
  (update-in ctx [:errors] conj err))

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
  "
  5.4.5.    dependencies
  5.4.5.1.  Valid values

  This keyword's value MUST be an object. Each value of this object MUST be either an object or an array.
  If the value is an object, it MUST be a valid JSON Schema. This is called a schema dependency.
  If the value is an array, it MUST have at least one element. Each element MUST be a string, and
  elements in the array MUST be unique. This is called a property dependency.

  TOC 5.4.5.2.  Conditions for successful validation

  TOC 5.4.5.2.1.  Schema dependencies
  For all (name, schema) pair of schema dependencies, if the instance has a property by this name,
  then it must also validate successfully against the schema.
  Note that this is the instance itself which must validate successfully,
  not the value associated with the property name.


  TOC 5.4.5.2.2.  Property dependencies
  For each (name, propertyset) pair of property dependencies,
  if the instance has a property by this name,
  then it must also have properties with the same names as propertyset.
  "
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

(defn additionalProperties [_ allowed? schema subj ctx]
  (if (or allowed? (not (map? subj)))
    ctx
    (if (cset/subset? (set (keys subj)) (set (keys (:properties schema))))
      ctx
      (add-error ctx {:expected (str "one of " (keys (:properties schema)))
                      :actual (cset/difference (set (keys subj)) (set (keys (:properties schema))))}))))


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

(defn check-items [_ item-schema schema subj ctx]
  (if-not (vector? subj)
    ctx
    (if (vector? item-schema)
      (if (not= (count item-schema) (count subj))
        (add-error ctx {:expected (str (count item-schema) " items in list")
                        :actual (count subj)})
        (reduce (fn [ctx [sch v]]
                  (validate* sch v ctx))
                ctx (map vector item-schema subj)))
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
  (if (some #(validate % subj) schemas) 
    ctx
    (add-error ctx {:expected (str "any of " schemas)
                    :actual "none of valid"
                    :details  subj})))

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

(defn ref-to-path [ref]
  (mapv (fn [x]
         (if (re-matches #"\d+" x)
           (read-string x)
           (keyword x)))
       (rest (str/split (.substring ref 1) #"/"))))


(defn resolve-ref [scope ref]
  (when (string? ref)
    (when-let [path (cond
                     (.startsWith ref "#") (ref-to-path ref)
                     ;; dirty
                     (.contains ref "#")   (ref-to-path (str "#" (second (str/split ref #"#" 2))))
                     :else nil)]
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

(defn check-ref [_ ref _ subj ctx]
  (println "REF:" ref)
  (if (cycle-refs? (:resolution-scope ctx) ref)
    (add-error ctx {:details (str "cycle refs" ref)})
    (if-let [schema (resolve-ref (:resolution-scope ctx) ref)]
      (do (println "RESOLVED INTO " schema)
          (validate* schema subj ctx))
      (add-error ctx {:expected (str "ref " ref)
                      :actual (str "not resolved")}))))

;; todo should be atom
(def validators
  {:modifiers #{:exclusiveMaximum
                :exclusiveMinimum}

   :type check-type

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
  (if (map? schema)
    (reduce
     (fn [ctx [key rule]]
       (if (contains? (:modifiers validators) key)
         ctx
         (if-let [h (get validators key)]
           (h key rule schema subj ctx)
           (add-error ctx {:desc "Unknown schema keyword" :details key}))))
     ctx
     schema)))

(defn check [schema subj]
  (validate* schema subj {:errors [] :warnings [] :resolution-scope schema}))

(defn validate [schema subj]
  (let [res (check schema subj)]
    (if (empty? (:errors res))
      true
      false)))
