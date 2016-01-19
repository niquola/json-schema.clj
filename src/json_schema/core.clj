(ns json-schema.core
  (:require [clojure.set :as cset]))

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
              (if (get subj (keyword required-key))
                ctx
                (add-error ctx {:expectend (str "field " required-key " is required")
                                :actual subj})))
            ctx requireds)))

(defn additionalProperties [_ allowed? schema subj ctx]
  (if allowed?
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

;; todo should be atom
(def validators
  {:modifiers #{:exclusiveMaximum
                :exclusiveMinimum}

   :type check-type

   :not check-not
   :oneOf check-one-of

   :properties check-properties

   :required check-required

   :patternProperties patternProperties

   :additionalProperties additionalProperties

   :items check-items

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
  (validate* schema subj {:errors [] :warnings []}))

(defn validate [schema subj]
  (let [res (check schema subj)]
    (if (empty? (:errors res))
      true
      false)))
