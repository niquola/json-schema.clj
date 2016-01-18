(ns json-schema.core)

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

(def validators
  {:modifiers #{:exclusiveMaximum :exclusiveMinimum} 

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
                                :operator >=})})

(defn validate* [schema subj ctx]
  (if (map? schema)
    (reduce
     (fn [ctx [key rule]]
       (if (contains? (:modifiers validators) key)
         ctx
         (if-let [h (get validators key)]
           (h key rule schema subj ctx)
           (add-error ctx {:desc "Unknown key " :details key}))))
     ctx
     schema)))

(defn validate [schema subj]
  (let [res (validate* schema subj {:errors []})]
    (if (empty? (:errors res))
      true
      false)))

