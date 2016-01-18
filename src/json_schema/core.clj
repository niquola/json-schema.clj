(ns json-schema.core)

(defn add-error [ctx err]
  (update-in ctx [:errors] conj err))

(defn minMaxItems [key rule schema subj ctx]
  (if-not (vector? subj)
    ctx
    (let [op (cond (= key :minItems) >=
                   (= key :maxItems) <=)
          cnt (count subj)]
      (if (op cnt rule)
        ctx
        (add-error ctx {:desc     :maxItems
                        :actual   cnt
                        :expected (str key " then " rule)})))))


(defn minMaxLength [key rule schema subj ctx]
  (println "length" key rule schema subj ctx)
  (if-not (string? subj)
    ctx
    (let [op   (cond (= key :minLength) <=
                     (= key :maxLength) >=)
          cnt  (.count (.codePoints subj))]
      (if (op rule cnt)
        ctx
        (add-error ctx {:desc     "maxLength"
                        :actual   cnt
                        :expected (str key " then " rule)})))))

(defn minimum [key rule schema subj ctx]
  (if-not (number? subj)
    ctx
    (let [op (if (:exclusiveMinimum schema) > >=)]
      (if (op subj rule)
        ctx
        (add-error ctx {:desc "maxLength"
                        :actual subj
                        :expected (str "more then " rule)})))))


(defn skip [key rule schema subj ctx] ctx)

(def validators
  {:maxItems minMaxItems
   :minItems minMaxItems

   :maxLength minMaxLength
   :minLength minMaxLength

   :minimum minimum
   :exclusiveMinimum skip
   :exclusiveMaximum skip})

(defn validate* [schema subj ctx]
  (if (map? schema)
    (reduce
     (fn [ctx [key rule]]
       (if-let [h (get validators key)]
         (h key rule schema subj ctx)
         (add-error ctx {:desc "Unknown key " :details key})))
     ctx
     schema)))

(defn validate [schema subj]
  (let [res (validate* schema subj {:errors []})]
    (if (empty? (:errors res))
      true
      false
      #_(:errors res))))

