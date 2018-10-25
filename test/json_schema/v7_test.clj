(ns json-schema.v7-test
  (:require
   [json-schema.utils :as u]
   [json-schema.core :refer :all]
   [clojure.test :refer :all]))

(def re-filter #"^.*$")


(deftest test-some-7

  ;; dependencies with boolean subschemas
  (let [validator (compile {:dependencies {:foo true, :bar false}})
        res (validator {:bar 2})]
     (is (= false (empty? (:errors res))))
    res)

  ;; if and else without then
  (let [validator (compile {:if {:exclusiveMaximum 0}, :else {:multipleOf 2}})
        res (validator -1)]
    (is (= true (empty? (:errors res))))
    res)


  ;; $ref to boolean schema true
  (let [validator (compile {:$ref "#/definitions/bool", :definitions {:bool true}})
        res (validator "foo")]
    (is (= true (empty? (:errors res))))
    res)

  (let [validator (compile {:$ref "#/definitions/bool", :definitions {:bool false}})
        res (validator "foo")]
    (is (= false (empty? (:errors res))))
    res)

  (let [validator (compile {:properties {:prop {:$ref "#/definitions/bool"}}, :definitions {:bool {:type "string"}}})
        res (validator {:prop 1})]
    (is (= false  (empty? (:errors res))))
    res)


  )

(deftest draft7-test
  (u/test-files (u/files "draft7" re-filter))
  )

