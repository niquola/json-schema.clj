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

  )

(deftest draft7-test
  (u/test-files (u/files "draft7" re-filter))
  )

