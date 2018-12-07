(ns json-schema.v2-test
  (:require [json-schema.v2 :as sut]
            [clojure.test :refer :all]))

(deftest basic-test

  (testing "type: object"
    (is (= (:errors (sut/validate {:type "object"} 1))
           [{:path [], :by [:type], :message "expected type [object], got [number]", :vlaue 1}]))

    (is (= (:errors (sut/validate {:type "object"} {}))
           [])))
  
  (testing "type: string"
    (is (= (:errors (sut/validate {:type "string"} 1))
           [{:path [], :by [:type], :message "expected type [string], got [number]", :vlaue 1}]))

    (is (= (:errors (sut/validate {:type "string"} "str"))
           []))
    )

  (testing "type: array of strings"
    (is (= (:errors (sut/validate {:type ["string" "object"]} 1))
           [{:path [], :by [:type], :message "expected type [string] or [object], got [number]", :vlaue 1}]))

    
    )





  )

