(ns json-schema.errors-test
  (:require  [clojure.test :refer :all]
             [json-schema.core :refer :all]))

(def schema-1
  {:type "object"
   :properties {:p0 {:constant 1}
                :p1 {:type "object"
                     :properties {:p2 {:type "object"
                                       :properties {:p3 {:constant 1}}}}}}})

(def schema-2
  {:type "object"
   :properties {:p0 {:constant 1} 
                :p1 {:type "array"
                     :items {:type "object"
                             :properties {:p2 {:type "array"
                                               :items {:constant 1}}}}}}})

(def schema-3
  {:properties {:p0 {}}
   :patternProperties {"a.*" {:properties {}
                              :patternProperties {"b.*" {:constant 1}}
                              :additionalProperties false}}
   :additionalProperties false})

(def schema-4
  {:properties {:p0 {}}
   :additionalProperties {:properties {}
                          :additionalProperties {:constant 1}}})

(def schema-5
  {:properties {:p0 {}
                :p1 {:contains {:constant 1}}}})


(defn- first-error [res]
  (:path (first (:errors res))))

(deftest test-errors
  (testing "path in error"

    (is (= [:p1 :p2 :p3]
           (first-error
            (validate schema-1 {:p0 1 :p1 {:p2 {:p3 2}}}))))

    (is (= [:p1 0 :p2 1]
           (first-error
            (validate schema-2 {:p1 [{:p2 [1 2]}]}))))

    (is (= [:a1 :b1]
           (first-error
            (validate schema-3 {:a1 {:b1 2}}))))

    (is (= [:a1 :b1]
           (first-error
            (validate schema-3 {:p0 1 :a1 {:b1 2}}))))

    (is (= [:a1 :b1]
           (first-error
            (validate schema-4 {:p0 1 :a1 {:b1 2}}))))

    (is (= [:p1]
           (first-error
            (validate schema-5 {:p0 1 :p1 [2 3]}))))))

