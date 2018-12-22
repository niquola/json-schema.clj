(ns json-schema.macroschema-test
  (:require [json-schema.macroschema :as sut]
            [clojure.test :refer :all]))

(deftest test-macroschema


  (def rsch {:required ["a" "b" "c"]})

  (is (= [{:desc "c is required" :path []}
          {:desc "b is required" :path []}
          {:desc "a is required" :path []}]
         (:errors (sut/test rsch {}))
         ))

  (is (= [{:desc "b is required" :path []}
          {:desc "a is required" :path []}]
         (:errors (sut/test rsch {:c false}))))

  (is (nil? (:errors (sut/test rsch 1 ))))

  (def tsch {:type "object"})

  (is (= [{:desc "Expected an object, got 1" :path []}]
        (:errors (sut/test tsch 1))))

  (is (= [{:desc "Expected an array, got 1" :path []}]
         (:errors (sut/test {:type "array"} 1))))

  (is (= [{:desc "Expected a string, got 1" :path []}]
         (:errors (sut/test {:type "string"} 1))))



  (sut/test {:properties {:a {:type "string"}}} {:a 1})


  (sut/test {:properties {:a {:type "string"}}} {:a 1})

  (def prop-sch
    {:type "object"
     :required ["b"]
     :properties {:a {:type "string"}
                  :c {:type "object"
                      :properties {:e {:type "string"}}}}})

  (is (= [{:desc "Expected a string, got true", :path [:c :e]}
          {:desc "Expected a string, got 1", :path [:a]}
          {:desc "b is required", :path []}]
         (:errors (sut/test prop-sch {:a 1 :c {:e true}}))))


  (is (= [{:desc "Expected a string, got 1", :path [:b]}
          {:desc "Expected a string, got 1", :path [:a]}]
         (:errors
          (sut/test
           {:properties {:a {:type "string"}
                         :b {:$ref "#/properties/a"}}}
           {:a 1 :b 1}))))


  )

