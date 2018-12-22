(ns json-schema.macroschema-test
  (:require [json-schema.macroschema :as sut]
            [json-schema.utils :as u]
            [clojure.test :refer :all]))

(deftest test-macroschema


  (def rsch {:required ["a" "b" "c"]})

  (is (= [{:desc "a is required" :path []}
          {:desc "b is required" :path []}
          {:desc "c is required" :path []}]
         (:errors (sut/test rsch {}))))

  (is (= [{:desc "a is required" :path []}
          {:desc "b is required" :path []}]
         (:errors (sut/test rsch {:c false}))))

  (is (empty? (:errors (sut/test rsch 1 ))))

  (def tsch {:type "object"})

  (is (= [{:desc "Expected an object, got 1" :path []}]
         (:errors (sut/test tsch 1))))

  (is (= [{:desc "Expected an array, got 1" :path []}]
         (:errors (sut/test {:type "array"} 1))))

  (is (= [{:desc "Expected a string, got 1" :path []}]
         (:errors (sut/test {:type "string"} 1))))

  (def tsch {:type ["object" "string"]})
  (sut/test tsch 1)


  (sut/test {:properties {:a {:type "string"}}} {:a 1})


  (sut/test {:properties {:a {:type "string"}}} {:a 1})

  (def prop-sch
    {:type "object"
     :required ["b"]
     :properties {:a {:type "string"}
                  :c {:type "object"
                      :properties {:e {:type "string"}}}}})

  (is (= [{:desc "b is required", :path []}
          {:desc "Expected a string, got 1", :path [:a]}
          {:desc "Expected a string, got true", :path [:c :e]}]
         (:errors (sut/test prop-sch {:a 1 :c {:e true}}))))


  (is (= [{:desc "Expected a string, got 1", :path [:a]}
          {:desc "Expected a string, got 1", :path [:b]}]
         (:errors
          (sut/test
           {:properties {:a {:type "string"}
                         :b {:$ref "#/properties/a"}}}
           {:a 1 :b 1}))
         ))

  (is (= [{:desc "Expected a string, got 1", :path [0]}
          {:desc "Expected a string, got 2", :path [1]}
          {:desc "Expected a string, got 3", :path [2]}]
         (:errors (sut/test {:items {:type "string"}} [1 2 3]))))

  (is (empty? (:errors (sut/test {:items {:type "string"}} "a"))))
  
  )



(def re-filter #"^.*$")

(deftest focus-3-test

  ;; (defn compile [sch] (sut/compile-schema sch))

  ;; (let [validator (compile {:properties {:bar {:type "integer", :required true}},
  ;;                           :extends {:properties {:foo {:type "string", :required true}}}})
  ;;       res (validator {:bar 2})]
  ;;   (is (= false (empty? (:errors res))))
  ;;   res)

  ;; (let [validator (compile {:properties {:bar {:type "integer", :required true}},
  ;;                           :extends
  ;;                           [{:properties {:foo {:type "string", :required true}}}
  ;;                            {:properties {:baz {:type "string", :required true}}}]})
  ;;       res (validator {:foo "quux", :bar 2, :baz "foo"})]
  ;;   (is (= true (empty? (:errors res))))
  ;;   res)

  ;; ;; validation of CSS colors
  ;; (let [validator (compile {:format "color"})
  ;;       res (validator "#CC8899")]
  ;;   (is (= true (empty? (:errors res))))
  ;;   res)

  ;; ;; validation of CSS colors
  ;; (let [validator (compile {:format "color"})
  ;;       res (validator "#00332520")]
  ;;   (is (= false (empty? (:errors res))))
  ;;   res)

  ;; ;;dependencies
  ;; (let [validator (compile {:dependencies {:bar "foo"}})
  ;;       res (validator {:bar 2})]
  ;;   (is (= false (empty? (:errors res))))
  ;;   res)


  ;; ;; ECMA 262 regex dialect recognition
  ;; (let [validator (compile {:format "regex"})
  ;;       res (validator "(?<=foobar")]
  ;;   (is (= false (empty? (:errors res))))
  ;;   res)

  ;; "by small number"
  ;; (let [validator (compile {:divisibleBy 2})
  ;;       res (validator 4)]
  ;;   (is (= true (empty? (:errors res))))
  ;;   res)

  ;; (let [validator (compile {:divisibleBy 3})
  ;;       res (validator 4)]
  ;;   (is (= false (empty? (:errors res))))
  ;;   res)

  ;; "types can include schemas"
  ;; (let [validator (compile {:type ["array" {:type "object"}]})
  ;;       res (validator {})]
  ;;   (is (= true (empty? (:errors res))))
  ;;   res)

  ;; "remote ref"
  ;; (u/with-server
  ;;   (fn []
  ;;     (let [validator (compile {:$ref "http://localhost:1234/integer.json"})
  ;;           res (validator "a")]
  ;;       (is (= false (empty? (:errors res))))
  ;;       res)))

  ;; (u/with-server
  ;;   (let [validator (compile {:id "http://localhost:1234/",
  ;;                             :items {:id "folder/", :items {:$ref "folderInteger.json"}}})
  ;;         res (validator [[1]])]
  ;;     (is (= true (empty? (:errors res))))
  ;;     res))

  )

(deftest draft3-test
  ;; (u/test-files (u/files "draft3" re-filter) #{"multiple extends" "ECMA 262 regex dialect recognition"})
  )

