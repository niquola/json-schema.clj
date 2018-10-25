(ns json-schema.v3-test
  (:refer-clojure :exclude [compile])
  (:require
   [json-schema.utils :as u]
   [json-schema.core :refer :all]
   [clojure.test :refer :all]))

(def re-filter #"^.*$")

(deftest focus-3-test

  (let [validator (compile {:properties {:bar {:type "integer", :required true}},
                            :extends {:properties {:foo {:type "string", :required true}}}})
        res (validator {:bar 2})]
    (is (= false (empty? (:errors res))))
    res)

  (let [validator (compile {:properties {:bar {:type "integer", :required true}},
                            :extends
                            [{:properties {:foo {:type "string", :required true}}}
                             {:properties {:baz {:type "string", :required true}}}]})
        res (validator {:foo "quux", :bar 2, :baz "foo"})]
    (is (= true (empty? (:errors res))))
    res)

  ;; validation of CSS colors
  (let [validator (compile {:format "color"})
        res (validator "#CC8899")]
    (is (= true (empty? (:errors res))))
    res)

  ;; validation of CSS colors
  (let [validator (compile {:format "color"})
        res (validator "#00332520")]
    (is (= false (empty? (:errors res))))
    res)

  ;;dependencies
  (let [validator (compile {:dependencies {:bar "foo"}})
        res (validator {:bar 2})]
    (is (= false (empty? (:errors res))))
    res)


  ;; ECMA 262 regex dialect recognition
  (let [validator (compile {:format "regex"})
        res (validator "(?<=foobar")]
    (is (= false (empty? (:errors res))))
    res)

  "by small number"
  (let [validator (compile {:divisibleBy 2})
        res (validator 4)]
    (is (= true (empty? (:errors res))))
    res)

  (let [validator (compile {:divisibleBy 3})
        res (validator 4)]
    (is (= false (empty? (:errors res))))
    res)

  "types can include schemas"
  (let [validator (compile {:type ["array" {:type "object"}]})
        res (validator {})]
    (is (= true (empty? (:errors res))))
    res)

  )

(deftest draft3-test
  (u/test-files (u/files "draft3" re-filter) #{"multiple extends" "ECMA 262 regex dialect recognition"})
  )
