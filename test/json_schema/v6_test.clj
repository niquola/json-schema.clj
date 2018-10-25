(ns json-schema.v6-test
  (:refer-clojure :exclude [compile])
  (:require
   [json-schema.core :refer :all]
   [json-schema.utils :as u]
   [clojure.test :refer :all]
   [clojure.string :as str]))

(def re-filter #"^.*$")

(deftest custom-6-test
  ;; const validation
  (let [validator (compile {:const 2})
        res (validator "a")]
    (is (= false (empty? (:errors res))))
    res)

  ;; exclusiveMinimum validation
  (let [validator (compile {:exclusiveMinimum 1.1})
        res (validator 1.1)]
    (is (= false (empty? (:errors res))))
    res)

  ;; exclusiveMinimum validation
  (let [validator (compile {:exclusiveMinimum 1.1})
        res (validator 0.6)]
    (is (= false (empty? (:errors res))))
    res)

  ;; propertyNames validation
  (let [validator (compile {:propertyNames {:maxLength 3}})
        res (validator {:foo {}, :foobar {}})]
    (is (= false (empty? (:errors res))))
    res)

  ;; validation of JSON-pointers (JSON String Representation)
  (let [validator (compile {:format "json-pointer"})
        res (validator "a")]
    (is (= false (empty? (:errors res))))
    res)

  (let [validator (compile {:format "json-pointer"})
        res (validator "/~0~")]
    (is (= false (empty? (:errors res))))
    res)

  ;; validation of JSON-pointers (JSON String Representation)
  (let [validator (compile {:format "json-pointer"})
        res (validator "/foo/bar~0/baz~1/%a")]
    (is (= true (empty? (:errors res))))
    res)

  ;; validation of URIs
  (let [validator (compile {:format "uri"})
        res (validator "http://foo.bar/?baz=qux#quux")]
    (is (= true (empty? (:errors res))))
    res)

  ;; validation of URIs
  (let [validator (compile {:format "uri"})
        res (validator "//foo.bar/?baz=qux#quux")]
    (is (= false (empty? (:errors res))))
    res)


  ;; if and then without else
  (let [validator (compile {:if {:exclusiveMaximum 0}, :then {:minimum -10}})
        res (validator -100)]
    (is (= false (empty? (:errors res))))
    res)

  )

(deftest draft6-test
  (u/test-files (u/files "draft6" re-filter)
                #{"an array of schemas for items"
                  "ECMA 262 regex non-compliance"
                  "some languages do not distinguish between different types of numeric value"
                  "validation of URIs"}
                ))

