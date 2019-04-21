(ns json-schema.v7-test
  (:require
   [json-schema.utils :as u]
   [json-schema.core :refer :all]
   [clojure.test :refer :all]
   [clojure.string :as str]))

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


  "an array of schemas for items"
  ;; not sure
  #_(let [validator (compile {:items [{:type "integer"} {:type "string"}]})
        res (validator {0 "invalid", 1 "valid", :length 2})]
    (is (= true (empty? (:errors res))))
    res)


  "validation of URI References"
  (let [validator (compile {:format "uri-reference"})
        res (validator "\\\\WINDOWS\\fileshare")]
    (is (= false (empty? (:errors res))))
    res)

  

  "format: uri-template"
  (let [validator (compile {:format "uri-template"})
        res (validator "http://example.com/dictionary/{term:1}/{term")]
    (is (= false (empty? (:errors res))))
    res)

  )


(deftest draft7-test
  (u/test-files (u/files "draft7" re-filter)
                #{"an array of schemas for items"
                  "some languages do not distinguish between different types of numeric value"
                  "ECMA 262 regex non-compliance"
                  "allOf with base schema"
                  "ref overrides any sibling keywords"

                  ;; "validation of URI References"
                  ;; "format: uri-template"
                  ;; "validation of IRIs"
                  ;; "validation of IRI References"
                  ;; "validation of host names"
                  "validation of Relative JSON Pointers (RJP)"
                  "validation of internationalized host names"
                  "validation of binary-encoded media type documents"
                  "validation of binary string-encoding"
                  "validation of string-encoded content based on media type"
                  })
  )

