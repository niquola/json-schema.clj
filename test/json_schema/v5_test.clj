(ns json-schema.v5-test
  (:refer-clojure :exclude [compile])
  (:require
   [json-schema.core :as sut]
   [json-schema.utils :as u]
   [clojure.test :refer :all]))

(def re-filter #"^.*$")

(deftest draft5-test

  "property name is the format for the property value"
  (let [validator (sut/compile {:additionalProperties {:format {:$data "0#"}}})
        res (validator {:date-time "1963-06-19T08:30:06.283185Z",
                        :email "joe.bloggs@example.com"})]
    (is (= true (empty? (:errors res))))
    res)

  (u/test-files (u/files "v5" re-filter)))

