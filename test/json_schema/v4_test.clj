(ns json-schema.v4-test
  (:require
   [json-schema.utils :as u]
   [clojure.test :refer :all]))

(def re-filter #"^.*$")

(deftest draft4-test
  (u/test-files (u/files "draft4" re-filter)))

