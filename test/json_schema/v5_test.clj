(ns json-schema.v5-test
  (:require
   [json-schema.utils :as u]
   [clojure.test :refer :all]))

(def re-filter #"^.*$")

(deftest draft5-test
  (u/test-files (u/files "v5" re-filter)))

