(ns json-schema.v6-test
  (:require
   [json-schema.utils :as u]
   [clojure.test :refer :all]))

(def re-filter #"^.*$")

(deftest draft6-test
  (u/test-files (u/files "draft6" re-filter)))

