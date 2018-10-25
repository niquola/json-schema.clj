(ns json-schema.v5-test
  (:refer-clojure :exclude [compile])
  (:require
   [json-schema.utils :as u]
   [clojure.test :refer :all]))

(def re-filter #"^.*$")

(deftest draft5-test
  (u/test-files (u/files "v5" re-filter)))

