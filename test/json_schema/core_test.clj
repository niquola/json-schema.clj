(ns json-schema.core-test
  (:require
   [json-schema.utils :as u]
   [clojure.test :refer :all]
   [json-schema.core :refer :all]
   [clojure.java.io :as io]))

(def re-filter #"^.*$")

(deftest custom-tests
  (u/test-files (u/files "custom-scenarios" re-filter)))

(deftest self-test
  (let [core (u/read-json (.getPath (io/resource "core-schema.json")))]
    (testing (is (validate core core)))))
