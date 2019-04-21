(ns json-schema.core-test
  (:require
   [json-schema.utils :as u]
   [clojure.test :refer :all]
   [json-schema.core :refer :all]
   [clojure.java.io :as io]))

(def re-filter #"^.*$")

(deftest custom-tests
  (u/test-files (u/files "custom-scenarios" re-filter)))

(deftest test-refs

  (is (= (mk-ref-with-ids
          "name.json#/definitions/orNull"
          ["http://localhost:1234/object"])
         "http://localhost:1234/name.json#/definitions/orNull"))
  
  (is (= (mk-ref-with-ids
          "folderInteger.json"
          ["http://localhost:1234/"
           "folder/"])
         "http://localhost:1234/folder/folderInteger.json"))

  (is (= (mk-ref-with-ids
          "folderInteger.json"
          ["https://google.com/schema"
           "http://localhost:1234/"
           "folder/"])
         "http://localhost:1234/folder/folderInteger.json"))



  )

(deftest self-test
  (let [core (u/read-json (.getPath (io/resource "core-schema.json")))]
    (testing (is (validate core core)))
    (validate core core))
  )
