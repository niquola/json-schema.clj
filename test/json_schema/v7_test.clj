(ns json-schema.v7-test
  (:require
   [json-schema.utils :as u]
   [json-schema.core :refer :all]
   [clojure.test :refer :all]))

(def re-filter #"^.*$")


(deftest draft7-test
  (u/test-files (u/files "draft7" re-filter))
  )

