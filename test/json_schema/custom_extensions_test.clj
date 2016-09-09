(ns json-schema.custom-extensions-test
  (:require  [clojure.test :refer :all]
             [json-schema.core :refer :all]))


(defn- first-error [res]
  (:path (first (:errors res))))

(def schema-1
  {:type "object"
   :typeProperty :type
   :definitions {:User {:properties {:type {:constant "User"}
                                     :name {:constant "nicola"}}}
                 :Role {:properties {:type {:constant "Role"}
                                     :name {:constant "admin"}}}}})

(deftest test-errors
  (testing "path in error"
    (is (= {:errors [{:desc "Could not resolve #/definitions/Ups", :path []}],
            :warnings []}
           (validate schema-1 {:type "Ups"})))

    (is (= {:errors [],:warnings []}
           (validate schema-1 {:type "User" :name "nicola"})))

    (is (= {:errors
            [{:expected "ivan equals nicola",
              :actual "not equal",
              :details "ivan",
              :path [:name]}],
            :warnings []}
           (validate schema-1 {:type "User" :name "ivan"})))

    (is (= {:errors
            [{:expected "nicola equals admin",
              :actual "not equal",
              :details "nicola",
              :path [:name]}],
            :warnings []}
           (validate schema-1 {:type "Role" :name "nicola"})))

    ))
