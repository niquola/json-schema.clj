(ns json-schema.custom-extensions-test
  (:require  [clojure.test :refer :all]
             [json-schema.core :refer :all]))


(defn- first-error [res]
  (:path (first (:errors res))))

(def schema-1
  {:type "object"
   :discriminator :type
   :definitions {:User {:properties {:type {:constant "User"}
                                     :name {:constant "nicola"}}}
                 :Role {:properties {:type {:constant "Role"}
                                     :name {:constant "admin"}}}}})

(deftest test-errors
  (testing "path in error"
    (is (= [{:message "Could not resolve #/definitions/Ups", :path []}]
           (:errors (validate schema-1 {:type "Ups"}))))

    (is (empty? (:errors (validate schema-1 {:type "User" :name "nicola"}))))

    (is (= [{:path [:name], :message "expeceted nicola, but ivan"}]
           (:errors (validate schema-1 {:type "User" :name "ivan"}))))

    (is (= [{:path [:name], :message "expeceted admin, but nicola"}]
           (:errors (validate schema-1 {:type "Role" :name "nicola"}))))


    ))


(def exclusive-schema
  {:type "object"
   :properties {:valueNumber {:type "number"}
                :valueString {:type "string"}
                :nested {:type "object"
                         :properties {:otherValueNumber {:type "number"}
                                      :otherValueString {:type "string"}}
                         :exclusiveProperties [{:properties [:otherValueString :otherValueNumber] :required true}]}}
   :required [:nested]
   :exclusiveProperties [{:properties [:valueNumber :valueString] :required true}]})

(deftest text-exclusive-props

  (is (= [{:path [:nested],
           :message "Properties otherValueString, otherValueNumber are mutually exclusive"}
          {:path [],
           :message "Properties valueNumber, valueString are mutually exclusive"}]
         (:errors (validate exclusive-schema {:valueNumber 1 :valueString "s"
                                      :nested {:otherValueString "s" :otherValueNumber 1}}))))

  (is (= [{:path [:nested]
           :message "One of properties :otherValueString, :otherValueNumber is required"}
          {:path []
           :message "One of properties :valueNumber, :valueString is required"}]
         (:errors (validate exclusive-schema {:name "Name" :nested {:key "val"}})))))


(def keyword-support
  {:type "object"
   :properties {:a {:type "string"}
                :b {:type "string" :enum ["a" :b]}}})

(deftest test-keyword-support

  (is (empty? (:errors (validate keyword-support {:a "string"}))))
  (is (empty? (:errors (validate keyword-support {:a :string}))))
  (is (empty? (:errors (validate keyword-support {:b :a}))))
  (is (empty? (:errors (validate keyword-support {:b "b"})))))

(def deferred-support
  {:type "object"
   :properties {:a {:type "string" :deferred {:message "Just do something"}}}})

(deftest test-deferred-support
  (is (= [{:path [:a] :value "ups" :deferred {:message "Just do something"}}]
         (:deferreds (validate deferred-support {:a "ups"})))))
