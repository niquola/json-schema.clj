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

(def schema-2
  {:type "object"
   :discriminator :type
   :definitions {:User {:properties {:type {:constant "User"}
                                     :name {:constant "nicola"}}}
                 :Role {:properties {:type {:constant "Role"}
                                     :name {:constant "admin"}}}}})


(deftest test-errors
  (testing "path in error"
    (is (= {:errors [{:desc "Could not resolve #/definitions/Ups", :path []}],
            :warnings []}
           (validate schema-1 {:type "Ups"})))

    (is (= nil
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

    (testing "with descrimintator: path in error"
      (is (= {:errors [{:desc "Could not resolve #/definitions/Ups", :path []}],
              :warnings []}
             (validate schema-2 {:type "Ups"})))

      (is (= nil
             (validate schema-2 {:type "User" :name "nicola"})))

      (is (= {:errors
              [{:expected "ivan equals nicola",
                :actual "not equal",
                :details "ivan",
                :path [:name]}],
              :warnings []}
             (validate schema-2 {:type "User" :name "ivan"})))

      (is (= {:errors
              [{:expected "nicola equals admin",
                :actual "not equal",
                :details "nicola",
                :path [:name]}],
              :warnings []}
             (validate schema-2 {:type "Role" :name "nicola"}))))

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

  (is (= {:errors
          [{:expected
            "Properties otherValueString,otherValueNumber are mutually exclusive",
            :path [:nested]}
           {:expected
            "Properties valueNumber,valueString are mutually exclusive",
            :path []}],
          :warnings []}
         (validate exclusive-schema {:valueNumber 1 :valueString "s"
                                     :nested {:otherValueString "s" :otherValueNumber 1}})))

  (is (= {:errors
          [{:expected
            "One of properties otherValueString,otherValueNumber is required",
            :path [:nested]}
           {:expected
            "One of properties valueNumber,valueString is required",
            :path []}],
          :warnings []}
         (validate exclusive-schema {:name "Name" :nested {:key "val"}})))

  )

(def one-of-required-schema
  {:type "object"
   :additionalProperties false
   :properties {:a {:type "string"}
                :_a {:type "string"}
                :b {:type "string"}
                :c {:type "string"}
                :_c {:type "string"}
                }
   :oneOfRequired [[:a :_a] :b [:c :_c]]})

(deftest text-one-of-required

  (is (nil? (validate one-of-required-schema {:a "a" :b "b" :c "c"})))
  (is (nil? (validate one-of-required-schema {:_a "a" :b "b" :c "c"})))
  (is (nil? (validate one-of-required-schema {:_a "a" :b "b" :_c "c"})))
  (is (nil? (validate one-of-required-schema {:a "a" :b "b" :_c "c"})))

  (is (= {:errors
          [{:expected "one of properties a or _a is required",
            :actual {:b "b", :c "c"},
            :path []}],
          :warnings []}
         (validate one-of-required-schema {:b "b" :c "c"})))

  )

(def keyword-support
  {:type "object"
   :properties {:a {:type "string"}}})

(deftest test-keyword-support

  (is (nil? (validate keyword-support {:a "string"})))
  (is (nil? (validate keyword-support {:a :string})))

  )
