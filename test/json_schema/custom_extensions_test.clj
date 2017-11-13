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

(deftest test-warnings

  (is (= [{:path [:extra], :message "extra property"}]
         (:warnings
          (validate {:type :object
                     :properties {:name {:type "string"}
                                  :email {:type "string"}}
                     :additionalProperties false
                     :required [:email]}
                    {:name "name" :email "email@ups.com" :extra "prop"}
                    {:config {:additionalProperties :warnings}}))))

  (is (empty?
       (:errors (validate {:type :object
                           :properties {:name {:type "string"}
                                        :email {:type "string"}}
                           :additionalProperties false
                           :required [:email]}
                          {:name "name" :email "email@ups.com" :extra "prop"}
                          {:config {:additionalProperties :warnings}})
                )))

  (is (= [{:path [:extra], :message "extra property"}]
         (:errors
          (validate {:type :object
                     :properties {:name {:type "string"}
                                  :email {:type "string"}}
                     :additionalProperties false
                     :required [:email]}
                    {:name "name" :email "email@ups.com" :extra "prop"}))))

  (is (empty?
       (:warnings (validate {:type :object
                           :properties {:name {:type "string"}
                                        :email {:type "string"}}
                           :additionalProperties false
                           :required [:email]}
                          {:name "name" :email "email@ups.com" :extra "prop"})))))

(deftest test-issue-4
  (is (not (empty? (:errors (validate {:type :object
                                       :properties {:name {:type "string"}}
                                       :required [:name]}
                                      {:name nil})))))

  (is
   (empty? (:errors (validate {:type :object
                               :properties {:name {:type "string"}}
                               :required ["name"]}
                              {:name "ups"})))))

(deftest test-issue-date-time
  (is (empty? (:errors (validate {:type :object
                                  :properties {:name {:type "string" :format "date-time"}}
                                  :required [:name]}
                                 {:name "2017-07-19T12:26:35.004052"})))))


(deftest required-in-property
  (validate
   {:properties {:bar {:type "integer", :required true}}}
   {:foo "quux"}))




(deftest test-issue-4
  (is (not (empty? (:errors (validate {:type :object
                                       :properties {:name {:type "string"}}
                                       :required [:name]}
                                      {:name nil})))))

  (is
   (empty? (:errors (validate {:type :object
                               :properties {:name {:type "string"}}
                               :required ["name"]}
                              {:name "ups"})))))


(deftest type-null-exception-issue-1
  (is (= (validate
          {:properties {:bar {:type "foo"}}}
          {:bar "quux"})
         {:errors [{:path [:bar], :message "Broken schema: unknown type :foo"}], :deferreds [], :warnings []})))

(deftest issue-3
  (is (=
       (validate
        {:oneOf [{:type "object"
                  :properties {:a {:type :string :deferred {:type :a}}}
                  :required [:a]}
                 {:type "object"
                  :properties {:b {:type :string :deferred {:type :b}}}
                  :required [:b]}]}
        {:a "ups"})
       {:errors [], :deferreds [{:path [:a], :value "ups", :deferred {:type :a}}], :warnings []}))
  
  (is (=
       (validate
        {:oneOf [{:type "object"
                  :properties {:a {:type :string :deferred {:type :a}}}
                  :required [:a]}
                 {:type "object"
                  :properties {:b {:type :string :deferred {:type :b}}}
                  :required [:b]}]}
        {:b "ups"})
       {:errors [], :deferreds [{:path [:b], :value "ups", :deferred {:type :b}}], :warnings []}))

  (is (=
       (validate
        {:oneOf [{:type "object"
                  :properties {:a {:type :string :deferred {:type :a}}}
                  :required [:a]}
                 {:type "object"
                  :properties {:b {:type :string :deferred {:type :b}}}
                  :required [:b]}]}
        {:c "ups"})
       {:errors
        [{:path [],
          :message
          "expeceted one of [{:type \"object\", :properties {:a {:type :string, :deferred {:type :a}}}, :required [:a]} {:type \"object\", :properties {:b {:type :string, :deferred {:type :b}}}, :required [:b]}], but no one is valid"}],
        :deferreds [],
        :warnings []}))

  )

(deftest test-subset
  (is (=
       (validate
        {:type "object"
         :properties {:a {:type "array"}
                      :b {:subset {:$data "#/a"}}}}
        {:a [1 2 3]
         :b [3 1 2]})
       {:errors [], :deferreds [], :warnings []}))
  (is (=
       (validate
        {:type "object"
         :properties {:a {:type "array"}
                      :b {:subset {:$data "#/a"}}}}
        {:a [1 2 3]
         :b [2 1]})
       {:errors [], :deferreds [], :warnings []}))
  (is (=
       (validate
        {:type "object"
         :properties {:a {:type "array"}
                      :b {:subset {:$data "#/a"}}}}
        {:a [1 2 3]
         :b [1]})
       {:errors [], :deferreds [], :warnings []}))
  (is (=
       (validate
        {:type "object"
         :properties {:a {:type "array"}
                      :b {:subset {:$data "#/a"}}}}
        {:a [1 2 3]
         :b []})
       {:errors [], :deferreds [], :warnings []}))
  (is (=
       (validate
        {:type "object"
         :properties {:a {:type "array"}
                      :b {:subset {:$data "#/a"}}}}
        {:a [1 2 3]
         :b [1 2 3 4]})
       {:errors [{:path [:b], :message "[1 2 3 4] is not a subset of [1 2 3]"}], :deferreds [], :warnings []}))
  (is (=
       (validate
        {:type "object"
         :properties {:a {:type "array"}
                      :b {:subset {:$data "#/a"}}}}
        {:a [1 2 3]
         :b [5]})
       {:errors [{:path [:b], :message "[5] is not a subset of [1 2 3]"}], :deferreds [], :warnings []}))
  (is (=
       (validate
        {:type "object"
         :properties {:b {:subset [2 3]}}}
        {:b [1 2 3]})
       {:errors [{:path [:b], :message "[1 2 3] is not a subset of [2 3]"}], :deferreds [], :warnings []}))
  (is (=
       (validate
        {:type "object"
         :properties {:b {:subset [2 3 1]}}}
        {:b [1 2 3]})
       {:errors [], :deferreds [], :warnings []})))
