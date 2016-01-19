(ns json-schema.core-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure.pprint :as pprint]
            [json-schema.core :refer :all]))


(defn filter-by-name [nm] (fn [fl] (re-matches nm (.getPath fl))))


(def re-filter #"^.*(max|min|type|patternProperties|enum|items|not|one|any|all|uniq|depend|ref).*")
(def re-filter #"^.*$")

(def settings
  {:resolve-ref-fn (fn [url]
                     (get {"http://localhost:1234/integer.json" {:type "integer"}
                           "http://localhost:1234/subSchemas.json#/refToInteger" {:type "integer"}
                           "http://localhost:1234/subSchemas.json#/integer" {:type "integer"}}
                          url))})

(deftest a-schema-test
  (doseq [test-file  (->> "draft4"
                          io/resource
                          io/file
                          file-seq
                          (filter (filter-by-name re-filter)))]
    (when (.isFile test-file)
      (let [test-case (json/parse-string (slurp (.getPath test-file)) keyword)]
        (doseq [scenario test-case]
          (testing (:description scenario)
            (doseq [test-item (:tests scenario)]
              (is (= (:valid test-item)
                     (validate (:schema scenario) (:data test-item) settings))
                  (str
                   (with-out-str
                     (pprint/pprint
                      (check  (:schema scenario) (:data test-item) settings)))
                   "\n"
                   (with-out-str
                     (pprint/pprint
                      (assoc test-item :schema (:schema scenario)))))))))))))

(comment
  (doseq [test-file  (->> "draft4"
                          io/resource
                          io/file
                          file-seq
                          (filter (filter-by-name re-filter)))]
    (when (.isFile test-file)
      (let [test-case (json/parse-string (slurp (.getPath test-file)) keyword)]
        (doseq [scenario test-case]
          (doseq [test-item (:tests scenario)]
            (is (= (:valid test-item)
                   (validate (:schema scenario) (:data test-item)))
                (pr-str (assoc test-item :schema (:schema scenario)))))))))
  )

