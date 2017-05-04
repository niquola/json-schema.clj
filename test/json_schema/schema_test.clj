(ns json-schema.schema-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure.pprint :as pprint]
            [org.httpkit.server :as srv]
            [json-schema.schema :refer :all]))


(comment "https://github.com/json-schema/json-schema/wiki/v5-Proposals")

(defn remote-server [req]
  (let [path (str "JSON-Schema-Test-Suite/remotes" (:uri req))]
    (if (.exists (io/file path))
      {:body (slurp path)}
      {:body (str "not found " path)})))

(defn with-server [f]
  ;; port number is important it's used in tests
  (let [srv (srv/run-server #'remote-server {:port 1234})]
    (try
      (f)
      (finally (srv)))))

(use-fixtures :once with-server)

(def re-filter #"^.*$")
(defn filter-by-name [nm] (fn [fl] (re-matches nm fl)))

(defn files [dir re-filter]
  (->> dir
       io/resource
       io/file
       file-seq
       (filter #(.isFile %))
       (map #(.getPath %))
       (filter (filter-by-name re-filter))))

(defn files-list [fs]
  (map (fn [x] (-> x
                   io/resource
                   io/file
                   (.getPath))) fs))


(defn read-json [path] (json/parse-string (slurp path) keyword))

(defn pp [x] (with-out-str (pprint/pprint x)))

(defn test-files [files]
  (doseq [test-file files]
    (let [test-case (read-json test-file)]
      (doseq [{:keys [schema tests description] :as scenario} test-case]
        (testing description
          (println "Test:" description)
          (println "Schema: " schema)
          (let [validator (compile schema)]

            (doseq [{:keys [data valid] :as test-item} tests]
              (let [result (validator  data)]
                (println test-item)
                (is (= valid (empty? (:errors result)))
                    (pp {:result result :schema schema :case test-item}))))))))))

(deftest a-schema-test
  (test-files (files "draft4" re-filter)))

(deftest custom-tests
  #_(test-files (files "custom-scenarios" re-filter)))

(def v5-files (files "v5" re-filter))

(deftest v5-schema-test
  (doseq [test-file v5-files]
    (let [test-case (read-json test-file)]
      (doseq [{:keys [schema tests description] :as scenario} test-case]
        (testing description
          (doseq [{:keys [data valid] :as test-item} tests]
            (let [result (validate  schema data)]
              (when-not (= valid (empty? (:errors result)))
                (println " res: " result)
                (println " item: " test-item)
                (println " sch: " schema))
              (is (= valid (empty? (:errors result)))
                  (pp {:result result :schema schema :case test-item})))))))))

(deftest self-test
  (let [core (read-json (.getPath (io/resource "core-schema.json")))]
    (testing (is (validate core core)))))
