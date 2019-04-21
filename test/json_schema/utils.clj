(ns json-schema.utils
  (:refer-clojure :exclude [compile])
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure.pprint :as pprint]
            [org.httpkit.server :as srv]
            [json-schema.core :refer :all]))


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
       (filter #(.isFile ^java.io.File %))
       (map #(.getPath ^java.io.File %))
       (filter (filter-by-name re-filter))))

(defn files-list [fs]
  (map (fn [x] (-> x
                   io/resource
                   io/file
                   (.getPath))) fs))

(defn read-json [path] (json/parse-string (slurp path) keyword))

(defn pp [x] (with-out-str (pprint/pprint x)))

(defn test-files [files & [skip-list]]
  (with-server (fn []
                 (let [skip-list (or  skip-list #{})]
                   (doseq [test-file files]
                     (let [test-case (read-json test-file)]
                       (doseq [{:keys [schema tests description] :as scenario} test-case]
                         (when-not (contains? skip-list description)
                           (testing (pr-str description)
                             (println "Test:" description)
                             (println "Schema: " schema)
                             (let [validator (compile schema)]
                               (doseq [{:keys [data valid] :as test-item} tests]
                                 (let [result (validator  data)]
                                   (println test-item)
                                   (is (= valid (empty? (:errors result)))
                                       (str
                                        "(let [validator (compile " (pp schema) ")
  res (validator " (pp data) " )
]
  (is (= "valid" (empty? (:errors " (pp result) "))))
res
)"))))))))))))))

