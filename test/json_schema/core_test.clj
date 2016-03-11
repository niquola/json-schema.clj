(ns json-schema.core-test
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

(defonce test-server (atom nil))

(defn stop-server []
  (when-let [srv @test-server] (srv)
            (reset! test-server nil)))

(defn start-server []
  (stop-server)
  (reset! test-server
         (srv/run-server #'remote-server {:port 1234})))


(start-server)

(comment (stop-server))

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

(def test-files (files "draft4" re-filter))

(defn read-json [path] (json/parse-string (slurp path) keyword))

(defn pp [x] (with-out-str (pprint/pprint x)))

(deftest a-schema-test
  (doseq [test-file test-files]
    (let [test-case (read-json test-file)]
      (doseq [{:keys [schema tests description] :as scenario} test-case]
        (testing description
          (doseq [{:keys [data valid] :as test-item} tests]
            (let [result (validate  schema data)]
              (is (= valid (empty? (:errors result)))
                  (pp {:result result :schema schema :case test-item})))))))))

(def v5-files (files-list
               ["v5/constant.json"
                "v5/$data/constant.json"
                "v5/$data/enum.json"
                "v5/$data/format.json"
                "v5/$data/maxItems.json"
                "v5/$data/maxLength.json"
                "v5/$data/maximum.json"
                "v5/$data/minimum.json"
                "v5/contains.json"]))

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
    (testing
        (is (valid? core core)))))

