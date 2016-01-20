(ns json-schema.core-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure.pprint :as pprint]
            [org.httpkit.server :as srv]
            [json-schema.core :refer :all]))


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

(def test-files (->> "draft4"
                     io/resource
                     io/file
                     file-seq
                     (filter #(.isFile %))
                     (map #(.getPath %))
                     (filter (filter-by-name re-filter))))

(defn read-json [path] (json/parse-string (slurp path) keyword))

(defn pp [x]
  (with-out-str
    (pprint/pprint x)))

(deftest a-schema-test
  (doseq [test-file test-files]
    (let [test-case (read-json test-file)]
      (doseq [{:keys [schema tests description] :as scenario} test-case]
        (testing description
          (doseq [{:keys [data valid] :as test-item} tests]
            (let [result (check  schema data)]
              (is (= valid (empty? (:errors result)))
                  (pp {:result result :schema schema :case test-item})))))))))

(comment
  (doseq [test-file test-files]
    (let [test-case (read-json test-file)]
      (doseq [{:keys [schema tests description] :as scenario} test-case]
        (doseq [{:keys [data valid] :as test-item} tests]
          (let [result (check  schema data)]
            (is (= valid (empty? (:errors result)))
                (pp {:result result :schema schema :case test-item})))))))
  )
