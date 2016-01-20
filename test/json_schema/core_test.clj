(ns json-schema.core-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure.pprint :as pprint]
            [org.httpkit.server :as srv]
            [json-schema.core :refer :all]))

(defn filter-by-name [nm] (fn [fl] (re-matches nm (.getPath fl))))

(def re-filter #"^.*(max|min|type|patternProperties|enum|items|not|one|any|all|uniq|depend|ref).*")
(def re-filter #"^.*ref.*$")
(def re-filter #"^.*$")

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
                     (validate (:schema scenario) (:data test-item)))
                  (str
                   (with-out-str
                     (pprint/pprint
                      (check  (:schema scenario) (:data test-item))))
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

