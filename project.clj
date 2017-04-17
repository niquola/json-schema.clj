(defproject json-schema "0.1.5"
  :description "Conformant json-schema implementation"
  :url "https://github.com/niquola/json-schema.clj "
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :resource-paths ["resources" "JSON-Schema-Test-Suite/tests"]
  :repositories   [["clojars"  {:url "https://clojars.org/repo" :sign-releases false}]]
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [cheshire "5.6.3"]]
  :profiles {:dev {:dependencies [[http-kit "2.2.0"]]}})
