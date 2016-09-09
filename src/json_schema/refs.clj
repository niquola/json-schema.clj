(ns json-schema.refs
  (:require [cheshire.core :as json]
            [clojure.string :as str]))

;; json reference - http://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03#page-3
;; json pointer -  http://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-07

(defn to-uri [x] (java.net.URI. x))

(defn resolve-uri [base-uri uri]
  (try
    (cond (nil? base-uri) uri
          (nil? uri) base-uri
          (and base-uri uri) (str (.resolve (to-uri base-uri) uri)))
    (catch Exception e
      (println "Ups" base-uri uri)
      nil)))


(defn decode-json-pointer [x]
  (-> x (str/replace #"~0" "~")
        (str/replace #"~1" "/")
        (str/replace #"%25" "%")))

(defn json-pointer [obj pointer]
  (let [path (mapv (fn [x] (if (re-matches #"\d+" x) (read-string x) (keyword (decode-json-pointer x))))
                   (rest (str/split pointer #"/")))]
    (get-in obj path)))

(defn defragment-uri [ref]
  (let [uri (to-uri ref)]
    (str/replace
     (str
      (java.net.URI. (.getScheme uri)
                     (.getUserInfo uri)
                     (.getHost uri)
                     (.getPort uri)
                     (.getPath uri)
                     (.getQuery uri)
                     ""))
     #"#$" "")))

(defn fragment [ref]
  (or (.getFragment (to-uri ref)) ""))

(defragment-uri "http://x.y.z/rootschema.json#foo")

(json-pointer {:a {:b 1}} (fragment (resolve-uri (defragment-uri "http://x.y.z/rootschema.json#foo")
                        "/ups#/a/b")))

(defn from-cache [ctx url]
  (when-not (get-in ctx [:docs ""])
    (throw (Exception. (str "LOAD [" url "] Empty man doc: " (pr-str ctx)))))
  (if-let [res (get-in ctx [:docs url])]
    [res ctx]
    (let [res (when-let [res (try (slurp url) (catch Exception e))]
                (json/parse-string res keyword))]
      [res (assoc-in ctx [:docs url] res)])))


(defn load-doc [ctx ref]
  (let [url (defragment-uri ref)]
    (from-cache ctx url)))

(defn set-resolution-context [ctx uri]
  (update-in ctx [:base-uri] #(resolve-uri % uri)))

(defn resolve-ref [ctx ref]
  (let [ rref     (resolve-uri (:base-uri ctx) ref)
        [doc ctx] (load-doc ctx rref)]
    [(json-pointer doc (fragment rref)) doc ctx]))

(defn cycle-refs? [ctx ref]
  (loop [ref ref visited #{}]
    (let [[new-ref doc ctx] (resolve-ref ctx ref)]
      (if (:$ref new-ref)
        (if (contains? visited ref)
          true
          (recur (:$ref new-ref) (conj visited ref)))
        false))))


(defn resolve-relative-ref
  "See rfc http://tools.ietf.org/html/draft-luff-relative-json-pointer-00"
  [doc current-path ref]
  (let [return-key? (not (nil? (re-matches #"^.*#$" ref)))
        ref (str/replace ref #"#$" "")
        path (str/split ref #"/")
        path (reduce (fn [acc x] (conj acc (if (re-matches #"^\d+$" x)
                                             (read-string x)
                                             (keyword (decode-json-pointer x)))))
                     [] path)
        backward (first path)
        path (rest path)
        absolute-path (concat (drop-last backward current-path) path)]
    (if return-key?
      (last absolute-path)
      (get-in doc absolute-path))))

