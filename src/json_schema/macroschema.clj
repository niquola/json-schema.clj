(ns json-schema.macroschema
  (:require [clojure.string :as str]))

(defn add-error [ctx msg]
  (update ctx :errors conj {:desc msg :path (:path ctx)}))

(declare expand)

(defmulti expand-key (fn [cctx k sch scope] k))

(defmethod expand-key :type
  [_ _ tp _]
  (cond
    (string? tp)
    (cond
      (= "object" (name tp))
      `(cond-> (not (map? ~'subj)) (add-error (str "Expected an object, got " ~'subj)))

      (= "string" (name tp))
      `(cond-> (not (string? ~'subj)) (add-error (str "Expected a string, got " ~'subj)))

      (= "integer" (name tp))
      `(cond-> (not (int? ~'subj)) (add-error (str "Expected a integer, got " ~'subj)))

      (= "number" (name tp))
      `(cond-> (not (number? ~'subj)) (add-error (str "Expected a integer, got " ~'subj)))

      (= "array" (name tp))
      `(cond-> (not (sequential? ~'subj)) (add-error (str "Expected an array, got " ~'subj)))
      :else
      (assert false (pr-str  "TODO type" tp)))
    :else
    (assert false (pr-str  "TODO type" tp))))

(defmethod expand-key :required
  [_ _ fields _]
  (cond (sequential? fields)
        (let [preds (mapcat
                     (fn [f]
                       (let [msg (str  f " is required")]
                         [`(nil? (get ~'subj ~(keyword f))) `(add-error  ~msg)])
                       ) fields)]
          `(cond-> (map? ~'subj) (cond-> ~@preds)))))

(defn push-path [ctx & ks]
  (update ctx :path into ks))

(defn pop-path [ctx]
  (update ctx :path pop))

(defmethod expand-key :properties
  [cctx _ props _]
  (let [props-e (mapcat
                 (fn [[k v]]
                   (let [lmbd (expand (push-path cctx (keyword k)) v)]
                     [`(not (nil? (get ~'subj ~(keyword k))))
                      `(-> (push-path ~(keyword k))
                           (~lmbd (get ~'subj ~(keyword k)))
                           (pop-path))])
                   ) props)]
    `(cond-> (map? ~'subj)
       (cond-> ~@props-e))))

(defn validate-ref [ctx subj rf]
  (if-let [r (when-let [refs (:$ref ctx)] (get @refs rf))]
    ((eval r) ctx subj)
    (add-error ctx (str "$ref " rf " could not be resolved"))))

(defmethod expand-key :$ref
  [cctx _ rf _]
  `(validate-ref ~'subj ~rf))

(defmethod expand-key :extends
  [cctx _ rf _])

(defmethod expand-key :format
  [cctx _ rf _])

(defmethod expand-key :dependencies
  [cctx _ rf _])

(defmethod expand-key :divisibleBy
  [cctx _ rf _])

(defn reduce-indexed [acc f col]
  (loop [[x & xs] col i 0 acc acc]
    (if (empty? xs)
      (f acc x i)
      (recur xs (inc i) (f acc x i)))))


(defmethod expand-key :items
  [cctx _ its _]
  (cond (map? its)
        (let [it-sch (expand cctx its)]
          `(cond->
            (sequential? ~'subj)
            (reduce-indexed
             (fn [ctx# x# i#]
               (-> ctx#
                   (push-path i#)
                   (~it-sch x#)
                   (pop-path)))
             ~'subj)))
        :else (println "TODO items" its)))

(defn expand [cctx sch]
  (let [pipe (->> sch
                  (mapv (fn [[k ksch]] (expand-key (push-path cctx k) k ksch sch)))
                  (filterv #(not (nil? %))))
        lmbd `(fn [~'ctx ~'subj] (-> ~'ctx ~@pipe))]
    (when-let [$ref (:$ref cctx)]
      (swap! $ref assoc (str "#/" (str/join "/" (mapv name (:path cctx)))) lmbd))
    lmbd))


;; to track $ref 
;; we create refs atom which is filled by compilation
;; and used in validation
(defn compile-schema [sch]
  (let [refs (atom {})
        cctx {:path [] :$ref refs}
        f (eval (expand cctx sch))]
    (fn [subj]
      (->
       (f {:path [] :errors [] :$ref refs} subj)
       (dissoc :$ref :path)))))

(defn test [sch subj]
  (let [sch-f (compile-schema sch)]
    (sch-f subj)))


(expand {} {:items {:type "string"}})




