(ns json-schema.macroschema
  (:require [clojure.string :as str]))



`(cond-> ~'ctx
   (nil? (get ~'subj :a) (add-error ~'ctx path ))
   (nil? (get ~'subj :a))
   (nil? (get ~'subj :a)))

(declare expand)

(defmulti expand-key (fn [cctx k sch scope] k))

(defmethod expand-key :type
  [_ _ tp _]
  (cond
    (= "object" (name tp))
    `(cond->
         (not (map? ~'subj)) (add-error (str "Expected an object, got " ~'subj)))

    (= "string" (name tp))
    `(cond->
         (not (string? ~'subj)) (add-error (str "Expected a string, got " ~'subj)))

    (= "array" (name tp))
    `(cond->
         (not (sequential? ~'subj)) (add-error (str "Expected an array, got " ~'subj)))))

(defmethod expand-key :required
  [_ _ fields _]
  (let [preds (mapcat
               (fn [f]
                 (let [msg (str  f " is required")]
                   [`(nil? (get ~'subj ~(keyword f))) `(add-error  ~msg)])
                 ) fields)]
    `(cond-> (map? ~'subj) (cond-> ~@preds))))

(defn push-path [ctx & ks]
  (update ctx :path into ks))

(defn pop-path [ctx]
  (update ctx :path pop))

(defmethod expand-key :properties
  [cctx _ props _]
  (let [props-e (mapcat
                 (fn [[k v]]
                   (let [lmbd (expand (push-path cctx :properties k) v)]
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

(defn expand [cctx sch]
  (let [pipe (mapv (fn [[k ksch]] (expand-key cctx k ksch sch)) sch)
        lmbd `(fn [~'ctx ~'subj] (-> ~'ctx ~@pipe))]
    (when-let [$ref (:$ref cctx)]
      (swap! $ref assoc (str "#/" (str/join "/" (mapv name (:path cctx)))) lmbd))
    lmbd))

(defn add-error [ctx msg]
  (update ctx :errors conj {:desc msg :path (:path ctx)}))

(defn compile-schema [sch]
  (let [refs (atom {})
        cctx {:path [] :$ref refs}
        f (eval (expand cctx sch))]
    (fn [subj]
      (->
       (f {:path [] :$ref refs} subj)
       (dissoc :$ref)))))

(defn test [sch subj]
  (let [sch-f (compile-schema sch)]
    (sch-f subj)))







