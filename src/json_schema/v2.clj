(ns json-schema.v2
  (:require [clojure.string :as str]))

(defn decode-json-pointer [x]
  (-> x (str/replace #"~0" "~")
      (str/replace #"~1" "/")
      (str/replace #"%25" "%")))

(defn compile-pointer [ref]
  (let [is-root-path (str/starts-with? ref "#")
        is-return-key (str/ends-with? ref "#")
        ref-path (->
                  ref
                  (str/replace #"(^#\/|#$)" "")
                  (str/split #"/")
                  (->>
                   (mapv (fn [x]
                           (if (re-matches #"^\d+$" x)
                             (read-string x)
                             (keyword (decode-json-pointer x)))))))]
    (if is-root-path
      (fn [ctx] (get-in (:doc ctx) ref-path))
      (if is-return-key
        (fn [ctx]
          (let [path (:path ctx)
                steps-back (first ref-path)
                ref-path (rest ref-path)
                absolute-path (concat (drop-last steps-back path) ref-path)]
            (last absolute-path)))
        (fn [ctx]
          (let [path (:path ctx)
                steps-back (first ref-path)
                ref-path (rest ref-path)
                absolute-path (concat (drop-last steps-back path) ref-path)]
            (get-in (:doc ctx) absolute-path)))))))

(defn $data-pointer [x]
  (when-let [d (:$data x)] (compile-pointer d)))

(defn add-warning [ctx message]
  (update-in ctx [:warning] conj {:path (:path ctx) :message message}))

(defn add-error [env ctx val-type message & [value]]
  (let [k (or (get-in ctx [:config val-type]) :errors)]
    (update ctx k
            (fn [x] (conj (or x [])
                          (cond-> {:path (:path ctx)
                                   :by (:path env)
                                   :message message}
                            value (assoc :value value)))))))


;; schema is function ctx v => ctx (with errors)

(defn true-schema  [ctx v] ctx)
(defn error-schema [env k msg]
  (fn [ctx v] (add-error env ctx k msg)))

(defmulti compile-key (fn [{k :key :as env}] k))

(defn fail [& args]
  (throw (Exception. (apply str args))))

(defmethod compile-key
  :default
  [env]
  (fail  "Unknown key " (:key env)))

(declare compile-schema)

(defmulti compile-type (fn [{k :key}] k))


(defmethod compile-type
  :object
  [env]
  (fn [ctx v]
    (if-not (map? v)
      (add-error env ctx :object "expected type [object]" v)
      ctx)))

(defmethod compile-type
  :string
  [env]
  (fn [ctx v]
    (if-not (string? v)
      (add-error env ctx :string "expected type [string]" v)
      ctx)))


(defmethod compile-type
  :default [{k :key}]
  (fail "TODO:" k))

(defmethod compile-key
  :type
  [{tp :schema pth :path :as env}]
  (cond
    (string? tp) (compile-type (assoc env :key (keyword tp)))
    (sequential? tp) (let [vs (->> tp
                                   (map-indexed (fn [i s]
                                                  (if (string? s)
                                                    (compile-type (assoc env :key (keyword s) :path (conj pth i)))
                                                    (compile-schema (assoc env :schema s :parent tp :path (conj pth i))))))
                                   doall)]
                       (fn [ctx v]
                         (loop [[vl & vls :as vs] vs]
                           (if-not vs
                             (add-error env ctx :type (str "expected type " (str/join " or " (mapv #(str "[" % "]") tp))) v)
                             (let [{errs :errors} (vl (assoc ctx :errors []) v)]
                               (if (empty? errs)
                                 ctx
                                 (recur vls)))))))
    :else (compile-schema env)))

(defn- keys-validators [{sch :schema s-pth :path :as env}]
  (->> sch
       (reduce (fn [acc [k v]]
                 (if-let [vf (compile-key (assoc env :key k :schema v :parent sch :path (conj s-pth k)))]
                   (conj acc vf)
                   acc))
               [])
       doall))

(defn- compile-keys [{sch :schema  :as env}]
  (let [validators (keys-validators env)]
    (fn [ctx v]
      (let [pth (:path ctx)]
        (reduce (fn [ctx vf] (vf (assoc ctx :path pth) v))
                ctx validators)))))

(defn compile-schema
  ;; env is compilation context
  ;; contains schema to complie
  ;; path of this schema 
  ;; original root schema
  ;; registry for refs
  [{sch :schema  :as env}]
  (cond
    (= true sch)    true-schema
    (= false sch)  (error-schema env :schema (str "schema is 'false', which means it's always fails"))
    (map? sch)     (compile-keys env)
    :else          (fail "Invalid schema " (pr-str sch))))


(defn validate [sch subj & [opts]]
  (let [s (compile-schema {:path [] :root sch :schema sch})]
    (s {:path [] :errors [] :warnings [] :derefs []} subj)))
