(ns json-schema.core
  (:refer-clojure :exclude [compile])
  (:require [cheshire.core :as json]
            [clojure.set]
            [clojure.string :as str])
  (:import java.net.URL
           java.time.LocalDate))

(set! *warn-on-reflection* true)

(declare compile)
(declare compile-registry)

(defn decode-json-pointer [x]
  (-> x (str/replace #"~0" "~")
      (str/replace #"~1" "/")
      (str/replace #"%25" "%")))

(defn json-pointer-to-path [x]
  (->> (str/split (str/replace x #"^#?/?" "") #"/")
       (filter #(not (str/blank? %)))
       (mapv decode-json-pointer)
       (mapv keyword)))

(defn resolve-pointer [obj pth]
  (loop [[p & ps] pth cur obj]
    (cond
      (nil? cur) nil
      (nil? p ) cur
      (map? cur) (let [next (or (get cur p) (get cur (keyword p)))]
                   (recur ps next))

      (and (sequential? cur) (re-matches #"^\d+" (name p)))
      (let [idx (Integer/parseInt (name p))]
        (nth cur idx))

      :else (assert false (str "not handled yet" p " " cur)))))



(defn num-comparator [a b]
  (cond (> a b) 1 (= a b) 0 :else -1))

(defn add-error [val-type ctx message]
  (let [k (or (get-in ctx [:config val-type]) :errors)]
    (update-in ctx [k] (fn [x] (if x (conj x {:path (:path ctx) :message message})
                                   [{:path (:path ctx) :message message}])))))

(defn add-deferred [ctx value annotation]
  (update-in ctx [:deferreds] conj {:path (:path ctx) :value value :deferred annotation}))

(defn reduce-indexed 
  "Reduce while adding an index as the second argument to the function"
  ([f coll]
   (reduce-indexed f (nth coll 0 nil) 0 (rest coll)))
  
  ([f init coll]
   (reduce-indexed f init 0 coll))
  
  ([f init ^long i coll]
   (if (empty? coll)
     init
     (let [v (nth coll 0 nil)
           fv (f init i v)]
       (recur f fv (inc i) (rest coll))))))


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
                steps-back (nth ref-path 0)
                ref-path (rest ref-path)
                absolute-path (concat (drop-last steps-back path) ref-path)]
            (last absolute-path)))
        (fn [ctx]
          (let [path (:path ctx)
                steps-back (nth ref-path 0 nil)
                ref-path (rest ref-path)
                absolute-path (concat (drop-last steps-back path) ref-path)]
            (get-in (:doc ctx) absolute-path)))))))

(defn compile-comparator
  [{name :name
    value-applicable? :applicable-value
    coerce-value      :coerce-value
    coerce-bound      :coerce-bound
    bound-applicable? :applicable-bound
    comparator-fn :comparator-fn
    message :message
    message-op :message-op
    exclusive :exclusive
    direction :direction
    bound :bound}]
  (fn schema-comparator [ctx v]
    (let [$bound (if (fn? bound) (bound ctx) bound)
          $bound (if (and (fn? coerce-bound) (some? $bound)) (coerce-bound $bound) $bound)
          $exclusive (if (fn? exclusive) (exclusive ctx) exclusive)
          op (if (= true $exclusive) < <=)]
      (cond
        (nil? $bound) ctx

        (and (some? $bound) (not (bound-applicable? $bound)))
        (add-error name ctx (str " could not compare with " $bound))

        (and (some? $exclusive) (not (boolean? $exclusive)))
        (add-error name ctx (str "exclusive flag should be boolean, got " $exclusive))

        (and (value-applicable? v)
               (bound-applicable? $bound)
               (not (op 0 (* direction (comparator-fn $bound (coerce-value v))))))

        (add-error name ctx (str "expected" message " " (coerce-value v) message-op $bound))
        :else ctx))))

(defn $data-pointer [x]
  (when-let [d (:$data x)] (compile-pointer d)))

(defn add-warning [ctx message]
  (update-in ctx [:warning] conj {:path (:path ctx) :message message}))

(defmulti schema-type (fn [k] k))

(defmulti schema-key (fn [k opts schema path c-ctx] (if (= :default k) :schemaDefault k)))

(defn build-ref [path]
  (str "#"
       (when (not (empty? path))
         (->> path
              (map (fn [x]
                     (cond
                       (string? x) x
                       (keyword? x) (subs (str x) 1)
                       :else (str x))))
              (str/join "/")
              (str "/")))))

(defn- compile-schema [schema path c-ctx]
  (let [schema-fn (cond
                    (= true schema)
                    (fn [ctx v] ctx)

                    (= false schema)
                    (fn [ctx v] (add-error :schema ctx (str "schema is 'false', which means it's always fails")))

                    (map? schema)
                    (let [c-ctx' (if-let [id (or (:id schema) (:$id schema))]
                                   (update c-ctx :ids (fn [x] (if x (conj x id) [id])))
                                   c-ctx)
                          validators (doall
                                      (reduce (fn [acc [k v]]
                                                (let [v (or ($data-pointer v) v)] ;; check for $data
                                                  (if-let [vf (schema-key k v schema path c-ctx')]
                                                    (conj acc vf)
                                                    acc)))
                                              [] (dissoc schema :title :id :$id)))]
                      (fn schema-keys [ctx v]
                        (let [pth (:path ctx)
                              iter (.iterator ^Iterable validators)]
                          (if (.hasNext iter)
                            (loop [ctx ctx]
                              (let [vf (.next iter)
                                    ctx (vf (assoc ctx :path pth) v)]
                                (if (.hasNext iter)
                                  (recur ctx)
                                  ctx)))
                            ctx))))
                    :else
                    (fn [ctx v] (add-error :schema ctx (str "Invalid schema " schema))))]
    (let [ref (build-ref path)]
      (update c-ctx :reg
              (fn [reg]
                (swap! reg update ref (fn [x] (if x x schema-fn)))
                (when-let [id (or (:id schema) (:$id schema))]
                  (when (str/starts-with? id "http")
                    (swap! reg update id (fn [x] (if x x schema-fn))))))))
    schema-fn))

(defn validate-string [ctx v]
  (cond
    (string? v)
    ctx
    (keyword? v)
    ctx
    :else 
    (add-error :string ctx "expected type of string")))

#_(if (not (or (string? v) (keyword? v)))
    (add-error :string ctx "expected type of string")
    ctx
    ;; TODO: why not blank
    #_(if (str/blank? (str/trim (name v)))
        (add-error :string ctx "expected not empty string")
        ctx))

(defmethod schema-type :string [_]
  validate-string)


(defmethod schema-type
  :boolean
  [_]
  (fn validate-boolean [ctx v]
    (if (boolean? v)
      ctx
      (add-error :boolean ctx "expected boolean"))))


(def date-regexp #"^-?[0-9]{4}(-(0[1-9]|1[0-2])(-(0[0-9]|[1-2][0-9]|3[0-1]))?)?$")

(defmethod schema-type
  :date
  [_]
  (fn validate-date [ctx v]
    (if (not (string? v))
      (add-error :date ctx "date should be encoded as string")
      (if (re-matches date-regexp v)
        ctx
        (add-error :date ctx "wrong date format")))))

(defmethod schema-type
  :number
  [_]
  (fn validate-number [ctx v]
    (if (not (number? v))
      (add-error :date ctx "expected number")
      ctx)))

(def uri-regexp #"^([^:]+)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]")

(defmethod schema-type
  :uri
  [_]
  (fn validate-uri [ctx v]
    (if (not (string? v))
      (add-error :uri ctx "uri should be encoded as string")
      (if (str/blank? v)
        (add-error :uri ctx "expected not empty string")
        (if (re-matches uri-regexp v)
          ctx
          (add-error :uri ctx "wrong uri format"))))))


(defmethod schema-type
  :integer
  [_]
  (fn validate-int [ctx v]
    (if (integer? v)
      ctx
      (add-error :integer ctx (str  "expected integer, got " v)))))

(def dateTime-regex #"^-?[0-9]{4}(-(0[1-9]|1[0-2])(-(0[0-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?(Z|[+-]((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?)?)?)?$")

(defmethod schema-type
  :datetime
  [_]
  (fn validate-datetime [ctx v]
    (if (not (string? v))
      (add-error :datetime ctx "datetime should be encoded as string")
      (if (re-matches dateTime-regex v)
        ctx
        (add-error :datetime  ctx "wrong datetime format")))))

(def time-regex #"^([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?$")

(defmethod schema-type
  :time
  [_]
  (fn validate-teme [ctx v]
    (if (not (string? v))
      (add-error :time ctx "time should be encoded as string")
      (if (re-matches time-regex v)
        ctx
        (add-error :time ctx "wrong time format")))))

(def oid-regex #"^[[0-9]+\.]*$")

(defmethod schema-type
  :oid
  [_]
  (fn validate-oid [ctx v]
    (if (not (string? v))
      (add-error :oid ctx "oid should be encoded as string")
      (if (re-matches oid-regex v)
        ctx
        (add-error :oid ctx "wrong oid format")))))

(def uuid-regex #"^([a-f\d]{8}(-[a-f\d]{4}){3}-[a-f\d]{12}?)$")

(defmethod schema-type
  :uuid
  [_]
  (fn validate-uuid [ctx v]
    (if (not (string? v))
      (add-error :uuid ctx "uuid should be encoded as string")
      (if (re-matches uuid-regex v)
        ctx
        (add-error :uuid ctx "wrong uuid format")))))

(def email-regex #"[^@]+@[^.]+\..*")


(defmethod schema-type
  :email
  [_]
  (fn validate-email [ctx v]
    (if (not (string? v))
      (add-error :email ctx "email should be encoded as string")
      (if (re-matches email-regex v)
        ctx
        (add-error :email ctx "wrong email format")))))

(defmethod schema-type
  :object
  [_]
  (fn validate-obj [ctx v]
    (if (map? v)
      ctx
      (add-error :object ctx "expected object"))))

(defmethod schema-type
  :array
  [_]
  (fn validate-array [ctx v]
    (if (sequential? v)
      ctx
      (add-error :array ctx "expected array"))))

(defmethod schema-type
  :null
  [_]
  (fn validate-null [ctx v]
    (if (nil? v)
      ctx
      (add-error :null ctx "expected null"))))

(defmethod schema-type
  :any
  [_]
  (fn validate-any [ctx v] ctx))

(defmethod schema-type
  nil 
  [_]
  (fn schema-type [ctx v]
    (if (nil? v)
      ctx
      (add-error :null ctx "expected null"))))

(defmethod schema-type
  :default
  [unknown]
  (fn schema-default [ctx v]
    (add-error :unknown-type ctx (str "Broken schema: unknown type " unknown))))


(defmethod schema-key
  :type
  [k opts schema path c-ctx]
  (if (sequential? opts)
    (let [validators (->> opts
                          (mapv (fn [o] (if (string? o) (schema-type (keyword o)) (compile-schema o (conj path :type) c-ctx))))
                          doall)]
      (fn schema-type [ctx v]
        (if (some
             (fn [validator]
               (let [{err :errors} (validator (assoc ctx :errors []) v)]
                 (empty? err))) validators)
          ctx
          (add-error :type ctx (str "expected type of one of " (str/join ", " opts))))))
    (schema-type (keyword opts))))

;; TODO: can optimize
(defmethod schema-key
  :properties
  [_ props {ap :additionalProperties :as schema} path c-ctx]
  (when (map? props)
    (let [props-validators
          (doall (->> props
                      (reduce (fn [acc [k v]]
                                (assoc acc k (compile-schema v (into path [:properties (keyword k)]) c-ctx)))
                              {})))
          requireds (->> props
                        (reduce (fn [acc [k v]]
                                  (if (= true (:required v))
                                    (conj acc k)
                                    acc)) []))
          req-validator (when (seq requireds)
                          (schema-key :required requireds schema path c-ctx))]
      (if (= ap false)
        (fn schema-props-ap [ctx v]
          (if (map? v)
            (let [pth (:path ctx)
                  ctx (if req-validator (req-validator ctx v) ctx)
                  iter (.iterator ^Iterable v)]
              (if (.hasNext iter)
                (loop [ctx ctx]
                  (let [^clojure.lang.MapEntry item (.next iter)
                        k (.getKey item)
                        kn (name k)
                        vv (.getValue item)

                        ctx (if (some? vv)
                              (if-let [vf (get props-validators k)]
                               (vf (assoc ctx :path (conj pth k)) vv)
                               (if (or (str/starts-with? kn "_")
                                       (str/starts-with? kn "fhir_"))
                                 ctx
                                 (add-error :additionalProperties (assoc ctx :path (conj pth k)) "extra property")))
                              ctx)]
                    (if (.hasNext iter)
                      (recur ctx)
                      ctx)))
                ctx))
            ctx))

        (fn schema-props [ctx v]
          (if (map? v)
            (let [pth (:path ctx)
                  ctx (if req-validator (req-validator ctx v) ctx)
                  iter (.iterator ^Iterable v)]
              (if (.hasNext iter)
                (loop [ctx ctx]
                  (let [^clojure.lang.MapEntry item (.next iter)
                        k (.getKey item)
                        vv (.getValue item)
                        ctx (if-let [vf (get props-validators k)]
                              (vf (assoc ctx :path (conj pth k)) vv)
                              ctx)]
                    (if (.hasNext iter)
                      (recur ctx)
                      ctx)))
                ctx))
            ctx))))))

(defmethod schema-key
  :maxProperties
  [_ bound schema path c-ctx]
  (compile-comparator
   {:name :maxProperties 
    :applicable-value map?
    :coerce-value count
    :applicable-bound number?
    :comparator-fn num-comparator 
    :message " number of properties "
    :message-op " >= "
    :direction 1
    :bound bound}))

(defmethod schema-key
  :minProperties
  [_ bound schema path c-ctx]
  (compile-comparator
   {:name :minProperties
    :applicable-value map?
    :coerce-value count
    :applicable-bound number?
    :comparator-fn num-comparator 
    :message " number of properties "
    :message-op " <= "
    :direction -1
    :bound bound}))

(defn is-divider? [v d]
  (when (and (number? v) (number? d))
    (re-matches #"^\d+(\.0)?$" (str (/ v d)))))

(defmethod schema-key
  :multipleOf
  [_ bound schema path c-ctx]
  (cond
    (number? bound)
    (fn schema-mof [ctx v]
      (if (and (number? v) (not (or (= 0 v) (is-divider? v bound))))
        (add-error :multipleOf ctx (str "expected " v " is multiple of " bound))
        ctx))
    (fn? bound)
    (fn schema-mof [ctx v]
      (let [$bound (bound ctx)]
        (cond
          (nil? $bound) ctx

          (and (some? $bound) (not (number? $bound)))
          (add-error :multipleOf ctx (str "could not find multiple of " v " and " $bound))

          (and (number? v) (not (or (= 0 v) (is-divider? v $bound))))
          (add-error :multipleOf ctx (str "expected " v " is multiple of " $bound))

          :else ctx)))
    :else nil))


(defmethod schema-key
  :divisibleBy
  [_ bound schema path c-ctx]
  (cond
    (number? bound)
    (fn schema-div [ctx v]
      (if (and (number? v) (not (or (= 0 v) (is-divider? v bound))))
        (add-error :divisibleBy ctx (str "expected " v " is divisible by  " bound))
        ctx))
    (fn? bound)
    (fn  schema-div [ctx v]
      (let [$bound (bound ctx)]
        (cond
          (nil? $bound) ctx

          (and (some? $bound) (not (number? $bound)))
          (add-error :multipleOf ctx (str "could not find divisible by " v " and " $bound))

          (and (number? v) (not (or (= 0 v) (is-divider? v $bound))))
          (add-error :multipleOf ctx (str "expected " v " is divisible by " $bound))

          :else ctx)))
    :else nil))

(defn json-compare [a b]
  (cond
    (and (string? a) (string? b))   (= a b)
    (and (keyword? a) (keyword? b)) (= a b)
    (and (string? a) (keyword? b)) (= a (name b))
    (and (keyword? a) (string? b)) (= (name a) b)
    :else (= a b)))

(defmethod schema-key
  :enum
  [_ enum schema path c-ctx]
  (if (fn? enum)
    (fn schema-enum [ctx v]
      (let [$enum (enum ctx)]
        (if-not (sequential? $enum)
          (if (nil? $enum)
            ctx
            (add-error :enum ctx (str "could not enum by " $enum)))
          (if-not (some (fn [ev] (json-compare ev v)) $enum)
            (add-error :enum ctx (str "expected one of " (str/join ", " $enum)))
            ctx))))
    (fn schema-enum [ctx v]
      (if-not (some (fn [ev] (json-compare ev v)) enum)
        (add-error :enum ctx (str "expected one of " (str/join ", " enum)))
        ctx))))



(defn const-impl
  [_ const schema path c-ctx]
  (if (fn? const)
    (fn schema-const [ctx v]
      (let [$const (const ctx)]
        (if-not (json-compare $const v)
          (add-error :constant ctx (str "expected " $const ", but " v))
          ctx)))
    (fn schema-const [ctx v]
      (if-not (json-compare const v)
        (add-error :constant ctx (str "expected " const ", but " v))
        ctx))))

(defmethod schema-key :constant
  [& args] (apply const-impl args))

(defmethod schema-key :const
  [& args] (apply const-impl args))

(defmethod schema-key
  :discriminator
  [_ prop schema path c-ctx]
  (fn schema-discriminator [ctx v]
    (if-let [tp (get v (keyword prop))]
      (if-let [validator (-> c-ctx
                             :reg
                             deref
                             (get (str "#/definitions/" tp)))]
        (validator ctx v)
        (add-error :discriminator ctx (str "Could not resolve #/definitions/" tp)))
      ctx)))

(defmethod schema-key
  :exclusiveProperties
  [_ ex-props schema path _]
  (fn schema-exclusive-props [ctx v]
    (if (map? v)
      (reduce
       (fn [ctx {props :properties required :required}]
         (let [num (select-keys v (mapv keyword props))]
           (cond
             (and (not required) (<= (count num) 1)) ctx
             (and required (= (count num) 1)) ctx

             (and required (= (count num) 0))
             (add-error :exclusiveProperties ctx (str "One of properties " (str/join ", " props) " is required"))

             (> (count num) 1)
             (add-error :exclusiveProperties ctx (str "Properties " (->> props (map name) (str/join ", ")) " are mutually exclusive"))

             :else ctx)
           )) ctx ex-props)
      ctx)))

(defmethod schema-key
  :dependencies
  [_ props schema path c-ctx]
  (assert (map? props))
  (let [props-validators
        (doall (reduce (fn [acc [k v]]
                         (assoc acc k
                                (cond
                                  (string? v)
                                  (fn [ctx vv]
                                    (if-not (contains? vv (keyword v))
                                      (add-error :dependencies ctx (str v " is required"))
                                      ctx))

                                  (and (sequential? v) (every? string? v))
                                  (let [req-keys (map keyword v)]
                                    (fn [ctx vv]
                                      (if-not (every? #(contains? vv %) req-keys)
                                        (add-error :dependencies ctx (str req-keys " are required"))
                                        ctx)))

                                  (or (map? v) (boolean? v))
                                  (compile-schema v (conj path :dependencies k) c-ctx)
                                  :else (fn [ctx v] ctx))))
                       {} props))]
    (fn schema-deps [ctx v]
      (if-not (map? v)
        ctx
        (let [pth (:path ctx)]
          (reduce (fn [ctx [k vf]]
                    (if (contains? v k) 
                      (vf (assoc ctx :path (conj pth k)) v)
                      ctx))
                  ctx props-validators))))))

(defmethod schema-key
  :patternProperties
  [_ props schema path c-ctx]
  (let [props-map
        (doall (reduce (fn [acc [k v]]
                         (assoc acc (re-pattern (name k)) (compile-schema v (conj path :patternProperties (name k)) c-ctx)))
                       {} props))]
    (fn schema-pattern-props [ctx v]
      (if-not (map? v)
        ctx
        (let [pth (:path ctx)]
          (reduce
           (fn [ctx [k vv]]
             (let [k-str (name k)
                   new-ctx (assoc ctx :path (conj pth k))]
               (reduce
                (fn [ctx [pat validator]]
                  (if (re-find pat k-str)
                    (validator new-ctx vv)
                    ctx))
                ctx props-map)))
           ctx v))))))

(defmethod schema-key
  :patternGroups
  [_ props schema path c-ctx]
  (let [props-map
        (doall (reduce (fn [acc [k {sch :schema :as g}]]
                         (assoc acc (re-pattern (name k))
                                (assoc g :schema (compile-schema sch (conj path :patternGroups) c-ctx))))
                       {} props))]
    (fn schema-pat-grp [ctx v]
      (if-not (map? v)
        ctx
        (reduce
         (fn [ctx [pat {validator :schema min :minimum max :maximum}]]
           (let [ctx (reduce
                      (fn [ctx [k vv]]
                        (let [pth (:path ctx)
                              k-str (name k)
                              new-ctx (assoc ctx :path (conj pth k))]
                          (if (re-find pat k-str)
                            (-> (validator new-ctx vv)
                                (assoc :patternGroupCount (inc (:patternGroupCount ctx)))
                                (assoc :path pth))
                            ctx)))
                      (assoc ctx :patternGroupCount 0) v)]
             (cond
               (and (nil? min) (nil? max)) ctx
               (and (some? min) (and (< (:patternGroupCount ctx) min)))
               (add-error :patternGroups ctx (str "patternGroup expects number of matched props " (:patternGroupCount ctx) " > " min))

               (and (some? max) (and (> (:patternGroupCount ctx) max)))
               (add-error :patternGroups ctx (str "patternGroup expects number of matched props " (:patternGroupCount ctx) " < " max))

               :else ctx)))
         ctx props-map)))))

(defmethod schema-key
  :extends
  [_ props schema path c-ctx]
  (let [validators (doall
                    (->>
                     (cond (sequential? props) props
                           (map? props) [props]
                           :else (assert false (str "Not impl extends " props)))
                     (mapv (fn [o] (compile-schema o (conj path :extends) c-ctx)))))]
    (fn schema-extends [ctx v]
      (let [pth (:path ctx)]
        (reduce (fn [ctx validator] (validator (assoc ctx :path pth) v))
                ctx validators)))))

(defmethod schema-key
  :allOf
  [_ options schema path c-ctx]
  (let [validators (doall (mapv (fn [o] (compile-schema o (conj path :allOf) c-ctx)) options))]
    (fn schema-all-of [ctx v]
      (let [pth (:path ctx)]
        (reduce (fn [ctx validator] (validator (assoc ctx :path pth) v))
                ctx validators)))))

(defmethod schema-key
  :switch
  [_ switch schema path c-ctx]
  (let [clauses (->> switch
                     (mapv (fn [clause]
                             (assoc clause
                                    :compiled
                                    (cond-> {}
                                      (:if clause) (assoc :if (compile-schema (:if clause) path c-ctx))
                                      (and (:then clause) (map? (:then clause)))
                                      (assoc :then (compile-schema (:then clause) path c-ctx))))))
                     doall)]
    (fn schema-switch [ctx v]
      (let [pth (:path ctx)]
        (loop [ctx ctx
               [cl & cls] clauses]
          (cond (nil? cl) ctx

                (:if cl)
                (let [if-validator (get-in cl [:compiled :if])
                      {errs :errors} (if-validator ctx v)]
                  (if (empty? errs)
                    (let [next-ctx (cond
                                    (= false (:then cl))
                                    (add-error :switch ctx (str "expected not matches " (:if cl)))

                                    (= true (:then cl))
                                    ctx

                                    (map? (:then cl))
                                    ;; todo handle continue
                                    ((get-in cl [:compiled :then]) ctx v)

                                    :else ctx)]
                      (if (:continue cl)
                        (recur next-ctx cls)
                        next-ctx))
                    (recur ctx cls)))

                (contains? cl :then)
                (cond
                  (= false (:then cl))
                  (add-error :switch ctx "switch failed - nothing matched")

                  (= true (:then cl))
                  ctx

                  (map? (:then cl))
                  (let [then-validator (get-in cl [:compiled :then])]
                    (then-validator ctx v)))

                :else (recur ctx cls)))))))

(defmethod schema-key
  :$id [& args] nil)

(defmethod schema-key
  :$schema [& args] nil)


(defmethod schema-key
  :if
  [_ if-expr {th :then el :else :as schema} path c-ctx]
  (let [pred-v (compile-schema if-expr path c-ctx)
        th-v   (compile-schema (or th true) path c-ctx)
        el-v   (compile-schema (or el true) path c-ctx)]
    (fn schema-if [ctx v]
      (if (empty? (:errors (pred-v ctx v)))
        (th-v ctx v)
        (el-v ctx v)))))

(defmethod schema-key
  :then
  [_ if-expr schema path c-ctx]
  nil)

(defmethod schema-key
  :else
  [_ if-expr schema path c-ctx]
  nil)


(defmethod schema-key
  :not
  [_ subschema schema path c-ctx]
  (let [validator (compile-schema subschema (conj path :not) c-ctx)]
    (fn schema-not [ctx v]
      (let [{errs :errors} (validator (assoc ctx :errors []) v)]
        (if (empty? errs)
          (add-error :not ctx (str "Expected not " subschema))
          ctx)))))

(defmethod schema-key
  :disallow
  [_ subschema schema path c-ctx]
  (let [validators (let [subsch (if (sequential? subschema) subschema [subschema])]
                     (doall (mapv (fn [sch]
                              (compile-schema (if (string? sch) {:type sch} sch) (conj path :disallow) c-ctx))
                            subsch)))]
    (fn schema-disallow [ctx v]
      (if (some (fn [vl] (-> (vl ctx v) :errors empty?)) validators)
        (add-error :disallow ctx (str "Disallowed by " (json/generate-string subschema)))
        ctx))))

(defmethod schema-key
  :anyOf
  [_ options schema path c-ctx]
  (let [validators (doall (mapv (fn [o] (compile-schema o (conj path :anyOf) c-ctx)) options))]
    (fn schema-any-of [ctx v]
      (if (some (fn [validator]
                   (let [res (validator (assoc ctx :errors []) v)]
                     (empty? (:errors res))))
                 validators)
        ctx
        (add-error :anyOf ctx (str "Non alternatives are valid"))))))

(defmethod schema-key
  :oneOf
  [_ options schema path c-ctx]
  (let [validators (doall (mapv (fn [o] (compile-schema o (conj path :oneOf) c-ctx)) options))]
    (fn schema-one-of [ctx v]
      (loop [cnt 0
             res nil
             validators validators]
        (if (empty? validators)
          (if (= 1 cnt)
            (update ctx :deferreds (fn [x] (into x (or (:deferreds res) []))))
            (add-error :oneOf ctx (str "expected one of " options ", but no one is valid")))
          (let [new-res ((nth validators 0) (assoc ctx :errors [] :deferreds []) v)]
            (if (empty? (:errors new-res))
              (if (> cnt 0)
                (add-error :oneOf ctx (str "expected one of " options ", but more then one are valid"))
                (recur (inc cnt) new-res (rest validators)))
              (recur cnt res (rest validators)))))))))

(defmethod schema-key :additionalProperties [& args] nil)

#_(defmethod schema-key
  :additionalProperties
  [_ ap {props :properties
         pat-props :patternProperties
         pat-g-props :patternGroups
         :as schema} path c-ctx]
  (let [props-keys (set (keys props))
        pat-props-regex (->> (keys (or pat-props {}))
                             (concat (keys (or pat-g-props {})))
                             (map (fn [x] (re-pattern (name x)))))
        is-path-prop? (fn [k] (let [k-str (name k)] (some #(re-find % k-str) pat-props-regex)))]
    (cond
      (= false ap)
      (fn schema-additional-props [ctx v]
        (if (map? v)
          (let [v-keys (set (keys v))
                extra-keys (clojure.set/difference v-keys props-keys)]
            (if-not (empty? extra-keys)
              (let [pth (:path ctx)]
                (reduce (fn [ctx k]
                          (if-not (is-path-prop? k)
                            (add-error :additionalProperties (assoc ctx :path (conj pth k)) "extra property")
                            ctx)
                          ) ctx extra-keys))
              ctx))
          ctx))

      (map? ap)
      (let [ap-validator (compile-schema ap (conj path :additionalProperties) c-ctx)]
        (fn schema-additional-props [ctx v]
          (if (map? v)
            (let [v-keys (set (keys v))
                  extra-keys (clojure.set/difference v-keys props-keys)]
              (if-not (empty? extra-keys)
                (let [pth (:path ctx)]
                  (reduce (fn [ctx k]
                            (if-not (is-path-prop? k)
                              (ap-validator (assoc ctx :path (conj pth k)) (get v k))
                              ctx)
                            ) ctx extra-keys))
                ctx))
            ctx)))

      :else (assert false (str "Ups do not know how to validate additionalProperties " ap)))))


(defn has-property? [v k]
  (and (contains? v (keyword k))
       (not (nil? (get v (keyword k))))))

(defmethod schema-key
  :required
  [_ props schema path c-ctx]
  (cond
    (sequential? props)
    (fn schema-req [ctx v]
      (if (map? v)
        (let [pth (:path ctx)]
          (reduce (fn [ctx k]
                    (if (has-property? v k)
                      ctx
                      (add-error  :requred ctx (str "Property " (name k) " is required"))))
                  ctx props))
        ctx))

    (fn? props)
    (fn schema-req [ctx v]
      (let [$props (props ctx)]
        (cond
          (nil? $props) ctx

          (not (sequential? $props))
          (add-error :required ctx (str "expected array of strings, but " $props))

          (map? v)
          (let [pth (:path ctx)]
            (reduce (fn [ctx k]
                      (if (has-property? v k)
                        ctx
                        (add-error :required ctx (str "Property " (name k) " is required"))))
                    ctx $props))
          :else ctx)))))

(defmethod schema-key
  :patternRequired
  [_ props schema path c-ctx]
  (cond
    (sequential? props)
    (let [re-props (set (map (fn [x] (re-pattern (name x))) props))]
      (fn schema-pat-req [ctx v]
        (if-not (map? v)
          ctx
          (let [not-matched-pats (reduce
                                  (fn [left-parts k]
                                    (let [k-str (name k)]
                                      (reduce (fn [acc p]
                                                (if-not (re-find p k-str)
                                                  (conj acc p)
                                                  acc))
                                              #{} left-parts)))
                                  re-props (keys v))]
            (if-not (empty? not-matched-pats)
              (add-error :patternRequired ctx (str "no properites, which matches " not-matched-pats))
              ctx)))))))

;; handled in items
(defmethod schema-key
  :additionalItems
  [_ items schema path _]
  nil)


(comment 
  (decode-json-pointer "#/tilda~0field"))

(defn to-uri [x]
  (try (java.net.URI. x)
       (catch Exception e
         (throw (Exception. (str "unable to parse \"" (pr-str x) "\" as uri"))))))

(defn uri-and-fragment [x]
  (when (string? x)
    (when-let [uri (to-uri x)]
      (let [fragment (.getFragment ^java.net.URI uri)
            endpoint (if fragment
                       (subs x 0 (- (count x) (count fragment) 1))
                       x)]
        [endpoint (str "#" fragment)]))))

(comment
  (uri-and-fragment "http://x.y.z/rootschema.json#foo")
  (uri-and-fragment "http://x.y.z/rootschema.json")
  )


(defn external-schema [ref c-ctx]
  (let [[uri frag] (uri-and-fragment ref)
        registry (:reg c-ctx)
        cache (or (get @registry uri)
                  (if-let [res (try (-> (slurp uri)
                                        (json/parse-string keyword))
                                    (catch Exception e
                                      (println "ERROR:" e)))]
                    (let [remote-registry (compile-registry res)]
                      (println "schema loaded from" uri)
                      (swap! registry assoc uri remote-registry)
                      remote-registry)
                    (println "Could not load " uri)))]
    (and cache (get @cache frag))))

(defn mk-ref-with-ids [r ids]
  ;; (println "Mkref " ids r)
  (if (str/starts-with? r "#")
    r
    (loop [pth []
           [it & its] (reverse ids)]
      (let [sub-pth (str/split it #"/")
            new-pth (into
                     (if (str/ends-with? it "/")
                       sub-pth
                       (into [] (butlast sub-pth)))
                     pth)]
        (if (or (empty? its) (str/starts-with? it "http"))
          (str (str/join "/" new-pth) "/" r)
          (recur new-pth its))))))

(defmethod schema-key
  :$ref
  [_ r schema path c-ctx]
  (let [ids (:ids c-ctx)
        r (if ids (mk-ref-with-ids r ids) r)
        r (decode-json-pointer r)
        registry (:reg c-ctx)]
    ;; (println "Pointer" r " from " path " ids " ids)
    (if-let [validator (and (str/starts-with? r "http") (external-schema r c-ctx))]
      (fn schema-ref [ctx v]
        (let [res (validator ctx v)]
          (assoc ctx :errors (into (:errors ctx) (:errors res)))))
      (fn schema-ref [ctx v]
        (if-let [val (get @registry r)]
          (val ctx v)
          (add-error :$ref ctx (str "Could not resolve $ref = " r)))))))

(defmethod schema-key
  :maximum
  [_ bound {ex :exclusiveMaximum} path c-ctx]
  (let [ex (or ($data-pointer ex) ex)]
    (compile-comparator
     {:name :maximum
      :applicable-value number?
      :coerce-value identity
      :applicable-bound number?
      :comparator-fn num-comparator 
      :message ""
      :message-op " < "
      :exclusive ex
      :direction 1
      :bound bound})))

(defmethod schema-key
  :exclusiveMaximum
  [_ bound {m :maximum} path c-ctx]
  (let [m (or ($data-pointer m) m)]
    (when (nil? m)
      (compile-comparator
       {:name :maximum
        :applicable-value number?
        :coerce-value identity
        :applicable-bound number?
        :comparator-fn num-comparator 
        :message ""
        :message-op " < "
        :exclusive true
        :direction 1
        :bound bound}))))



(defmethod schema-key
  :minimum
  [_ bound {ex :exclusiveMinimum} path c-ctx]
  (let [ex (or ($data-pointer ex) ex)]
    (compile-comparator
     {:name :minimum
      :applicable-value number?
      :coerce-value identity
      :applicable-bound number?
      :comparator-fn num-comparator 
      :message ""
      :message-op " > "
      :exclusive ex
      :direction -1
      :bound bound})))

(defmethod schema-key
  :exclusiveMinimum 
  [_ bound {m :minimum} path c-ctx]
  (let [m (or ($data-pointer m) m)]
    (when (nil? m)
      (compile-comparator
       {:name :minimum
        :applicable-value number?
        :coerce-value identity
        :applicable-bound number?
        :comparator-fn num-comparator 
        :message ""
        :message-op " > "
        :exclusive true
        :direction -1
        :bound bound}))))




(defn string-utf8-length [x]
  (when (string? x)
    (.count (.codePoints ^java.lang.String x))))

(defmethod schema-key
  :maxLength
  [_ bound schema path c-ctx]
  (compile-comparator
   {:name :maxLength
    :applicable-value string?
    :coerce-value string-utf8-length
    :applicable-bound number?
    :comparator-fn num-comparator 
    :message " string length "
    :message-op " < "
    :direction 1
    :bound bound}))

(defmethod schema-key
  :minLength
  [_ bound schema path c-ctx]
  (compile-comparator
   {:name :minLength
    :applicable-value string?
    :coerce-value string-utf8-length
    :applicable-bound number?
    :comparator-fn num-comparator 
    :message " string length "
    :message-op " > "
    :direction -1
    :bound bound}))


(defmulti compile-format-coerce (fn [fmt] fmt))

(defmethod compile-format-coerce
  :date [_]
  identity)

(defmethod compile-format-coerce
  :datetime [_]
  identity)

(defmethod compile-format-coerce
  :time [_]
  (fn [v] (str/replace v #"(Z|[+-]\d+:\d+)$" ""))) 

(defmethod compile-format-coerce
  :default [_]
  identity) 
  
(comment 
  ((compile-format-coerce :time)
   "13:15:17.000+01:00"))

(defmethod schema-key
  :formatMaximum
  [_ bound {fmt :format ex :exclusiveFormatMaximum} path c-ctx]
  (when-not (= "unknown" fmt)
    (let [ex (or ($data-pointer ex) ex)]
      (compile-comparator
       {:name :formatMaximum
        :applicable-value string?
        :coerce-value (compile-format-coerce (keyword fmt))
        :coerce-bound (compile-format-coerce (keyword fmt))
        :applicable-bound string?
        :comparator-fn compare
        :message " value "
        :message-op " <= "
        :direction 1
        :exclusive ex
        :bound bound}))))

(defmethod schema-key :exclusiveFormatMaximum [& _] nil)
(defmethod schema-key :exclusiveFormatMinimum [& _] nil)

(defmethod schema-key
  :formatMinimum
  [_ bound {fmt :format ex :exclusiveFormatMinimum} path c-ctx]
  (when-not (= "unknown" fmt)
    (let [ex (or ($data-pointer ex) ex)]
      (compile-comparator
       {:name :formatMinimum
        :applicable-value string?
        :coerce-value (compile-format-coerce (keyword fmt))
        :coerce-bound (compile-format-coerce (keyword fmt))
        :applicable-bound string?
        :comparator-fn compare
        :message " value "
        :message-op " >= "
        :exclusive ex
        :direction -1
        :bound bound}))))

(defmethod schema-key
  :schemaDefault
  [_ bound schema path c-ctx]
  ;; nop in json-schema
  )

(defmethod schema-key
  :uniqueItems
  [k uniq schema path c-ctx]
  (cond
    (= true uniq)
    (fn schema-uniq [ctx v]
      (if (and (sequential? v) (not (= (count v) (count (set v)))))
        (add-error :uniqueItems ctx "expected unique items")
        ctx))

    (fn? uniq)
    (fn schema-uniq  [ctx v]
      (let [$uniq (uniq ctx)]
        (cond
          (nil? $uniq) ctx

          (not (boolean? $uniq))
          (add-error :uniqueItems ctx (str "uniq flag ref should be boolean, but " $uniq))

          (= false $uniq) ctx

          (and (sequential? v) (not (= (count v) (count (set v)))))
          (add-error :uniqueItems ctx "expected unique items")

          :else ctx)))))

(defmethod schema-key
  :default
  [k subschema schema path c-ctx]
  (println "Unknown schema" k ": " subschema " " path)
  (when (and (empty? path) (map? subschema))
    (compile-schema subschema (conj path k) c-ctx))
  nil)

(defmethod schema-key
  :description
  [k desc schema path c-ctx]
  ;; nop
  nil)

(defmethod schema-key
  :definitions
  [k subschema schema path c-ctx]
  (when (map? subschema)
    (doseq [[k sch] subschema]
      (when (or (map? sch) (boolean? sch))
        (compile-schema sch (into path [:definitions (keyword k)]) c-ctx)))))

(defmethod schema-key
  :maxItems
  [_ bound schema path c-ctx]
  (compile-comparator
   {:name :maxItems
    :applicable-value sequential?
    :coerce-value count
    :applicable-bound number?
    :comparator-fn num-comparator 
    :message " array lenght "
    :message-op " >= "
    :direction 1
    :bound bound}))

(defmethod schema-key
  :minItems
  [_ bound schema path c-ctx]
  (compile-comparator
   {:name :minItems
    :applicable-value sequential?
    :coerce-value count
    :applicable-bound number?
    :comparator-fn num-comparator 
    :message " array lenght "
    :message-op " <= "
    :direction -1
    :bound bound}))


(def format-regexps
  {
   "date"      #"^(\d{4})-(\d{2})-(\d{2})$"
   "time"      #"^(\d{2}):(\d{2}):(\d{2})(\.\d+)?([zZ]|(\+|\-)(\d{2}):(\d{2}))?$"
   "email"     email-regex
   "hostname"  #"^([-a-zA-Z0-9]{0,64}\.)+[-a-zA-Z0-9]{0,64}$"
   "host-name" #"^([-a-zA-Z0-9]{0,64}\.)+[-a-zA-Z0-9]{0,64}$"
   "ipv4"      #"^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"
   "ipv6"      #"^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$"
   "color"     #"^(#(?:[0-9a-fA-F]{2}){2,3}|#[0-9a-fA-F]{3}|(?:rgba?|hsla?)\((?:\d+%?(?:deg|rad|grad|turn)?(?:,|\s)+){2,3}[\s\/]*[\d\.]+%?\)|black|silver|gray|white|maroon|red|purple|fuchsia|green|lime|olive|yellow|navy|blue|teal|aqua|orange|aliceblue|antiquewhite|aquamarine|azure|beige|bisque|blanchedalmond|blueviolet|brown|burlywood|cadetblue|chartreuse|chocolate|coral|cornflowerblue|cornsilk|crimson|darkblue|darkcyan|darkgoldenrod|darkgray|darkgreen|darkgrey|darkkhaki|darkmagenta|darkolivegreen|darkorange|darkorchid|darkred|darksalmon|darkseagreen|darkslateblue|darkslategray|darkslategrey|darkturquoise|darkviolet|deeppink|deepskyblue|dimgray|dimgrey|dodgerblue|firebrick|floralwhite|forestgreen|gainsboro|ghostwhite|gold|goldenrod|greenyellow|grey|honeydew|hotpink|indianred|indigo|ivory|khaki|lavender|lavenderblush|lawngreen|lemonchiffon|lightblue|lightcoral|lightcyan|lightgoldenrodyellow|lightgray|lightgreen|lightgrey|lightpink|lightsalmon|lightseagreen|lightskyblue|lightslategray|lightslategrey|lightsteelblue|lightyellow|limegreen|linen|mediumaquamarine|mediumblue|mediumorchid|mediumpurple|mediumseagreen|mediumslateblue|mediumspringgreen|mediumturquoise|mediumvioletred|midnightblue|mintcream|mistyrose|moccasin|navajowhite|oldlace|olivedrab|orangered|orchid|palegoldenrod|palegreen|paleturquoise|palevioletred|papayawhip|peachpuff|peru|pink|plum|powderblue|rosybrown|royalblue|saddlebrown|salmon|sandybrown|seagreen|seashell|sienna|skyblue|slateblue|slategray|slategrey|snow|springgreen|steelblue|tan|thistle|tomato|turquoise|violet|wheat|whitesmoke|yellowgreen|rebeccapurple)$"
   "ip-address" #"^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"
   "uri-reference"    #".*"
   "uri-template"    #".*"
   "idn-hostname" #"^.*$"
   "iri-reference" #"^.*$"
   "iri" #"^.*$"
   "idn-email" #"^.*@.*$"
   "relative-json-pointer" #"^.*$"
   "unknownformat"   #"^.*$"
   "unknown"   #"^.*$"})

(defn valid-regex? [x]
  (try 
    (re-pattern x)
    nil
    (catch Exception e
      (.getMessage e))))

(defn valid-pointer? [x]
  (if (string? x)
    (when-not (= "" x)
      (if-not (re-matches #"^/.*" x)
        "json-pointer should start with /"
        (->> (str/split x #"/")
             (rest)
             (reduce
              (fn [acc tok]
                (if (str/includes? (str/replace tok #"~[01]" "") "~")
                  (str (and acc (str acc "; "))
                       " ~ should be escaped [" tok "]")) 
                ) nil))))
    "json pointer should be string"))

(defn valid-uri? [x]
  (when (or
       (str/starts-with? x "/")
       (str/starts-with? x "\\")
       (not (str/includes? x ":"))
       (str/includes? x " ")
       (re-matches #"^http(s)?://\d+:.*" x))
    x))

(defn valid-uri-reference? [x]
  (when (or (str/starts-with? x "\\")
            (str/includes? x "\\")
            (str/includes? x " "))
    x))



(defn valid-uri-template? [x]
  (when (re-find #"\{[^}]+$" x)
    x))


(defn valid-date-time? [x]
  (try
    (java.time.LocalDate/parse x java.time.format.DateTimeFormatter/ISO_DATE_TIME)
    nil
    (catch Exception e
      (.getMessage e))))


(def format-fns
  {"regex" valid-regex?
   "uri"    valid-uri?
   "uri-reference"    valid-uri-reference?
   "uri-template"    valid-uri-template?
   "iri"    valid-uri?
   "iri-reference"    valid-uri-reference?
   "date-time" valid-date-time?
   "json-pointer"   valid-pointer?})

(defmethod schema-key
  :format
  [_ fmt schema path c-ctx]
  (if (fn? fmt)
    (fn schema-fmt [ctx v]
      (if-let [$fmt (fmt ctx)]
        (if-let [fmt-fn (and (or (string? $fmt) (keyword? $fmt))
                             (get format-fns (name $fmt)))]
          (if-let [err (fmt-fn v)]
            (add-error :format ctx (str "expected format " $fmt ", but [" err "]"))
            ctx)
          (if-let [regex (get format-regexps (if (keyword? $fmt) (name $fmt) $fmt))]
            (if (and (string? v) (not (re-find regex v)))
              (add-error :format ctx (str "expected format " $fmt))
              ctx)
            (add-error :format ctx (str "no format for " $fmt))))
        ctx))

    (if (or (string? fmt) (keyword fmt))
      (if-let [fmt-fn (get format-fns (name fmt))]
        (fn schema-fmt2 [ctx v]
          (if (and v (string? v))
            (if-let [err (fmt-fn v)]
              (add-error :format ctx (str "expected format " fmt ", but [" err "]"))
              ctx)
            ctx))
        (let [regex (get format-regexps (name fmt))]
          (fn schema-fmt3 [ctx v]
            (if (nil? regex)
              (add-error :format ctx (str "Unknown format " fmt))
              (if (and (string? v) (not (re-find regex v)))
                (add-error :format ctx (str "expected format " fmt))
                ctx)))))
      (assert false (pr-str fmt)))))

(defmethod schema-key
  :pattern
  [_ fmt schema path c-ctx]
  (cond
    (string? fmt)
    (let [regex (re-pattern fmt)]
      (fn schema-patt [ctx v]
        (if (and (string? v) (not (re-find regex v)))
          (add-error :pattern ctx (str "expected format " fmt))
          ctx)))

    (fn? fmt)
    (fn schema-patt [ctx v]
      (let [$fmt (fmt ctx)]
        (cond
          (nil? $fmt) ctx

          (not (or (string? $fmt) (keyword? $fmt)))
          (add-error :pattern ctx (str "could not interpret as pattern " $fmt))

          (and (string? v) (not (re-find (re-pattern (name $fmt)) v)))
          (add-error :pattern ctx (str "expected '" v "' matches pattern '" $fmt "'"))

          :else ctx)))))


(defmethod schema-key
  :contains
  [_ subsch schema path c-ctx]
  (let [validator (compile-schema subsch path c-ctx)]
    (fn schema-contains [ctx v]
      (if (and (sequential? v)
               (not (some (fn [vv]
                            (let [{err :errors} (validator (assoc ctx :errors []) vv)]
                              (empty? err))
                            ) v)))
        (add-error :contains ctx (str "expected contains " subsch))
        ctx))))

(defmethod schema-key
  :propertyNames
  [_ prop-schema schema path c-ctx]
  (let [validator (compile-schema prop-schema path c-ctx)]
    (fn schema-prop-names [ctx v]
      (if (map? v)
        (reduce
         (fn [ctx prop]
           (let [{err :errors} (validator (assoc ctx :errors []) (name prop))]
             (if-not (empty? err)
               (add-error :propertyNames ctx (str "Invalid property name - " (name prop) ": "
                                                  (->> err
                                                       (mapv :message)
                                                       (str/join "; "))))
               ctx)))
         ctx (keys v))
        ctx))))

(defmethod schema-key
  :subset
  [_ arr _ _ _]
  (fn schema-subset [ctx v]
    (assert (or (fn? arr) (sequential? arr)))
    (let [arr (if (fn? arr) (arr ctx) arr)]
      (if (clojure.set/subset? (set v) (set arr))
        ctx
        (add-error :subset ctx (str v " is not a subset of " arr))))))

(defmethod schema-key
  :deferred
  [_ annotation schema path c-ctx]
  (fn schema-deferred [ctx v]
    (add-deferred ctx v annotation)))

(defmethod schema-key
  :items
  [_ items {ai :additionalItems :as schema} path c-ctx]
  (cond
    (or (map? items) (boolean? items))
    (let [validator (compile-schema items (conj path :items) c-ctx)]
      (fn schema-items [ctx vs]
        (if-not (sequential? vs)
          ctx
          (if-not validator
            ctx
            (let [pth (:path ctx)
                  cnt (dec (count vs))
                  iter (.iterator ^Iterable vs)]
              (if (.hasNext iter)
                (loop [ctx ctx i 0]
                  (let [v (.next iter)
                        ctx (validator (assoc ctx :path (conj pth i)) v)]
                    (if (.hasNext iter)
                      (recur ctx (inc i))
                      ctx))) ctx))))))
    #_(reduce-indexed
       (fn schema-items-map [ctx idx v]
         (validator (assoc ctx :path (conj pth idx)) v))
       ctx vs)

    (sequential? items)
    (let [validators (doall (map-indexed (fn [idx x]
                                           (if (or (map? x) (boolean? x))
                                             (compile-schema x (into path [:items idx]) c-ctx)
                                             (assert false (pr-str "Items:" items)))) items))
          ai-validator (when-not (or (nil? ai) (boolean? ai)) (compile-schema ai path c-ctx))]
      (fn schema-items2 [ctx vs]
        (if-not (sequential? vs)
          (add-error :items ctx "expected array")
          (let [pth (:path ctx)]
            (loop [idx 0
                   validators validators
                   vs vs
                   ctx ctx]

              (let [new-ctx (assoc ctx :path (conj pth idx))]
                (cond
                  (and (empty? vs) (empty? validators)) ctx
                  (and (not (empty? vs)) (= true ai)) ctx
                  (and (not (empty? vs))
                       (empty? validators)
                       (= false ai)) (add-error :items new-ctx "additional items not allowed")

                  (and (not (empty? vs))
                       (empty? validators)
                       ai-validator) 
                  (recur  (inc idx) [] (rest vs) (ai-validator new-ctx (nth vs 0 nil)))

                  (and (not (empty? vs))
                       (not (empty? validators))) 
                  (recur (inc idx) (rest validators) (rest vs)
                         ((nth validators 0 nil) new-ctx (nth vs 0 nil)))

                  :else ctx ;; not sure

                  )))))))

    :else (assert false (pr-str "(:" schema))))


(defn compile [schema & [ctx]]
  (let [vf (compile-schema schema [] {:reg (atom {:original schema})
                                      :root-id (or (:id schema) (:$id schema))})
        ctx (merge {:path [] :errors [] :deferreds [] :warnings []} (or ctx {}))]
    (fn [v & [lctx]]
      (let [res (vf (merge (assoc ctx :doc v) (or lctx {})) v)]
        (if (map? res)
          (dissoc res :doc :path :config)
          (assert false (str "Unexpected validator result " res)))))))

(defn compile-registry [schema & [ctx]]
  (let [registry (atom {"#" :lock})
        vf (compile-schema schema [] {:reg registry})]
    (swap! registry assoc "#" vf)
    registry))

(defn validate [schema value & [ctx]]
  (let [validator (compile schema ctx)]
    (validator value ctx)))

(comment

  (validate
   {:properties
    {:finalDate {:format "date", :formatMaximum {:$data "1/beforeThan"}},
     :beforeThan {}}}
   {:finalDate "2015-11-09", :beforeThan "2015-08-17"})

  (validate {:type :object
             :properties {:name {:type "string"}
                          :extra "filed"}
             :required [:email]}
            {:name 5}
            {:config {:additionalProperties :warnings}})

  (validate {:minimum 5} 3)

  (validate {:oneOf [{:type "integer"} {:minimum 2}]} 1.5)
  (keys
   @(compile-registry
     {:patternProperties {:.* {:type "object"
                               :properties {:name {:type "string"}}}}})
   )

  (validate {:constant 5} 5)
  (validate {:constant 5} 4)

  (validate {:constant 5} 4 {:config {:warnings {:additionalProperties true}}})


  (validate
   {:properties {:sameAs {:constant {:$data "1/thisOne"}}, :thisOne {}}}
   {:sameAs 5, :thisOne 6})


  (validate {:minimum 1.1, :exclusiveMinimum true} 1.1)
  (validate {:maximum 1.1, :exclusiveMaximum true} 1.1)
  (validate {:minimum 1.1, :exclusiveMinimum true} 1.0)

  (validate {:exclusiveMinimum 1.1} 1.1)

  (validate {:maximum 1.1, :exclusiveMaximum true} 1.2)

  (validate {:maximum 1.1} 1.1)
  (validate {:minimum 2} 1.1)

  (validate {:maximum 1} 1.1)

  (validate {:minimum 1.1} 1.1)

  (num-comparator 1.1 1.1)
  (num-comparator 1.2 1.1)

  (num-comparator 0.9 1.1)

  (compare "2014-12-03" "2015-08-17")

  (validate {:maxLength 2} "aaa")

  (validate {:maxLength 2} "aaa")

  (validate {:minimum 25} 20)

  (validate
   {:properties
    {:finalDate {:format "date", :formatMaximum {:$data "1/beforeThan"}},
     :beforeThan {}}}
   {:finalDate "2015-11-09", :beforeThan "2015-08-17"})


  (validate
   {:properties {:shouldMatch {}, :string {:pattern {:$data "1/shouldMatch"}}}}
   {:shouldMatch "^a*$", :string "abc"})

  (validate
   {:switch
    [{:if {:minimum 10}, :then {:multipleOf 2}, :continue true}
     {:if {:minimum 20}, :then {:multipleOf 5}}]}
   35)

  (validate {:description "positive integer <=1000 with one non-zero digit",
             :switch [{:if {:not {:minimum 1}} :then false}
                      {:if {:maximum 10} :then true}
                      {:if {:maximum 100} :then {:multipleOf 10}}
                      {:if {:maximum 1000} :then {:multipleOf 100}}
                      {:then false}]} 1001)


  (def vvv
    (compile {:type :object
               :properties {:name {:type "string"}
                            :email {:type "string"}}
               :additionalProperties false
               :required [:email]}))

  (vvv {:name "name" :email "email@ups.com" :extra "prop"}
       {:config {:additionalProperties :warnings}})

  (vvv {:name "name" :email "email@ups.com" :extra "prop"})

  (validate {:type :object
             :properties {:name {:type "string"}
                          :email {:type "string"}}
             :additionalProperties false
             :required [:email]}
            {:name "name" :email "email@ups.com" :extra "prop"}
            {:config {:additionalProperties :warnings}})

  )
