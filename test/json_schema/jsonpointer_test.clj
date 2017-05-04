(ns json-schema.jsonpointer-test
  (:require
   [json-schema.schema :as sut]
   [clojure.test :refer :all]))


;; see http://tools.ietf.org/html/draft-luff-relative-json-pointer-00

(def doc
  {:foo [:bar :baz],
   :highly {:nested {:objects true}}})

(def table
  {[:foo 1] [["0" :baz]
             ["1/0" :bar]
             ["2/highly/nested/objects" true]
             ["0#"                      1]
             ["1#"                      :foo]]
   [:highly :nested] [["0/objects"                 true]
                      ["1/nested/objects"          true]
                      ["2/foo/0"                   :bar]
                      ["0#"                      :nested]
                      ["1#"                      :highly]]
   [:any]           [["#/foo/0" :bar]
                     ["#/foo/1" :baz]
                     ["#/highly/nested/objects" true]
                     ["#/uexisting" nil]]})

(deftest test-errors
  (testing "path in error"
    (doseq [[path tests] table]
      (doseq [[ref val] tests]
        (is (= val ((sut/compile-pointer ref) {:path path :doc doc}))
            (pr-str [path ref val]))))))
