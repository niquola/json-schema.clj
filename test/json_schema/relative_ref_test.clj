(ns json-schema.relative-ref-test
  (:require  [clojure.test :refer :all]
             [json-schema.refs :refer :all]))

;; see http://tools.ietf.org/html/draft-luff-relative-json-pointer-00

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
                      ["1#"                      :highly]]})

(deftest test-errors
  (testing "path in error"
    (doseq [[start-path tests] table]
      (doseq [[path val] tests]
        (is (= val
               (resolve-relative-ref doc start-path path))
            (pr-str [start-path path val]))))))
