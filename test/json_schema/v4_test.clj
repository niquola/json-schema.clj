(ns json-schema.v4-test
  (:require
   [json-schema.core :as sut]
   [json-schema.utils :as u]
   [clojure.test :refer :all]))

(def re-filter #"^.*$")

(deftest draft4-test


  (testing
      "valid definition"
    (let [validator (sut/compile {:$ref "http://json-schema.org/draft-04/schema#"})
          res (validator {:definitions {:foo {:type "integer"}}})]
      (is (= true (empty? (:errors res))))
      res))


  "invalid definition"
  (let [validator (sut/compile {:$ref "http://json-schema.org/draft-04/schema#"})
        res (validator {:definitions {:foo {:type 1}}})]
    (is (= false (empty? (:errors res))))
    res)


  "base URI change"
  (u/with-server 
    #(let [validator (sut/compile {:id "http://localhost:1234/",:items {:id "folder/", :items {:$ref "folderInteger.json"}}})
          res (validator [[1]])]
      (is (= true (empty? (:errors res))))
      res))


  "base URI change - change folder in subschema"
  (u/with-server 
    #(let [validator (sut/compile {:id "http://localhost:1234/scope_change_defs2.json",
                                  :type "object",
                                  :properties {:list {:$ref "#/definitions/baz/definitions/bar"}},
                                  :definitions {:baz
                                                {:id "folder/",
                                                 :definitions
                                                 {:bar {:type "array", :items {:$ref "folderInteger.json"}}}}}})
          res (validator {:list [1]})]
      (is (= true (empty? (:errors res))))
      res))

  "escaped pointer ref"
  (let [validator (sut/compile {(keyword ":tilda~field ") {:type "integer"},
                                :slash/field {:type "integer"},
                                (keyword "percent%field") {:type "integer"},
                                :properties {:tilda {:$ref "#/tilda~0field"},
                                             :slash {:$ref "#/slash~1field"},
                                             :percent {:$ref "#/percent%25field"}}})
        res (validator {:slash 123})]
    (is (= true (empty? (:errors res))))
    res)


  "Recursive references between schemas"
  (let [validator (sut/compile {:id "http://localhost:1234/tree",
                                :description "tree of nodes",
                                :type "object",
                                :properties
                                {:meta {:type "string"},
                                 :nodes {:type "array", :items {:$ref "node"}}},
                                :required ["meta" "nodes"],
                                :definitions
                                {:node {:id "http://localhost:1234/node",
                                  :description "node",
                                  :type "object",
                                  :properties {:value {:type "number"}, :subtree {:$ref "tree"}},
                                  :required ["value"]}}})
        res (validator {:meta "root",
                        :nodes
                        [{:value 1,
                          :subtree {:meta "child", :nodes [{:value 1.1} {:value 1.2}]}}
                         {:value 2,
                          :subtree {:meta "child", :nodes [{:value 2.1} {:value 2.2}]}}]})]
    (is (= true (empty? (:errors res))))
    res)

  (u/test-files (u/files "draft4" re-filter)
                #{"an array of schemas for items"
                  "ECMA 262 regex non-compliance"
                  "base URI change - change folder in subschema"
                  "ref overrides any sibling keywords"
                  "escaped pointer ref"
                  "allOf with base schema"
                  "validation of URIs"}))

