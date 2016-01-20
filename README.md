# json-schema

[![Build Status](https://travis-ci.org/niquola/json-schema.clj.svg?branch=master)](https://travis-ci.org/niquola/json-schema.clj)

Implementation JSON schema in clojure, which passes specs :)

## Usage

```clj
(require '[json-schema.core :as sch])

(sch/validate schema instance) ;; return true or false
(sch/check schema instance) ;; return {:errors [...] :warnings [...]}

```

## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
