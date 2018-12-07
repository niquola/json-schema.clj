# json-schema

[![Build Status](https://travis-ci.org/niquola/json-schema.clj.svg?branch=master)](https://travis-ci.org/niquola/json-schema.clj)


[![Clojars Project](https://img.shields.io/clojars/v/json-schema.svg)](https://clojars.org/json-schema)


[![Dependency Status](https://www.versioneye.com/user/projects/56a0a4412c2fab0029000406/badge.svg?style=flat)](https://www.versioneye.com/user/projects/56a0a4412c2fab0029000406)


Implementation JSON schema in Clojure, which passes specs (https://github.com/json-schema/JSON-Schema-Test-Suite)

## Usage

```clj
(require '[json-schema.core :as sch])

(sch/valid? schema instance) ;; return true or false
(sch/validate schema instance) ;; return {:errors [...] :warnings [...]}

```

## Partial support for v5

* constant
* contains

See https://github.com/json-schema/json-schema/wiki/v5-Proposals

## Road Map

* errors formatting
* add the ability to use custom keywords
* performance tests
* v5 support


## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
