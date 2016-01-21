# json-schema

[![Build Status](https://travis-ci.org/niquola/json-schema.clj.svg?branch=master)](https://travis-ci.org/niquola/json-schema.clj)


[![Clojars Project](https://img.shields.io/clojars/v/json-schema.svg)](https://clojars.org/json-schema)


[![Dependency Status](https://www.versioneye.com/user/projects/56a0a4412c2fab0029000406/badge.svg?style=flat)](https://www.versioneye.com/user/projects/56a0a4412c2fab0029000406)


Implementation JSON schema in clojure, which passes specs (https://github.com/json-schema/JSON-Schema-Test-Suite)

## Usage

```clj
(require '[json-schema.core :as sch])

(sch/valid? schema instance) ;; return true or false
(sch/validate schema instance) ;; return {:errors [...] :warnings [...]}

```


## Road Map

* add ability to custom keywords
* performance tests


## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
