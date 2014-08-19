Common Lisp REST Server
=======================

*rest-server* is a library for writing REST Web APIs in Common Lisp

[![Build Status](https://travis-ci.org/mmontone/cl-rest-server.svg?branch=master)](https://travis-ci.org/mmontone/cl-rest-server)

Features:

* Method matching
  - Based on HTTP method (GET, PUT, POST, DELETE)
  - Based on Accept request header
  - URL parsing (argument types)

* Serialization
  - Different serialization types (JSON, XML, S-expressions)

* Error handling
  - Development and production modes

* Validation via schemas

* Annotations for api logging, caching, permission checking, and more.

* Authentication
  - Different methods (token based, oauth)

* Documentation
  - Via Swagger: http://swagger.wordnik.com
