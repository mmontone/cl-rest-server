(defpackage :rest-server-tests
  (:use :cl 
	:rest-server 
	:fiveam
	:rest-server.serialize
	:rest-server.schema
	:rest-server.logging
	:rest-server.mop)
  (:export :run-tests :debug-tests))

(defpackage :api-test
  (:use :rest-server 
	:rest-server.serialize
	:rest-server.schema
	:rest-server.logging
	:rest-server.error
	:cl))

(in-package :rest-server-tests)

(def-suite rest-server-tests :description "rest-server system tests")

(defvar *api-acceptor*)

(defun run-tests ()
  (let ((*api-acceptor*
	 (start-api 'api-test::api-test "localhost" 8181)))
    (run! 'rest-server-tests)
    (stop-api *api-acceptor*)))

(defun debug-tests ()
  (debug! 'rest-server-tests))

(in-suite rest-server-tests)
