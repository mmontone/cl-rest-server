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

(def-fixture api-fixture ()
  (let ((*api-acceptor* (start-api 'api-test::api-test :port 8181 
								   :access-log-destination nil)))
	(rs::with-text-content-types 
	  (&body))
    (stop-api *api-acceptor*)))

(defvar *auth-api*)  

(def-fixture auth-api-fixture ()
  (let ((*auth-api* (start-api 'auth-api-test :port 8182
							   :access-log-destination nil)))
	(rs::with-text-content-types 
	  (&body))
    (stop-api *auth-api*)))

(defmacro deftest (name &body body)
  `(test (,name :compile-at :definition-time)
     ,@body))

(defun run-tests ()
  (with-fixture api-fixture ()
		(with-fixture auth-api-fixture ()
			      (run! 'rest-server-tests))))

(defun debug-tests ()
  (with-fixture api-fixture ()
		(with-fixture auth-api-fixture ()
			      (debug! 'rest-server-tests))))

(in-suite rest-server-tests)
