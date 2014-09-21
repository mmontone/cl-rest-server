(in-package :rest-server-tests)

(in-suite rest-server-tests)

;; OAuth API tests

(defpackage :oauth-test
  (:use :rest-server :cl))

(in-package :oauth-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-api oauth-api
      (:title "Api test"
	      :documentation "This is an api test")
    (resource (:produces (:json :xml)
			 :consumes (:json)
			 :documentation "Protected resource"
			 :path "/resource"
			 :authorizations (:token :oauth))
	   (get-resource (:request-method :get
					  :produces (:json)
					  :path "/resource"
					  :documentation "Access a protected resource"
					  :authorizations (:oauth))
			 ()))))

(implement-api-function oauth-api get-resource ()
  "You have accessed the resource!!")

(rs::define-oauth-resource oauth-api)

(in-package :rest-server-tests)

(defparameter *acceptor*
  (start-api 'oauth-test::oauth-api "localhost" 8187 :development))

;(stop-api *acceptor*)

(drakma:http-request "http://localhost:8187/oauth/register")

(drakma:http-request "http://localhost:8187/oauth/token?scope=lala")

(drakma:http-request "http://localhost:8187/resource")
