(defpackage :rest-server
  (:use :cl)
  (:export #:define-api
	   #:implement-api-function
	   #:with-serializer
	   #:with-serializer-output
	   #:element
	   #:attribute
	   #:elements
	   #:serialize
	   #:with-element
	   #:with-attribute
	   #:with-elements-list
	   #:with-list-member
	   #:add-list-member
	   #:set-attribute
	   #:name
	   #:attributes
	   #:value
	   #:find-api
	   #:with-api
	   #:define-api-function
	   #:make-api-function
	   #:with-api-backend
	   #:start-api
	   #:start-api-documentation
	   #:*development-mode*
	   #:http-error
	   #:http-not-found-error
	   #:http-internal-server-error
	   #:http-authorization-required-error
	   #:http-forbidden-error
	   #:serialize-with-schema
	   #:find-schema
	   #:schema
	   #:define-schema))