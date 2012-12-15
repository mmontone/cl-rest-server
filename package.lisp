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
	   #:value))