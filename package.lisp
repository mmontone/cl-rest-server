(defpackage :rest-server
  (:use :cl)
  (:export #:define-api
	   #:implement-api-function
	   #:with-serializer
	   #:with-serializer-output
	   #:element
	   #:attribute
	   #:elements))