(defpackage :rest-server
  (:use :cl :closer-mop)
  (:shadowing-import-from
   :closer-mop
   #:defclass
   #:standard-class
   #:defmethod
   #:standard-generic-function
   #:ensure-generic-function
   #:defgeneric
   #:defclass
   #:standard-class
   #:defmethod
   #:standard-generic-function
   #:ensure-generic-function
   #:defgeneric)
  (:export #:define-api
	   #:implement-api-function
	   #:set-reply-content-type
	   #:with-reply-content-type
	   #:with-json-reply
	   #:with-xml-reply
	   #:with-serializer
	   #:with-serializer-output
	   #:accept-serializer
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
	   #:find-api-function
	   #:with-api
	   #:list-api-resources
	   #:resource
	   #:resource-name
	   #:resource-documentation
	   #:resource-path
	   #:define-api-resource
	   #:list-api-resource-functions
	   #:define-api-function
	   #:make-api-function
	   #:format-api-function-url
	   #:format-absolute-api-function-url
	   #:with-api-backend
	   #:start-api
	   #:start-api-documentation
	   #:start-api-logging
	   #:*development-mode*
	   #:*server-development-mode*
	   #:http-error
	   #:http-not-found-error
	   #:http-internal-server-error
	   #:http-authorization-required-error
	   #:http-forbidden-error
	   #:serialize-with-schema
	   #:find-schema
	   #:schema
	   #:define-schema
	   #:serializable-class
	   #:define-serializable-class
	   #:serializable-class-schema
	   #:validation-error
	   #:self-reference
	   #:with-pagination
	   #:clear-cache))
