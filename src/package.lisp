(defpackage :rest-server
  (:nicknames :rs)
  (:use :cl)
  (:export #:define-api
	   #:implement-resource-operation
	   #:configure-resource-operation-implementation
	   #:configure-api
	   #:configure-api-resource
	   #:implement-resource-operation-case
	   #:set-reply-content-type
	   #:with-reply-content-type
	   #:with-json-reply
	   #:with-xml-reply
	   #:with-posted-content
	   #:find-api
	   #:find-resource-operation
	   #:with-api
	   #:list-api-resources
	   #:resource
	   #:resource-name
	   #:resource-documentation
	   #:resource-path
	   #:define-api-resource
	   #:with-api-resource
	   #:list-api-resource-functions
	   #:define-swagger-resource
	   #:define-resource-operation
	   #:make-resource-operation
	   #:format-resource-operation-url
	   #:format-absolute-resource-operation-url
	   #:with-api-backend
	   #:start-api
	   #:stop-api
	   #:start-api-documentation
	   #:self-reference
	   #:with-pagination
	   #:with-content
	   #:with-permission-checking
	   #:clear-cache
	   ;; Decorations
	   #:caching
	   #:fetch-content
	   #:permission-checking
	   #:cors-api
	   #:cors-resource))

(defpackage #:rest-server.logging
  (:nicknames #:rs.log)
  (:use :cl :rest-server)
  (:export #:start-api-logging
	   #:stop-api-logging
	   #:enable-api-logging
	   #:disable-api-logging
	   #:logging-api
	   #:logging
	   #:*api-logging-output*
	   #:logging-enabled))

(defpackage #:rest-server.serialize
  (:nicknames #:rs.serialize)
  (:use :cl :rest-server)
  (:export
   #:with-serializer
   #:with-serializer-output
   #:accept-serializer
   #:element
   #:attribute
   #:elements
   #:serialize
   #:with-element
   #:with-attribute
   #:with-list
   #:with-list-member
   #:add-list-member
   #:set-attribute
   #:name
   #:attributes
   #:value
   #:boolean-value
   #:list-value
   #:serialize-value
   #:serialization
   #:unserialization
   #:*default-serializer*))

(defpackage #:rest-server.schema
  (:nicknames #:rs.schema)
  (:use :cl :rest-server :rs.serialize)
  (:export #:serialize-with-schema
	   #:find-schema
	   #:schema
	   #:define-schema
	   #:validation
	   #:parse-with-schema
	   #:unserialize-with-schema))

(defpackage #:rest-server.mop
  (:nicknames #:rs.mop)
  (:use :cl :rest-server :rs.serialize :rs.schema)
  (:export #:serializable-class
	   #:serializable-class-schema
	   #:define-serializable-class))

(defpackage #:rest-server.error
  (:nicknames #:rs.error)
  (:use #:cl #:rest-server :rs.serialize)
  (:export #:*development-mode*
	   #:*server-development-mode*
	   #:http-error
	   #:http-not-found-error
	   #:http-internal-server-error
	   #:http-authorization-required-error
	   #:http-forbidden-error
	   #:http-not-acceptable-error
	   #:http-unsupported-media-type-error
	   #:validation-error
	   #:error-handling
	   #:with-condition-handling))
