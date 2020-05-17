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
           #:*extract-reply-content-type-from-url*
           #:*default-reply-content-type*
           #:with-json-reply
           #:with-xml-reply
           #:with-posted-content
           #:with-content
           #:find-api
           #:find-resource-operation
           #:with-api
           #:list-api-resources
           #:resource
           #:resource-name
           #:resource-documentation
           #:resource-path
           #:body-schema
           #:define-api-resource
           #:with-api-resource
           #:list-api-resource-functions
           #:define-resource-operation
           #:make-resource-operation
           #:format-resource-operation-url
           #:format-absolute-resource-operation-url
           #:with-api-backend
           #:start-api
           #:stop-api
           #:*debug-mode*
           #:*server-debug-mode*
           #:start-api-documentation
           #:api-docs-mixin
           #:self-reference
           #:with-pagination
           #:with-resource
           #:let-resource
           #:let-resource*
           #:with-permission-checking
           #:clear-cache
           #:&posted-content
           #:&resource-operation
           #:*signal-client-function-errors*
           #:with-signal-client-function-errors
           #:encode-file-data-uri-scheme
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
           #:api-log-for
           #:logging-api
           #:logging
           #:logging-enabled))

(defpackage #:rest-server.error
  (:nicknames #:rs.error)
  (:use #:cl #:rest-server :generic-serializer)
  (:export
   #:*catch-errors*
   #:*server-catch-errors*
   #:http-error
   #:http-not-found-error
   #:http-internal-server-error
   #:http-authorization-required-error
   #:http-forbidden-error
   #:http-not-acceptable-error
   #:http-unsupported-media-type-error
   #:http-bad-request
   #:http-method-not-allowed-error
   #:error-handling
   #:with-error-handler))

(defpackage #:rest-server.auth
  (:nicknames #:rs.auth)
  (:use #:cl #:rest-server)
  (:export
   #:authorization
   #:oauth2-authorization
   #:token-authorization
   #:*auth*
   #:authorize
   #:auth-result
   #:auth-success
   #:auth-not-present
   #:auth-fail))
