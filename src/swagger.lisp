(defpackage rs.swagger
  (:use :cl :rest-server)
  (:export :define-swagger-resource))

(in-package :rs.swagger)

(defmacro define-swagger-resource (api-name)
  (let ((api (find-api api-name)))
    `(progn
       ;; Swagger docs API definition
       (with-api ,api-name
         (define-api-resource api-docs (:documentation "Swagger API docs"
                                                       :path "/api-docs")
           (api-docs-index (:request-method :get
                                            :path "/api-docs"
                                            :produces (:json))
                           ())
           ,@(loop for resource in (list-api-resources api)
                collect
                  (let ((fname (intern (format nil "API-DOCS-~A" (string-upcase (resource-name resource))))))
                    `(,fname (:request-method :get
                                              :path ,(format nil "/api-docs~A" (resource-path resource))
                                              :produces (:json))
                             ()))))

         ;; Swagger docs API  implementation
         (implement-resource-operation ,api-name api-docs-index ()
           (swagger-api-docs-index (find-api ',api-name)))

         ;; Configure the API to work with Swagger
         (configure-api ',api-name
                        '(:swagger)
                        '(:cors :enabled t))

         ,@(loop for resource in (list-api-resources api)
              collect
                (let ((fname (intern (format nil "API-DOCS-~A" (string-upcase (resource-name resource))))))
                  `(implement-resource-operation ,api-name ,fname ()
                     (let ((api (find-api ',api-name)))
                       (swagger-resource-spec api
                                              (find-api-resource ',(resource-name resource) :api api)
                                              (format nil "http://~A:~A"
                                                      (hunchentoot:acceptor-address hunchentoot:*acceptor*)
                                                      (hunchentoot:acceptor-port hunchentoot:*acceptor*)))))))))))

(defun swagger-api-docs-index (api)
  (setf (hunchentoot:header-out "Content-Type") "application/json")
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (with-output-to-string (json:*json-output*)
    (encode-swagger-api api json:*json-output*)))

(defun encode-swagger-api (api stream)
  (let ((json:*json-output* stream))
    (json:with-object ()
      (json:encode-object-member :api-version (or (rs::version api) "0.1"))
      (json:encode-object-member :swagger "2.0")
      (json:as-object-member (:paths)
                             (json:with-array ()
                               (loop for resource in (list-api-resources api)
                                  when (not (equalp (resource-name resource) 'api-docs))
                                  do
                                    (json:as-array-member ()
                                      (json:with-object ()
                                        (json:encode-object-member :path (resource-path resource))
                                        (json:encode-object-member :description (resource-documentation resource)))))))
      (json:as-object-member (:info)
                             (json:with-object ()
                               (json:encode-object-member :title (or (rs::title api)
                                                                     (rs::name api)))
                               (json:encode-object-member :description (rs::api-documentation api))))
      (json:as-object-member (:paths)
                             (json:with-object ()
                               (loop for path in (list-api-paths api)
                                  do
                                    (encode-swagger-path path stream))))
      (json:as-object-member (:definitions)
                             (encode-swagger-definitions
                              schemata::*schemas*))
      )))

(defun list-api-paths (api)
  (let ((api-operations (alexandria:flatten
                         (loop for resource in (list-api-resources api)
                            collect (alexandria:hash-table-values
                                     (rs::resource-operations resource))))))
    (group-by:group-by api-operations
                       :key #'rest-server::path
                       :test #'equalp
                       :value #'identity)))

(defun resource-apis (resource)
  "Get Swagger apis from the resource"
  (group-by:group-by (rest-server::list-api-resource-functions resource)
                     :key #'rest-server::path
                     :test #'equalp
                     :value #'identity))

(defgeneric encode-argument-type (argument-type &optional stream)
  (:method ((argument-type rs::argument-type) &optional (stream json:*json-output*))
    (json:encode-object-member :type (string-downcase (princ-to-string (rs::argument-type-spec argument-type)))) stream)
  (:method ((argument-type rs::string-argument-type) &optional (stream json:*json-output*))
    (json:encode-object-member :type "string" stream))
  (:method ((argument-type rs::integer-argument-type) &optional (stream json:*json-output*))
    (json:encode-object-member :type "integer" stream))
  (:method ((argument-type rs::boolean-argument-type) &optional (stream json:*json-output*))
    (json:encode-object-member :type "boolean" stream)))  

(defun encode-swagger-operation (operation &optional (stream json:*json-output*))
  (let ((json:*json-output* stream))
    (flet ((encode-parameter (parameter required-p)
             (if (not required-p)
                 (with-accessors ((name rs::argument-name)
                                  (type rs::argument-type)
                                  (default-value rs::argument-default)
                                  (documentation rs::argument-documentation))
                     parameter
                   (declare (ignore default-value))
                   (json:with-object ()
                     (json:encode-object-member :name name)
                     (encode-argument-type type stream)
                     (json:encode-object-member :in "query")
                     (json:encode-object-member :description documentation)
                     (json:encode-object-member :required :false)
                     (json:encode-object-member :allow-multiple :false)))
                 (with-accessors ((name rs::argument-name)
                                  (type rs::argument-type)
                                  (documentation rs::argument-documentation))
                     parameter
                   (json:with-object ()
                     (json:encode-object-member :name name)
                     (encode-argument-type type stream)
                     (json:encode-object-member :in "path")
                     (json:encode-object-member :description documentation)
                     (json:encode-object-member :required t)
                     (json:encode-object-member :allow-multiple :false))))))
      (json:as-object-member ((string-downcase (princ-to-string (rs::request-method operation))))
                             (json:with-object ()
                               (json:encode-object-member :method (string-upcase (symbol-name (rs::request-method operation))))
                               (json:encode-object-member :summary (or
                                                                    (rs::api-summary operation)
                                                                    (and (rs::api-documentation operation)
                                                                         (subseq (rs::api-documentation operation) 0 (min 30 (length (rs::api-documentation operation)))))))
                               (json:encode-object-member :description (rs::api-documentation operation))
                               (json:encode-object-member :operation-id (string-downcase (symbol-name (rs::name operation))))
                               #+nil(json:encode-object-member :type (resource-operation-type operation))
                               (when (rs::produces operation)
                                 (json:encode-object-member :produces (mapcar #'mime-to-string (rs::produces operation))))
                               (when (rs::consumes operation)
                                 (json:encode-object-member :consumes (mapcar #'mime-to-string (rs::consumes operation))))
                               (json:as-object-member (:parameters)
                                                      (json:with-array ()
                                                        (loop for parameter in (rs::required-arguments operation)
                                                           do
                                                             (json:as-array-member ()
                                                               (encode-parameter parameter t)))
                                                        (loop for parameter in (rs::optional-arguments operation)
                                                           do
                                                             (json:as-array-member ()
                                                               (encode-parameter parameter nil)))
                                                        (when (member (rs::request-method operation) (list :post :put :patch))
                                                          (json:as-array-member ()
                                                            (json:with-object ()
                                                              (json:encode-object-member :name "body")
                                                              (json:encode-object-member :in "body")
                                                              (json:as-object-member (:schema)
                                                                                     (json:with-object ()
                                                                                       (json:encode-object-member "$ref" (format nil "#/definitions/~A" (string-downcase (princ-to-string (body-schema operation)))))))
                                                              (json:encode-object-member :description "The content")
                                                              (json:encode-object-member :required t)
                                                              #+nil(json:encode-object-member :allow-multiple :false))))))
                               #+nil(json:as-object-member (:security)
                                                           (encode-swagger-authorizations (authorizations operation)))
                               )))))

(defun encode-schema-ref (schema &optional (stream json:*json-output*))
  (let ((json:*json-output* stream))
    (json:with-object ()
      (json:encode-object-member "$ref" (format nil "#/definitions/~A" schema)))))

(defun encode-swagger-path (path stream)
  (let ((json:*json-output* stream))
    (json:as-object-member ((first path))
                           (json:with-object ()
                             (loop for operation in (cdr path)
                                do (encode-swagger-operation operation stream))))))

(defun encode-swagger-resource (resource stream)
  (let ((json:*json-output* stream))
    (json:as-object-member ((resource-path resource))
                           (json:with-object ()
                             (loop for operation in (alexandria:hash-table-values (rs::resource-operations resource))
                                do (encode-swagger-operation operation stream))))))

(defun swagger-resource-spec (api resource base-url)
  (declare (ignore api base-url))
  (setf (hunchentoot:header-out "Content-Type") "application/json")
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (with-output-to-string (json:*json-output*)
    (encode-swagger-resource resource json:*json-output*)))

(defun mime-to-string (mime-type)
  (cond
    ((stringp mime-type)
     mime-type)
    ((keywordp mime-type)
     (ecase mime-type
       (:json "application/json")
       (:xml "application/xml")
       (:plain "text/plain")
       (:html "text/html")
       (:sexp "application/sexp")
       (:raw "application/octets")))
    (t (error "Invalid mime type ~S" mime-type))))

(defun encode-swagger-definitions (schemas &optional (stream json:*json-output*))
  (json:with-object (stream)
    (loop for schema-name being the hash-keys of schemas
       using (hash-value schema)
       do
         (json:as-object-member ((string-downcase (princ-to-string schema-name)) stream)
                                (encode-swagger-definition schema stream)))))

(defun encode-swagger-definition (schema-object &optional (stream json:*json-output*))
  (let ((json:*json-output* stream))
    (flet ((encode-definition-property (attribute)
             (json:as-object-member ((schemata::attribute-name attribute))
                                    (json:with-object ()
                                      (let ((attribute-type (schemata::attribute-type attribute)))
                                        (cond
                                          ((keywordp attribute-type) ;; A primitive type
                                           (json:encode-object-member
                                            :type
                                            (string-downcase (princ-to-string attribute-type))))
                                          ((symbolp attribute-type) ;; A schema reference
                                           (json:encode-object-member
                                            "$ref" (format nil "#/definitions/~A" (string-downcase (princ-to-string attribute-type)))))
                                          ((listp attribute-type) ;; A composite type
                                           (ecase (first attribute-type)
                                             (:list (json:encode-object-member
                                                     :type "array")
                                                    (json:as-object-member (:items)
                                                                           (json:with-object ()
                                                                             (cond
                                                                               ((keywordp (second attribute-type))
                                                                                (json:encode-object-member :type
                                                                                                           (string-downcase
                                                                                                            (princ-to-string (second attribute-type)))))
                                                                               ((symbolp (second attribute-type))
                                                                                (json:encode-object-member "$ref"
                                                                                                           (format nil "#/definitions/~A"
                                                                                                                   (string-downcase (princ-to-string (second attribute-type))))))
                                                                               (t (json:encode-object-member :type "object")
                                                                                ;(error "Don't know how to encode this")
                                                                                  )))))
                                             (:option (json:encode-object-member :enum
                                                                                 (rest attribute-type)))
                                             (:object
                                              (json:encode-object-member :type "object")
                                        ;(error "Not implemented yet")
                                              )))))
                                      (let ((desc (or (schemata::attribute-option :description attribute)
                                                      (schemata::attribute-option :documentation attribute))))
                                        (when desc
                                          (json:encode-object-member :description desc)))))))
      (json:with-object ()
        (json:encode-object-member :type "object")
        (json:encode-object-member :required
                                   (mapcar #'schemata::attribute-name
                                           (remove-if
                                            #'schemata::attribute-optional-p
                                            (schemata::object-attributes schema-object))))
        (json:as-object-member (:properties)
                               (json:with-object ()
                                 (loop for attribute in (schemata::object-attributes schema-object)
                                    do
                                      (encode-definition-property attribute))))))))

(defun encode-swagger-authorization (authorization)
  (json:with-object ()
    (json:encode-object-member :type (cond
                                       ((subtypep authorization 'rs.auth::token-authorization)
                                        "apiKey")
                                       ((subtypep authorization 'rs.auth::oauth2-authorization)
                                        "oauth2")
                                       (t (error "Not implemented"))))
    (json:encode-object-member :pass-as "header")
    (when (subtypep authorization 'rs.auth::token-authorization)
      (json:encode-object-member :keyname ""))))

(defun encode-swagger-authorizations (authorizations)
  (json:with-object ()
    (loop for authorization in authorizations
       do (json:as-object-member ((class-name (class-of authorization)))
                                 (encode-swagger-authorization authorization)))))

(defclass swagger-resource (api-resource)
  ())

(defmethod resource-execute-function-implementation
    :after
    ((resource swagger-resource)
     resource-operation-implementation
     request)
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*"))

(defmethod process-api-resource-option ((option (eql :swagger)) resource &rest args)
  (declare (ignore args))
  (dynamic-mixins:ensure-mix resource 'swagger-resource))

(defclass swagger-api (api-definition)
  ())

(defmethod api-execute-function-implementation :after
    ((api swagger-api) resource-operation-implementation
     resource request)
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*"))

(defmethod process-api-option ((option (eql :swagger)) api &rest args)
  (declare (ignore args))
  (dynamic-mixins:ensure-mix api 'swagger-api))
