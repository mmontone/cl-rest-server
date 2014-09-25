(in-package :rest-server)

(defclass api-resource ()
  ((name :initarg :name
	 :initform (error "Provide the resouce name")
	 :accessor resource-name
	 :documentation "The resource name")
   (resource-path
    :initarg :path
    :initform (error "Provide the resource path")
    :accessor resource-path
    :documentation "The resource path. Should be a relative url")
   (documentation
    :initarg :documentation
    :initform nil
    :accessor resource-documentation
    :documentation "The resource documentation")
   (resource-operations :initarg :resource-operations
		  :initform (make-hash-table :test #'equalp)
		  :accessor resource-operations
		  :documentation "The api functions of the resource")
   (models :initarg :models
	   :initform nil
	   :accessor resource-models
	   :documentation "Resource models")
   (produces :initarg :produces
	     :initform nil
	     :accessor resource-produces
	     :documentation "A list of MIME types the APIs on this resource can produce. This is global to all APIs but can be overridden on specific API calls.")
   (consumes :initarg :consumes
	     :initform nil
	     :accessor resource-consumes
	     :documentation "A list of MIME types the APIs on this resource can consume. This is global to all APIs but can be overridden on specific API calls.")
   (authorizations :initarg :authorizations
		   :initform nil
		   :accessor resource-authorizations
		   :documentation "A list of authorizations schemes required for the operations listed in this API declaration. Individual operations may override this setting. If there are multiple authorization schemes described here, it means they're all applied.")
   (options :initarg :options
	    :initform nil
	    :accessor resource-options
	    :documentation "Options applied to resource api functions. Can be overwritten in api function options"))
  (:documentation "An api resource. Contains api functions"))

(defmethod print-object ((api-resource api-resource) stream)
  (print-unreadable-object (api-resource stream :type t :identity t)
    (format stream "~A ~S"
	    (resource-name api-resource)
	    (resource-path api-resource))))

(defmethod initialize-instance :after ((api-resource api-resource) &rest initargs)
  (declare (ignore initargs))

  ;; Validate the resource
  ;(validate resource-operation)

  ;; Configure the resource
  ;(configure-api-resource api-resource)

  ;; Install the resource
  (when *register-api-resource*
    (let ((api (or *api* (error "Specify the api"))))
      (setf (gethash (resource-name api-resource) (resources api))
            api-resource))))

(defmethod list-api-resource-functions ((api-resource api-resource))
  (loop for resource-operation being the hash-values of (resource-operations api-resource)
       collect resource-operation))

(defmethod allowed-methods ((api-resource api-resource))
  (let ((methods nil))
    (loop for resource-operation in (list-api-resource-functions api-resource)
	 do (pushnew (request-method resource-operation) methods))
    methods))

(defgeneric resource-http-options (resource api)
  (:method ((resource api-resource) api)
    (flet ((format-allowed-methods (methods)
	   (format nil "~{~A~^, ~}"
		   (mapcar #'string methods))))
    (setf (hunchentoot:header-out "Allow")
	  (format-allowed-methods (allowed-methods resource))))))

(defmacro with-api-resource (resource &body body)
  "Execute body under resource scope.
   Example:
   (with-api-resource users
      (define-resource-operation get-user :get (:url-prefix \"users/{id}\")
                                    '((:id :integer))))"
  `(call-with-api-resource ',resource (lambda () ,@body)))

(defun call-with-api-resource (resource function)
  (let ((*api-resource* (if (symbolp resource)
			    (find-api-resource resource)
			    resource)))
    (funcall function)))

(defun find-api-resource (name &key (error-p t) api)
  "Find api resource by name in the current api"
  (let ((api (or api
		 *api*
		 (error "No api in scope"))))
    (multiple-value-bind (resource found-p)
	(gethash name (resources api))
      (when (and (not found-p)
		 error-p)
	(error "Resource ~S not found in ~A" name api))
      resource)))

(defmacro define-api-resource (name options &body functions)
  "Define an api resource."
  `(progn
     (apply #'make-instance 
	    'api-resource 
	    :name ',name
	    ',options)
     (with-api-resource ,name
      ,@(loop for x in functions
         collect `(define-resource-operation ,@x)))
     ;; Generate client API access function
     ,@(let ((*register-resource-operation* nil))
            (loop for x in functions
               collect (client-stub
                        name
                        (destructuring-bind (name attributes args &rest options) x
                          (make-resource-operation
                           name
                           attributes
                           args
                           options))
			(or (and (getf options :package)
				 (find-package (getf options :package)))
			    *package*))))))

(defmacro implement-api-resource (api-name name-and-options &body resource-operations-implementations)
  "Define an api resource implementation"
  (multiple-value-bind (name options)
      (if (listp name-and-options)
	  (values (first name-and-options)
		  (rest name-and-options))
	  (values name-and-options nil))
    `(let* ((api (find-api ',api-name))
	    (resource (find-api-resource ',name :api api)))
       (process-api-resource-options resource ',options)

       ;; Define api function implementations
       ,@(loop for resource-operation-implementation in resource-operations-implementations
	    collect `(implement-resource-operation ,api-name ,@resource-operation-implementation)))))

(defmethod resource-execute-function-implementation ((resource api-resource)
						     resource-operation-implementation
						     request)
  (execute-resource-operation-implementation resource-operation-implementation request))

(defmethod resource-matches-request-p ((resource api-resource) request)
  (cl-ppcre:scan (format nil "^~A" (resource-path resource))
		 (hunchentoot:request-uri request)))

(defgeneric process-api-resource-option (option-name resource &rest args)
  (:method (option-name resource &rest args)
    (error "~A is not a valid resource option" option-name)))

(defun process-api-resource-options (resource options)
  (loop for option in options
       do (process-api-resource-option (first option) resource (rest option))))

(defun configure-api-resource (api-or-name resource-name &rest options)
  (let ((api (if (symbolp api-or-name)
		 (find-api api-or-name)
		 api-or-name)))
    (let ((resource (find-api-resource resource-name :api api)))
      (process-api-resource-options resource options))))
