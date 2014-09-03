(in-package :rest-server)

(defparameter +cors-default-allow-headers+
  (list "Cache-Control"
	"Pragma"
	"Origin"
	"Authorization"
	"Content-Type"
	"X-Requested-With"))

(defparameter +cors-default-allow-methods+ (list :get :put :post))

(defclass cors-mixin ()
  ((cors-enabled :initarg :cors-enabled
		 :accessor cors-enabled
		 :initform t)
   (cors-allow-origin :initarg :cors-allow-origin
		      :accessor cors-allow-origin
		      :initform "*")
   (cors-allow-headers :initarg :cors-allow-headers
		       :accessor cors-allow-headers
		       :initform +cors-default-allow-headers+)
   (cors-allow-methods :initarg :cors-allow-methods
		       :accessor cors-allow-methods
		       :initform +cors-default-allow-methods+)))

(defun format-cors-allow-headers (headers)
  (format nil "~{~A~^, ~}" headers))

(defun format-cors-allow-methods (methods)
  (format nil "~{~A~^, ~}" (mapcar (alexandria:compose #'string-upcase
						      #'string)
				  methods)))

(defun send-cors-headers (cors)
  (when (cors-enabled cors)
    (setf (hunchentoot:header-out "Access-Control-Allow-Origin")
	  (cors-allow-origin cors))
    (setf (hunchentoot:header-out "Access-Control-Allow-Headers")
	  (format-cors-allow-headers (cors-allow-headers cors)))
    (setf (hunchentoot:header-out "Access-Control-Allow-Methods")
	  (format-cors-allow-methods (cors-allow-methods cors)))))			  

(defmethod resource-execute-function-implementation
    :after
    ((resource cors-mixin)
     api-function-implementation
     request)
  (send-cors-headers resource))

(defmethod process-api-resource-option ((option (eql :cors)) resource
					&key (enabled t)
					  (allow-origin "*")
					  (allow-headers +cors-default-allow-headers+)
					  (allow-methods +cors-default-allow-methods+))
  (dynamic-mixins:ensure-mix resource 'cors-resource)
  (setf (cors-enabled resource) enabled)
  (setf (cors-allow-origin resource) allow-origin)
  (setf (cors-allow-headers resource) allow-headers)
  (setf (cors-allow-methods resource) allow-methods))

(defclass cors-api (cors-mixin)
  ())

(defmethod api-execute-function-implementation :after
    ((api cors-api) api-function-implementation
     resource request)
  (send-cors-headers api))

(defmethod api-function-http-options :after
  ((api cors-api) api-function)
  (send-cors-headers api))

(defmethod resource-http-options :after
  (resource (api cors-api))
  (send-cors-headers api))

(defmethod process-api-option ((option (eql :cors)) api
			       &key
				 (enabled t)
				 (allow-origin "*")
				 (allow-headers +cors-default-allow-headers+)
				 (allow-methods +cors-default-allow-methods+))
  (dynamic-mixins:ensure-mix api 'cors-api)
  (setf (cors-enabled api) enabled)
  (setf (cors-allow-origin api) allow-origin)
  (setf (cors-allow-headers api) allow-headers)
  (setf (cors-allow-methods api) allow-methods))
