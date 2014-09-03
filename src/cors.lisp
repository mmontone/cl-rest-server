(in-package :rest-server)

(defclass cors-resource (api-resource)
  ((cors-enabled :initarg :cors-enabled
		 :accessor cors-enabled
		 :initform t)
   (cors-origin :initarg :cors-origin
	   :accessor cors-origin
	   :initform "*")))

(defmethod resource-execute-function-implementation
    :after
    ((resource cors-resource)
     api-function-implementation
     request)
  (when (cors-enabled resource)
    (setf (hunchentoot:header-out "Access-Control-Allow-Origin")
	  (cors-origin resource))))

(defmethod process-api-resource-option ((option (eql :cors)) resource &key (enabled t) (origin "*"))
  (dynamic-mixins:ensure-mix resource 'cors-resource)
  (setf (cors-enabled resource) enabled)
  (setf (cors-origin resource) origin))

(defclass cors-api (api-definition)
  ((cors-enabled :initarg :cors-enabled
		 :accessor cors-enabled
		 :initform t)
   (cors-origin :initarg :cors-origin
	   :accessor cors-origin
	   :initform "*")))

(defmethod api-execute-function-implementation :after
    ((api cors-api) api-function-implementation
     resource request)
  (when (cors-enabled api)
    (setf (hunchentoot:header-out "Access-Control-Allow-Origin")
	  (cors-origin api))))

(defmethod process-api-option ((option (eql :cors)) api &key (enabled t) (origin "*"))
  (dynamic-mixins:ensure-mix api 'cors-api)
  (setf (cors-enabled api) enabled)
  (setf (cors-origin api) origin))
