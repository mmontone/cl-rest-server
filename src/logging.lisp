(in-package :rest-server.logging)

;; Defaults and api

(defvar *api-logging-output* *standard-output* "Where the API logging message go")

(defun start-api-logging ()
  (log5:start-sender 'api-info  
		     (log5:stream-sender :location *api-logging-output*)
		     :category-spec 'rs::rest-server
		     :output-spec '(log5::time 
				    log5::message)))

(defun stop-api-logging ()
  (log5:stop-sender 'api-info))

;; Api function logging

(defclass logging-resource-operation-implementation-decoration
    (rs::resource-operation-implementation-decoration)
  ()
  (:metaclass closer-mop:funcallable-standard-class))
  
(defmethod rs::process-resource-operation-implementation-option
    ((option (eql :logging))
     resource-operation-implementation
     &key (enabled t))
  (if enabled
      (make-instance 'logging-resource-operation-implementation-decoration
		     :decorates resource-operation-implementation)
      resource-operation-implementation))
  
(defmethod rs::execute :around ((decoration logging-resource-operation-implementation-decoration)
			    &rest args)
  (log5:log-for (rest-server::rest-server) "API: Handling ~A ~A by ~A"
		(hunchentoot:request-method*)
		(hunchentoot:request-uri*)
		(name (rest-server::resource-operation decoration)))
  (let ((posted-content (when (hunchentoot:raw-post-data :external-format :utf8)
			  (hunchentoot:raw-post-data :external-format :utf8))))
    (when posted-content (log5:log-for (rs::rest-server) "Posted content: ~A" posted-content)))
  (let ((result (call-next-method)))
    (log5:log-for (rs::rest-server) "Response: ~A" result)
    result))

(cl-annot:defannotation logging (args resource-operation-implementation)
    (:arity 2)
  `(rs::configure-resource-operation-implementation
    (name (resource-operation ,resource-operation-implementation))
    (list :logging ,@args)))

;; Api logging

(defclass logging-api ()
  ((logging-enabled :initarg :logging-enabled
		    :initform t
		    :accessor logging-enabled)))

(defmethod rest-server::process-api-option ((option (eql :logging)) api
					    &key (enabled t))			 
  (dynamic-mixins:ensure-mix api 'logging-api)
  (setf (logging-enabled api) enabled))

(defmethod rest-server::api-execute-function-implementation :around ((api logging-api) resource-operation-implementation resource request)
  (log5:log-for (rs::rest-server) "API: Handling ~A ~A by ~A"
		(hunchentoot:request-method*)
		(hunchentoot:request-uri*)
		(rs::name (rs::resource-operation resource-operation-implementation)))
  (let ((posted-content (when (hunchentoot:raw-post-data :external-format :utf8)
			  (hunchentoot:raw-post-data :external-format :utf8))))
    (when posted-content (log5:log-for (rs::rest-server) "Posted content: ~A" posted-content)))
  (let ((result (call-next-method)))
    (log5:log-for (rs::rest-server) "Response: ~A" result)
    result))

(defun enable-api-logging (api-name &optional (start t))
  (dynamic-mixins:ensure-mix (find-api api-name) 'logging-api)
  (when start
    (start-api-logging)))

(defun disable-api-logging (api-name &optional (stop t))
  (dynamic-mixins:delete-from-mix (find-api api-name) 'logging-api)
  (when stop
    (stop-api-logging)))
