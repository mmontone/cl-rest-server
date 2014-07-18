(in-package :rest-server)

(defclass logging-api-function-implementation-decoration
    (api-function-implementation-decoration)
  ()
  (:metaclass closer-mop:funcallable-standard-class))
  
(defmethod process-api-function-implementation-option
    ((option (eql :logging))
     api-function-implementation
     &key enabled)
  (if enabled
      (make-instance 'logging-api-function-implementation-decoration
		     :decorates api-function-implementation)
      api-function-implementation))
  
(defmethod execute :around ((decoration logging-api-function-implementation-decoration)
			    &rest args)
  (log5:log-for (rest-server) "API: Handling ~A ~A by ~A"
		(hunchentoot:request-method*)
		(hunchentoot:request-uri*)
		(name (api-function decoration)))
  (let ((posted-content (when (hunchentoot:raw-post-data :external-format :utf8)
			  (hunchentoot:raw-post-data :external-format :utf8))))
    (when posted-content (log5:log-for (rest-server) "Posted content: ~A" posted-content)))
  (let ((result (call-next-method)))
    (log5:log-for (rest-server) "Response: ~A" result)
    result))
