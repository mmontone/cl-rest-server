(in-package :rest-server-tests)

(define-api decorated-api
    (:title "Decorated"
	    :documentation "This is a decorated api")
  (decorations (:produces (:json)
			 :consumes (:json)
			 :documentation "Decorations test"
			 :path "/decorations")
	       (simple-decoration (:produces (:json)
					     :consumes (:json)
					     :documentation "Decorations test"
					     :path "/decorations/simple-decoration")
				  ())))

(defclass simple-api-decoration (rs::api-implementation-decoration)
  ()
  (:metaclass rs::decorator-class))

(defmethod rs::api-execute-function-implementation :around
    ((api-definition simple-api-decoration)
     api-function-implementation
     resource
     request)
  (let ((reply (call-next-method)))
    (format nil "<API DECORATION>~A</API DECORATION>" reply)))

(defmethod rs::process-api-implementation-option ((option-name (eql :simple-decoration))
						  api-implementation &rest args)
  (make-instance 'simple-api-decoration :decoratee api-implementation))

(rs::configure-api-implementation 'decorated-api '(:simple-decoration))

(defclass swagger-resource-decoration (rs::api-resource-decoration)
  ()
  (:metaclass rs::decorator-class))

(defmethod rs::resource-execute-function-implementation :around
    ((resource swagger-resource-decoration)
     api-function-implementation
     request)
  (let ((reply (call-next-method)))
    (format nil "<SWAGGER>~A</SWAGGER>" reply)))

(defmethod rs::process-resource-implementation-option ((option-name (eql :swagger))
						       resource &rest args)
  (make-instance 'swagger-resource-decoration :decoratee resource))
