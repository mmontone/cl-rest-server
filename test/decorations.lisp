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

(implement-resource-operation decorated-api simple-decoration ()
  "hello")

(defclass simple-api-decoration (rs::api-definition)
  ())

(defmethod rs::api-execute-function-implementation :around
    ((api-definition simple-api-decoration)
     resource-operation-implementation
     resource
     request)
  (let ((reply (call-next-method)))
    (format nil "<API DECORATION>~A</API DECORATION>" reply)))

(defmethod rs::process-api-option
    ((option-name (eql :simple-decoration))
     api &rest args)
  (declare (ignorable args))
  (dynamic-mixins:ensure-mix api 'simple-api-decoration))

(rs::configure-api 'decorated-api '(:simple-decoration))

(find-api 'decorated-api)

(start-api 'decorated-api "localhost" 8085)

(drakma:http-request "http://localhost:8085/decorations/simple-decoration")

(rs::configure-api 'decorated-api '(:swagger))

(drakma:http-request "http://localhost:8085/decorations/simple-decoration")

(defclass simple-resource-decoration ()
  ())

(defmethod rs::process-api-resource-option ((option (eql :simple-decoration)) resource &rest args)
  (declare (ignorable args))
  (dynamic-mixins:ensure-mix resource 'simple-resource-decoration))

(defmethod rs::resource-execute-function-implementation :around
    ((resource simple-resource-decoration)
     resource-operation-implementation
     request)
  (let ((reply (call-next-method)))
    (format nil "<RESOURCE DECORATION>~A</RESOURCE DECORATION>" reply)))

(rs::configure-api-resource 'decorated-api 'decorations '(:simple-decoration))

(drakma:http-request "http://localhost:8085/decorations/simple-decoration")
