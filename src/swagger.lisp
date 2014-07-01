(in-package :rest-server)

(defmacro define-swagger-resource (api-name)
  (let ((api (find-api api-name)))
    `(progn
       ;; Swagger docs API definition
       (with-api ,api-name
	 (define-api-resource api-docs (:documentation "Swagger API docs"
						       :path "/api-docs")
	   (api-docs-index (:request-method :get
					    :uri-prefix "/api-docs"
					    :produces (:json))
			   ())
	   ,@(loop for resource in (list-api-resources api)
		collect
		  (let ((fname (intern (format nil "API-DOCS-~A" (string-upcase (resource-name resource))))))
		    `(,fname (:request-method :get
					      :uri-prefix ,(format nil "/api-docs/~A" (string-downcase (resource-name resource)))
					      :produces (:json))
			     ()))))

	 ;; Swagger docs API  implementation
	 (implement-api-function ,api-name api-docs-index ()
	   (swagger-api-spec (find-api ',api-name)))
     
	 ,@(loop for resource in (list-api-resources api)
	      collect
		(let ((fname (intern (format nil "API-DOCS-~A" (string-upcase (resource-name resource))))))
		  `(implement-api-function ,api-name ,fname ()
		     (let ((api (find-api ',api-name)))
		       (swagger-resource-spec api
					      (find-api-resource ',(resource-name resource) :api api)
					      "http://localhost:8181")))))))))
  
(defun swagger-api-spec (api)
  (setf (hunchentoot:header-out "Content-Type") "application/json")
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (with-output-to-string (json:*json-output*)
    (json:with-object ()
      (json:encode-object-member :api-version (or (version api) "0.1"))
      (json:encode-object-member :swagger-version "1.2")
      (json:as-object-member (:apis)
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
	  (json:encode-object-member :title (or (title api)
						(name api)))
	  (json:encode-object-member :description (api-documentation api)))))))

(defun resource-apis (resource)
  "Get Swagger apis from the resource"
  (group-by:group-by (rest-server::list-api-resource-functions resource)
		     :key #'rest-server::uri-prefix
		     :test #'equalp
		     :value #'identity))

(defun encode-swagger-operation (operation)
  (flet ((encode-parameter (parameter required-p)
	   (if (not required-p)
	       (destructuring-bind (name type default-value documentation) parameter
		 (declare (ignore default-value))
		 (json:with-object ()
		   (json:encode-object-member :name name)
		   (json:encode-object-member :type type)
		   (json:encode-object-member :description documentation)
		   (json:encode-object-member :required :false)
		   (json:encode-object-member :allow-multiple :false)))
	       (destructuring-bind (name type documentation) parameter
		 (json:with-object ()
		   (json:encode-object-member :name name)
		   (json:encode-object-member :type type)
		   (json:encode-object-member :description documentation)
		   (json:encode-object-member :required t)
		   (json:encode-object-member :allow-multiple :false))))))
    (json:with-object ()
      (json:encode-object-member :method (string-upcase (symbol-name (request-method operation))))
      (json:encode-object-member :summary (api-summary operation))
      (json:encode-object-member :notes (api-documentation operation))
      (json:encode-object-member :nickname (symbol-name (name operation)))
      #+nil(json:encode-object-member :type (api-function-type operation))
      (when (produces operation)
	(json:encode-object-member :produces (mapcar #'mime-to-string (produces operation))))
      (when (consumes operation)
	(json:encode-object-member :consumes (mapcar #'mime-to-string (consumes operation))))      
      (json:as-object-member (:parameters)
	(json:with-array ()
	  (loop for parameter in (arguments operation)
	     do
	       (json:as-array-member ()
		 (encode-parameter parameter t))))))))
      
(defun swagger-resource-spec (api resource base-url)
  (setf (hunchentoot:header-out "Content-Type") "application/json")
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (with-output-to-string (json:*json-output*)
    (json:with-object ()
      (json:encode-object-member
       :api-version (or (version api) "0.1"))
      (json:encode-object-member
       :swagger-version "1.2")
      (json:encode-object-member
       :base-path base-url)
      (json:encode-object-member
       :resource-path (resource-path resource))
      (json:encode-object-member
       :produces (mapcar #'mime-to-string (resource-produces resource)))
      (json:as-object-member (:apis)
	(json:with-array ()
	  (loop for api in (resource-apis resource)
	     do
	       (json:as-array-member ()
		 (destructuring-bind (path &rest operations) api
		   (json:with-object ()
		     (json:encode-object-member :path path)
		     (json:as-object-member (:operations)
		       (json:with-array ()
			 (loop for operation in operations
			    do (json:as-array-member ()
				 (encode-swagger-operation operation))))))))))))))

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
       (:sexp "application/sexp")))
    (t (error "Invalid mime type ~S" mime-type))))
