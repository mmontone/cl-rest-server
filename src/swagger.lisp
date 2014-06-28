(in-package :rest-server)

(defmacro define-swagger-resource (api)
  `(with-api ,(name api)
     (define-api-resource api-docs (:documentation "Swagger API docs"
						   :path "/api-docs")
	 (api-docs-index (:request-method :get
					  :uri-prefix "/api-docs"
					  :produces (:json))
			 ())
	 ,@(loop for resource in (list-api-resources api)
		collect `(,(intern (format nil "API-DOCS-~A" (string-upcase (resource-name resource))))
			   (:request-method :get
					    :uri-prefix ,(format nil "/api-docs/~A" (string-downcase (resource-name resource)))
					    :produces (:json))
			   ())))))
  
(defun swagger-api-spec (api)
  (with-output-to-string (json:*json-output*)
    (json:with-object ()
      (json:encode-object-member :api-version (or (version api) "0.1"))
      (json:encode-object-member :swagger-version "1.2")
      (json:as-object-member (:apis)
	(loop for resource in (list-api-resources api)
	   do
	     (json:with-object ()
	       (json:encode-object-member :path (resource-path resource))
	       (json:encode-object-member :description (resource-documentation resource)))))
      (json:as-object-member (:info)
	(json:encode-object-member :title (or (title api)
					      (name api)))
	(json:encode-object-member :description (api-documentation api))))))

(defun swagger-resource-spec (api resource base-url)
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
	  (loop for function in (list-api-resource-functions resouce)
	       do
	       (json:with-object ()
		 (json:encode-object-member :path (uri-prefix function))
		 (json:as-object-member (:operations)
		   
		   ))
      
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

(defun export-swagger-spec (api directory)
  (ensure-directories-exist directory)
  (let ((api-filename (merge-pathnames (name api) directory)))
    (with-open-file (f pathname :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (swagger-encode-api api f)))
  (ensure-directories-exist 
  (loop for resource in (list-api-resources api)
       do
       (let ((resource-filename (

(defun create-swagger-spec (api)
  
		     
