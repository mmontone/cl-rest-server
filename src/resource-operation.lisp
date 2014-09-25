(in-package :rest-server)

(defparameter *resource-operation-arguments-types*
  (list :boolean
	:integer
	:string
	:list))

(defparameter *default-reply-content-type* "application/json")

(defun set-reply-content-type (content-type)
  (setf (hunchentoot:header-out "Content-Type") content-type))

(defun call-with-reply-content-type (content-type function)
  (set-reply-content-type content-type)
  (funcall function))

(defmacro with-reply-content-type ((content-type) &body body)
  `(call-with-reply-content-type ,content-type (lambda () ,@body)))

(defmacro with-json-reply (&body body)
  `(with-reply-content-type ("application/json")
     ,@body))

(defmacro with-xml-reply (&body body)
  `(with-reply-content-type ("application/xml")
     ,@body))

(defmacro with-posted-content (args posted-content &body body)
  `(let ,(loop for arg in args
	    collect `(,arg (cdr (assoc ,(make-keyword arg) ,posted-content))))
     ,@body))

(defmacro define-resource-operation (name attributes args &rest options)
  "Helper macro to define an api function"
  `(make-resource-operation ',name ',attributes ',args ',options))

(defclass resource-operation ()
  ((name :initarg :name
	 :accessor name
	 :initform (error "Provide the name"))
   (path :initarg :path
	 :accessor path
	 :initform (error "Provide the api function path")
	 :documentation "The api function path")
   (request-method :initarg :request-method
		   :accessor request-method
		   :initform :get)
   (required-arguments :initarg :required-arguments
                       :accessor required-arguments
                       :initform nil)
   (optional-arguments :initarg :optional-arguments
                       :accessor optional-arguments
                       :initform nil)
   (body-type :initarg :body-type
	      :initform :string
	      :accessor body-type
	      :documentation "The type of the body argument (when request-method is :PUT or :POST)")
   (consumes :initarg :consumes
	     :accessor consumes
	     :initform nil
	     :documentation "MIME types that the resource-operation consumes. Overwrites MIME types defined in the resource")
   (produces :initarg :produces
	     :accessor produces
	     :initform nil
	     :documentation "MIME types that the resource-operation produces. Overwrites MIME types defined in the resource")
   (resource :initarg :resource
	     :accessor resource
	     :initform (when *register-resource-operation*
			 (or *api-resource* (error "Provide the api resource")))
	     :documentation "The api resource the function belongs to")
   (authorizations :initarg :authorizations
		   :accessor authorizations
		   :initform nil
		   :documentation "Api function authorizations spec")
   (options :initarg :options
            :accessor options
            :initform nil
	    :documentation "API function options. Extensible. Pluggins data goes here.")
   (summary :initarg :summary
	    :accessor api-summary
	    :initform nil
	    :documentation "API function short description")
   (documentation :accessor api-documentation
		  :initarg :documentation
		  :initform nil
		  :documentation "API function description"))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "The api function description"))

;; resource-operation

(defmethod initialize-instance :after ((resource-operation resource-operation) &rest initargs)
  (declare (ignore initargs))

  ;; Parse the uri to obtain the required parameters
  #+nil(let ((args-in-uri (multiple-value-bind (scanner vars)
			      (parse-resource-operation-path (path resource-operation))
			    (declare (ignore scanner))
			    vars)))
	 (setf (required-arguments resource-operation)
	       (append args-in-uri (required-arguments resource-operation))))

  ;; Parse the optional parameters
  #+nil(setf (optional-arguments resource-operation)
	     (mapcar (lambda (arg)
		       (setf (first arg)
			     (make-keyword (symbol-name (first arg))))
		       arg)
		     (alexandria:copy-sequence '(or cons null)
					       (optional-arguments resource-operation))))

  ;; Validate the function
  (validate resource-operation)

  ;; Install the api function
  (when *register-resource-operation*
    (let ((resource (or *api-resource* (error "Specify the api resource"))))
      ;; Validate the resource path and the api function path
      (when (not (cl-ppcre:scan (format nil "^~A" (resource-path resource))
				(path resource-operation)))
	(error "The resource path ~A and the api function path ~A don't match"
	       (resource-path resource)
	       (path resource-operation)))
      (setf (gethash (name resource-operation) (resource-operations resource))
            resource-operation))))

;; (defmethod initialize-instance :after ((resource-operation resource-operation) &rest args)
;;   (closer-mop:set-funcallable-instance-function
;;    resource-operation
;;    (lambda (&rest args)
;;      (call-resource-operation resource-operation args))))

;; (defun call-resource-operation (resource-operation args))

(defmethod print-object ((resource-operation resource-operation) stream)
  (print-unreadable-object (resource-operation stream :type t :identity t)
    (format stream "~A ~A ~S"
	    (name resource-operation)
	    (request-method resource-operation)
	    (path resource-operation))))

(defmethod validate ((resource-operation resource-operation))
  ;; Check resource-operation arguments have the right format
  (loop for arg in (required-arguments resource-operation)
       do
       (assert (and (listp arg)
                    (symbolp (first arg))
                    (keywordp (second arg))
                    (member (second arg) *resource-operation-arguments-types*)
                    (or (not (third arg))
                        (stringp (third arg))))
               nil
               "The argument is not in the right format ~A.
                Right format: (<name> <type> <documentation>)"
               arg))

    (loop for arg in (optional-arguments resource-operation)
       do
       (assert (and (listp arg)
                    (symbolp (first arg))
                    (keywordp (second arg))
                    (member (second arg) *resource-operation-arguments-types*)
                    ;(resource-operation-typep (third arg) (second arg))
                    (or (not (nth 3 arg))
                        (stringp (nth 3 arg))))
               nil
               "The argument is not in the right format ~A.
                Right format: (<name> <type> <default-value> <documentation>)"
               arg))
  
  ; Ensure uri parameters have been declared
  (let ((args-in-uri (multiple-value-bind (scanner vars)
                         (parse-resource-operation-path (path resource-operation))
                       (declare (ignore scanner))
                       vars)))
    (loop for arg in args-in-uri
	 do
	 (assert (member arg (mapcar #'first (required-arguments resource-operation)))
		 nil
		 "Argument ~a not declared in ~a" arg resource-operation))))

(defun make-resource-operation (name attributes args options)
  "Make an api function."
  (multiple-value-bind (required-arguments optional-arguments)
      (parse-resource-operation-arguments args)
    (apply #'make-instance 'resource-operation
	   :name name
	   :options options
	   :required-arguments required-arguments
	   :optional-arguments optional-arguments
	   attributes)))

(defmethod find-resource-operation ((api symbol) function-name &optional (error-p t))
  (find-resource-operation (find-api api) function-name error-p))

(defmethod find-resource-operation ((api api-definition) function-name &optional (error-p t))
  (loop for resource being the hash-values of (resources api)
       when (gethash function-name (resource-operations resource))
       do (return-from find-resource-operation
	    (gethash function-name (resource-operations resource))))
  (when error-p
    (error "api function not found ~A" function-name)))

(defmethod format-api-url ((api-name symbol) function-name &rest args)
  (let ((api (find-api api-name)))
    (apply #'format-api-url api function-name args)))

(defmethod format-api-url ((api api-definition) function-name &rest args)
  (let ((resource-operation (find-resource-operation api function-name)))
    (replace-vars-in-url (url-pattern resource-operation) args)))

(defmethod resource-operation-http-options ((api api-definition)
				      (resource-operation resource-operation))
  (setf (hunchentoot:header-out "Allow")
	(string-upcase (string (request-method resource-operation)))))

(defclass resource-operation-implementation ()
  ((resource-operation :initarg :resource-operation
		 :initform (error "Provide the api function")
		 :accessor resource-operation
		 :documentation "The api function that is implemented")
   (primary :initarg :primary
	    :initform (error "Provide the primary function")
	    :accessor primary
	    :documentation "Primary function")
   (options :initarg :options
	    :initform nil
	    :accessor options
	    :documentation "resource-operation options"))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "resource-operation implementation"))

(defmethod initialize-instance :after
    ((resource-operation-implementation resource-operation-implementation)
     &rest initargs)
  (declare (ignore initargs))
  (closer-mop:set-funcallable-instance-function
   resource-operation-implementation
   (lambda (&rest args)
     (apply #'execute resource-operation-implementation args))))

(defmethod execute ((resource-operation-implementation resource-operation-implementation) &rest args)
  (when (verify-authentication (resource-operation resource-operation-implementation))
    (apply (primary resource-operation-implementation) args)))

(defmacro implement-resource-operation (api-name name-and-options args &body body)
  "Define an api function implementation"
  (multiple-value-bind (name options)
      (if (listp name-and-options)
	  (values (first name-and-options)
		  (rest name-and-options))
	  (values name-and-options nil))
    (let* ((api (find-api api-name))
	   (resource-operation (find-resource-operation api name nil)))
      (when resource-operation
	(validate-resource-operation-implementation-arguments args resource-operation)))
    `(let* ((api (find-api ',api-name))
	    (resource-operation (find-resource-operation api ',name))
	    (resource-operation-implementation
	     (make-instance 'resource-operation-implementation
			    :resource-operation resource-operation
			    :primary (lambda ,args
				       ,@body)
			    :options ',options)))
       (let ((decorated-function
	      (process-resource-operation-implementation-options
	       resource-operation-implementation)))
	 (setf (get ',name :resource-operation-implementation)
	       decorated-function)))))

(defun validate-resource-operation-implementation-arguments (args resource-operation)
  (flet ((ensure (thing message &rest args)
	   (when (not thing)
	     (apply #'error message args))))
    (multiple-value-bind (required optional rest keyword)
	(alexandria:parse-ordinary-lambda-list args)
      (declare (ignore optional rest))
      (if (member (request-method resource-operation) (list :put :post))
	  (progn
	    (ensure (equalp (symbol-name (first args)) "POSTED-CONTENT")
		    "POSTED-CONTENT argument was not provided as first parameter of the api function implementation")
	    (loop for arg0 in (cdr (required-arguments resource-operation))
	       for arg1 in required
	       do
		 (let ((arg0-name (first arg0))
		       (arg1-name arg1))
		   (ensure (equalp (symbol-name arg0-name)
				   (symbol-name arg1-name))
			   "Invalid api function implementation args list: ~A <> ~A"
			   arg0-name arg1-name))))
	  ;; else
	  (loop for arg0 in (required-arguments resource-operation)
	     for arg1 in required
	     do
	       (let ((arg0-name (first arg0))
		     (arg1-name arg1))
		 (ensure (equalp (symbol-name arg0-name)
				 (symbol-name arg1-name))
			 "Invalid api function implementation args list: ~A <> ~A"
			 arg0-name arg1-name))))
      (let ((optional-args-names
	     (mapcar (alexandria:compose #'symbol-name #'cadar)
		     keyword))
	    (resource-operation-optional-args
	     (mapcar (alexandria:compose #'symbol-name #'car)
				     (optional-arguments resource-operation))))
	(loop for optional-arg in optional-args-names
	   do
	     (ensure (member optional-arg
			     resource-operation-optional-args
			     :test #'equalp)
		     "Optional argument ~A not found in api function implementation args"
		     optional-arg))))))		   

(defun configure-resource-operation-implementation
    (name &rest options)
  "Configure or reconfigure an already existent api function implementation"
  (let* ((resource-operation-implementation
	  (find-resource-operation-implementation name)))
    (let ((processed-resource-operation resource-operation-implementation))
      (loop for option in (reverse options)
	 do (destructuring-bind (option-name &rest args) option
	      (setf processed-resource-operation
		    (apply #'process-resource-operation-implementation-option
			   option-name
			   processed-resource-operation
			   args))))
      (setf (get name :resource-operation-implementation)
	    processed-resource-operation))))

(defun process-resource-operation-implementation-options (resource-operation-implementation)
  (let ((processed-resource-operation resource-operation-implementation))
    (loop for option in (reverse (options resource-operation-implementation))
	 do (destructuring-bind (option-name &rest args) option
	      (setf processed-resource-operation
		    (apply #'process-resource-operation-implementation-option
			   option-name
			   processed-resource-operation
			   args))))
    processed-resource-operation))

(defgeneric process-resource-operation-implementation-option
    (option-name resource-operation-implementation &key)
  (:method (option-name resource-operation-implementation &key)
    (error "Option ~A is not valid" option-name))
  (:documentation "Overwrite this in decorations"))

(defclass resource-operation-implementation-decoration ()
  ((decorates :initarg :decorates
	      :initform (error "Provide the thing being decorated")
	      :accessor decorates
	      :documentation "The thing being decorated"))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((decoration resource-operation-implementation-decoration)
				       &rest initargs)
  (declare (ignore initargs))
  (closer-mop:set-funcallable-instance-function
   decoration
   (lambda (&rest args)
     (apply #'execute decoration args))))

(defmethod execute ((decoration resource-operation-implementation-decoration) &rest args)
  (apply (decorates decoration) args))

(defmethod resource-operation ((decoration resource-operation-implementation-decoration))
  (resource-operation (decorates decoration)))

(defun find-resource-operation-implementation (name)
  (or (get name :resource-operation-implementation)
      (error "Api function ~A not implemented" name)))

(defun execute-resource-operation-implementation (function-implementation request)
  (let ((resource-operation (resource-operation function-implementation)))
    (with-condition-handling
      (let ((args (extract-function-arguments resource-operation request)))
	(apply function-implementation
	       (append 
		(when (member (request-method resource-operation) (list :put :post))
		  (let ((posted-content
			 (when (hunchentoot:raw-post-data :external-format :utf8)
			   (hunchentoot:raw-post-data :external-format :utf8))))
		    (log5:log-for (rest-server) "Posted content: ~A" posted-content)
		    (list (parse-posted-content posted-content))))
		args))))))

(defun find-optional-argument (name resource-operation)
  (or
   (find name (optional-arguments resource-operation)
	 :key (alexandria:compose #'make-keyword #'first))
   (error "Optional argument ~A not found in ~A ~A" 
	  name 
	  resource-operation 
	  (optional-arguments resource-operation))))

(defun resource-operation-matches-request-p (resource-operation request)
  (let ((scanner (parse-resource-operation-path (path resource-operation))))
    (and (cl-ppcre:scan scanner (hunchentoot:request-uri request)) 
         (or (equalp (hunchentoot:request-method request) :options)
	     (equalp (request-method resource-operation)
		     (hunchentoot:request-method request))))))

;; url formatting

(defgeneric format-resource-operation-url (resource-operation &rest args)
  (:documentation "Print the api function url")
  (:method ((resource-operation resource-operation) &rest args)
    (let ((url-noparams
	   (replace-vars-in-url (url-pattern-noparams resource-operation) args))
	  (optional-args
	   (loop
	      for key in args by #'cddr
	      for value in (cdr args) by #'cddr
	      for optional-arg = (find key (optional-arguments resource-operation)
				       :key (alexandria:compose #'make-keyword #'first))
	      when optional-arg
	      collect (format-optional-url-arg optional-arg value))))
      (format nil "~A~@[?~{~A~^&~}~]"
	      url-noparams
	      optional-args))))

(defun format-absolute-resource-operation-url (resource-operation &rest args)
  (let ((base-url (format nil "~A://~A:~A"
			  (if (hunchentoot:acceptor-ssl-p hunchentoot:*acceptor*)
			      "https"
			      "http")
			  (hunchentoot:acceptor-address hunchentoot:*acceptor*)
			  (hunchentoot:acceptor-port hunchentoot:*acceptor*))))
    (format nil "~A~A"
	    base-url
	    (apply #'format-resource-operation-url
		   resource-operation
		   args))))

(defun format-optional-url-arg (arg value)
  (destructuring-bind (name type default-value documentation) arg
    (format nil "~A=~A" (string-downcase name)
	    (%format-optional-url-arg type arg value))))

(defgeneric %format-optional-url-arg (type arg value)
  (:method ((type (eql :string)) arg value)
    (assert (stringp value))
    value)
  (:method ((type (eql :boolean)) arg value)
    (if value "true" "false"))
  (:method ((type (eql :integer)) arg value)
    (assert (integerp value))
    (princ-to-string value))
  (:method ((type (eql :list)) arg value)
    (assert (listp value))
    (format nil "~{~A~^,~}" value)))

(defun parse-resource-operation-arguments (arguments-spec)
  (loop with required-arguments = '()
     with optional-arguments ='()
     with optional = nil
     for argument in arguments-spec
     do
     (if (equalp argument '&optional)
         (setf optional t)
         ;; else
         (if optional
             (push argument optional-arguments)
             (push argument required-arguments)))
     finally (return (values (reverse required-arguments)
                             (reverse optional-arguments)))))

(defun parse-parameter (string)
  (cl-ppcre:register-groups-bind (query-param var)
      ("^(.+)={(.+)+}$" string)
    (cons (intern (string-upcase var) :keyword) query-param)))

(defun parse-parameters (string)
  (assert (char= (char string 0) #\?))
  (mapcar #'parse-parameter
	  (cl-ppcre:split "&" (subseq string 1))))

;; Client implementation

(defun client-stub (api-name resource-operation &optional (package *package*))
  (let ((request-url (gensym "REQUEST-URL-"))
        (response (gensym "RESPONSE-"))
	(status-code (gensym "STATUS-CODE-")))
    (let ((required-args (required-arguments resource-operation))
	  (optional-args (optional-arguments resource-operation)))
      `(progn
	 (defun ,(intern (symbol-name (name resource-operation)) package)
             ,(append
	       (when (member (request-method resource-operation) '(:post :put))
		 (list 'posted-content))
	       (loop for x in required-args collect 
		    (intern (symbol-name (car x)) package))
	       (cons '&key
		     (append
		      (list '(accept "application/json"))
		      (when optional-args
			(loop for x in optional-args collect 
			     (list (intern (symbol-name (first x)) package) 
				   (third x)
				   (intern (format nil "~A-PROVIDED-P" (symbol-name (first x))) package))))
		      (when (member (request-method resource-operation) '(:post :put))
			(list '(content-type "application/json"))))))
	   ,(api-documentation resource-operation)
           (log5:log-for (rest-server) "Client stub: ~A" ',(name resource-operation))
           (assert *api-backend* nil "Error: this is an API function. No api backend selected. Wrap this function call with with-api-backend")
           (let ((,request-url (format nil "~A~A" 
                                       *api-backend* 
                                       (replace-vars-in-url 
                                        ,(url-pattern-noparams resource-operation)
                                        (list
                                         ,@(loop for x in required-args 
                                              collect (make-keyword (car x))
                                              collect (intern (symbol-name (car x)) package)))))))
             (log5:log-for (rest-server)  "Request: ~A ~A" ,(request-method resource-operation) ,request-url)
             ,(when (member (request-method resource-operation) 
                            '(:post :put))
                    `(log5:log-for (rest-server) "Posted content: ~A"
                                   posted-content))
             (multiple-value-bind (,response ,status-code)
		 (drakma:http-request 
		  ,request-url
		  :method ,(request-method resource-operation)
		  :proxy *rest-server-proxy*
		  :content ,(when (member (request-method resource-operation) 
					  '(:post :put))
				  `(babel:string-to-octets posted-content))
		  :content-type ,(when (member (request-method resource-operation) 
					       '(:post :put))
				       'content-type)
		  :parameters (append
			       ,@(loop for x in optional-args
				    collect
				      `(when ,(intern (format nil "~A-PROVIDED-P" (symbol-name (car x))) package)
					 (list (cons 
						,(symbol-name (car x))
						(format nil "~A" ,(intern (symbol-name 
									   (car x)) package)))))))
		  :additional-headers (list (cons "Accept" accept))) 
	       (log5:log-for (rest-server) "Response: ~A" ,response)
	       (values ,response ,status-code))))))))

(defgeneric url-pattern-noparams (resource-operation))
(defmethod url-pattern-noparams ((resource-operation resource-operation))
  (let* ((pattern (path resource-operation))
	(pos (position #\? pattern)))
    (if pos
	(subseq pattern 0 pos)
	pattern)))

;; Conditional dispatching

(defclass conditional-dispatch-decoration (resource-operation-implementation-decoration)
  ((predicate :initarg :predicate
	      :initform (error "Provide the predicate")
	      :accessor predicate
	      :documentation "The dispatching predicate. Execution takes place only when it is true.")
   (when-true :initarg :when-true
	      :initform (error "Provide the function to evaluate when the predicate it true")
	      :accessor when-true
	      :documentation "The function to evaluate when the predicate is true"))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod process-resource-operation-implementation-option
    ((option (eql :conditional-dispatch))
     resource-operation-implementation
     &key (enabled t)
       predicate
       when-true)
  (if enabled
      (make-instance 'conditional-dispatch-decoration
		     :decorates resource-operation-implementation
		     :predicate predicate
		     :when-true when-true)
      resource-operation-implementation))

(defmethod execute :around ((decoration conditional-dispatch-decoration)
			    &rest args)
  (if (apply (predicate decoration) args)
      (apply (when-true decoration) args)
      ; else
      (call-next-method)))
    
(defmacro implement-resource-operation-case (name accept-content-type args &body body)
  "Implement an api function case"
  `(configure-resource-operation-implementation
    ',name
    (list
     :conditional-dispatch
     :predicate (lambda (&rest args)
		  (declare (ignore args))
		  (and (hunchentoot:header-in* "accept")
		       (let ((accepts (split-sequence:split-sequence #\, (hunchentoot:header-in* "accept"))))
			 (intersection (list ,accept-content-type)
				       accepts :test #'equalp))))
     :when-true (lambda ,args
		  (with-reply-content-type (,accept-content-type)
		    ,@body)))))
