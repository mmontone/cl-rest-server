(in-package :rest-server)

(defparameter *api-function-arguments-types*
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

(defmacro define-api-function (name attributes args &rest options)
  "Helper macro to define an api function"
  `(make-api-function ',name ',attributes ',args ',options))

(defclass api-function ()
  ((name :initarg :name
	 :accessor name
	 :initform (error "Provide the name"))
   (path :initarg :path
	 :accessor path
	 :initform nil
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
	     :documentation "MIME types that the api-function consumes. Overwrites MIME types defined in the resource")
   (produces :initarg :produces
	     :accessor produces
	     :initform nil
	     :documentation "MIME types that the api-function produces. Overwrites MIME types defined in the resource")
   (resource :initarg :resource
	     :accessor resource
	     :initform (when *register-api-function*
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

;; api-function

(defmethod initialize-instance :after ((api-function api-function) &rest initargs)
  (declare (ignore initargs))

  ;; Parse the uri to obtain the required parameters
  #+nil(let ((args-in-uri (multiple-value-bind (scanner vars)
                         (parse-api-function-path (path api-function))
                       (declare (ignore scanner))
                       vars)))
    (setf (required-arguments api-function)
          (append args-in-uri (required-arguments api-function))))

  ;; Parse the optional parameters
  #+nil(setf (optional-arguments api-function)
	(mapcar (lambda (arg)
		  (setf (first arg)
			(make-keyword (symbol-name (first arg))))
		  arg)
		(alexandria:copy-sequence '(or cons null)
					  (optional-arguments api-function))))

  ;; Validate the function
  (validate api-function)

  ;; Install the api function
  (when *register-api-function*
    (let ((resource (or *api-resource* (error "Specify the api resource"))))
      (setf (gethash (name api-function) (api-functions resource))
            api-function))))

;; (defmethod initialize-instance :after ((api-function api-function) &rest args)
;;   (closer-mop:set-funcallable-instance-function
;;    api-function
;;    (lambda (&rest args)
;;      (call-api-function api-function args))))

;; (defun call-api-function (api-function args))

(defmethod print-object ((api-function api-function) stream)
  (print-unreadable-object (api-function stream :type t :identity t)
    (format stream "~A ~A ~S"
	    (name api-function)
	    (request-method api-function)
	    (path api-function))))

(defmethod validate ((api-function api-function))
  ;; Check api-function arguments have the right format
  (loop for arg in (required-arguments api-function)
       do
       (assert (and (listp arg)
                    (symbolp (first arg))
                    (keywordp (second arg))
                    (member (second arg) *api-function-arguments-types*)
                    (or (not (third arg))
                        (stringp (third arg))))
               nil
               "The argument is not in the right format ~A.
                Right format: (<name> <type> <documentation>)"
               arg))

    (loop for arg in (optional-arguments api-function)
       do
       (assert (and (listp arg)
                    (symbolp (first arg))
                    (keywordp (second arg))
                    (member (second arg) *api-function-arguments-types*)
                    ;(api-function-typep (third arg) (second arg))
                    (or (not (nth 3 arg))
                        (stringp (nth 3 arg))))
               nil
               "The argument is not in the right format ~A.
                Right format: (<name> <type> <default-value> <documentation>)"
               arg))
  
  ; Ensure uri parameters have been declared
  (let ((args-in-uri (multiple-value-bind (scanner vars)
                         (parse-api-function-path (path api-function))
                       (declare (ignore scanner))
                       vars)))
    (loop for arg in args-in-uri
	 do
	 (assert (member arg (mapcar #'first (required-arguments api-function)))
		 nil
		 "Argument ~a not declared in ~a" arg api-function))))

(defun make-api-function (name attributes args options)
  "Make an api function."
  (multiple-value-bind (required-arguments optional-arguments)
      (parse-api-function-arguments args)
    (apply #'make-instance 'api-function
	   :name name
	   :options options
	   :required-arguments required-arguments
	   :optional-arguments optional-arguments
	   attributes)))

(defmethod find-api-function ((api symbol) function-name)
  (find-api-function (find-api api) function-name))

(defmethod find-api-function ((api api-definition) function-name)
  (loop for resource being the hash-values of (resources api)
       when (gethash function-name (api-functions resource))
       do (return-from find-api-function
	    (gethash function-name (api-functions resource))))
  (error "api function not found ~A" function-name))

(defmethod format-api-url ((api-name symbol) function-name &rest args)
  (let ((api (find-api api-name)))
    (apply #'format-api-url api function-name args)))

(defmethod format-api-url ((api api-definition) function-name &rest args)
  (let ((api-function (find-api-function api function-name)))
    (replace-vars-in-url (url-pattern api-function) args)))

(defclass api-function-implementation ()
  ((api-function :initarg :api-function
		 :initform (error "Provide the api function")
		 :accessor api-function
		 :documentation "The api function that is implemented")
   (primary :initarg :primary
	    :initform (error "Provide the primary function")
	    :accessor primary
	    :documentation "Primary function")
   (options :initarg :options
	    :initform nil
	    :accessor options
	    :documentation "api-function options"))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "api-function implementation"))

(defmethod initialize-instance :after
    ((api-function-implementation api-function-implementation)
     &rest initargs)
  (declare (ignore initargs))
  (closer-mop:set-funcallable-instance-function
   api-function-implementation
   (lambda (&rest args)
     (apply #'execute api-function-implementation args))))

(defmethod execute ((api-function-implementation api-function-implementation) &rest args)
  (when (verify-authentication (api-function api-function-implementation))
    (apply (primary api-function-implementation) args)))

(defmacro implement-api-function (api-name name-and-options args &body body)
  "Define an api function implementation"
  (multiple-value-bind (name options)
      (if (listp name-and-options)
	  (values (first name-and-options)
		  (rest name-and-options))
	  (values name-and-options nil))
      `(let* ((api (find-api ',api-name))
	      (api-function (find-api-function api ',name))
	      (api-function-implementation
	       (make-instance 'api-function-implementation
			      :api-function api-function
			      :primary (lambda ,args
					 ,@body)
			      :options ',options)))
	 (let ((decorated-function
		(process-api-function-implementation-options
		 api-function-implementation)))
	   (setf (get ',name :api-function-implementation)
		 decorated-function)))))

(defun configure-api-function-implementation
    (name &rest options)
  "Configure or reconfigure an already existent api function implementation"
  (let* ((api-function-implementation
	  (find-api-function-implementation name)))
    (let ((processed-api-function api-function-implementation))
      (loop for option in (reverse options)
	 do (destructuring-bind (option-name &rest args) option
	      (setf processed-api-function
		    (apply #'process-api-function-implementation-option
			   option-name
			   processed-api-function
			   args))))
      (setf (get name :api-function-implementation)
	    processed-api-function))))

(defun process-api-function-implementation-options (api-function-implementation)
  (let ((processed-api-function api-function-implementation))
    (loop for option in (reverse (options api-function-implementation))
	 do (destructuring-bind (option-name &rest args) option
	      (setf processed-api-function
		    (apply #'process-api-function-implementation-option
			   option-name
			   processed-api-function
			   args))))
    processed-api-function))

(defgeneric process-api-function-implementation-option
    (option-name api-function-implementation &rest args)
  (:method (option-name api-function-implementation &rest args)
    (error "Option ~A is not valid" option-name))
  (:documentation "Overwrite this in decorations"))

(defclass api-function-implementation-decoration ()
  ((decorates :initarg :decorates
	      :initform (error "Provide the thing being decorated")
	      :accessor decorates
	      :documentation "The thing being decorated"))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((decoration api-function-implementation-decoration)
				       &rest initargs)
  (declare (ignore initargs))
  (closer-mop:set-funcallable-instance-function
   decoration
   (lambda (&rest args)
     (apply #'execute decoration args))))

(defmethod execute ((decoration api-function-implementation-decoration) &rest args)
  (apply (decorates decoration) args))

(defmethod api-function ((decoration api-function-implementation-decoration))
  (api-function (decorates decoration)))

(defun find-api-function-implementation (name)
  (or (get name :api-function-implementation)
      (error "Api function ~A not implemented" name)))

(defun execute-api-function-implementation (api-function function-implementation request)
  (with-condition-handling
    (let ((args (extract-function-arguments api-function request)))
      (apply function-implementation
	     (append 
	      (when (member (request-method api-function) (list :put :post))
		(let ((posted-content
		       (when (hunchentoot:raw-post-data :external-format :utf8)
			 (hunchentoot:raw-post-data :external-format :utf8))))
		  (log5:log-for (rest-server) "Posted content: ~A" posted-content)
		  (list (parse-posted-content posted-content))))
	      args)))))

(defun find-optional-argument (name api-function)
  (or
   (find name (optional-arguments api-function)
	 :key (alexandria:compose #'make-keyword #'first))
   (error "Optional argument ~A not found in ~A ~A" 
	  name 
	  api-function 
	  (optional-arguments api-function))))

(defun api-function-matches-request-p (api-function request)
  (let ((scanner (parse-api-function-path (path api-function))))
    (and (cl-ppcre:scan scanner (hunchentoot:request-uri request)) 
         (equalp (request-method api-function) (hunchentoot:request-method request)))))

;; url formatting

(defgeneric format-api-function-url (api-function &rest args)
  (:documentation "Print the api function url")
  (:method ((api-function api-function) &rest args)
    (let ((url-noparams
	   (replace-vars-in-url (url-pattern-noparams api-function) args))
	  (optional-args
	   (loop
	      for key in args by #'cddr
	      for value in (cdr args) by #'cddr
	      for optional-arg = (find key (optional-arguments api-function)
				       :key (alexandria:compose #'make-keyword #'first))
	      when optional-arg
	      collect (format-optional-url-arg optional-arg value))))
      (format nil "~A~@[?~{~A~^&~}~]"
	      url-noparams
	      optional-args))))

(defun format-absolute-api-function-url (api-function &rest args)
  (let ((base-url (format nil "~A://~A:~A"
			  (if (hunchentoot:acceptor-ssl-p hunchentoot:*acceptor*)
			      "https"
			      "http")
			  (hunchentoot:acceptor-address hunchentoot:*acceptor*)
			  (hunchentoot:acceptor-port hunchentoot:*acceptor*))))
    (format nil "~A~A"
	    base-url
	    (apply #'format-api-function-url
		   api-function
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

(defun parse-api-function-arguments (arguments-spec)
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

(defun client-stub (api-name api-function &optional (package *package*))
  (let ((request-url (gensym "REQUEST-URL-"))
        (response (gensym "RESPONSE-"))
	(status-code (gensym "STATUS-CODE-")))
    (let ((required-args (required-arguments api-function))
	  (optional-args (optional-arguments api-function)))
      `(progn
	 (defun ,(intern (symbol-name (name api-function)) package)
             ,(append
	       (when (member (request-method api-function) '(:post :put))
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
		      (when (member (request-method api-function) '(:post :put))
			(list '(content-type "application/json"))))))
	   ,(api-documentation api-function)
           (log5:log-for (rest-server) "Client stub: ~A" ',(name api-function))
           (assert *api-backend* nil "Error: this is an API function. No api backend selected. Wrap this function call with with-api-backend")
           (let ((,request-url (format nil "~A~A" 
                                       *api-backend* 
                                       (replace-vars-in-url 
                                        ,(url-pattern-noparams api-function)
                                        (list
                                         ,@(loop for x in required-args 
                                              collect (make-keyword (car x))
                                              collect (intern (symbol-name (car x)) package)))))))
             (log5:log-for (rest-server)  "Request: ~A ~A" ,(request-method api-function) ,request-url)
             ,(when (member (request-method api-function) 
                            '(:post :put))
                    `(log5:log-for (rest-server) "Posted content: ~A"
                                   posted-content))
             (multiple-value-bind (,response ,status-code)
		 (drakma:http-request 
		  ,request-url
		  :method ,(request-method api-function)
		  :proxy *rest-server-proxy*
		  :content ,(when (member (request-method api-function) 
					  '(:post :put))
				  `(babel:string-to-octets posted-content))
		  :content-type ,(when (member (request-method api-function) 
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

(defgeneric url-pattern-noparams (api-function))
(defmethod url-pattern-noparams ((api-function api-function))
  (let* ((pattern (path api-function))
	(pos (position #\? pattern)))
    (if pos
	(subseq pattern 0 pos)
	pattern)))

;; Conditional dispatching

(defclass conditional-dispatch-decoration (api-function-implementation-decoration)
  ((predicate :initarg :predicate
	      :initform (error "Provide the predicate")
	      :accessor predicate
	      :documentation "The dispatching predicate. Execution takes place only when it is true.")
   (when-true :initarg :when-true
	      :initform (error "Provide the function to evaluate when the predicate it true")
	      :accessor when-true
	      :documentation "The function to evaluate when the predicate is true"))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod process-api-function-implementation-option
    ((option (eql :conditional-dispatch))
     api-function-implementation
     &key (enabled t)
       predicate
       when-true)
  (if enabled
      (make-instance 'conditional-dispatch-decoration
		     :decorates api-function-implementation
		     :predicate predicate
		     :when-true when-true)
      api-function-implementation))

(defmethod execute :around ((decoration conditional-dispatch-decoration)
			    &rest args)
  (if (apply (predicate decoration) args)
      (apply (when-true decoration) args)
      ; else
      (call-next-method)))
    
(defmacro implement-api-function-case (name accept-content-type args &body body)
  "Implement an api function case"
  `(configure-api-function-implementation
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
