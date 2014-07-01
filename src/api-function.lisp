(in-package :rest-server)

(defparameter *api-function-arguments-types*
  (list :boolean
	:integer
	:string
	:list))

(defmacro define-api-function (name attributes args &rest options)
  "Helper macro to define an api function"
  `(make-api-function ',name ',attributes ',args ',options))

(defclass api-function ()
  ((name :initarg :name
	 :accessor name
	 :initform (error "Provide the name"))
   (uri-prefix :initarg :uri-prefix
               :accessor uri-prefix
               :initform nil)
   (request-method :initarg :request-method
		   :accessor request-method
		   :initform :get)
   (arguments :initarg :arguments
	      :initform nil
	      :accessor arguments
	      :documentation "The api function arguments")
   (consumes :initarg :consumes
	     :accessor consumes
	     :initform nil
	     :documentation "MIME types that the api-function consumes. Overwrites MIME types defined in the resource")
   (produces :initarg :produces
	     :accessor produces
	     :initform nil
	     :documentation "MIME types that the api-function produces. Overwrites MIME types defined in the resource")     
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
                         (parse-uri-prefix (uri-prefix api-function))
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
	    (uri-prefix api-function))))

(defmethod validate ((api-function api-function))
					; Ensure path parameters have been declared
  (let ((args-in-uri (multiple-value-bind (scanner vars)
                         (parse-uri-prefix (uri-prefix api-function))
                       (declare (ignore scanner))
                       vars)))
    (let ((path-arguments (path-arguments api-function)))
      (loop for arg in args-in-uri
	 do
	   (assert (member (symbol-name arg)
			   (mapcar #'argument-name path-arguments)
			   :test #'equalp
			   :key #'symbol-name)
		   nil
		   "Argument ~a not declared in ~a" arg api-function)))))

(defmethod required-arguments ((api-function api-function))
  (remove-if-not #'required-p (arguments api-function)))

(defmethod optional-arguments ((api-function api-function))
  (remove-if #'required-p (arguments api-function)))

(defmethod path-arguments ((api-function api-function))
  (remove-if-not (lambda (arg)
		   (equalp (param-type arg) :path))
		 (arguments api-function)))

(defmethod query-arguments ((api-function api-function))
  (remove-if-not (lambda (arg)
		   (equalp (param-type arg) :query))
		 (arguments api-function)))

(defmethod body-argument ((api-function api-function))
  (find-if (lambda (arg)
		    (equalp (param-type arg) :body))
	   (arguments api-function)))

(defun make-api-function (name attributes args options)
  "Make an api function."
  (apply #'make-instance 'api-function
	   :name name
	   :arguments (parse-api-function-arguments args)
	   :options options
	   attributes))

(defmethod find-api-function ((api symbol) function-name)
  (find-api-function (find-api api) function-name))

(defmethod find-api-function ((api api-definition) function-name)
  (loop for resource being the hash-values of (resources api)
       when (gethash function-name (api-functions resource))
       do (return-from find-api-function
	    (gethash function-name (api-functions resource))))
  (error "api function not found ~A" function-name))

(defun format-api-url (api function-name &rest args)
  (let ((api-function (find-api-function api function-name)))
    (replace-vars-in-url (url-pattern api-function) args)))

(defparameter *argument-types* (list :integer
				     :long
				     :float
				     :double
				     :string
				     :byte
				     :boolean
				     :date
				     :date-time))

(defparameter *param-types* (list :path :query :body :header :form))

(defclass api-function-argument ()
  ((name :initarg :name
	 :accessor argument-name
	 :initform (error "Provide the argument name (:name)")
	 :type symbol
	 :documentation "The argument name")
   (type :initarg :type
	 :accessor argument-type
	 :initform (error "Provide the argument type (:type)")
	 :documentation "The argument type")
   (param-type :initarg :param-type
	       :accessor param-type
	       :initform (error "Provide the parameter type (:param-type)")
	       :type (member (:path :query :body :header :form))
	       :documentation "The type of the parameter (that is, the location of the parameter in the request). The value MUST be one of these values: :path, :query, :body, :header, :form")
   (format :initarg :format
	   :accessor argument-format
	   :initform nil
	   :documentation "The argument format")
   (default-value
       :initarg :default-value
     :accessor default-value
     :initform nil
     :documentation "The default value")
   (enum :initarg :enum
	 :accessor argument-enum
	 :initform nil
	 :documentation "A fixed list of possible values. If this field is used in conjunction with the defaultValue field, then the default value MUST be one of the values defined in the enum")
   (minimum :initarg :minimum
	    :accessor argument-minimum
	    :initform nil
	    :documentation "The minimum valid value for the type, inclusive. If this field is used in conjunction with the defaultValue field, then the default value MUST be higher than or equal to this value.")
   (maximum :initarg :maximum
	    :accessor argument-maximum
	    :initform nil
	    :documentation "The maximum valid value for the type, inclusive. If this field is used in conjunction with the defaultValue field, then the default value MUST be lower than or equal to this value.")
   (allow-multiple :initarg :allow-multiple
		   :accessor allow-multiple
		   :initform nil
		   :documentation "Another way to allow multiple values for a :query parameter. If used, the query parameter may accept comma-separated values. The field may be used only if paramType is :query, :header or :path")
   (required-p :initarg :required-p
	       :accessor required-p
	       :initform nil
	       :documentation "A flag to note whether this parameter is required. If this field is not included, it is equivalent to adding this field with the value false")
   (documentation :initarg :documentation
		  :accessor argument-documentation
		  :initform nil
		  :type string
		  :documentation "Recommended. A brief description of this parameter."))
  (:documentation "An api-function argument"))

(defmethod initialize-instance :after ((arg api-function-argument) &rest initargs)
  (declare (ignore initargs))
  (validate-api-function-argument arg))

(defmethod validate-api-function-argument ((arg api-function-argument))
  (assert (member (argument-type arg) *argument-types*) nil
	  "Argument type invalid ~A" (argument-type arg))
  (assert (member (param-type arg) *param-types*) nil
	  "Param type ~A is invalid" (param-type arg))
  (assert (or (not (equalp (param-type arg) :body))
	      (equalp (symbol-name (argument-name arg)) "body"))
	  nil "Argument name should be 'body'"))

(defmethod print-object ((arg api-function-argument) stream)
  (print-unreadable-object (arg stream :type t :identity t)
    (format stream "~A::~A(~A) REQUIRED: ~A"
	    (argument-name arg)
	    (argument-type arg)
	    (param-type arg)
	    (required-p arg))))

(defun parse-api-function-argument (arg-spec)
  (apply #'make-instance 'api-function-argument
	 :name (first arg-spec)
	 (rest arg-spec)))

(defclass api-function-implementation ()
  ((api-function :initarg :api-function
		 :initform (error "Provide the api function")
		 :accessor api-function
		 :documentation "The api function that is implemented")
   (around :initarg :around
	   :initform nil
	   :accessor around
	   :documentation "Around functions")
   (before :initarg :before
	   :initform nil
	   :accessor before
	   :documentation "Before functions")
   (primary :initarg :primary
	    :initform (error "Provide the primary function")
	    :accessor primary
	    :documentation "Primary function")
   (after :initarg :after
	  :initform nil
	  :accessor after
	  :documentation "After functions")
   (options :initarg :options
	    :initform nil
	    :accessor options
	    :documentation "api-function options"))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "api-function implementation"))

(defmethod initialize-instance :after ((api-function-implementation api-function-implementation)
				       &rest initargs)
  (declare (ignore initargs))
  (loop for option in (options api-function-implementation)
       do (configure-api-function-implementation-option
	   (first option)
	   api-function-implementation
	   option))
  (closer-mop:set-funcallable-instance-function
   api-function-implementation
   (lambda (&rest args)
     (apply #'execute api-function-implementation args))))

(defmethod execute-before-primary-after ((api-function-implementation api-function-implementation) args)
  (loop for before in (before api-function-implementation)
       do (apply before args))
  (prog1
      (apply (primary api-function-implementation) args)
    (loop for after in (after api-function-implementation)
       do (apply after args))))

(defun call-around-function (api-function-implementation function arounds args)
  (handler-bind
      ((call-next-function-condition
	(let ((around (first arounds)))
	  #'(lambda (c)
	      (let ((result (if (not around)
				(execute-before-primary-after
				 api-function-implementation args)
					;else
				(call-around-function api-function-implementation
						      around (rest arounds) args))))
		(invoke-restart (find-restart 'continue c) result))))))
	    (apply function args)))

(defmethod execute ((api-function-implementation api-function-implementation) &rest args)
  (if (not (around api-function-implementation))
      (execute-before-primary-after api-function-implementation args)
      ; else
      (call-around-function api-function-implementation
			    (first (around api-function-implementation))
			    (rest (around api-function-implementation))
			    args)))  
	
(defmacro implement-api-function (api-name name-and-options args &body body)
  "Define an api function implementation"
  (multiple-value-bind (name options)
      (if (listp name-and-options)
	  (values (first name-and-options)
		  (rest name-and-options))
	  (values name-and-options nil))
      `(let* ((api (find-api ',api-name))
	      (api-function-implementation
	       (make-instance 'api-function-implementation
			      :api-function (find-api-function api ',name)
			      :primary (lambda ,args
					 ,@body)
			      :options ',options)))
	 (setf (get ',name :api-function-implementation)
	       api-function-implementation))))

(define-condition call-next-function-condition ()
  ())

(defun call-next-function ()
  (restart-case 
    (signal 'call-next-function-condition)
    (continue (&rest args)
      (apply #'values args))))

(defun add-around-function (api-function-implementation function)
  (push function (around api-function-implementation)))

(defmethod configure-api-function-implementation-option
    ((option-name (eql :logging))
     api-function-implementation
     option)
  (when (getf (cdr option) :enabled)
    (add-around-function api-function-implementation
	(lambda (&rest args)
	  (declare (ignore args))
	  (log5:log-for (rest-server) "API: Handling ~A ~A by ~A"
			(hunchentoot:request-method*)
			(hunchentoot:request-uri*) (name (api-function api-function-implementation)))
	  (let ((posted-content (when (hunchentoot:raw-post-data :external-format :utf8)
				  (hunchentoot:raw-post-data :external-format :utf8))))
	    (when posted-content (log5:log-for (rest-server) "Posted content: ~A" posted-content)))
	  (let ((result (call-next-function)))
	    (log5:log-for (rest-server) "Response: ~A" result)
	    result)))))

(defmethod configure-api-function-implementation-option
    ((option-name (eql :serialization))
     api-function-implementation
     option)
  (when (getf (cdr option) :enabled)
    (add-around-function api-function-implementation
	(lambda (&rest args)
	  (with-output-to-string (s)
	    (with-serializer-output s
	      (with-serializer (rest-server::accept-serializer)
		(serialize
		 (call-next-function)
		 *serializer*
		 s))))))))

(defun find-api-function-implementation (name)
  (or (get name :api-function-implementation)
      (error "Api function ~A not implemented" name)))

(defun execute-api-function-implementation (api-function function-implementation request)
  (with-condition-handling
    (let ((arg-values (extract-function-arguments-values api-function request)))
      (apply function-implementation arg-values))))

(defun find-optional-argument (name api-function)
  (or
   (find name (optional-arguments api-function)
	 :key (alexandria:compose #'make-keyword #'first))
   (error "Optional argument ~A not found in ~A ~A" 
	  name 
	  api-function 
	  (optional-arguments api-function))))

(defun api-function-matches-request-p (api-function request)
  (let ((scanner (parse-uri-prefix (uri-prefix api-function))))
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
  (loop for arg-spec in arguments-spec
     collect (parse-api-function-argument arg-spec)))     

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
        (response (gensym "RESPONSE-")))
    (let ((required-args (required-arguments api-function))
	  (optional-args (optional-arguments api-function)))
      `(progn
	 (defun ,(intern (symbol-name (name api-function)) package)
             ,(append
	       (loop for arg in required-args collect 
		     (intern (symbol-name (argument-name arg)) package))
	       (if optional-args
		   (cons '&key (loop for arg in optional-args collect 
				     (list (intern (symbol-name (argument-name arg)) package) 
					   (default-value arg))))))
	   ,(api-documentation api-function)
           (log5:log-for (rest-server) "Client stub: ~A" ',(name api-function))
           (assert *api-backend* nil "Error: this is an API function. No api backend selected. Wrap this function call with with-api-backend")
           (let ((,request-url (format nil "~A~A" 
                                       *api-backend* 
                                       (replace-vars-in-url 
                                        ,(url-pattern-noparams api-function)
                                        (list
                                         ,@(loop for arg in (path-arguments api-function) 
                                              collect (make-keyword (argument-name arg))
                                              collect (intern (symbol-name (argument-name arg)) package)))))))
             (log5:log-for (rest-server)  "Request: ~A ~A" ,(request-method api-function) ,request-url)
	     (let ((,response (drakma:http-request 
                               ,request-url
                               :method ,(request-method api-function)
                               :proxy *rest-server-proxy*
                               :content ,(let ((body-argument (body-argument api-function)))
					      (when body-argument
						`(babel:string-to-octets ,(intern (symbol-name (argument-name body-argument))))))
                               :parameters (list 
                                            ,@(loop for arg in (query-arguments api-function)
                                                 collect
                                                 (progn
                                                   `(cons 
                                                     ,(symbol-name (argument-name arg))
                                                     (format nil "~A" ,(intern (symbol-name 
                                                                                (argument-name arg)) package)))))))))
               (log5:log-for (rest-server) "Response: ~A" ,response)
               ,response)))))))

(defgeneric url-pattern-noparams (api-function))
(defmethod url-pattern-noparams ((api-function api-function))
  (let* ((pattern (uri-prefix api-function))
	(pos (position #\? pattern)))
    (if pos
	(subseq pattern 0 pos)
	pattern)))
