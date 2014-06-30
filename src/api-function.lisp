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
   (required-arguments :initarg :required-arguments
                       :accessor required-arguments
                       :initform nil)
   (optional-arguments :initarg :optional-arguments
                       :accessor optional-arguments
                       :initform nil)
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
                         (parse-uri-prefix (uri-prefix api-function))
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

(defun format-api-url (api function-name &rest args)
  (let ((api-function (find-api-function api function-name)))
    (replace-vars-in-url (url-pattern api-function) args)))

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
    (let ((args (extract-function-arguments api-function request)))
      (apply function-implementation
	     (append 
	      (when (member (request-method api-function) (list :put :post))
		(let ((posted-content (when (hunchentoot:raw-post-data :external-format :utf8)
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
        (response (gensym "RESPONSE-")))
    (let ((required-args (required-arguments api-function))
	  (optional-args (optional-arguments api-function)))
      `(progn
	 (defun ,(intern (symbol-name (name api-function)) package)
             ,(append
                (if (member (request-method api-function) '(:post :put))
                    (list 'posted-content)
                    nil)
                (loop for x in required-args collect 
		     (intern (symbol-name (car x)) package))
                (if optional-args
                    (cons '&key (loop for x in optional-args collect 
				     (list (intern (symbol-name (first x)) package) 
					   (third x))))))
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
             (let ((,response (drakma:http-request 
                               ,request-url
                               :method ,(request-method api-function)
                               :proxy *rest-server-proxy*
                               :content ,(when (member (request-method api-function) 
                                                     '(:post :put))
                                             `(babel:string-to-octets posted-content))
                               :parameters (list 
                                            ,@(loop for x in optional-args
                                                 collect
                                                 (progn
                                                   `(cons 
                                                     ,(symbol-name (car x))
                                                     (format nil "~A" ,(intern (symbol-name 
                                                                                (car x)) package)))))))))
               (log5:log-for (rest-server) "Response: ~A" ,response)
               ,response)))))))

(defgeneric url-pattern-noparams (api-function))
(defmethod url-pattern-noparams ((api-function api-function))
  (let* ((pattern (uri-prefix api-function))
	(pos (position #\? pattern)))
    (if pos
	(subseq pattern 0 pos)
	pattern)))
