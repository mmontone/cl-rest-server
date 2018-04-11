(in-package :rest-server)

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
  "Bind ARGS to POSTED-CONTENT. POSTED-CONTENT is supposed to be an alist.
Also, argx-P is T iff argx is present in POSTED-CONTENT"
  (flet ((predicate-name (arg)
           (intern (format nil "~A-P" arg))))
    `(let ,(loop for arg in args
              collect `(,arg (cdr (assoc ,(make-keyword arg) ,posted-content)))
              collect `(,(predicate-name arg)
                         (assoc ,(make-keyword arg) ,posted-content)))
       (declare (ignorable ,@(mapcar #'predicate-name args)))
       ,@body)))

(defmacro with-text-content-types (&body body)
  `(call-with-text-content-types (lambda () ,@body)))

(defun call-with-text-content-types (function)
  (let ((drakma:*text-content-types*
         (append drakma:*text-content-types*
                 (list (cons "application" "json")
                       (cons "application" "xml")))))
    (funcall function)))

(defgeneric encode-posted-content (content content-type)
  (:method ((content cons) (content-type (eql :json)))
    (babel:string-to-octets (json:encode-json-to-string content)))
  (:method (content content-type)
    content))

(defmacro define-resource-operation (name attributes args &rest options)
  "Helper macro to define an resource operation"
  `(make-resource-operation ',name ',attributes ',args ',options))

(defclass resource-operation ()
  ((name :initarg :name
         :accessor name
         :initform (error "Provide the name"))
   (path :initarg :path
         :accessor path
         :initform (error "Provide the resource operation path")
         :documentation "The resource operation path")
   (request-method :initarg :request-method
                   :accessor request-method
                   :initform :get)
   (required-arguments :initarg :required-arguments
                       :accessor required-arguments
                       :initform nil)
   (optional-arguments :initarg :optional-arguments
                       :accessor optional-arguments
                       :initform nil)
   (body-schema :initarg :body-schema
                :accessor body-schema
                :initform nil
                :documentation "The type of the body schema (when request-method is :PUT or :POST)")
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
                   :documentation "resource operation authorizations spec")
   (options :initarg :options
            :accessor options
            :initform nil
            :documentation "resource operation options. Extensible. Pluggins data goes here.")
   (summary :initarg :summary
            :accessor api-summary
            :initform nil
            :documentation "resource operation short description")
   (documentation :accessor api-documentation
                  :initarg :documentation
                  :initform nil
                  :documentation "resource operation description"))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "The resource operation description"))

;; resource-operation

(defmethod initialize-instance :after ((resource-operation resource-operation) &rest initargs)
  (declare (ignore initargs))

  ;; Validate the operation
  (let ((args-in-uri (multiple-value-bind (scanner vars)
                         (parse-resource-operation-path (path resource-operation))
                       (declare (ignore scanner))
                       vars)))
    (loop for arg in args-in-uri
       do
         (assert (member arg (mapcar #'argument-name (required-arguments resource-operation)))
                 nil
                 "Argument ~a not declared in ~a" arg resource-operation)))

  ;; Install the resource operation
  (when *register-resource-operation*
    (let ((resource (or *api-resource* (error "Specify the api resource"))))
      ;; Validate the resource path and the resource operation path
      (when (not (cl-ppcre:scan (format nil "^~A" (resource-path resource))
                                (path resource-operation)))
        (error "The resource path ~A and the resource operation path ~A don't match"
               (resource-path resource)
               (path resource-operation)))
      (setf (gethash (name resource-operation) (resource-operations resource))
            resource-operation))))

(defmethod print-object ((resource-operation resource-operation) stream)
  (print-unreadable-object (resource-operation stream :type t :identity t)
    (format stream "~A ~A ~S"
            (name resource-operation)
            (request-method resource-operation)
            (path resource-operation))))

(defun make-resource-operation (name attributes args options)
  "Make an resource operation."
  (multiple-value-bind (required-arguments optional-arguments)
      (parse-resource-operation-arguments args)
    (apply #'make-instance 'resource-operation
           :name name
           :options options
           :required-arguments required-arguments
           :optional-arguments optional-arguments
           attributes)))

(defmethod find-resource-operation ((api symbol) operation-name &optional (error-p t))
  (find-resource-operation (find-api api) operation-name error-p))

(defmethod find-resource-operation ((api api-definition) operation-name &optional (error-p t))
  (loop for resource being the hash-values of (resources api)
     when (gethash operation-name (resource-operations resource))
     do (return-from find-resource-operation
          (gethash operation-name (resource-operations resource))))
  (when error-p
    (error "resource operation not found ~A" operation-name)))

(defmethod format-api-url ((api-name symbol) operation-name &rest args)
  (let ((api (find-api api-name)))
    (apply #'format-api-url api-name operation-name args)))

(defmethod format-api-url ((api api-definition) operation-name &rest args)
  (let ((resource-operation (find-resource-operation api operation-name)))
    (replace-vars-in-url (url-pattern resource-operation) args)))

(defmethod resource-operation-http-options ((api api-definition)
                                            (resource-operation resource-operation))
  (setf (hunchentoot:header-out "Allow")
        (string-upcase (string (request-method resource-operation)))))

(defclass resource-operation-argument ()
  ((name :initarg :name
         :type symbol
         :accessor argument-name
         :documentation "The argument name")
   (url-name :initarg :url-name
             :type string
             :accessor argument-url-name
             :documentation "The name with which the argument appears in the url")
   (type :initarg :type
         :accessor argument-type
         :documentation "The argument type")
   (default
       :initarg :default
     :accessor argument-default
     :initform nil
     :documentation "The argument default value")
   (required-p :initarg :required-p
               :accessor argument-required-p
               :initform t
               :documentation "Whether the argument is required or not")
   (documentation :initarg :documentation
                  :accessor argument-documentation
                  :initform nil
                  :documentation "The argument documentation")))

(defmethod print-object ((argument resource-operation-argument) stream)
  (print-unreadable-object (argument stream :type t :identity t)
    (format stream "~S (~S) required: ~A"
            (argument-name argument)
            (argument-type argument)
            (argument-required-p argument))))

(defclass resource-operation-implementation ()
  ((resource-operation :initarg :resource-operation
                       :initform (error "Provide the resource operation")
                       :accessor resource-operation
                       :documentation "The resource operation that is implemented")
   (primary :initarg :primary
            :initform (error "Provide the primary function")
            :accessor primary
            :documentation "Primary function")
   (accept :initarg :accept
		   :initform nil
		   :accessor accept
		   :documentation "The content type(s) accepted by this implementation.")
   (content-type :initarg :content-type
				 :initform nil
				 :accessor content-type
				 :documentation "The content type(s) generated by this implementation.")  
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
  (rs.auth::call-verifying-authorization
   (resource-operation resource-operation-implementation)
   (lambda ()
     (apply (primary resource-operation-implementation) args))))

(defun resop-impl-args (args resop-impl)
  (multiple-value-bind (lambda-list special-args)
      (extract-special-arguments args)
    (loop :for special-arg :in special-args
       :do
       (case (car special-arg)
         (:&posted-content (setf lambda-list
                                (add-keyword-argument
                                 (list (list :_posted-content (cdr special-arg)))
                                 lambda-list)))
         (:&resource-operation (setf lambda-list (add-keyword-argument
                                                 (list (list :_resource-operation (cdr special-arg)))
                                                 lambda-list)))
         (t (error "Invalid argument: ~A in resource operation implementation: ~A"
                   (car special-arg)
                   resop-impl))))
	(when (not (find '&key lambda-list))
	  (setf lambda-list (append lambda-list (list '&key))))
	lambda-list))

(defmacro implement-resource-operation (api-name name-and-options args &body body)
  "Define an resource operation implementation"
  (multiple-value-bind (name options)
      (if (listp name-and-options)
          (values (first name-and-options)
                  (rest name-and-options))
          (values name-and-options nil))
    (let* ((api (find-api api-name))
           (resource-operation (find-resource-operation api name nil)))
      (when resource-operation
        (validate-resource-operation-implementation-arguments
         (resop-impl-args args resource-operation)
         resource-operation))
	  `(let* ((api (find-api ',api-name))
			  (resource-operation (find-resource-operation api ',name))
			  (resource-operation-implementation
			   (make-instance 'resource-operation-implementation
							  :resource-operation resource-operation
							  :primary (lambda ,(resop-impl-args args resource-operation)
                                         ,@body)
							  :options ',options)))
         (let ((decorated-function
                (process-resource-operation-implementation-options
                 resource-operation-implementation)))
		   (register-resource-operation-implementation decorated-function))))))

(defun validate-resource-operation-implementation-arguments (args resource-operation)
  (flet ((ensure (thing message &rest args)
           (when (not thing)
             (apply #'error message args))))
    (multiple-value-bind (required optional rest keyword)
        (alexandria:parse-ordinary-lambda-list args)
      (declare (ignore optional rest))
	  (ensure (eql (length (required-arguments resource-operation))
				   (length required))
			  "Required arguments lists don't match: ~A ~A"
			  (mapcar #'argument-name (required-arguments resource-operation))
			  required)
      (loop for arg0 in (required-arguments resource-operation)
         for arg1 in required
         do
           (let ((arg0-name (argument-name arg0))
                 (arg1-name arg1))
             (ensure (equalp (symbol-name arg0-name)
                             (symbol-name arg1-name))
                     "Invalid resource operation implementation args list: ~A <> ~A"
                     arg0-name arg1-name)))
      (let ((optional-args-names
             (mapcar (alexandria:compose #'string #'caar)
                     keyword))
            (resource-operation-optional-args
             (mapcar (alexandria:compose #'string #'argument-name)
                     (optional-arguments resource-operation))))
        (let ((invalid-optional-args
               (set-difference (set-difference optional-args-names
                                        (list "_POSTED-CONTENT" "_RESOURCE-OPERATION")
                                               
                                               :test #'equalp)
                               resource-operation-optional-args
                               :test #'equalp)))
          (ensure (not invalid-optional-args)
                  "Invalid ~A implementation args: ~A"
                  resource-operation
                  invalid-optional-args))
        (let ((missing-optional-args
               (set-difference resource-operation-optional-args
                               optional-args-names
                               :test #'equalp)))
          (ensure (not missing-optional-args)
                  "Missing args in ~A implementation: ~A"
                  resource-operation
                  missing-optional-args))
        (when (member (request-method resource-operation) (list :put :post :patch))
          (ensure (member :_posted-content (mapcar #'caar keyword))
                  "&POSTED-CONTENT not found in ~A implementation lambda-list" resource-operation)
          )))))

(defun configure-resource-operation-implementation
    (name &rest options)
  "Configure or reconfigure an already existent resource operation implementation"
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
      (register-resource-operation-implementation
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

(defmethod accept ((decoration resource-operation-implementation-decoration))
  (accept (decorates decoration)))

(defmethod content-type ((decoration resource-operation-implementation-decoration))
  (content-type (decorates decoration)))

(defun find-resource-operation-implementation (name &key accept content-type)
  (or (find-if (lambda (roi)
				 (match-resource-operation-implementation
				  roi
				  :accept accept
				  :content-type content-type))
			   (get name :resource-operation-implementation))
      (error "resource operation ~A not implemented" name)))

(defun register-resource-operation-implementation (resource-operation-implementation)
  (let ((key (name (resource-operation resource-operation-implementation))))
	(push resource-operation-implementation
		  (get key :resource-operation-implementation))
	resource-operation-implementation))

(defun match-resource-operation-implementation (roi &key accept content-type)
  (and 
   (or (null accept)
	   (null (accept roi))
	   (member accept (accept roi)))
   (or (null content-type)
	   (null (content-type roi))
	   (member content-type (content-type roi)))))

(defun execute-resource-operation-implementation (function-implementation request)
  (let ((resource-operation (resource-operation function-implementation)))
    (log5:with-context (cons :resource
                             (list :name (name resource-operation)
                                   :path (path resource-operation)
                                   :method (request-method resource-operation)))
      (rs.error:with-error-handler ()
        (let ((args (extract-function-arguments resource-operation request)))
          (apply function-implementation
                 (append
                  args
                  (when (member (request-method resource-operation) (list :put :post :patch))
                    (let ((posted-content (get-posted-content request)))
                      (log5:log-for (rest-server) "Posted content: ~A" posted-content)
                      (list :_posted-content (parse-posted-content posted-content))))
                  (list :_resource-operation resource-operation)
                  (list :allow-other-keys t))))))))

(defun find-optional-argument (name resource-operation)
  (or
   (find name (optional-arguments resource-operation)
         :key (alexandria:compose #'make-keyword #'argument-name))
   (rs.schema:validation-error "Optional argument ~A not found in ~A ~A"
                               name
                               resource-operation
                               (optional-arguments resource-operation))))

(defun resource-operation-matches-request-p (resource-operation request)
  (let ((scanner (parse-resource-operation-path (path resource-operation))))
    (and (cl-ppcre:scan scanner (request-uri request))
         (or (equalp (hunchentoot:request-method request) :options)
             (equalp (request-method resource-operation)
                     (hunchentoot:request-method request))))))

;; url formatting

(defgeneric format-resource-operation-url (resource-operation &rest args)
  (:documentation "Print the resource operation url")
  (:method ((resource-operation resource-operation) &rest args)
    (let ((url-noparams
           (replace-vars-in-url (url-pattern-noparams resource-operation) args))
          (optional-args
           (loop
              for key in args by #'cddr
              for value in (cdr args) by #'cddr
              for optional-arg = (find key (optional-arguments resource-operation)
                                       :key (alexandria:compose #'make-keyword #'argument-name))
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
  (format nil "~A=~A"
          (or (argument-url-name arg)
              (string-downcase (string (argument-name arg))))
          (format-argument-value value (argument-type arg))))

(defun parse-resource-operation-argument (arg)
  (destructuring-bind (name type &optional documentation) arg
    (make-instance 'resource-operation-argument
                   :name name
                   :url-name (string-downcase (string name))
                   :type (parse-argument-type type)
                   :required-p t
                   :documentation documentation)))

(defun parse-optional-resource-operation-argument (arg)
  (destructuring-bind (name type default-value &optional documentation) arg
    (make-instance 'resource-operation-argument
                   :name name
                   :url-name (string-downcase (string name))
                   :type (parse-argument-type type)
                   :required-p nil
                   :default default-value
                   :documentation documentation)))

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
               (push (parse-optional-resource-operation-argument argument) optional-arguments)
               (push (parse-resource-operation-argument argument) required-arguments)))
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

(defun authorization-string (token)
  (cond
    ((stringp token)
     token)
    ((listp token)
     (format nil "~A ~A"
             (access:access token :token-type)
             (access:access token :access-token)))))

(defun client-stub (resource-operation &key (package *package*)
                                         export-p)
  (let ((request-url (gensym "REQUEST-URL-"))
        (response (gensym "RESPONSE-"))
        (status-code (gensym "STATUS-CODE-")))
    (let ((required-args (required-arguments resource-operation))
          (optional-args (optional-arguments resource-operation))
          (client-stub-name (intern (symbol-name (name resource-operation)) package)))
      `(progn
         (defun ,client-stub-name
             ,(append
               (when (member (request-method resource-operation) '(:post :put :patch))
                 (list 'posted-content))
               (loop for arg in required-args collect
                    (intern (symbol-name (argument-name arg)) package))
               (cons '&key
                     (append
                      (list '(accept "application/json"))
                      (list '(additional-headers ()))
                      (list '(parse-response t))
                      (list '(encode-request-arguments t))
                      (list '(error-p *signal-client-function-errors*))
                      (when (authorizations resource-operation)
                        (list '(authorization (error "Provide an authorization value"))))
                      (when optional-args
                        (loop for arg in optional-args collect
                             (list (intern (symbol-name (argument-name arg)) package)
                                   (argument-default arg)
                                   (intern (format nil "~A-PROVIDED-P"
                                                   (symbol-name (argument-name arg)))
                                           package))))
                      (when (member (request-method resource-operation) '(:post :put :patch))
                        (list '(content-type "application/json"))))))
           ,@(when (api-documentation resource-operation)
               (list (api-documentation resource-operation)))
           (declare (ignorable encode-request-arguments))
           (log5:log-for (rest-server) "Client stub: ~A" ',(name resource-operation))
           (assert *api-backend* nil "Error: this is an resource operation. No api backend selected. Wrap this function call with with-api-backend")
           (let ((,request-url (format nil "~A~A"
                                       *api-backend*
                                       (replace-vars-in-url
                                        ,(url-pattern-noparams resource-operation)
                                        (list
                                         ,@(loop for arg in required-args
                                              collect (make-keyword (argument-name arg))
                                              collect `(if encode-request-arguments
                                                           (format-argument-value
                                                            ,(intern (symbol-name (argument-name arg)) package)
                                                            (parse-argument-type ',(argument-type-spec (argument-type arg))))
                                                           ,(intern (symbol-name (argument-name arg)) package))))))))
             (log5:log-for (rest-server)  "Request: ~A ~A" ,(request-method resource-operation) ,request-url)
             ,(when (member (request-method resource-operation)
                            '(:post :put :patch))
                `(log5:log-for (rest-server) "Posted content: ~A"
                               posted-content))
             (multiple-value-bind (,response ,status-code)
                 (with-text-content-types
                   (drakma:http-request
                    ,request-url
                    :method ,(request-method resource-operation)
                    :proxy *rest-server-proxy*
                    :content ,(when (member (request-method resource-operation)
                                            '(:post :put :patch))
                                `(encode-posted-content posted-content
                                                        (parse-content-type content-type)))
                    :content-type ,(when (member (request-method resource-operation)
                                                 '(:post :put :patch))
                                     'content-type)
                    :parameters (append
                                 ,@(loop for arg in optional-args
                                      collect
                                        `(when ,(intern (format nil "~A-PROVIDED-P" (symbol-name (argument-name arg))) package)
                                           (list (cons
                                                  ,(symbol-name (argument-name arg))
                                                  (if encode-request-arguments
                                                      (format-argument-value
                                                       ,(intern (symbol-name
                                                                 (argument-name arg)) package)
                                                       (parse-argument-type ',(argument-type-spec (argument-type arg))))
                                                      ,(intern (symbol-name
                                                                (argument-name arg)) package)))))))
                    :additional-headers (append
                                         (list (cons "Accept" accept)
                                               ,@(when (authorizations resource-operation)
                                                   '((cons "Authorization" (authorization-string authorization)))))
                                         additional-headers)))
               (log5:log-for (rest-server) "Response: ~A" ,response)
               (log5:log-for (rest-server) "Status: ~A" ,status-code)
               (handle-api-response ,response
                                    ,status-code
                                    accept
                                    parse-response
                                    error-p))))
         ,@(when export-p
             `((export ',client-stub-name ,(package-name package))))))))

(defun handle-api-response (response status-code accept parse-response error-p)
  (cond
    ((and (>= status-code 200)
          (< status-code 400))
     (values
      (if parse-response
          (parse-api-response response accept parse-response)
          response)
      status-code))
    ((assoc status-code rs.error::*http-status-codes-conditions*)
     (if error-p
         (error (cdr (assoc status-code rs.error::*http-status-codes-conditions*)))
         (values
          (if parse-response
              (parse-api-response response accept parse-response)
              response)
          status-code)))
    (t
     (if error-p
         (rs.error:http-error status-code "Error")
         (values
          (if parse-response
              (parse-api-response response accept parse-response)
              response)
          status-code)))))

(defun parse-api-response (response accept format)
  (if (functionp format)
      (funcall format response)
      (let ((parsed-accept
             (string-case:string-case (accept)
               ("text/xml" :xml)
               ("application/xml" :xml)
               ("text/html" :html)
               ("application/json" :json)
               ("text/lisp" :sexp)
               (t (error "Could not parse accept: ~A" accept)))))
        (%parse-api-response response parsed-accept format))))

(defgeneric %parse-api-response (response accept format)
  (:method (response (accept (eql :json)) format)
    (let ((parsed (json:decode-json-from-string response)))
      (case format
        (:plist (alexandria:alist-plist parsed))
        (:alist parsed)
        (t parsed))))
  (:method (response (accept (eql :sexp)) format)
    (read-from-string response)))

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
  "Implement an resource operation case"
  `(configure-resource-operation-implementation
    ',name
    (list
     :conditional-dispatch
     :predicate (lambda (&rest args)
                  (declare (ignore args))
                  (and (rs::request-reply-content-type hunchentoot:*request*)
                       (let ((accepts (split-sequence:split-sequence #\, (rs::request-reply-content-type hunchentoot:*request*))))
                         (intersection (list ,accept-content-type)
                                       accepts :test #'equalp))))
     :when-true (lambda ,args
                  (with-reply-content-type (,accept-content-type)
                    ,@body)))))

(defun self-reference (&rest args)
  (rs.serialize:set-attribute
   :href
   (apply #'format-absolute-resource-operation-url *resource-operation* args)))

(defmethod process-resource-operation-implementation-option
    ((option (eql :consumes))
     resource-operation-implementation
     &key content-types #+(or abcl ecl)&allow-other-keys)
  (setf (accept resource-operation-implementation) content-types)
  resource-operation-implementation)

(defmethod process-resource-operation-implementation-option
    ((option (eql :produces))
     resource-operation-implementation
     &key content-types #+(or abcl ecl)&allow-other-keys)
  (setf (content-type resource-operation-implementation) content-types)
  resource-operation-implementation)
