(in-package :rest-server)

(defparameter *apis* (make-hash-table :test #'equalp)
  "Global hashtable containing the apis defined")
(defparameter *api* nil "The current api")
(defvar *api-function-arguments-types*
  (list :boolean :integer :string))
(defvar *rest-server-proxy* nil)
(defvar *register-api-function* t "Whether to try to register the api function on creation. Bind to nil to prevent that")

;; Toplevel api

(defun find-api (name)
  "Find api by name"
  (gethash name *apis*))

(defmacro with-api (api &body body)
  "Execute body under api scope.
   Example:
   (with-api test-api
      (define-api-function get-user :get (:url-prefix \"users/{id}\")
                                    '((:id :integer))))"
  `(call-with-api ',api (lambda () ,@body)))

(defun call-with-api (api function)
  (let ((*api* (if (symbolp api) (find-api api)  api)))
    (funcall function)))

(defmacro define-api-function (name method options args)
  "Helper macro to define an api function"
  `(make-api-function ',name ',method ',options ',args))

(defvar *api-backend* nil "API backend address") 

(defmacro with-api-backend (backend &body body)
  "Execute the client api function calling backend"
  `(call-with-api-backend ,backend (lambda () ,@body)))

(defun call-with-api-backend (backend function)
  (let ((*api-backend* backend))
    (funcall function)))

(defmacro define-api (name options &body functions)
  "Define an api."
  `(progn
     (make-instance 
      'api-definition 
      :name ',name
      ,@options)
     (with-api ,name
      ,@(loop for x in functions
         collect (parse-api-method-definition x)))
     ,@(let ((*register-api-function* nil))
            (loop for x in functions
               collect (client-stub
                        name
                        (destructuring-bind (name options args) x
                          (make-api-function
                           name
                           (getf options :method)
                           options
                           args)))))))

(defun start-api (api address port &optional (api-implementation-package *package*))
  "Start an api at address and port.
   api-implementation-package: is the package where the api-functions are implemented."
  (hunchentoot:start
   (make-instance 'api-acceptor
                  :address address
		  :port port
                  :api (if (symbolp api)
                           (find-api api)
                           api)
                  :api-implementation-package api-implementation-package)))

;; Hunchentoot api acceptor

(defclass api-acceptor (hunchentoot:acceptor)
  ((api :initarg :api
        :accessor api
        :initform (error "Provide the api"))
   (api-implementation-package :initarg :api-implementation-package
                       :accessor api-implementation-package
                       :initform *package*))
  (:documentation "Hunchentoot api acceptor"))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor api-acceptor) request)
  (loop for api-function being the hash-value of (functions (api acceptor))
     when (api-function-matches-request-p api-function request)
     return (let ((result 
		   (execute-api-function-implementation 
		    api-function
		    (find-api-function-implementation (name api-function) acceptor)
		    request)))
	      (if (stringp result) result (prin1-to-string result)))
     finally (call-next-method)))

;; The api class
(defclass api-definition ()
  ((name :accessor name :initarg :name)
   (functions :accessor functions :initarg :functions :initform (make-hash-table :test #'equalp))
   (content-types :initarg :content-types
                  :initform nil
                  :accessor content-types)
   (version :initarg :version
            :initform nil
            :accessor version)
   (documentation :accessor api-documentation :initarg :documentation :initform nil))
  (:documentation "The api class"))

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
   (content-types :initarg :content-types
                  :accessor content-types
                  :initform :all)
   (options :initarg :options
            :accessor options
            :initform nil)
   (documentation :accessor api-documentation
		  :initarg :documentation))
  (:metaclass sb-mop:funcallable-standard-class)
  (:documentation "The api function description"))


;; Parsing

(defun request-uri-prefix (request-uri)
  (if (find #\? request-uri)
      (subseq request-uri 0 (position #\? request-uri))
      request-uri))

(defun request-uri-parameters (request-uri)
  (ppcre:register-groups-bind (arguments)
      (".*\\?(.*)$" request-uri)
    (when arguments
      (loop for argument-and-value in
           (split-sequence:split-sequence #\& arguments)
           collect
           (let ((argument-and-value-split
                  (split-sequence:split-sequence #\= argument-and-value)))
             (cons (intern (string-upcase (car argument-and-value-split)))
                   (cadr argument-and-value-split)))))))             

(defun uri-match-p (uri-prefix request-uri)
  (cl-ppcre:scan 
   (parse-uri-prefix uri-prefix)
   request-uri))

(defun url-pattern (api-function)
  (parse-uri-prefix (uri-prefix api-function)))

(defun replace-vars-in-url (url plist)
  (labels ((do-replace (url plist)
	     (if (null plist)
		 url
		 (let* ((var (car plist))
			(val (cadr plist))
			(pattern `(:sequence 
				   "{" 
				   ,(string-downcase (symbol-name var))
                                   (:greedy-repetition 0 nil (:inverted-char-class #\{ #\}))
				   "}")))
		   (do-replace 
		       (cl-ppcre:regex-replace 
			pattern 
			url
			(format nil "~A" val))
		     (cddr plist))))))
    (do-replace url plist)))

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

(defun parse-api-url (request-string)
  (multiple-value-bind (scanner vars)
	(parse-uri-prefix (request-uri-prefix request-string))
    (let ((parameters 
	   (when (find #\? request-string)
	     (parse-parameters 
	      (subseq request-string (position #\? request-string))
	      ))))
      (values scanner vars parameters))))

(defun parse-uri-prefix (string)
  (let* ((vars nil)
	 (uri-prefix-regex (remove 
			    nil
			    (loop for x in (cl-ppcre:split '(:register (:char-class #\{ #\})) string :with-registers-p t)
			       with status = :norm
			       collect 
			       (case status
				 (:norm
				  (cond ((string= x "{")
					 (setf status :invar)
					 nil)
					((string= x "}")
					 (error "Parse error"))
					(t x)))
				 (:invar
				  (cond ((string= x "}")
					 (setf status :norm)
					 nil)
					((string= x "{")
					 (error "Parse error"))
					(t 
					 (push (intern (string-upcase x))
					       vars)
					 `(:register (:non-greedy-repetition 1 nil (:inverted-char-class #\/ #\?))))))))))
	 (scanner
	  `(:sequence
	    :start-anchor
	    (:alternation
	     (:sequence
	      ,@uri-prefix-regex)
	     (:sequence
	      ,@uri-prefix-regex
	      #\?
	      (:non-greedy-repetition 0 nil :everything)))
	    :end-anchor)))
    (values scanner vars)))

(defun parse-var-value (string type)
  (case type
    (:boolean (cond
                ((equalp string "nil") nil)
                ((equalp string "false") nil)
                ((equalp string "true") t)
		((equalp string "yes") t)
		((equalp string "no") nil)
                ((equalp string "t") t)
		(t (error "Cannot parse ~s as boolean" string))))
    (:integer (or (parse-integer string :junk-allowed t)
		  (error "Cannot parse ~s as integer" string))) 
    (:string (string-trim (list #\") string))
    (t string)))

(defun extract-function-arguments (api-function request)
  (let ((scanner (parse-uri-prefix (uri-prefix api-function))))
    (multiple-value-bind (replaced-uri args) 
	(ppcre:scan-to-strings scanner (hunchentoot:request-uri request))
      (declare (ignore replaced-uri))
      (let ((args (loop for arg across args
		     when arg
		     collect arg)))
	(let ((required-args
	       (loop
		  for reqarg in (required-arguments api-function)
		  for arg in args
		  collect (parse-var-value arg (second reqarg))))
	      (optional-args
	       (loop 
		  for (var . string) in (request-uri-parameters (hunchentoot:request-uri request))
		  for optarg = (find-optional-argument (make-keyword var) api-function)
		  appending 
		  (list (make-keyword (symbol-name (first optarg)))
                        (parse-var-value string (second optarg))))))
	  (append required-args optional-args))))))

;; Implementation

;; api-definition

(defmethod print-object ((api-definition api-definition) stream)
  (print-unreadable-object (api-definition stream :type t :identity t)
    (format stream "~A" (name api-definition))))

(defun register-api-definition (api-definition)
  (setf (gethash (name api-definition) *apis*) api-definition))

(defmethod initialize-instance :after ((api-definition api-definition) &rest initargs)
  (declare (ignore initargs))
  
  
  (register-api-definition api-definition))

(defun backend-var (backend)
  (intern (format nil "*BACKEND-URL-~A*" backend) :rest-server))

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

  (configure-api-function api-function)

  ;; Install the api function
  (when *register-api-function*
    (let ((api (or *api* (error "Specify the api"))))
      (setf (gethash (name api-function) (functions api))
            api-function))))

;; (defmethod initialize-instance :after ((api-function api-function) &rest args)
;;   (sb-mop:set-funcallable-instance-function
;;    api-function
;;    (lambda (&rest args)
;;      (call-api-function api-function args))))

;; (defun call-api-function (api-function args))

(defmethod print-object ((api-function api-function) stream)
  (print-unreadable-object (api-function stream :type t :identity t)
    (format stream "~A ~A"
	    (name api-function)
	    (request-method api-function))))

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

(defun make-api-function (name method options args)
  "Make an api function."
  (multiple-value-bind (required-arguments optional-arguments)
      (parse-api-function-arguments args)
    (make-instance 'api-function
                   :name name
                   :request-method method
                   :uri-prefix (getf options :uri-prefix)
                   :documentation (getf options :documentation)
                   :options options
                   :required-arguments required-arguments
                   :optional-arguments optional-arguments)))

(defmethod find-api-function ((api symbol) function-name)
  (find-api-function (find-api api) function-name))

(defmethod find-api-function ((api api-definition) function-name)
  (gethash function-name (functions api)))

(defun format-api-url (api function-name &rest args)
  (let ((api-function (find-api-function api function-name)))
    (replace-vars-in-url (url-pattern api-function) args)))

(defmacro implement-api-function (name-and-options args &body body)
  "Define an api function implementation"
  (multiple-value-bind (name options)
      (if (listp name-and-options)
	  (values (first name-and-options)
		  (alexandria:plist-alist (rest name-and-options)))
	  (values name-and-options nil))
    (let ((parsed-lambda-list (multiple-value-list (sb-int:parse-lambda-list args))))
      `(defgeneric ,name (,@(first parsed-lambda-list) &key ,@(mapcar #'first (nth 5 parsed-lambda-list)))
	 (:method-combination method-combination-utilities:lax)
	 (:method ,args
	   ,@body)
	 ,@(loop for (option . val) in options
	      collect (%expand-api-function-wrapping name options option val name args))))))

(defun %expand-api-function-wrapping (api-function-name
				      api-function-options
				      option value name args)
  (declare (ignore name))
  (let ((wrapper (expand-api-function-wrapping api-function-name
					       api-function-options
					       option value args)))
    (when wrapper
      `(:method :around ,option ,args
		,wrapper))))

(defmethod expand-api-function-wrapping (api-function-name api-function-options
					 (option (eql :logging)) enabled args)
  (declare (ignore args api-function-options))
  (if enabled
      `(progn
	 (log5:log-for (rest-server) "API: Handling ~A ~A by ~A"
		       (hunchentoot:request-method*)
		       (hunchentoot:request-uri*) ',api-function-name)
	 (let ((posted-content (when (hunchentoot:raw-post-data :external-format :utf8)
				 (hunchentoot:raw-post-data :external-format :utf8))))
	   (when posted-content (log5:log-for (rest-server) "Posted content: ~A" posted-content)))
	 (let ((result (call-next-method)))
	   (log5:log-for (rest-server) "Response: ~A" result)
	   result))
      `(call-next-method)))

(defun find-api-function-implementation (name acceptor)
  (or (ignore-errors (symbol-function (intern (symbol-name name) (api-implementation-package acceptor))))
      (error "Api function ~A not implemented in package ~A" name (api-implementation-package acceptor))))

(defvar *parse-posted-content* :use-request-content-type
  "How to parse the posted content type. One of:
   * :use-request-content-type: parse posted content using the format specified in the request's content type
   * :infer: Try different methods of parsing the content type until success
   * :raw: Leave the posted content unparsed. Just pass the string.")

(defun parse-posted-content (posted-content &optional (method *parse-posted-content*))
  (ecase method
    (:use-request-content-type
     ;; Use the request content type to parse the posted content
     (let ((content-type (hunchentoot:content-type*)))
       (let ((format
	      (cond
		((member content-type (list "text/xml" "application/xml"))
		 :xml)
		((member content-type (list "application/json"))
		 :json)
		((member content-type (list "application/lisp" "text/lisp"))
		 :sexp)
		(t (error 'http-unsupported-media-type-error "Content type not supported ~A" content-type)))))
	 (parse-api-input format posted-content))))
    (:infer
     (error "Not implemented"))
     
    ;; Infer the posted content format. Try parsing with different
    ;; methods until success
    (:raw posted-content)))

(defun execute-api-function-implementation (api-function function-implementation request)
  (let ((args (extract-function-arguments api-function request)))
    (apply function-implementation
	   (append 
	    (when (member (request-method api-function) (list :put :post))
	      (let ((posted-content (when (hunchentoot:raw-post-data :external-format :utf8)
				      (hunchentoot:raw-post-data :external-format :utf8))))
		(log5:log-for (rest-server) "Posted content: ~A" posted-content)
		(list (parse-posted-content posted-content))))
	    args))))

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

(defun add-wrapping-function (api-function function)
  )

(defmethod configure-api-function ((api-function api-function))
  (let ((options (alexandria:plist-alist (options api-function))))
    (loop for option in options
       do
         (configure-api-function-option (car option) option api-function))))

(defmethod configure-api-function-option (option option-value api-function)
  )

(defmethod configure-api-function-option ((option (eql :logging)) option-value api-function)
  (add-wrapping-function api-function
                         (lambda (next-function)
                           (if (cadr option-value)
                               (progn
                                 (log5:log-for (rest-server) "API: Handling ~A ~A by ~A"
                                               (hunchentoot:request-method*)
                                               (hunchentoot:request-uri*) (name api-function))
                                 (let ((posted-content (when (hunchentoot:raw-post-data :external-format :utf8)
                                                         (hunchentoot:raw-post-data :external-format :utf8))))
                                   (when posted-content (log5:log-for (rest-server) "Posted content: ~A" posted-content)))
                                 (let ((result (funcall next-function)))
                                   (log5:log-for (rest-server) "Response: ~A" result)
                                   result))
                               ;; else
                               (funcall next-function)))))

(defun authenticate-key (token)

  )
  
(defmethod configure-api-function-option ((option (eql :authenticate)) option-value api-function)
  (add-wrapping-function api-function
                         (lambda (next-function)
                           (if (not (authenticate-key (getf hunchentoot:*request* :token)))
			       (error "Authentication error")
			       (funcall next-function)))))

(defun parse-api-method-definition (method-spec)
  (destructuring-bind (name options args) method-spec
    `(define-api-function ,name ,(getf options :method)
                       ,options
                       ,args)))

;; Client implementation

(defun client-stub (api-name api-function)
  (let ((request-url (gensym "REQUEST-URL-"))
        (response (gensym "RESPONSE-")))
    (let ((required-args (required-arguments api-function))
	  (optional-args (optional-arguments api-function)))
      `(progn
	 (defun ,(name api-function)
             ,(append
                (if (member (request-method api-function) '(:post :put))
                    (list 'posted-content)
                    nil)
                (loop for x in required-args collect 
		     (intern (symbol-name (car x))))
                (if optional-args
                    (cons '&key (loop for x in optional-args collect 
				     (list (intern (symbol-name (first x))) 
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
                                              collect (intern (symbol-name (car x)))))))))
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
                                             `(sb-ext:string-to-octets posted-content))
                               :parameters (list 
                                            ,@(loop for x in optional-args
                                                 collect
                                                 (progn
                                                   `(cons 
                                                     ,(symbol-name (car x))
                                                     (format nil "~A" ,(intern (symbol-name 
                                                                                (car x)))))))))))
               (log5:log-for (rest-server) "Response: ~A" ,response)
               ,response)))))))

(defgeneric url-pattern-noparams (api-function))
(defmethod url-pattern-noparams ((api-function api-function))
  (let* ((pattern (uri-prefix api-function))
	(pos (position #\? pattern)))
    (if pos
	(subseq pattern 0 pos)
	pattern)))