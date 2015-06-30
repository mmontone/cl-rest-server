(in-package :rest-server)

(defparameter *apis* (make-hash-table :test #'equalp)
  "Global hashtable containing the apis defined")
(defparameter *api* nil "The current api")

(defvar *rest-server-proxy* nil)

(defparameter *register-api-resource* t "Wether to register the created resource in the current API")
(defparameter *api-resource* nil "The current api resource")

(defvar *register-resource-operation* t
  "Whether to try to register the resource operation on creation. Bind to nil to prevent that")

;; Util

(defmethod json:encode-json ((o (eql :false))
                        &optional (stream json:*json-output*))
  (json::write-json-chars "false" stream))

;; Toplevel api

(defun find-api (name &key (error-p t))
  "Find api by name"
  (multiple-value-bind (api found-p)
      (gethash name *apis*)
    (when (and (not found-p)
	       error-p)
      (error "API ~S not found" name))
    api))

(defmacro with-api (api &body body)
  "Execute body under api scope.
   Example:
   (with-api test-api
      (define-resource-operation get-user :get (:url-prefix \"users/{id}\")
                                    '((:id :integer))))"
  `(call-with-api ',api (lambda () ,@body)))

(defun call-with-api (api function)
  (let ((*api* (if (symbolp api) (find-api api)  api)))
    (funcall function)))

(defvar *api-backend* nil "API backend address") 

(defmacro with-api-backend (backend &body body)
  "Execute the client resource operation calling backend"
  `(call-with-api-backend ,backend (lambda () ,@body)))

(defun call-with-api-backend (backend function)
  (let ((*api-backend* backend))
    (funcall function)))

(defmacro define-api (name options &body resources)
  "Define an api."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (apply #'make-instance 
	    'api-definition 
	    :name ',name
	    ',options)
     (with-api ,name
      ,@(loop for x in resources
	     collect `(define-api-resource ,@x)))))

(defun start-api (api address port &optional (development-mode rs.error:*development-mode*)
				     configuration-args)
  "Start an api at address and port.

   In production mode, we bind the api directly. In development mode, we only bind the API name in order to be able to make modifications to the api"
  (when configuration-args
    (configure-api api configuration-args)) 
  (let ((api (if (equalp development-mode :development)
		 (if (symbolp api)
		     api
		     (api-name api))
		 ;; else
		 (if (symbolp api)
		     (find-api api)
		     api))))
    (let ((api-acceptor (make-instance 'api-acceptor
				       :address address
				       :port port
				       :api api
				       :development-mode development-mode)))
      (hunchentoot:start api-acceptor)
      api-acceptor)))

(defun stop-api (api-acceptor)
  (hunchentoot:stop api-acceptor))

;; Hunchentoot api acceptor

(defclass api-acceptor (hunchentoot:acceptor)
  ((api :initarg :api
	:initform (error "Provide the api"))
   (development-mode :initarg :development-mode
		     :accessor development-mode
		     :initform rs.error:*development-mode*))
  (:documentation "Hunchentoot api acceptor"))

(defmethod api ((acceptor api-acceptor))
  (let ((api-or-name (slot-value acceptor 'api)))
    (if (symbolp api-or-name)
	(find-api api-or-name)
	api-or-name)))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor api-acceptor) request)
  (or 
   (api-dispatch-request (api acceptor) request)
   (rs.error::with-condition-handling
     (error 'rs.error:http-not-found-error
	    :format-control "Resource not found"))))

;; The api class
(defclass api-definition ()
  ((name :accessor name
	 :initarg :name
	 :initform nil
	 :type symbol
	 :documentation "The api id")
   (title :accessor title
	  :initarg :title
	  :initform nil
	  :type string
	  :documentation "A descriptive title for the api")
   (resources :accessor resources
	      :initarg :resources
	      :initform (make-hash-table :test #'equalp))
   (version :initarg :version
            :initform nil
            :accessor version)
   (documentation :accessor api-documentation
		  :initarg :documentation
		  :initform nil)
   (authorization-enabled 
    :initarg :authorization-enabled
    :accessor authorization-enabled
    :initform t))
  (:documentation "The api class"))

(defgeneric api-http-options (api)
  (:method api-http-options ((api api-definition))
	   (setf (hunchentoot:header-out "Server") "Hunchentoot")))

(defgeneric api-dispatch-request (api request)
  (:method :around ((api api-definition) request)
	   (let ((*api* api))
	     (call-next-method)))
  (:method ((api api-definition) request)
    (flet ((resource-operation-dispatch ()
	     (let ((rs.error:*development-mode* (development-mode hunchentoot:*acceptor*)))
	       (loop for resource in (list-api-resources api)
		  when (resource-matches-request-p resource request)
		  do
		    (loop for resource-operation in (list-api-resource-functions resource)
		       when (resource-operation-matches-request-p resource-operation request)
		       do (return-from api-dispatch-request
			    (if (equalp (hunchentoot:request-method request) :options)
				(progn
				  (resource-operation-http-options api resource-operation)
				  "")
					; else
				(let* ((*resource-operation* resource-operation)
				       (resource-operation-implementation
					(find-resource-operation-implementation (name resource-operation)))
				       (result 
					(api-execute-function-implementation
					 api
					 resource-operation-implementation
					 resource
					 request)))
				  (if (stringp result) result (prin1-to-string result)))))))
	       ;; Return nil as the request could not be handled
	       nil)))
      (if (equalp (hunchentoot:request-method request)
		  :options)
	  (if (equalp (hunchentoot:request-uri request) "*")
	      ;; http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html
	      ;; If the Request-URI is an asterisk ("*"), the OPTIONS request is intended
	      ;; to apply to the server in general rather than to a specific resource.
	      (progn
		(api-http-options api)
		"")
	      ;; If the Request-URI is not an asterisk, the OPTIONS request applies only to
	      ;; the options that are available when communicating with that resource.
	      (progn
		(loop
		   for resource in (list-api-resources api)
		   when (equalp (hunchentoot:request-uri request)
				(resource-path resource))
		   do (return-from api-dispatch-request
			(progn
			  (resource-http-options resource api)
			  "")))
		;; else, try to dispatch an resource operation
		(resource-operation-dispatch)))
	  ;; else, dispatch to an resource operation
	  (resource-operation-dispatch)))))

(defmacro implement-api (api-name options &body resource-implementations)
  "Implement an api"
  `(let* ((api-definition (find-api ',api-name)))
     (process-api-options api-definition ',options)

     ;; Implement resources
     ,@(loop for resource-implementation in resource-implementations
	  collect `(implement-api-resource ,api-name ,@resource-implementation))))

(defun configure-api
    (api-or-name &rest options)
  "Configure or reconfigure an already existent api"
  (let ((api (if (symbolp api-or-name)
		 (find-api api-or-name)
		 api-or-name)))
    (process-api-options api options)))

(defun process-api-options (api options)
  (loop for option in (reverse options)
     do (destructuring-bind (option-name &rest args) option
	  (apply #'process-api-option option-name
		 api
		 args))))

(defgeneric process-api-option
    (option-name api &rest args)
  (:method (option-name api &rest args)
    (declare (ignore args))
    (error "~A is not a valid api option" option-name))
  (:documentation "Overwrite this in decorations"))

(defmethod api-execute-function-implementation ((api-definition api-definition)
						resource-operation-implementation
						resource
						request)
  (resource-execute-function-implementation
   resource
   resource-operation-implementation
   request))

(defmethod list-api-resources ((api-definition api-definition))
  (loop for resource being the hash-values of (resources api-definition)
       collect resource))

(defmethod resource-operations ((api-definition api-definition))
  (loop for resource in (list-api-resources api-definition)
     appending (alexandria:hash-table-values (resource-operations resource))))

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
   (parse-resource-operation-path uri-prefix)
   request-uri))

(defun url-pattern (resource-operation)
  (parse-resource-operation-path (path resource-operation)))

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


(defun parse-api-url (request-string)
  (multiple-value-bind (scanner vars)
	(parse-resource-operation-path (request-uri-prefix request-string))
    (let ((parameters 
	   (when (find #\? request-string)
	     (parse-parameters 
	      (subseq request-string (position #\? request-string))
	      ))))
      (values scanner vars parameters))))

(defun parse-resource-operation-path (string)
  (let* ((vars nil)
	 (path-regex (remove 
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
	      ,@path-regex)
	     (:sequence
	      ,@path-regex
	      #\?
	      (:non-greedy-repetition 0 nil :everything)))
	    :end-anchor)))
    (values scanner vars)))

(defun extract-function-arguments (resource-operation request)
  (let ((scanner (parse-resource-operation-path (path resource-operation))))
    (multiple-value-bind (replaced-uri args) 
	(ppcre:scan-to-strings scanner (hunchentoot:request-uri request))
      (declare (ignore replaced-uri))
      (let ((args (loop for arg across args
		     when arg
		     collect arg)))
	(let ((required-args
	       (loop
		  for reqarg in (required-arguments resource-operation)
		  for arg in args
		  collect (parse-argument-value arg (second reqarg))))
	      (optional-args
	       (loop 
		  for (var . string) in (request-uri-parameters (hunchentoot:request-uri request))
		  for optarg = (find-optional-argument (make-keyword var) resource-operation)
		  appending 
		  (list (make-keyword (symbol-name (first optarg)))
                        (parse-argument-value string (second optarg))))))
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

(defvar *parse-posted-content* :use-request-content-type
  "How to parse the posted content type. One of:
   * :use-request-content-type: parse posted content using the format specified in the request's content type
   * :infer: Try different methods of parsing the content type until success
   * :raw: Leave the posted content unparsed. Just pass the string.")

(defun some-test (list test)
  (let ((res nil))
    (loop for elem in list
	 while (not res)
	 do (setf res (funcall test elem)))
    res))

(defgeneric parse-api-input (format string)
  (:documentation "Parses content depending on its format"))

(defmethod parse-api-input ((format (eql :json)) string)
  (json:decode-json-from-string string)) 

(defun fold-tree (f g tree)
  (if (listp tree)
      (let ((name (first tree))
	    (children (cdr tree)))
	(funcall f
		 (cons name (mapcar (lambda (child)
				      (fold-tree f g child))
				    children))))
	(funcall g tree)))

(defmethod parse-api-input ((format (eql :xml)) string)
  (let ((data
	 (cxml:parse string (make-xmls-builder))))
    (fold-tree (lambda (node)
		 (cond
		   ((equalp (car node) "_ITEM")
		    ;; It is a list item
		    (cons :li (string-trim '(#\") (cadr node))))
		   ((equalp (aref (car node) 0) #\_)
		    ;; It is an object
		    (cdr node))
		   ((stringp (cadr node))
		    (cons (make-keyword (first node))
			  (string-trim '(#\") (cadr node))))
		   ((and (listp (cadr node))
			 (equalp (first (cadr node)) :li))
			;; The attribute value is a list
			(cons (make-keyword (first node))
			      (mapcar #'cdr (cdr node))))
		   (t
		    (let ((attr-name (make-keyword (first node)))
			  (attr-value (cdr node)))
		      (cons attr-name attr-value)))))
	       #'identity
	       data)))

(defmethod parse-api-input ((format (eql :sexp)) string)
  (read-from-string string))

;; TODO: content negotiation is wrong!! Very wrong!!

(defun parse-posted-content (posted-content &optional (method *parse-posted-content*))
  (ecase method
    (:use-request-content-type
     ;; Use the request content type to parse the posted content
     (let ((content-type (hunchentoot:header-in* :content-type)))
       (let ((format
	      (cond
		((some-test (list "text/xml" "application/xml")
			    (lambda (ct)
			      (cl-ppcre:scan ct content-type)))
		 :xml)
		((some-test (list "application/json")
			    (lambda (ct)
			      (cl-ppcre:scan ct content-type)))
		 :json)
		((some-test (list "application/lisp" "text/lisp")
			    (lambda (ct)
			      (cl-ppcre:scan ct content-type)))
		 :sexp)
		(t (error 'rs.error:http-unsupported-media-type-error
			  :format-control "Content type not supported ~A"
			  :format-arguments (list content-type))))))
	 (parse-api-input format posted-content))))
    (:infer
     (error "Not implemented"))
     
    ;; Infer the posted content format. Try parsing with different
    ;; methods until success
    (:raw posted-content)))

;; Content fetching decoration
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-content ((var content) &body body)
    `(let ((,var ,content))
       (if (not ,var)
	   (error 'rs.error:http-not-found-error)
	   (progn ,@body)))))

(defclass content-fetching-resource-operation-implementation-decoration
    (resource-operation-implementation-decoration)
  ((function :initarg :function
	     :accessor content-fetching-function
	     :initform (error "Provide the content fetching function")
	     :documentation "Function for content fetching")
   (argument :initarg :argument
	     :accessor content-fetching-argument
	     :initform (error "Provide the argument to use for fetching")
	     :documentation "Argument to pass to content-fetching-function")
   (bind :initarg :bind
	 :accessor content-fetching-bind
	 :initform '(:append :first)
	 :documentation "Content binding spec"))
  (:metaclass closer-mop:funcallable-standard-class))
  
(defmethod process-resource-operation-implementation-option
    ((option (eql :fetch-content))
     resource-operation-implementation
     &rest args &key enabled #+abcl &allow-other-keys)
  (if enabled
      (apply #'make-instance 'content-fetching-resource-operation-implementation-decoration
		     `(:decorates ,resource-operation-implementation ,@args :allow-other-keys t))
      resource-operation-implementation))
  
(defmethod execute :around ((decoration content-fetching-resource-operation-implementation-decoration)
			    &rest args)
  (let ((fargs 
	 (extract-function-arguments-to-plist *resource-operation* hunchentoot:*request*)))
    (with-content (content (funcall (content-fetching-function decoration)
				    (getf (content-fetching-argument decoration) fargs)))
      (apply #'call-next-method (cons content args)))))

(cl-annot:defannotation fetch-content (args resource-operation-implementation)
    (:arity 2)
  `(configure-resource-operation-implementation
    (name (resource-operation ,resource-operation-implementation))
    (list :fetch-content ,@args)))

;; Generic permission checking

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-permission-checking (check &body body)
    `(call-with-permission-checking ,check (lambda () ,@body))))

(defun call-with-permission-checking (check function)
  (if (not check)
      (error 'rs.error:http-forbidden-error)
      (funcall function)))

(defclass permission-checking-resource-operation-implementation-decoration
    (resource-operation-implementation-decoration)
  ((check :initarg :check
	  :accessor permission-check
	  :initform (error "Provide the permission checking function")
	  :documentation "Function for permission checking"))
  (:metaclass closer-mop:funcallable-standard-class))
  
(defmethod process-resource-operation-implementation-option
    ((option (eql :permission-checking))
     resource-operation-implementation
     &rest args &key enabled #+abcl &allow-other-keys)
  (if enabled
      (apply #'make-instance 'permission-checking-resource-operation-implementation-decoration
		     `(:decorates ,resource-operation-implementation ,@args :allow-other-keys t))
      resource-operation-implementation))
  
(defmethod execute :around ((decoration permission-checking-resource-operation-implementation-decoration)
			    &rest args)
    (with-permission-checking (apply (permission-check decoration) args)
      (call-next-method)))

(cl-annot:defannotation permission-checking (args resource-operation-implementation)
    (:arity 2)
  `(configure-resource-operation-implementation
    (name (resource-operation ,resource-operation-implementation))
    (list :permission-checking ,@args)))
