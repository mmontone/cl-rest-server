(in-package :rest-server)

(defparameter *apis* (make-hash-table :test #'equalp)
  "Global hashtable containing the apis defined")
(defparameter *api* nil "The current api")
(defvar *rest-server-proxy* nil)

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
      (define-api-function get-user :get (:url-prefix \"users/{id}\")
                                    '((:id :integer))))"
  `(call-with-api ',api (lambda () ,@body)))

(defun call-with-api (api function)
  (let ((*api* (if (symbolp api) (find-api api)  api)))
    (funcall function)))

(defvar *api-backend* nil "API backend address") 

(defmacro with-api-backend (backend &body body)
  "Execute the client api function calling backend"
  `(call-with-api-backend ,backend (lambda () ,@body)))

(defun call-with-api-backend (backend function)
  (let ((*api-backend* backend))
    (funcall function)))

(defmacro define-api (name options &body resources)
  "Define an api."
  `(progn
     (apply #'make-instance 
	    'api-definition 
	    :name ',name
	    ',options)
     (with-api ,name
      ,@(loop for x in resources
	     collect `(define-api-resource ,@x)))))

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
   (resources :accessor resources
	      :initarg :resources
	      :initform (make-hash-table :test #'equalp))
   (version :initarg :version
            :initform nil
            :accessor version)
   (documentation :accessor api-documentation
		  :initarg :documentation
		  :initform nil))
  (:documentation "The api class"))

(defmethod list-api-resources ((api-definition api-definition))
  (loop for resource being the hash-values of (resources api-definition)
       collect resource))

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
    (:list (split-sequence:split-sequence #\, string))
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
		(t (error 'http-unsupported-media-type-error "Content type not supported ~A" content-type)))))
	 (parse-api-input format posted-content))))
    (:infer
     (error "Not implemented"))
     
    ;; Infer the posted content format. Try parsing with different
    ;; methods until success
    (:raw posted-content)))
