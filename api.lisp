(in-package :rest-server)

(defparameter *apis* (make-hash-table :test #'equalp))
(defparameter *api* nil "The current api")

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

(defclass api-acceptor (hunchentoot:acceptor)
  ((api :initarg :api
        :accessor api
        :initform (error "Provide the api"))
   (api-implementation-package :initarg :api-implementation-package
                       :accessor api-implementation-package
                       :initform *package*))
  (:documentation "Accepts api requests"))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor api-acceptor) request)
  (loop for api-function in (functions (api acceptor))
     when (api-function-matches-request-p request)
       return (execute-api-function-implementation (api acceptor)
                                                   api-function
                                                   (find-api-function-implementation (name api-function) acceptor)
                                                   request)
       finally (call-next-method)))

(defun find-api-function-implementation (name acceptor)
  (or (ignore-errors (symbol-function (intern name (api-implementation-package acceptor))))
      (error "Api function ~A not implemented in package ~A" name (api-implementation-package acceptor))))

(defun execute-api-function-implementation (api api-function function-implementation request)
  (let ((args (extract-function-arguments api api-function request)))
    (apply function-implementation args)))

(defclass api-definition ()
  ((name :accessor name :initarg :name)
   (functions :accessor functions :initarg :functions :initform (make-hash-table :test #'equalp))
   (options :accessor options :initarg :options :initform nil)
   (documentation :accessor api-documentation :initarg :documentation :initform nil)))

(defun start-api (api address port &optional (api-implementation-package *package*))
  (hunchentoot:start
   (make-instance 'api-acceptor
                  :address address
		  :port port
                  :api api
                  :api-implementation-package api-implementation-package)))

(defun api-function-matches-request-p (api api-function request)
  (and (equalp (hunchentoot:request-uri*) (uri-prefix api-function))
       (equalp (request-method api-function) (hunchentoot:request-method*))
       (member (hunchentoot:content-type*) (content-types api-function))))  

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
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod validate ((api-function api-function))
  )

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
                           (when (not (authenticate-key (getf hunchentoot:*request* :token)))
                             (error "Authentication error"))
                           (funcall next-function))))

(defvar *register-api-function* t)

(defmethod initialize-instance :after ((api-function api-function) &rest initargs)
  (declare (ignore initargs))

  ;; Parse the uri to obtain the required parameters
  (multiple-value-bind (scanner vars)
      (parse-api-url (uri-prefix api-function))
    (declare (ignore scanner))
    (setf (required-arguments api-function) vars))
  
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

(defun make-api-function (name method options args)
  (multiple-value-bind (required-arguments optional-arguments)
      (parse-api-function-arguments args)
    (apply #'make-instance
           'api-function
           (list
            :name name
            :request-method method
            :documentation (getf options :documentation)
            :options options
            :required-arguments (append (multiple-value-bind (scanner vars)
                                            (parse-uri-prefix (getf options :uri-prefix))
                                          (declare (ignore scanner))
                                          vars)
                                        required-arguments)
            :optional-arguments optional-arguments))))

(defmacro with-api (api &body body)
  `(call-with-api ',api (lambda () ,@body)))

(defun call-with-api (api function)
  (let ((*api* (if (symbolp api) (find-api api)  api)))
    (funcall function)))

(defmacro define-api-function (name method options args)
  `(make-api-function ',name ',method ',options ',args))

(defun parse-api-method-definition (method-spec)
  (destructuring-bind (name options args) method-spec
    `(define-api-function ,name ,(getf options :method)
                       ,options
                       ,args)))

(defun replace-vars-in-url (url plist)
  (labels ((do-replace (url plist)
	     (if (null plist)
		 url
		 (let* ((var (car plist))
			(val (cadr plist))
			(pattern `(:sequence 
				   "{" 
				   ,(string-downcase (symbol-name var))
				   "}")))
		   (do-replace 
		       (cl-ppcre:regex-replace 
			pattern 
			url
			(format nil "~A" val))
		     (cddr plist))))))
    (do-replace url plist)))

(defmethod print-object ((api-definition api-definition) stream)
  (print-unreadable-object (api-definition stream :type t :identity t)
    (format stream "~A" (name api-definition))))

(defun register-api-definition (api-definition)
  (setf (gethash (name api-definition) *apis*) api-definition))

(defmethod initialize-instance :after ((api-definition api-definition) &rest initargs)
  (declare (ignore initargs))
  (register-api-definition api-definition))

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
	 (scanner
	  `(:sequence
	     :START-ANCHOR
	    ,@(remove 
	       nil
	       (loop for x in (cl-ppcre:split '(:register (:CHAR-CLASS #\{ #\})) string :with-registers-p t)
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
			    (push (parse-api-function-var x)
				  vars)
			    `(:register (:NON-GREEDY-REPETITION 1 nil (:INVERTED-CHAR-CLASS #\/)))))))))
	    :END-ANCHOR)))
    (values scanner 
	    vars)))

(defun parse-api-function-var (spec)
  (destructuring-bind (var-name var-type &optional default-value)
      (split-sequence:split-sequence #\ (string-trim (list #\ ) spec))
    (let ((parsed-type (read-from-string (string-trim (list #\ ) var-type))))
      (list (intern (string-upcase (string-trim (list #\ ) var-name)))
            parsed-type
            (and default-value
                 (parse-var-value (string-trim (list #\ ) default-value)
                                  parsed-type))
          (and default-value t)))))

;; (parse-api-function-var "x :boolean true")
;; (parse-api-function-var "y :integer 222")

(defun parse-var-value (string type)
  (case type
    (:boolean (cond
                ((equalp string "NIL") nil)
                ((equalp string "false") nil)
                ((equalp string "true") t)
                ((equalp string "t") t)))
    (:integer (parse-integer string))))

(defun backend-var (backend)
  (intern (format nil "*BACKEND-URL-~A*" backend) :rest-server))

(defmacro with-backend (backend url &body body)
  `(let ((,(backend-var backend) ,url)) ,@body))
  
(defmacro define-api (name options &body functions)
  `(progn
     (make-instance 
      'api-definition 
      :name ,name
      :options ',options)
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

(defvar *rest-server-proxy* nil)

(defun client-stub (api-name api-function)
  (let ((request-url (gensym "REQUEST-URL-"))
        (response (gensym "RESPONSE-")))
    (multiple-value-bind 
          (scanner vars params)      
        (parse-api-url (uri-prefix api-function))
      (declare (ignore scanner))
      `(progn
         (defvar ,(backend-var api-name) nil)
         (defun ,(name api-function)
             ,(progn
               (append
                (if (member (request-method api-function) '(:post :put))
                    (list 'posted-content)
                    nil)
                (loop for x in vars collect (intern (symbol-name x)))
                (if params
                    (cons '&key (loop for x in params collect (intern (symbol-name (car x))))))))
           ,(api-documentation api-function)
           (log5:log-for (rest-server) "Client stub: ~A" ',(name api-function))
           (assert ,(backend-var api-name) nil "Error: this is an API function. No api backend selected. Wrap this function call with with-backend")
           (let ((,request-url (format nil "~A~A" 
                                       ,(backend-var api-name) 
                                       (replace-vars-in-url 
                                        ,(url-pattern-noparams api-function)
                                        (list
                                         ,@(loop for x in vars 
                                              collect x
                                              collect (intern (symbol-name x))))))))
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
                                            ,@(loop for x in params
                                                 collect
                                                 (progn
                                                   `(cons 
                                                     ,(cdr x)
                                                     (format nil "~A" ,(intern (symbol-name 
                                                                                (car x)))))))))))
               (log5:log-for (rest-server) "Response: ~A" (sb-ext:octets-to-string
                 ,response
                 :external-format :utf8
                 ))
               (sb-ext:octets-to-string
                ,response
                :external-format :utf8
                ))))))))

(defgeneric url-pattern-noparams (api-function))
(defmethod url-pattern-noparams ((api-function api-function))
  (let* ((pattern (url-pattern api-function))
	(pos (position #\? pattern)))
    (if pos
	(subseq pattern 0 pos)
	pattern)))

(defmacro implement-api (name uri-prefix &body methods)
  (check-type name keyword)
  (check-type uri-prefix string)
  (labels ((api-def (fname)
	     (let ((api (find name *apis* :key #'name)))
	       (or (find fname (functions api)
			 :key #'name)
		   (error "Function ~A not found in ~A"
			  fname name)))))
    `(let ((api (find ,name *apis* :key #'name)))
       (unless api
	 (error "API ~A not found" ,name))
       (push 
	(lambda (request)
	  (when			   
	      (and (eql (hunchentoot:request-method request) 
			:get)
		   (cl-ppcre:scan 
		    (list :sequence :start-anchor ,uri-prefix)
		    (hunchentoot:script-name request)
		    ))
	    (lambda ()
	      (setf (hunchentoot:content-type*) "text/plain")
              (with-output-to-string (str)
		(format str "API methods:~%~%")
		(loop for x in (functions api)
		   do (format str "~A~%  ~A~%~%" 
			      (url-pattern x)
			      (api-documentation x))))
	      )))
	hunchentoot:*dispatch-table*)
       (list
	  ,@(loop for (name args . body) in methods
	       collect 
	       (multiple-value-bind (scanner vars parameters)
		   (parse-api-url (format nil "~A~A" 
					  uri-prefix 
					  (url-pattern (api-def name))))
		 ;(declare (ignore vars parameters))
		 (let ((func (api-def name)))
		   `(push 
		       (lambda (request)
			 (when			   
			     (and (eql (hunchentoot:request-method request) 
				       ,(request-method func))
				  (cl-ppcre:scan 
				   ',scanner 
				   (hunchentoot:script-name request)))
			   (lambda ()
                             (log5:log-for (rest-server) "API: Handling ~A ~A by ~A"
                                           (hunchentoot:request-method request)
                                           (hunchentoot:request-uri request) ',(name func))
                             (let ((posted-content (when (hunchentoot:raw-post-data :external-format :utf8)
                                                     (hunchentoot:raw-post-data :external-format :utf8))))
                               (when posted-content (log5:log-for (rest-server) "Posted content: ~A" posted-content)))
                             (multiple-value-bind 
				     (start end starts ends)
				   (cl-ppcre:scan 
				      ',scanner 
				      (hunchentoot:script-name request))
				   (declare (ignore start end))
				   (setf
				    (hunchentoot:header-out 
				     "Cache-Control")
				    (if (and (hunchentoot:get-parameter "max-age")
					     (eql (hunchentoot:request-method request) 
						  :get))
					(format nil "public, max-age=~A"
						(hunchentoot:get-parameter "max-age"))
					"public, max-age=0"))
				   (apply
				    (lambda (,@args)
				      (let ((res
					     (progn
					       ,@body)))
                                        (log5:log-for (rest-server) "Response: ~A" res)
                                        ;(setf (hunchentoot:content-type*) "application/json")
                                        (sb-ext:string-to-octets
                                         res
                                         :external-format :utf8)))
				    (loop for x in ',args
				       for k = (intern (symbol-name x) :keyword)
				       collect (cond 
						 ((eq k :posted-content)
                                                  (when (hunchentoot:raw-post-data :external-format :utf8)
                                                    (hunchentoot:raw-post-data :external-format :utf8)))
						 ((position k ',vars :key #'car)
						  (let ((pos (position k ',(reverse vars) :key #'car)))
                                                    (subseq 
						     (hunchentoot:script-name request)
						     (aref starts pos)
						     (aref ends pos))))
						 (t (hunchentoot:get-parameter (cdr (assoc k ',parameters)) request)))))))))
		       hunchentoot:*dispatch-table*
		       )))))
       )))

(defmacro with-api-backend (&body body)
  `(rest-server::with-backend :bonanza2 
       (format nil  "http://~A/api/"
	       (if (boundp 'hunchentoot:*request*)
		   (hunchentoot:host)
		   "localhost:8988"
		   ))
    ,@body))

(defun find-api (name)
  (gethash name *apis*))

(defmethod find-api-function ((api symbol) function-name)
  (find-api-function (find-api api) function-name))

(defmethod find-api-function ((api api-definition) function-name)
  (gethash function-name (functions api)))

(defun format-api-url (api function-name &rest args)
  (let ((api-function (find-api-function api function-name)))
    (replace-vars-in-url (url-pattern api-function) args)))

