(in-package :rest-server)

(defparameter *apis* nil)
(defparameter *api* nil "The current api")

(defun start-api (api port)
  (hunchentoot:start
   (make-instance 'hunchentoot:acceptor
		  :port port
		  :request-dispatcher
		  (lambda (request)
		    (api-request-dispatcher api request)))))

(defun api-request-dispatcher (api request)
  (loop for definition being the hash-values of (defintions api)
       when (definition-matches definition request)
       do (return-from api-request-dispatcher
	    (funcall definition request))))

(defclass api-function ()
  ((name :initarg :name
	 :accessor name
	 :initform (error "Provide the name"))
   (url-prefix :initarg :url-prefix
               :accessor url-prefix
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
   (documentation :accessor api-documentation
		  :initarg :documentation))
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod validate ((api-function api-function))
  )

(defmethod initialize-instance :after ((api-function api-function) &rest initargs)
  (declare (ignore initargs))

  ;; Validate first
  (validate api-function)

  ;; Install the api function
  (let ((api (or *api* (error "Specify the api"))))
    (setf (gethash api (name api-function))
          api-function)))

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

(defun make-api-function (name method options args)
  (apply #'make-instance
	 'api-function
	 (append
	  (list
	   :name name
	   :request-method method
	   :url-pattern url-pattern
	   :documentation documentation)
	  args)))

(defmacro with-api (api &body body)
  `(call-with-api ,api (lambda () ,@body)))

(defun call-with-api (api function)
  (let ((*api* api))
    (funcall function)))

(defmacro define-api-function (api name method options args)
  `(with-api ,api
     (make-api-function ,name ,method ,options ,args)))

(defun parse-api-method-definition (
  

;; example:

;; (

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
 
(defclass api-definition ()
  ((name :accessor name :initarg :name)
   (functions :accessor functions :initarg :functions :initform nil)
   (options :accessor options :initarg :options :initform nil)
   (documentation :accessor api-documentation :initarg :documentation :initform nil)))

(defmethod print-object ((api-definition api-definition) stream)
  (print-unreadable-object (api-definition stream :type t :identity t)
    (format stream "~A" (name api-definition))))

(defun parse-parameter (string)
  (cl-ppcre:register-groups-bind (query-param var)
      ("^(.+)={(.+)+}$" string)
    (cons (intern (string-upcase var) :keyword) query-param)))

(defun parse-parameters (string)
  (assert (char= (char string 0) #\?))
  (mapcar #'parse-parameter
	  (cl-ppcre:split "&" (subseq string 1))))

(defun gen-regexp (string)
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
			    (push (intern (string-upcase x) :keyword)
				  vars)
			    `(:register (:NON-GREEDY-REPETITION 1 nil (:INVERTED-CHAR-CLASS #\/)))))))))
	    :END-ANCHOR)
	   ))
    (values scanner 
	    vars)))

(defun parse-api-url (request-string)
  (multiple-value-bind (scanner vars)
	(gen-regexp
	 (if (find #\? request-string)
	     (subseq request-string 0 (position #\? request-string))
	     request-string))
    (let ((parameters 
	   (when (find #\? request-string)
	     (parse-parameters 
	      (subseq request-string (position #\? request-string))
	      ))))
      (values scanner vars parameters))))

(defun backend-var (backend)
  (intern (format nil "*BACKEND-URL-~A*" backend) :rest-server))

(defmacro with-backend (backend url &body body)
  `(let ((,(backend-var backend) ,url)) ,@body))
  
(defmacro define-api (name &body methods)
  `(progn
     ,@(loop for x in methods
	 for fdef = (apply #'parse-api-method-definition x)
	  collect (client-stub name fdef))
     (pushnew
      (make-instance 
       'api-definition 
       :name ,name
       :functions
       (loop for x in ',methods
	  collect (apply #'parse-api-method-definition x)))
      *apis*
      :key #'name)))

(defvar *rest-server-proxy* nil)

(defun client-stub (api-name api-function)
  (let ((request-url (gensym "REQUEST-URL-"))
        (response (gensym "RESPONSE-")))
    (multiple-value-bind 
          (scanner vars params)      
        (parse-api-url (url-pattern api-function))
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
						 ((position k ',vars)
						  (let ((pos (position k ',(reverse vars))))
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
  (find name *apis* :key #'name))

(defmethod find-api-function ((api symbol) function-name)
  (find-api-function (find-api api) function-name))

(defmethod find-api-function ((api api-definition) function-name)
  (find function-name (functions api) :key #'name))

(defun format-api-url (api function-name &rest args)
  (let ((api-function (find-api-function api function-name)))
    (replace-vars-in-url (url-pattern api-function) args)))