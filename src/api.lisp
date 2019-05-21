(in-package :rest-server)

(defun get-posted-content (&optional (request hunchentoot:*request*))
  (hunchentoot:raw-post-data
   :force-text
   (text-content-type?
    (parse-content-type
     (hunchentoot:header-in "content-type" request)))))

(defun text-content-type? (content-type)
  (member content-type *text-content-types*))

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
       ,@(let ((client-package (or (find-package (getf options :client-package))
                                   *package*)))
           (loop for resource in resources
              collect (destructuring-bind (name resource-options &body operations)
                          resource
                        (let ((resource-options
                               (list* :client-package
                                      (package-name
                                       (or (getf options :client-package)
                                           client-package))
                                      :export-client-functions
                                      (or (getf resource-options :export-client-functions)
                                          (getf options :export-client-functions))
                                      resource-options)))

                          `(define-api-resource ,name ,resource-options ,@operations))))))))

(defun start-api (api address port &rest args)
  "Start an api at address and port.

   In production mode, we bind the api directly. In debug mode, we only bind the API name in order to be able to make modifications to the api (definition) in development time"
  (when (getf args :config)
    (configure-api api (getf args :config)))
  (let ((api (if (getf args :debug-mode)
                 (if (symbolp api)
                     api
                     (name api))
                 ;; else
                 (if (symbolp api)
                     (find-api api)
                     api))))
    (let ((api-acceptor (apply #'make-instance
                               'api-acceptor
                               :address address
                               :port port
                               :api api
                               :allow-other-keys t
                               args)))
      (hunchentoot:start api-acceptor)
      api-acceptor)))

(defun stop-api (api-acceptor)
  (hunchentoot:stop api-acceptor))

;; Hunchentoot api acceptor

(defclass api-acceptor (hunchentoot:acceptor)
  ((api :initarg :api
        :initform (error "Provide the api"))
   (catch-errors :initarg :catch-errors
                 :accessor catch-errors
                 :initform rs.error:*catch-errors*
                 :documentation "If T, then errors are catched and handled. Default: NIL")
   (error-handler :initarg :error-handler
                  :accessor error-handler
                  :initform nil
                  :documentation "Custom API error handler")
   (debug-mode :initarg :debug-mode
               :accessor debug-mode
               :initform *debug-mode*))
  (:documentation "Hunchentoot api acceptor"))

(defmethod api ((acceptor api-acceptor))
  (let ((api-or-name (slot-value acceptor 'api)))
    (if (symbolp api-or-name)
        (find-api api-or-name)
        api-or-name)))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor api-acceptor) request)
  (rs.error::with-error-handler ((or (error-handler acceptor) rs.error::*default-error-handler*))
    (or
     (api-dispatch-request (api acceptor) request)
     (error 'rs.error:http-not-found-error
            :format-control "Resource not found"))))

;; The api class
(defclass api-definition ()
  ((name :accessor name
         :initarg :name
         :type symbol
         :documentation "The api id")
   (title :accessor title
          :initarg :title
          :initform nil
          :type (or string null)
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
   (client-package :initarg :client-package
                   :initform *package*
                   :accessor client-package)
   (export-client-functions :initarg :export-client-functions
                            :initform nil
                            :accessor export-client-functions)
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
             (let ((rs.error:*catch-errors* (catch-errors hunchentoot:*acceptor*))
                   (*debug-mode* (debug-mode hunchentoot:*acceptor*)))
               (loop for resource in (list-api-resources api)
                    do (log5:log-for (and rest-server log5:dribble) "Matching resource: ~A" resource)
                  when (resource-matches-request-p resource request)
                  do
                    (rs.log::api-log-for api "Resource matched: ~A" resource)
                    (loop for resource-operation in (list-api-resource-functions resource)
                         do (log5:log-for (and rest-server log5:dribble) "Matching resource operation: ~A" resource-operation)
                       when (resource-operation-matches-request-p resource-operation request)
                       do
                         (log5:log-for (and rest-server log5:dribble) "Resource operation matched: ~A" resource-operation)
                         (return-from api-dispatch-request
                            (if (equalp (hunchentoot:request-method request) :options)
                                (progn
                                  (resource-operation-http-options api resource-operation)
                                  "")
                                        ; else
                                (let* ((*resource-operation* resource-operation)
                                       (resource-operation-implementation
                                        (find-resource-operation-implementation
                                         (name resource-operation)
                                         :accept (parse-content-type (request-reply-content-type request))
                                         :content-type (parse-content-type (hunchentoot:header-in* "content-type"))))
                                       (result
                                        (api-execute-function-implementation
                                         api
                                         resource-operation-implementation
                                         resource
                                         request)))
                                  (if (stringp result)
                                      result
                                      (prin1-to-string result)))))))
               ;; Return nil as the request could not be handled
               nil)))
      (log5:log-for (and rest-server log5:dribble) "Handling request: ~A" request)
      (if (equalp (hunchentoot:request-method request)
                  :options)
          (if (equalp (request-uri request) "*")
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
                   when (equalp (request-uri request)
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
                                     (push (intern x)
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
  (let* ((request-uri (request-uri request))
         (scanner (parse-resource-operation-path (path resource-operation))))
    (multiple-value-bind (replaced-uri args)
        (ppcre:scan-to-strings scanner request-uri)
      (declare (ignore replaced-uri))
      (let ((args (loop for arg across args
                     when arg
                     collect (hunchentoot:url-decode arg))))
        (let ((required-args
               (loop
                  for reqarg in (required-arguments resource-operation)
                  for arg in args
                  collect (parse-argument-value arg (argument-type reqarg))))
              (optional-args
               (loop
                  for (var . string) in (request-uri-parameters request-uri)
                  for optarg = (find-optional-argument (make-keyword var) resource-operation)
                  appending
                    (list (make-keyword (string (argument-name optarg)))
                          (parse-argument-value (hunchentoot:url-decode string)
                                                (argument-type optarg))))))
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

(defun extract-url-content-type (url)
  "Extracts the expected content-type from url, when present. Example: 
(extract-url-accepted-content-type \"http://localhost/users/1.json\")"
  (flet ((extension-mime (extension)
           (string-case:string-case (extension :default (error "Invalid content-type extension: ~A" extension))
             ("json" "application/json")
             ("lisp" "text/lisp")
             ("xml" "application/xml")
             ("html" "text/html"))))           
    (let* ((splitted-url
            (split-sequence:split-sequence #\? url))
           (uri-path (first splitted-url))
           (uri-query (second splitted-url))
           (regex (ppcre:create-scanner '(:sequence (:register (:greedy-repetition 1 nil :everything))
                                          #\.
                                          (:register (:greedy-repetition 1 nil (:inverted-char-class #\/)) (:alternation #\? :end-anchor))))))
      (ppcre:register-groups-bind (path extension)
          (regex uri-path)
        (values (concatenate 'string path (if uri-query
                                              (format nil "?~A" uri-query)
                                              ""))
                (extension-mime extension))))))

(defun request-uri (request)
  (if *extract-reply-content-type-from-url*
      (multiple-value-bind (uri content-type)
          (extract-url-content-type (hunchentoot:request-uri request))
        (or uri (hunchentoot:request-uri request)))
      (hunchentoot:request-uri request)))

(defun request-reply-content-type (request)
  (or
   (if *extract-reply-content-type-from-url*
       (multiple-value-bind (uri content-type)
           (extract-url-content-type (hunchentoot:request-uri request))
         (or content-type (hunchentoot:header-in "accept" request)))
       (hunchentoot:header-in "accept" request))
   *default-reply-content-type*))

(defgeneric parse-api-input (format string)
  (:documentation "Parses content depending on its format"))

(defmethod parse-api-input ((format (eql :json)) string)
  (handler-case
      (json:decode-json-from-string string)
    (json:json-syntax-error (e)
      (error 'rs.error:http-bad-request
             :format-control (simple-condition-format-control e)
             :format-arguments (simple-condition-format-arguments e)))))

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

(defmethod parse-api-input ((format (eql :raw)) data)
  data)

(defun parse-content-type (content-type)
  (cond
    ((some-test (list "text/xml" "application/xml")
                (lambda (ct)
                  (cl-ppcre:scan ct content-type)))
     :xml)
    ((some-test (list "application/json")
                (lambda (ct)
                  (cl-ppcre:scan ct content-type)))
     :json)
    ((some-test (list "application/lisp" "text/html")
                (lambda (ct)
                  (cl-ppcre:scan ct content-type)))
     :html)
    ((some-test (list "application/lisp" "text/lisp")
                (lambda (ct)
                  (cl-ppcre:scan ct content-type)))
     :sexp)
    (t :raw
       #+nil(error 'rs.error:http-unsupported-media-type-error
                   :format-control "Content type not supported ~A"
                   :format-arguments (list content-type)))))

(defun parse-posted-content (posted-content &optional (method *parse-posted-content*))
  (ecase method
    (:use-request-content-type
     ;; Use the request content type to parse the posted content
     (let ((format (parse-content-type  (hunchentoot:header-in* :content-type))))
       (parse-api-input format posted-content)))
    (:infer
     (error "Not implemented"))

    ;; Infer the posted content format. Try parsing with different
    ;; methods until success
    (:raw posted-content)))

;; Resource fetching decoration
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-resource ((var resource &optional message &rest args) &body body)
    `(let ((,var ,resource))
       (if (not ,var)
           (error 'rs.error:http-not-found-error
                  ,@(when message
                      (list :format-control message
                            :format-arguments `(list ,@args))))

           (progn ,@body))))

  (defmacro let-resource (bindings &body body)
    `(let ,(loop for binding in bindings
              collect
                (destructuring-bind (var resource &optional message &rest args) binding
                  `(,var (or ,resource
                             (error 'rs.error:http-not-found-error
                                    ,@(when message
                                        (list :format-control message
                                              :format-arguments `(list ,@args))))))))
       ,@body))
  (defmacro let-resource* (bindings &body body)
    `(let* ,(loop for binding in bindings
               collect
                 (destructuring-bind (var resource &optional message &rest args) binding
                   `(,var (or ,resource
                              (error 'rs.error:http-not-found-error
                                     ,@(when message
                                         (list :format-control message
                                               :format-arguments `(list ,@args))))))))
       ,@body)))

(defclass resource-fetching-resource-operation-implementation-decoration
    (resource-operation-implementation-decoration)
  ((function :initarg :function
             :accessor resource-fetching-function
             :initform (error "Provide the resource fetching function")
             :documentation "Function for resource fetching")
   (argument :initarg :argument
             :accessor resource-fetching-argument
             :initform (error "Provide the argument to use for fetching")
             :documentation "Argument to pass to resource-fetching-function")
   (bind :initarg :bind
         :accessor resource-fetching-bind
         :initform '(:append :first)
         :documentation "Resource binding spec"))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod process-resource-operation-implementation-option
    ((option (eql :fetch-resource))
     resource-operation-implementation
     &rest args &key enabled #+(or abcl ecl)&allow-other-keys)
  (if enabled
      (apply #'make-instance 'resource-fetching-resource-operation-implementation-decoration
             `(:decorates ,resource-operation-implementation ,@args :allow-other-keys t))
      resource-operation-implementation))

(defmethod execute :around ((decoration resource-fetching-resource-operation-implementation-decoration)
                            &rest args)
  (let ((fargs
         (extract-function-arguments-to-plist *resource-operation* hunchentoot:*request*)))
    (with-resource (resource (funcall (resource-fetching-function decoration)
                                      (getf (resource-fetching-argument decoration) fargs)))
      (apply #'call-next-method (cons resource args)))))

(cl-annot:defannotation fetch-resource (args resource-operation-implementation)
    (:arity 2)
  `(configure-resource-operation-implementation
    (name (resource-operation ,resource-operation-implementation))
    (list :fetch-resource ,@args)))

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
     &rest args &key enabled #+(or abcl ecl)&allow-other-keys)
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
