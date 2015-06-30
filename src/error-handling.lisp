(in-package :rest-server.error)

;; Error handling configuration

(defvar *development-mode* :production "Api development mode. One of :development, :testing, :production. Influences how errors are handled from the api")

(defvar *server-development-mode* nil "Global server development mode. Takes precedence over *development-mode* when handling errors")

(defvar *log-error-backtrace* t "Log the errors backtrace if T")

;; We have to disable hunchentoot handler and enable ours
(setf hunchentoot:*catch-errors-p* nil)

;; Conditions

(define-condition http-error (simple-error)
  ((status-code :initarg :status-code
                :initform (error "Provide the status code")
                :accessor status-code))
  (:report (lambda (c s)
             (format s "HTTP error ~A" (status-code c)))))

(defun http-error (status-code)
  (error 'http-error :status-code status-code))

(define-condition http-not-found-error (http-error)
  ()
  (:default-initargs
   :status-code hunchentoot:+http-not-found+))

(define-condition http-internal-server-error (http-error)
  ()
  (:default-initargs
   :status-code hunchentoot:+http-internal-server-error+))

(define-condition http-authorization-required-error (http-error)
  ()
  (:default-initargs
   :status-code hunchentoot:+http-authorization-required+))

(define-condition http-forbidden-error (http-error)
  ()
  (:default-initargs
   :status-code hunchentoot:+http-forbidden+))

(define-condition http-service-unavailable-error (http-error)
  ()
  (:default-initargs
   :status-code hunchentoot:+http-service-unavailable+))

(define-condition http-unsupported-media-type-error (http-error)
  ()
  (:default-initargs
   :status-code hunchentoot:+http-unsupported-media-type+))

(define-condition http-not-acceptable-error (http-error)
  ()
  (:default-initargs
   :status-code hunchentoot:+http-not-acceptable+))   

(defparameter *http-status-codes-conditions*
  '((404 . http-not-found-error)
    (401 . http-authorization-required-error)
    (403 . http-forbidden-error)
    (500 . http-internal-server-error)
    (415 . http-unsupported-media-type-error)))

(defvar *conditions-mapping* nil "Assoc list mapping application conditions to HTTP conditions. (.i.e. permission-denied-error to http-forbidden-error)")

(defgeneric decode-response (response content-type)
  (:method (response content-type)
    (error "Not implemented")))

(defmethod decode-response (response (content-type (eql :json)))
  (json:decode-json-from-string
          (babel:octets-to-string
           response
           :external-format :utf8)))

(defun handle-response (response status-code content-type)
  (cond
    ((and (>= status-code 200)
          (< status-code 400))
     (decode-response response content-type))
    ((assoc status-code *http-status-codes-conditions*)
     (error (cdr (assoc status-code *http-status-codes-conditions*))))
    (t (http-error status-code))))

(defmacro with-condition-handling (&body body)
  `(%with-condition-handling (lambda () (progn ,@body))))

(define-condition harmless-condition ()
  ()
  (:documentation "Inherit your condition from this if you dont want your condition to be catched by the error handler. (.i.e validation-errors to be serialized to the server, always)"))

(defmethod http-return-code (condition)
  hunchentoot:+http-ok+)

(defmethod http-return-code ((condition http-error))
  (status-code condition))

(defmethod http-return-code ((condition error))
  hunchentoot:+http-internal-server-error+)

(defmethod setup-reply-from-error ((error error))
  (setf (hunchentoot:return-code*)
	hunchentoot:+http-internal-server-error+)
  (log5:log-for (rs::rest-server) "ERROR: ~A" error)
  (when *log-error-backtrace*
    #+nil(log5:log-for (rest-server)
		       (trivial-backtrace:backtrace-string))
    
    (trivial-backtrace:print-backtrace error :output rs.log::*api-logging-output*))
  "Error")

(defmethod setup-reply-from-error ((condition http-error))
  (setf (hunchentoot:return-code*) (http-return-code condition))
  (when (simple-condition-format-control condition)
    (apply #'format
	   nil
	   (simple-condition-format-control condition)
	   (simple-condition-format-arguments condition))))	  

(defvar *retry-after-seconds* 5)

(defmethod setup-reply-from-error ((error http-service-unavailable-error))
  "We add a retry-after header for the user to try again. The retry-after header value is in seconds"
  (call-next-method)
  (setf (hunchentoot:header-out "Retry-After") *retry-after-seconds*))

(defun %with-condition-handling (function)
  (let ((development-mode (or *server-development-mode*
			      *development-mode*)))
    (labels ((serialize-condition (condition)
	       (with-output-to-string (s)
		 (rs.serialize:with-serializer-output s
		   (rs.serialize:with-serializer rs.serialize:*default-serializer*
		     (rs.serialize:serialize condition)))))
	     (handle-condition (condition)
	       (if (equalp development-mode :production)
		   (setup-reply-from-error condition)
					; else, we are in :testing, serialize the condition
		   (serialize-condition condition))))
      (if (equalp development-mode :development)
	  (handler-case (funcall function)
	    (harmless-condition (c)
	      (serialize-condition c)))
	  (handler-case (funcall function)
	    (harmless-condition (c)
	      (serialize-condition c))
	    (error (c)
	      (handle-condition c)))))))

(defmethod rs.serialize::serialize-value ((serializer (eql :json)) (error simple-error) stream &rest args) 
  "Serialize error condition"
  (json:encode-json-alist 
   (list (cons :condition (type-of error))
	 (cons :message (simple-condition-format-control error)))
   stream))

(defmethod rs.serialize::serialize-value ((serializer (eql :json))
			    (error simple-error) 
			    stream &rest args)
  "Serialize simple error condition"
  (json:with-object (stream)
    (json:as-object-member (:condition stream)
      (json:encode-json (type-of error) stream))
    (json:as-object-member (:message stream)
      (json:encode-json (simple-condition-format-control error) stream))))

;; Plugging

(defclass error-handling-resource-operation-implementation-decoration
    (rs::resource-operation-implementation-decoration)
  ()
  (:metaclass closer-mop:funcallable-standard-class))
  
(defmethod rs::process-resource-operation-implementation-option
    ((option (eql :error-handling))
     resource-operation-implementation
     &key (enabled t)
       #+abcl &allow-other-keys)
  (if enabled
      (make-instance 'error-handling-resource-operation-implementation-decoration
		     :decorates resource-operation-implementation)
      resource-operation-implementation))
  
(defmethod execute :around ((decoration error-handling-resource-operation-implementation-decoration)
			    &rest args)
  (with-condition-handling
    (call-next-method)))

(cl-annot:defannotation error-handling (args resource-operation-implementation)
    (:arity 2)
  `(rs::configure-resource-operation-implementation
    (rs::name (rs::resource-operation ,resource-operation-implementation))
    (list :error-handling ,@args)))
