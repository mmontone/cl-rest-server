(in-package :rest-server)

;; Error handling configuration

(defvar *development-mode* :production "Api development mode. One of :development, :testing, :production. Influences how errors are handled from the api")

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
   :status-code 404))

(define-condition http-internal-server-error (http-error)
  ()
  (:default-initargs
   :status-code 500))

(define-condition http-authorization-required-error (http-error)
  ()
  (:default-initargs
   :status-code 401))

(define-condition http-forbidden-error (http-error)
  ()
  (:default-initargs
   :status-code 403))

(define-condition http-service-unavailable (http-error)
  ()
  (:default-initargs
   :status-code 503))

(define-condition http-unsupported-media-type-error (http-error)
  ()
  (:default-initargs
   :status-code 415))

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
	hunchentoot:+http-internal-server-error+))

(defmethod setup-reply-from-error ((condition http-error))
  (setf (hunchentoot:return-code*) (http-return-code condition)))

(defvar *retry-after-seconds* 5)

(defmethod setup-reply-from-error ((error http-service-unavailable))
  "We add a retry-after header for the user to try again. The retry-after header value is in seconds"
  (call-next-method)
  (setf (hunchentoot:header-out "Retry-After") *retry-after-seconds*))

(defun %with-condition-handling (function)
  (labels ((serialize-condition (condition)
	     (with-output-to-string (s)
	       (with-serializer-output s
		 (with-serializer *default-serializer*
		   (serialize condition)))))
           (handle-condition (condition)
             (if (equalp *development-mode* :production)
		 (setup-reply-from-error condition)
		 ; else, we are in :testing, serialize the condition
		 (serialize-condition condition))))
    (if (equalp *development-mode* :development)
	(handler-case (funcall function)
	  (harmless-condition (c)
	    (serialize-condition c)))
        (handler-case (funcall function)
	  (harmless-condition (c)
	    (serialize-condition c))
          (error (c)
            (handle-condition c))))))

(defmethod serialize-value ((serializer (eql :json)) (error simple-error) stream) 
  "Serialize error condition"
  (json:encode-json-alist 
   (list (cons :condition (type-of error))
	 (cons :message (simple-condition-format-control error)))
   stream))

(defmethod serialize-value ((serializer (eql :json))
			    (error simple-error) 
			    stream)
  "Serialize simple error condition"
  (json:with-object (stream)
    (json:as-object-member (:condition stream)
      (json:encode-json (type-of error) stream))
    (json:as-object-member (:message stream)
      (json:encode-json (simple-condition-format-control error) stream))))

;; Plugging

(defclass error-handling-api-function-implementation-decoration
    (api-function-implementation-decoration)
  ()
  (:metaclass closer-mop:funcallable-standard-class))
  
(defmethod process-api-function-implementation-option
    ((option (eql :error-handling))
     api-function-implementation
     &key enabled)
  (if enabled
      (make-instance 'error-handling-api-function-implementation-decoration
		     :decorates api-function-implementation)
      api-function-implementation))
  
(defmethod execute :around ((decoration error-handling-api-function-implementation-decoration)
			    &rest args)
  (with-condition-handling
    (call-next-method)))
