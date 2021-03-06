(in-package :rest-server.error)

;; Error handling configuration

(defvar *catch-errors* t)

(defvar *server-catch-errors*)

(defvar *error-handler* 'log-api-error)

(defvar *default-error-handler* 'default-error-handler)

;; Conditions

(define-condition http-error (simple-error)
  ((status-code :initarg :status-code
                :initform (error "Provide the status code")
                :accessor status-code)
   (info :initarg :info
         :initform nil
         :accessor http-error-info
         :documentation "A plist of extra error info to be serialized back"))
  (:report (lambda (c s)
             (format s "HTTP error ~A: ~A ~A"
                     (status-code c)
                     (apply #'format nil (simple-condition-format-control c)
                            (simple-condition-format-arguments c))
                     (http-error-info c)))))

(defun http-error (status-code datum &rest args)
  (error 'http-error
         :status-code status-code
         :format-control datum
         :format-arguments args))

(defgeneric log-error-p (error)
  (:documentation "Returns true for conditions that should be logged."))

(defmethod log-error-p ((error error))
  "Errors are logged"
  t)

(defmethod log-error-p (error)
  "The default is not to log conditions"
  nil)

(defmethod log-error-p ((error http-error))
  "HTTP errors are not logged"
  nil)

(defmethod log-error-p ((error schemata:validation-error))
  "Validation errors are not logged"
  nil)

(defun default-error-handler (error)
  (when (log-error-p error)
    (log-api-error error))
  (setup-reply-from-error error))

(defmethod log-api-error ((error error))
  (log5:log-for (rs::rest-server log5:error+) "ERROR: ~A" error)
  (log5:log-for (rs::rest-server log5:error+)
                (trivial-backtrace:print-backtrace error :output nil))
  (format *debug-io* "ERROR: ~a~%" error)
  (trivial-backtrace:print-backtrace error))

(defmethod log-api-error ((error http-error))
  ;; We don't want to log HTTP "error" signals like
  ;; an application error
  )

(defmacro with-error-handler ((&optional (error-handler '*default-error-handler*))
                              &body body)
  `(call-with-error-handler ,error-handler
                            (lambda () (progn ,@body))))

(define-condition http-not-found-error (http-error)
  ()
  (:default-initargs
   :status-code hunchentoot:+http-not-found+
    :format-control "Resource not found"))

(define-condition http-internal-server-error (http-error)
  ()
  (:default-initargs
   :status-code hunchentoot:+http-internal-server-error+
    :format-control "Internal server error"))

(define-condition http-bad-request (http-error)
  ()
  (:default-initargs
   :status-code hunchentoot:+http-bad-request+
    :format-control "Bad request"))

(define-condition http-authorization-required-error (http-error)
  ()
  (:default-initargs
   :status-code hunchentoot:+http-authorization-required+
    :format-control "Authorization required"))

(define-condition http-forbidden-error (http-error)
  ()
  (:default-initargs
   :status-code hunchentoot:+http-forbidden+
    :format-control "Forbidden"))

(define-condition http-service-unavailable-error (http-error)
  ()
  (:default-initargs
   :status-code hunchentoot:+http-service-unavailable+
    :format-control "Service unavailable"))

(define-condition http-unsupported-media-type-error (http-error)
  ()
  (:default-initargs
   :status-code hunchentoot:+http-unsupported-media-type+
    :format-control "Unsupported media type"))

(define-condition http-not-acceptable-error (http-error)
  ()
  (:default-initargs
   :status-code hunchentoot:+http-not-acceptable+
    :format-control "Not acceptable"))

(define-condition http-method-not-allowed-error (http-error)
  ()
  (:default-initargs
   :status-code hunchentoot:+http-method-not-allowed+
    :format-control "Method not allowed"))

(defparameter *http-status-codes-conditions*
  '((404 . http-not-found-error)
    (400 . http-bad-request)
    (401 . http-authorization-required-error)
    (403 . http-forbidden-error)
    (500 . http-internal-server-error)
    (415 . http-unsupported-media-type-error)))

(defun serialize-error (error)
  (let ((serializer (rs::accept-serializer)))
    (set-reply-content-type (generic-serializer::serializer-content-type serializer))
    (with-output-to-string (s)
      (generic-serializer:with-serializer-output s
        (generic-serializer:with-serializer serializer
          (generic-serializer:serialize error))))))

(defmethod generic-serializer:serialize ((error error) &optional
                                                   (serializer generic-serializer::*serializer*)
                                                   (stream generic-serializer::*serializer-output*) &rest args)
  (declare (ignore args))
  (generic-serializer:with-object ("error" :serializer serializer
                                     :stream stream)
    (generic-serializer:set-attribute
     "detail"
     (let ((debug-mode (if (boundp 'rs:*server-debug-mode*)
                           rs:*server-debug-mode*
                           rs:*debug-mode*)))
       (if (not debug-mode)
           "Internal server error"
           ;; else, debug mode
           (with-output-to-string (s)
             (princ error s)
             (terpri s)
             (trivial-backtrace:print-backtrace-to-stream s))))
     :serializer serializer
     :stream stream)))

(defmethod generic-serializer:serialize ((error http-error) &optional
                                                        (serializer generic-serializer::*serializer*)
                                                        (stream generic-serializer::*serializer-output*) &rest args)
  (declare (ignore args))
  (generic-serializer:with-object ("error" :serializer serializer
                                     :stream stream)
    (generic-serializer:set-attribute
     "detail"
     (apply #'format
            nil
            (simple-condition-format-control error)
            (simple-condition-format-arguments error))
     :serializer serializer
     :stream stream)
    (alexandria:doplist (key val (http-error-info error))
        (generic-serializer:set-attribute
         (princ-to-string key)
         val))))

;; http-return-code decides the HTTP status code to return for
;; the signaled condition. Implement this method for new conditions.
;; Example:

(defmethod http-return-code ((error schemata:validation-error))
  hunchentoot:+http-bad-request+)

(defmethod generic-serializer:serialize ((error schemata:validation-error)
                                   &optional
                                     (serializer generic-serializer::*serializer*)
                                     (stream generic-serializer::*serializer-output*) &rest args)
  (declare (ignore args))
  (generic-serializer:with-object ("error" :serializer serializer
                                     :stream stream)
    (generic-serializer:set-attribute
     "message"
     (apply #'format
            nil
            (simple-condition-format-control error)
            (simple-condition-format-arguments error))
     :serializer serializer
     :stream stream)))

(defmethod http-return-code ((condition error))
  hunchentoot:+http-internal-server-error+)

(defmethod http-return-code ((condition http-error))
  (status-code condition))

(defmethod setup-reply-from-error ((error error))
  (setf (hunchentoot:return-code*)
        (http-return-code error))
  (serialize-error error))

(defmethod setup-reply-from-error ((error http-error))
  (setf (hunchentoot:return-code*)
        (http-return-code error))
  (serialize-error error))

(defvar *retry-after-seconds* 5)

(defmethod setup-reply-from-error ((error http-service-unavailable-error))
  "We add a retry-after header for the user to try again. The retry-after header value is in seconds"
  (call-next-method)
  (setf (hunchentoot:header-out "Retry-After") *retry-after-seconds*))

(defun call-with-error-handler (error-handler function)
  (let ((catch-errors (if (boundp '*server-catch-errors*)
                          *server-catch-errors*
                          *catch-errors*))
        (error-handler (or (and (symbolp error-handler)
                                (symbol-function error-handler))
                           error-handler)))
    (if catch-errors
        (handler-case
            (funcall function)
          (error (e)
            (funcall error-handler e)))
        (funcall function))))

;; Plugging

(defclass error-handling-resource-operation-implementation-decoration
    (rs::resource-operation-implementation-decoration)
  ((error-handler :initarg :error-handler
                  :accessor error-handler
                  :initform *default-error-handler*))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod rs::process-resource-operation-implementation-option
    ((option (eql :error-handling))
     resource-operation-implementation
     &key (enabled t)
       #+(or abcl ecl) &allow-other-keys)
  (if enabled
      (make-instance 'error-handling-resource-operation-implementation-decoration
                     :decorates resource-operation-implementation)
      resource-operation-implementation))

(defmethod execute :around ((decoration error-handling-resource-operation-implementation-decoration)
                            &rest args)
  (declare (ignore args))
  (with-error-handler ((error-handler decoration))
    (call-next-method)))

(cl-annot:defannotation error-handling (args resource-operation-implementation)
    (:arity 2)
  `(rs::configure-resource-operation-implementation
    (rs::name (rs::resource-operation ,resource-operation-implementation))
    (list :error-handling ,@args)))
