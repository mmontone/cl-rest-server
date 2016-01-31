(in-package :rest-server.error)

;; Error handling configuration

(defvar *catch-errors* t)

(defvar *server-catch-errors*)

(defvar *error-handler* 'log-api-error)

(defvar *default-error-handler* 'default-error-handler)
                                  
(defun default-error-handler (error)
  (log-api-error error)
  (setup-reply-from-error error))

(defun log-api-error (error)
  (log5:log-for (rs::rest-server log5:error) "ERROR: ~A" error)
  (log5:log-for (rs::rest-server log5:debug)
                (with-output-to-string (s)
                  (trivial-backtrace:print-backtrace error :output s))))

(defmacro with-error-handler ((&optional (error-handler '*default-error-handler*))
                              &body body)
  `(call-with-error-handler ,error-handler
                            (lambda () (progn ,@body))))

;; We have to disable hunchentoot handler and enable ours
(setf hunchentoot:*catch-errors-p* nil)

;; Conditions

(define-condition http-error (simple-error)
  ((status-code :initarg :status-code
                :initform (error "Provide the status code")
                :accessor status-code))
  (:report (lambda (c s)
             (format s "HTTP error ~A" (status-code c)))))

(defun http-error (status-code datum &rest args)
  (error 'http-error
         :status-code status-code
         :format-control datum
         :format-arguments args))

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

(defparameter *http-status-codes-conditions*
  '((404 . http-not-found-error)
    (400 . http-bad-request)
    (401 . http-authorization-required-error)
    (403 . http-forbidden-error)
    (500 . http-internal-server-error)
    (415 . http-unsupported-media-type-error)))

(defun serialize-error (error)
  (let ((serializer (rs.serialize:accept-serializer)))
    (set-reply-content-type (rs.serialize::serializer-content-type serializer))
    (with-output-to-string (s)
      (rs.serialize:with-serializer-output s
        (rs.serialize:with-serializer serializer
          (rs.serialize:serialize error))))))

(defmethod rs.serialize:serialize ((error error) &optional
                                                   (serializer rs.serialize::*serializer*)
                                                   (stream rs.serialize::*serializer-output*) &rest args)
  (declare (ignore args))
  (rs.serialize:with-element ("error" :serializer serializer
                                      :stream stream)
    (rs.serialize:set-attribute
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
             (trivial-backtrace:print-backtrace error :output s))))
     :serializer serializer
     :stream stream)))

(defmethod rs.serialize:serialize ((error http-error) &optional
                                                        (serializer rs.serialize::*serializer*)
                                                        (stream rs.serialize::*serializer-output*) &rest args)
  (declare (ignore args))
  (rs.serialize:with-element ("error" :serializer serializer
                                      :stream stream)
    (rs.serialize:set-attribute
     "detail"
     (apply #'format
            nil
            (simple-condition-format-control error)
            (simple-condition-format-arguments error))
     :serializer serializer
     :stream stream)))

;; http-return-code decides the HTTP status code to return for
;; the signaled condition. Implement this method for new conditions.
;; Example:

(defmethod http-return-code ((error rs.schema:validation-error))
  hunchentoot:+http-bad-request+)

(defmethod rs.serialize:serialize ((error rs.schema:validation-error)
                                   &optional
                                     (serializer rs.serialize::*serializer*)
                                     (stream rs.serialize::*serializer-output*) &rest args)
  (declare (ignore args))
  (rs.serialize:with-element ("error" :serializer serializer
                                      :stream stream)
    (rs.serialize:set-attribute
     "detail"
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
                  :accessor error-handler))
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
  (with-error-handler ((error-handler decoration))
    (call-next-method)))

(cl-annot:defannotation error-handling (args resource-operation-implementation)
    (:arity 2)
  `(rs::configure-resource-operation-implementation
    (rs::name (rs::resource-operation ,resource-operation-implementation))
    (list :error-handling ,@args)))
