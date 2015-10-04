(in-package :rest-server)

(defparameter *resource-operation* nil "The current resource operation")

(log5:defcategory rest-server)

(defparameter *signal-client-function-errors* t "When t, signal an exception in an error ocurrs in an api client function")

(defun call-with-signal-client-function-errors (signal-p function)
  (let ((*signal-client-function-errors* signal-p))
	(funcall function)))

(defmacro with-signal-client-function-errors ((signal-p *signal-client-function-errors*)
											  &body body)
  `(call-with-signal-client-function-errors ,signal-p (lambda () ,@body)))

(defmacro with-content ((&key (setter ':=)) &body body)
  "Macro to build HTTP content to pass in client functions.

Example:

(with-api-backend *api-backend*
  (let ((content (with-content ()
                    (:= :name \"name\")
                    (when some-condition
                       (:= :attr 22)))))
  (app.api-client:my-client-function :content content)))
"
  (alexandria:with-unique-names (content)
	`(let ((,content '()))
	   (flet ((,setter (key value)
				(push (cons key value) ,content)))
		 ,@body
		 ,content))))
