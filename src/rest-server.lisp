(in-package :rest-server)

(defparameter *resource-operation* nil "The current resource operation")

(log5:defcategory rest-server)

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
