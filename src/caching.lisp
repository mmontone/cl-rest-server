(in-package :rest-server)

(defgeneric clear-cache (api-function-implementation)
  (:method ((api-function-name symbol))
    (clear-cache (find-api-function-implementation api-function-name)))
  (:method ((decoration api-function-implementation-decoration))
    (clear-cache (decorates decoration)))
  (:method ((api-function-implementation t))
    ;; Do nothing
    ))

(defclass caching-api-function-implementation-decoration
    (api-function-implementation-decoration)
  ()
  (:metaclass closer-mop:funcallable-standard-class))

(defclass etag-validation-decoration (caching-api-function-implementation-decoration)
  ((etag :initarg :etag
	 :initform nil
	 :accessor etag))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod clear-cache ((etag-validation etag-validation-decoration))
  (setf (etag etag-validation) nil))

(defclass last-modified-validation-decoration (caching-api-function-implementation-decoration)
  ()
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod process-api-function-implementation-option
    ((option (eql :caching))
     api-function-implementation
     &key
       (type :etag)
       (enabled t))
  (if enabled
      (ecase type
	(:etag (make-instance 'etag-validation-decoration
			      :decorates api-function-implementation))
	(:last-modified (error "Not implemented")))
      api-function-implementation)) 

(defmethod generate-etag ((decoration etag-validation-decoration)
			  thing)
  (easy-etag thing))

(defun easy-etag (thing)
  (cl-base64:usb8-array-to-base64-string (md5:md5sum-sequence thing) :uri t))

(defmethod execute :around ((decoration etag-validation-decoration)
			    &rest args)
  (declare (ignore args))
  (let ((client-etag (hunchentoot:header-in* "If-None-Match")))
    (if (and client-etag (equalp (etag decoration) client-etag))
	;; The etags match, content not modified
	(setf (hunchentoot:return-code*)
	      hunchentoot:+http-not-modified+)
	;; else, calculate etag and return to client
	(let ((content (call-next-method)))
	  (let ((content-etag
		 (easy-etag (if (typep content 'sequence)
				content
				(princ-to-string content)))))
	    ;; Save the current etag
	    (setf (etag decoration)
		  content-etag)
	    ;; Pass the etag to the client
	    (setf (hunchentoot:header-out "ETag")
		  content-etag)
	    ;; Output response
	    content)))))
