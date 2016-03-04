(in-package :rest-server)

(defun extract-function-arguments-to-plist (resource-operation request)
  (let ((scanner (parse-resource-operation-path (path resource-operation))))
    (multiple-value-bind (replaced-uri args)
        (ppcre:scan-to-strings scanner (request-uri request))
      (declare (ignore replaced-uri))
      (let ((args (loop for arg across args
                     when arg
                     collect arg)))
        (let ((required-args
               (loop
                  for reqarg in (required-arguments resource-operation)
                  for arg in args
                  appending
                    (list (make-keyword (symbol-name (first reqarg)))
                          (parse-argument-value arg (second reqarg)))))
              (optional-args
               (loop
                  for (var . string) in (request-uri-parameters (hunchentoot:request-uri request))
                  for optarg = (find-optional-argument (make-keyword var) resource-operation)
                  appending
                    (list (make-keyword (symbol-name (first optarg)))
                          (parse-argument-value string (second optarg))))))
          (append required-args optional-args))))))

(defgeneric clear-cache (resource-operation-implementation &optional id)
  (:method ((resource-operation-name symbol) &optional id)
    (declare (ignorable id))
    (clear-cache (find-resource-operation-implementation resource-operation-name) id))
  (:method ((decoration resource-operation-implementation-decoration) &optional id)
    (clear-cache (decorates decoration) id))
  (:method ((resource-operation-implementation t) &optional id)
    (declare (ignore resource-operation-implementation id))
    ;; Do nothing
    ))

(defclass caching-resource-operation-implementation-decoration
    (resource-operation-implementation-decoration)
  ()
  (:metaclass closer-mop:funcallable-standard-class))

(defclass etag-validation-decoration (caching-resource-operation-implementation-decoration)
  ((etags :initarg :etags
          :initform (make-hash-table :test #'equalp)
          :accessor etags
          :documentation "ETags")
   (etag :initarg :etag
         :initform nil
         :accessor etag
         :documentation "ETag")
   (content-id :initarg :content-id
               :initform nil
               :accessor content-id
               :documentation "The id that identifies the content.
For instance, if the resource-operation fetches users, then it's the resource operation {id} parameter"))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod clear-cache ((etag-validation etag-validation-decoration) &optional id)
  (if id
      (remhash id (etags etag-validation))
                                        ; else
      (setf (etag etag-validation) nil)))

(defclass last-modified-validation-decoration (caching-resource-operation-implementation-decoration)
  ()
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod process-resource-operation-implementation-option
    ((option (eql :caching))
     resource-operation-implementation
     &rest args
     &key
       (type :etag)
       (enabled t)
       &allow-other-keys)
  (let ((args (copy-list args)))
    (remf args :type)
    (remf args :enabled)
    (if enabled
        (ecase type
          (:etag (apply #'make-instance
                        'etag-validation-decoration
                        `(:decorates ,resource-operation-implementation
                                     ,@args)))
          (:last-modified (error "Not implemented")))
        resource-operation-implementation)))

(defmethod generate-etag ((decoration etag-validation-decoration)
                          thing)
  (easy-etag thing))

(defun easy-etag (thing)
  (cl-base64:usb8-array-to-base64-string (md5:md5sum-sequence thing) :uri t))

(defmethod execute :around ((decoration etag-validation-decoration)
                            &rest args)
  (declare (ignore args))
  (flet ((get-etag ()
           (if (content-id decoration)
               (let ((id (getf args (content-id decoration))))
                 (assert id)
                 (gethash id (etags decoration)))
                                        ;else
               (etag decoration)))
         (set-etag (etag)
           (if (content-id decoration)
               (let ((id (getf args (content-id decoration))))
                 (assert id)
                 (setf (gethash id (etags decoration)) etag))
                                        ;else
               (setf (etag decoration) etag))))
    (let ((client-etag (hunchentoot:header-in* "If-None-Match")))
      (if (and client-etag (equalp (get-etag) client-etag))
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
              (set-etag content-etag)
              ;; Pass the etag to the client
              (setf (hunchentoot:header-out "ETag")
                    content-etag)
              ;; Output response
              content))))))

(cl-annot:defannotation caching (args resource-operation-implementation)
    (:arity 2)
  `(configure-resource-operation-implementation
    (name (resource-operation ,resource-operation-implementation))
    (list :caching ,@args)))
