(defpackage rs.openapi
  (:use :rest-server :cl)
  (:export :define-api-from-spec
           :define-schemas-from-spec
           :export-api-spec))

(in-package :rs.openapi)

(defun alist (x)
  (if (hash-table-p x)
      (alexandria:hash-table-alist x)
      x))

(defun common-prefix (strings)
  (flet ((all-equal (set)
           (= 1
              (length (remove-duplicates set :test 'equalp)))))
    (loop for n downfrom (apply 'min (mapcar 'length strings)) to 1
       do
         (when (all-equal (mapcar (lambda (str)
                                    (subseq str 0 n))
                                  strings))
           (return-from common-prefix (subseq (first strings) 0 n))))
    nil))

(defun -> (&rest args)
  (apply #'access:accesses args))

(defun symbolicate (string)
  ;;(intern (json:camel-case-to-lisp string))
  (intern (json::simplified-camel-case-to-lisp string)))

(defun read-spec-file (filepath &key type)
  (let ((ext (or type (alexandria:make-keyword (string-upcase (pathname-type filepath))))))
    (ecase ext
      (:json (let ((json:*identifier-name-to-key* 'identity)
                   (json:*json-identifier-name-to-lisp* 'identity))
               (json:decode-json-from-source filepath)))
      (:yaml (cl-yaml:parse filepath)))))

(defun define-api-from-v3-spec (name superclasses options spec)
  (flet ((@ (&rest path)
             (apply #'access:accesses spec path)))
    `(rs:define-api ,name ,superclasses
         (:title ,(@ "info" "title")
                 :documentation ,(@ "info" "description")
                 ,@options)
       ,@(loop for resource in (collect-resources spec)
            collect (inline-resource-from-v3-spec resource)))))

(defun inline-resource-from-v3-spec (resource)
  `(,(first resource) (:produces (:json)
                                 :consumes (:json)
                                 :documentation "TODO"
                                 :path ,(common-prefix (mapcar (lambda (r) (string (car r))) (cdr resource))))
     ,@(loop for operation in (cdr resource)
          collect (inline-operation-from-v3-spec operation))))

(defun parse-authorization (security-name)
  (intern (format nil "~A-AUTH" (symbolicate security-name))))

(defun inline-operation-from-v3-spec (operation)
  (destructuring-bind (path method &rest args) operation
    (let ((security (mapcar (alexandria:compose 'parse-authorization
                                                'caar)
                            (-> args "security"))))
      `(,(symbolicate (-> args "operationId"))
       (:request-method ,(alexandria:make-keyword (string-upcase method))
                        :produces (:json)
                        :consumes (:json)
                        :path ,(string path)
                        :authorizations ,security
                        :documentation ,(-> args "description"))
       ,(operation-parameters-from-spec operation)))
  ))

(defun operation-parameters-from-spec (operation)
  (let ((params (-> (cddr operation) "parameters"))
        (result (make-list 0)))
    (let ((required-params (remove-if-not (lambda (param)
                                            (-> param "required"))
                                          params))
          (optional-params (remove-if (lambda (param)
                                        (-> param "required"))
                                      params)))
      (loop for param in required-params
         do (setf result (append result (list (required-param-from-spec param)))))
      (when (> (length optional-params) 0)
        (setf result (append result '(&optional)))
        (loop for param in optional-params
           do (setf result (append result (list (optional-param-from-spec param)))))))
    result))

(defun parse-value (string type)
  "TODO"
  string)

(defun required-param-from-spec (param)
  `(,(symbolicate (-> param "name"))
     ,(param-type-from-spec param)
     ,(-> param "description")))

(defun optional-param-from-spec (param)
  `(,(symbolicate (-> param "name"))
     ,(param-type-from-spec param)
     ,(when (not (null (-> param "default")))
        (parse-value (-> param "default") (param-type-from-spec param)))
     ,(-> param "description")))

(defun param-type-from-spec (param)
  (cond
    ((and (equalp (-> param "schema" "type") "string")
          (equalp (-> param "schema" "format") "date"))
     :date)
    ((and (equalp (-> param "schema" "type") "string")
          (equalp (-> param "schema" "format") "date-time"))
     :timestamp)
    (t (alexandria:make-keyword (string-upcase (-> param "schema" "type"))))))


(defun collect-resources (spec)
  (let ((resources (make-hash-table :test 'equalp)))
    (loop for path in (alist (-> spec "paths"))
       do
         (loop for operation in (alist (cdr path))
              when (member (car operation) '("get" "post" "put" "patch" "delete") :test 'equalp)
            do
              (let ((tag (first (-> (cdr operation) "tags"))))
                (assert (not (null tag)) nil "Operation ~A is not tagged." (car operation))
                (let ((tag-symbol (symbolicate tag)))
                  (if (null (gethash tag-symbol resources))
                      (setf (gethash tag-symbol resources)
                            (list (cons (car path) operation)))
                      (push (cons (car path) operation) (gethash tag-symbol resources)))))))
    (alexandria:hash-table-alist resources)))

(defmacro define-api-from-spec (name superclasses options filepath)
  (let ((spec (read-spec-file filepath)))
    (define-api-from-v3-spec name superclasses options spec)))

;; OpenAPI export

(defun call-with-destination-stream (function destination)
  (cond
    ((null destination)
     (with-output-to-string (s)
       (funcall function s)))
    ((typep destination 'pathname)
     (with-open-file (s destination :direction :output :if-exists :supersede :if-does-not-exist :create)
       (funcall function s)))
    ((typep destination 'stream)
     (funcall function destination))
    ((eql destination t)
     (funcall function t))
    (t (error "Invalid destination: ~A" destination))))

(defun export-api-spec (api &optional destination)
  (flet ((render-api (stream)
           (render-api-spec api stream)))
    (call-with-destination-stream #'render-api destination)))

(defun api-paths (api)
  (let ((paths (make-hash-table :test 'equalp)))
    (loop for resource being the hash-values of (rs::resources api)
       do
         (loop for operation being the hash-values of (rs::resource-operations resource)
            do
              (push (list :tag (rs:resource-name resource)
                          :operation operation)
                    (gethash (rs::path operation)
                             paths))))
    paths))

(defun render-api-spec (api stream)
  (flet ((@= (&rest args)
           (apply #'json:encode-object-member args)))
    (let ((json:*json-output* stream))
      (json:with-object ()
        (@= "openapi" "3.0.0")
        (json:as-object-member ("info")
          (json:with-object ()
            (@= "description" (rs::api-documentation api))
            (@= "version" "1.0.0")
            (@= "title" (rs::title api))))
        (json:as-object-member ("tags")
          (json:with-array ()
            (loop for resource being the hash-values of (rs::resources api)
               do
                 (json:as-array-member ()
                   (json:with-object ()
                     (@= "name" (string (rs:resource-name resource)))
                     (@= "description" (rs:resource-documentation resource)))))))
        (json:as-object-member ("paths")
          (json:with-object ()
            (loop for path being the hash-keys of (api-paths api)
               using (hash-value operations)
               do (encode-api-path path operations)))
          )))))

(defun encode-api-path (path operations)
  (json:as-object-member (path)
    (json:with-object ()
      (loop for spec in operations do
           (let ((tag (getf spec :tag))
                 (operation (getf spec :operation)))
             (json:as-object-member ((string-downcase (string (rs::request-method operation))))
               (json:with-object ()
                 (json:encode-object-member "tags" (list (string tag)))
                 (json:encode-object-member "summary" (rs::api-documentation operation))
                 (json:encode-object-member "description" (rs::api-documentation operation))
                 (json:encode-object-member "operationId" (rs::name operation))
                 (json:as-object-member ("parameters")
                   (json:with-array ()
                     (loop for arg in (rs::required-arguments operation)
                        do (json:as-array-member ()
                             (encode-path-parameter arg :in "path")))
                     (loop for arg in (rs::optional-arguments operation)
                        do (json:as-array-member ()
                             (encode-path-parameter arg :in "query")))
                     ))
                 )))))))

(defun encode-path-parameter (param &key (in "query"))
  (json:with-object ()
    (json:encode-object-member "name" (rs::argument-name param))
    (json:as-object-member ("schema")
      (json:with-object ()
        (json:encode-object-member "type" (openapi-type (rs::argument-type param)))))
    (json:encode-object-member "description" (rs::argument-documentation param))
    (json:encode-object-member "in" in)))

(defgeneric openapi-type (type))
(defmethod openapi-type ((type rs::integer-argument-type))
  "integer")
(defmethod openapi-type ((type rs::string-argument-type))
  "string")

;; Schemas definition

(defun parse-schemas-from-spec (spec)
  (parse-schemas-from-v3-spec spec))

(defun parse-schemas-from-v3-spec (spec)
  (loop for (schema-name . schema-def) in (alist (access:accesses spec "components" "schemas"))
     collect
       (cons (intern (json::simplified-camel-case-to-lisp schema-name))
             (schemata.json-schema:schema-from-json-schema schema-def))))

(defmacro define-schemas-from-spec (filepath)
  (let ((spec (read-spec-file filepath)))
    `(progn
       ,@(loop for (schema-name . schema-def) in (parse-schemas-from-spec spec)
              collect `(schemata:define-schema ,schema-name ,schema-def)))))
