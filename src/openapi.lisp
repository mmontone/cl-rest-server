(defpackage rs.openapi
  (:use :rest-server :cl)
  (:export :define-api-from-spec))

(in-package :rs.openapi)

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

(setf (fdefinition '->) #'access:accesses)

(declaim (ftype (function (pathname) t) define-api-from-spec))

(defun %define-api-from-spec (name filepath)
  (let ((json:*identifier-name-to-key* 'identity)
               (json:*json-identifier-name-to-lisp* 'identity))
    (let ((spec (json:decode-json-from-source filepath)))
      (define-api-from-v3-spec name spec))))

(defun define-api-from-v3-spec (name spec)
  (flet ((@ (&rest path)
             (apply #'access:accesses spec path)))
  `(rs:define-api ,name
       (:title ,(@ "info" "title")
               :documentation ,(@ "info" "description"))
     ,@(loop for resource in (collect-resources spec)
          collect (inline-resource-from-v3-spec resource)))))

(defun inline-resource-from-v3-spec (resource)
  `(,(first resource) (:produces (:json)
                                 :consumes (:json)
                                 :documentation "TODO"
                                 :path ,(common-prefix (mapcar (lambda (r) (string (car r))) (cdr resource))))
     ,@(loop for operation in (cdr resource)
         collect (inline-operation-from-v3-spec operation))))

(defun inline-operation-from-v3-spec (operation)
  (destructuring-bind (path method &rest args) operation
    `(,(intern (-> args "operationId"))
       (:request-method ,(alexandria:make-keyword (string-upcase method))
                          :produces (:json)
                          :consumes (:json)
                          :path ,(string path)
                          :documentation ,(-> args "description"))
         ,(operation-parameters-from-spec operation)))
  )

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
         do (setf result (append result (list (param-from-spec param)))))
      (when (> (length optional-params) 0)
         (setf result (append result '(&optional)))
         (loop for param in optional-params
            do (setf result (append result (list (param-from-spec param)))))))
    result))

(defun param-from-spec (param)
  `(,(intern (-> param "name"))
     ,(alexandria:make-keyword (string-upcase (-> param "schema" "type")))
     ,(-> param "description")))

(defun collect-resources (spec)
  (let ((resources (make-hash-table :test 'equalp)))
    (loop for path in (-> spec "paths")
       do (loop for operation in (cdr path)
             do
               (let ((tag (first (-> (cdr operation) "tags"))))
                 (assert (not (null tag)) nil "Operation ~A is not tagged." (car operation))
                 (let ((tag-symbol (intern tag)))
                   (if (null (gethash tag-symbol resources))
                       (setf (gethash tag-symbol resources)
                             (list (cons (car path) operation)))
                       (push (cons (car path) operation) (gethash tag-symbol resources)))))))
    (alexandria:hash-table-alist resources)))
                         
(defmacro define-api-from-spec (name filepath)
  (%define-api-from-spec name (eval filepath)))
