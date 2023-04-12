(in-package :rest-server)

;; Validation

(defclass validation-resource-operation-implementation-decoration
    (rs::resource-operation-implementation-decoration)
  ((schema :initarg :schema
           :accessor validation-schema
           :initform (error "Provide the validation schema"))
   (format :initarg :format
           :accessor validation-format
           :initform :json))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod rs::process-resource-operation-implementation-option
    ((option (eql :validation))
     resource-operation-implementation
     &key (enabled t)
       (schema (error "Provide the validation schema"))
       (format :json)
       #+(or abcl ecl) &allow-other-keys)
  (if enabled
      (make-instance 'validation-resource-operation-implementation-decoration
                     :schema schema
                     :format format
                     :decorates resource-operation-implementation)
      resource-operation-implementation))

(defmethod rs::execute :around ((decoration validation-resource-operation-implementation-decoration)
                                &rest args)
  (let ((posted-content (first args))) ;; Asume the posted content is in the first argument
    (let ((valid-p (schemata:validate-with-schema (validation-schema decoration)
                                                  posted-content)))
      (if (not valid-p)
          (error "The posted content is invalid")
          (call-next-method)))))

(cl-annot:defannotation validation (args resource-operation-implementation)
    (:arity 2)
  `(rs::configure-resource-operation-implementation
    (rs::name (rs::resource-operation ,resource-operation-implementation))
    (list :validation ,@args)))

;; Unserialization

(defclass unserialization-resource-operation-implementation-decoration
    (resource-operation-implementation-decoration)
  ((schema :initarg :schema
           :accessor unserialization-schema
           :initform (error "Provide the unserialization schema"))
   (format :initarg :format
           :accessor unserialization-format
           :initform :json))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod rs::process-resource-operation-implementation-option
    ((option (eql :unserialization))
     resource-operation-implementation
     &key (enabled t)
       (schema (error "Provide the unserialization schema"))
       (format :json)
       #+(or abcl ecl) &allow-other-keys)
  (if enabled
      (make-instance 'unserialization-resource-operation-implementation-decoration
                     :schema schema
                     :format format
                     :decorates resource-operation-implementation)
      resource-operation-implementation))

(defmethod rs::execute :around ((decoration unserialization-resource-operation-implementation-decoration)
                                &rest args)
  (let ((posted-content (first args))) ;; Asume the posted content is in the first argument
    (apply #'call-next-method
           (schemata:unserialize-with-schema (unserialization-schema decoration)
                                             posted-content
                                             (unserialization-format decoration))
           (rest args))))

(cl-annot:defannotation unserialization (args resource-operation-implementation)
    (:arity 2)
  `(rs::configure-resource-operation-implementation
    (rs::name (rs::resource-operation ,resource-operation-implementation))
    (list :unserialization ,@args)))
