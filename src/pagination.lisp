(in-package :rest-server)

(defmacro with-pagination ((&rest args
                                  &key (page (error "Provide the page"))
                                  (object-name "pagination")
                                  &allow-other-keys)
                           &body body)
  (let* ((fargs (copy-list args)))
    (remf fargs :page)
    (remf fargs :object-name)
    `(generic-serializer:with-object (,object-name)
       (generic-serializer:set-attribute :page ,page)
       (generic-serializer:set-attribute :next (format-absolute-resource-operation-url *resource-operation* :page (1+ ,page) ,@fargs))
       (generic-serializer:set-attribute :previous (format-absolute-resource-operation-url *resource-operation* :page (1- ,page),@fargs))
       (generic-serializer:with-attribute (:results)
         ,@body))))

(defun make-pagination-object (&rest args
                               &key (page (error "Provide the page"))
                                 (object-name "pagination")
                                 (results (error "Provide the results"))
                                 &allow-other-keys)
  (let* ((fargs (copy-list args)))
    (remf fargs :page)
    (remf fargs :object-name)
    (remf fargs :results)
    (if (zerop (length results))
        (generic-serializer:object object-name
                             (generic-serializer:attribute :results results))
        ;; else
        (generic-serializer:object object-name
                             (generic-serializer:attribute :page page)
                             (generic-serializer:attribute :next
                                                     (format-absolute-resource-operation-url *resource-operation*
                                                                                             `(:page (1+ ,page)
                                                                                                     ,@fargs)))
                             (generic-serializer:attribute :previous
                                                     (format-absolute-resource-operation-url *resource-operation*
                                                                                             `(:page (1- ,page)
                                                                                                     ,@fargs)))
                             (generic-serializer:attribute :results results)))))

;; (defclass pagination ()
;;   ((function :initarg :function
;;       :initform (error "Provide the function")
;;       :accessor pagination-function)
;;    (page :initarg :page
;;   :initform (error "Provide the page number")
;;   :accessor pagination-page)
;;    (page-size :initarg :page-size
;;        :initform (error "Provide the page size")
;;        :accessor pagination-page-size)))

;; (defun apply-pagination (pagination)
;;   (apply (pagination-function pagination)
;;   (pagination-offset pagination)
;;   (pagination-segment pagination)))

;; (defmethod encode-pagination (pagination resource-operation (serializer (eql :json)) stream &rest args)
;;   (let ((objects (apply-pagination pagination)))
;;     (json:with-object (stream)
;;       (json:as-object-member (:objects stream)
;;  (loop for object in objects
;;     do
;;       (apply #'serialize object serializer stream args)))
;;       (json:encode-object-member :page (pagination-page pagination) stream)
;;       (json:encode-object-member :next
;;               (rest-server::format-resource-operation-url
;;                resource-operation
;;                :page (1+ (pagination-page pagination))
;;                args))
;;       (json:encode-object-member :previous
;;               (rest-server::format-resource-operation-url
;;                resource-operation
;;                :page (1- (pagination-page pagination))
;;                args)))))
