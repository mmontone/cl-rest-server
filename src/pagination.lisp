(in-package :rest-server)

(defmacro with-pagination ((&rest args
				  &key (page (error "Provide the page"))
				  (element-name "pagination")
				  &allow-other-keys)
			   &body body)
  (let* ((fargs (copy-list args)))
    (remf fargs :page)
    (remf fargs :element-name)
    `(with-element (,element-name)
       (set-attribute :page ,page)
       (set-attribute :next (format-absolute-api-function-url *api-function* :page (1+ ,page) ,@fargs))
       (set-attribute :previous (format-absolute-api-function-url *api-function* :page (1- ,page),@fargs))
       (with-attribute (:results)
	 ,@body))))

(defun make-pagination-element (&rest args
				&key (page (error "Provide the page"))
				  (element-name "pagination")
				  (results (error "Provide the results"))
				  &allow-other-keys)
  (let* ((fargs (copy-list args)))
    (remf fargs :page)
    (remf fargs :element-name)
    (remf fargs :results)
    (element element-name
	     (attribute :page page)
	     (attribute :next
			(format-absolute-api-function-url *api-function*
							  `(:page (1+ ,page)
								  ,@fargs)))
	     (attribute :previous
			(format-absolute-api-function-url *api-function*
							  `(:page (1- ,page)
								  ,@fargs)))
	     (attribute :results results))))	     

;; (defclass pagination ()
;;   ((function :initarg :function
;; 	     :initform (error "Provide the function")
;; 	     :accessor pagination-function)
;;    (page :initarg :page
;; 	 :initform (error "Provide the page number")
;; 	 :accessor pagination-page)
;;    (page-size :initarg :page-size
;; 	      :initform (error "Provide the page size")
;; 	      :accessor pagination-page-size)))

;; (defun apply-pagination (pagination)
;;   (apply (pagination-function pagination)
;; 	 (pagination-offset pagination)
;; 	 (pagination-segment pagination)))	     

;; (defmethod encode-pagination (pagination api-function (serializer (eql :json)) stream &rest args)
;;   (let ((objects (apply-pagination pagination)))
;;     (json:with-object (stream)
;;       (json:as-object-member (:objects stream)
;; 	(loop for object in objects
;; 	   do
;; 	     (apply #'serialize object serializer stream args)))
;;       (json:encode-object-member :page (pagination-page pagination) stream)
;;       (json:encode-object-member :next
;; 				 (rest-server::format-api-function-url
;; 				  api-function
;; 				  :page (1+ (pagination-page pagination))
;; 				  args))
;;       (json:encode-object-member :previous
;; 				 (rest-server::format-api-function-url
;; 				  api-function
;; 				  :page (1- (pagination-page pagination))
;; 				  args)))))
