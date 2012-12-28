(in-package :rest-server)

(defclass serializable-object ()
  ())

(defclass serializable-class (standard-class)
  ())

(defclass serializable-slot-definition (standard-slot-definition)
  ((serializable
    :initform t
    :type boolean
    :accessor serializable-slot-p
    :initarg :serialize)
   (serialization-type
    :initform nil
    :accessor serialization-type
    :initarg :serialization-type)
   (serialize-accessor
    :initform nil
    :type function
    :accessor serialize-accessor
    :initarg :serialize-accessor)
   (serialization-name
    :initform nil
    :type (or string symbol)
    :accessor serialization-name
    :initarg :serialization-name)
   (toggle-option
    :initform nil
    :type symbol
    :accessor toggle-option
    :initarg toggle-option)))


;; Slots

(defclass serializable-direct-slot-definition (serializable-slot-definition standard-direct-slot-definition)
  ())

(defclass serializable-effective-slot-definition (serializable-slot-definition standard-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class serializable-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'serializable-direct-slot-definition))

(defmethod effective-slot-definition-class ((class serializable-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'serializable-effective-slot-definition))

;; Inheritance

(defmethod validate-superclass ((sub serializable-class)
                                (sup standard-class))
  (declare (ignore sub sup))
  t)

(defmethod initialize-instance :around ((class serializable-class) &rest initargs
					&key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses
            thereis (subtypep class (find-class 'serializable-object)))
      ;; 'serializable-object is already one of the (indirect) superclasses
      (call-next-method)
      ;; 'serializable-object is not one of the superclasses, so we have to add it
      (apply #'call-next-method class :direct-superclasses
	     (append direct-superclasses
		     (list (find-class 'serializable-object)))
	     initargs)))

(defmethod reinitialize-instance :around ((class serializable-class) 
					  &rest initargs 
					  &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p
      ;; if direct superclasses are explicitly passed
      ;; this is exactly like above
      (if (loop for class in direct-superclasses
	     thereis (subtypep class (find-class 'serializable-object)))
	  (call-next-method)
	  (apply #'call-next-method class :direct-superclasses
		 (append direct-superclasses
			 (list (find-class 'serializable-object)))
		 initargs))
      ;; if direct superclasses are not explicitly passed
      ;; we _must_ not change anything
      (call-next-method))) 

(defun superclass-member-p (class superclasses)
  "Searches superclass list for class"
  (some #'(lambda (superclass)
	    (or (eq class superclass)
		(let ((supers (closer-mop:class-direct-superclasses superclass)))
		  (when supers
		    (superclass-member-p class supers)))))
	superclasses))

(defun ensure-class-inherits-from (class from-classnames direct-superclasses)
  (let* ((from-classes (mapcar #'find-class from-classnames))
	 (has-persistent-objects 
	  (every #'(lambda (class) (superclass-member-p class direct-superclasses))
		 from-classes)))
    (if (not (or (member class from-classes) has-persistent-objects))
	(progn
	  (dolist (class from-classes)
	    (setf direct-superclasses (remove class direct-superclasses)))
	  (append direct-superclasses from-classes))
	direct-superclasses)))

(defgeneric serializable-slots (object)
  (declare (optimize speed))
  (:documentation 
   "Return a list of slot-definitions to serialize. The default
    is to call serializable-slots-using-class with the object 
    and the objects class")
  (:method ((object standard-object))
   (serializable-slots-using-class object (class-of object)))
#+(or sbcl cmu openmcl allegro)
  (:method ((object structure-object))
   (serializable-slots-using-class object (class-of object)))
  (:method ((object condition))
   (serializable-slots-using-class object (class-of object))))

; unfortunately the metaclass of conditions in sbcl and cmu 
; are not standard-class

(defgeneric serializable-slots-using-class (object class)
  (declare (optimize speed))
  (:documentation "Return a list of slot-definitions to serialize.
   The default calls compute slots with class")
  (:method ((object t) (class serializable-class))
    (closer-mop:class-slots class)))

(defmethod transactional-slot-p ((object serializable-object) slot-name)
  (some (lambda (slot)
	  (equalp (slot-value slot 'sb-pcl::name) slot-name))
	(serializable-slots object)))