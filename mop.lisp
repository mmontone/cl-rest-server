(in-package :rest-server)

(defclass serializable-object ()
  ())

(defclass serializable-class (standard-class)
  ((serialization-name :initarg :serialization-name
		       :accessor serialization-name
		       :type symbol
		       :initform nil))
  (:documentation "Metaclass for serializable objects"))

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
   (serialization-accessor
    :initform nil
    :type function
    :accessor serialization-accessor
    :initarg :serialize-accessor)
   (serialization-name
    :initform nil
    :type (or string symbol)
    :accessor serialization-name
    :initarg :serialization-name)
   (serialization-optional
    :initform nil
    :type boolean
    :accessor serialization-optional
    :initarg :serialization-optional)
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

(defmethod initialize-instance :after ((serializable-slot serializable-direct-slot-definition) &rest initargs)
  (declare (ignore initargs))
  (assert (or (not (serializable-slot-p serializable-slot))
	      (serialization-type serializable-slot))
	  nil
	  "Provide the serialization type for slot ~A" serializable-slot))

(defmethod compute-effective-slot-definition ((class serializable-class)
                                              slot-name direct-slots)
  (declare (ignore slot-name))
  (let ((effective-slot (call-next-method))
	(direct-slots (remove-if-not (lambda (slot)
				       (typep slot 'serializable-direct-slot-definition))
				     direct-slots)))
    (unless (null (cdr direct-slots))
      (error "More than one :serialize specifier"))
    (let ((direct-slot (car direct-slots)))
      (setf (serializable-slot-p effective-slot)
            (serializable-slot-p direct-slot)
	    (serialization-name effective-slot)
	    (serialization-name direct-slot)
	    (serialization-type effective-slot)
	    (serialization-type direct-slot)
	    (serialization-accessor effective-slot) 
	    (serialization-accessor direct-slot)
	    (serialization-optional effective-slot)
	    (serialization-optional direct-slot)
	    (toggle-option effective-slot)
	    (toggle-option direct-slot)))
    effective-slot))

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

(defmacro define-serializable-class (name direct-superclasses direct-slots &rest options)
  "Helper macro to define serializable classes"
  `(defclass ,name ,direct-superclasses
     ,direct-slots
     (:metaclass serializable-class)
     ,@options))

(defun serializable-class-schema (serializable-class)
  "Generate a schema using the serializable class meta info"
  (let ((serialization-name 
	 (or (and (serialization-name serializable-class)
		  (first (serialization-name serializable-class)))
	     (class-name serializable-class))))
    (list :element serialization-name
	  (loop for slot in (class-slots serializable-class)
	     when (and (typep slot 'serializable-effective-slot-definition)
		       (serializable-slot-p slot))
	     collect
	       (let ((serialization-name (or (serialization-name slot)
					     (slot-definition-name slot)))
		     (serialization-accessor (serialization-accessor slot))
		     (toggle-option (toggle-option slot))
		     (serialization-type (serialization-type slot))
		     (serialization-optional (serialization-optional slot)))
		 `(,serialization-name 
		   ,serialization-type
		   ,@(when serialization-accessor
			   (list :accessor serialization-accessor))
		   ,@(when toggle-option
			   (list :toggle toggle-option))
		   ,@(when serialization-optional
			   (list :optional t))))))))

(defmethod serialize ((object serializable-object) &optional (serializer *serializer*)
		      (stream *serializer-output*))
  (serialize-with-schema (serializable-class-schema (class-of object))
			 object
			 serializer
			 stream))