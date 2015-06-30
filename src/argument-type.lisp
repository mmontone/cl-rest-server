(in-package :rest-server)

(defvar *argument-types* nil)

(defclass argument-type ()
  ())

(defun parse-argument-type (spec)
  (loop 
     :for argument-type in *argument-types*
     :for parsed-argument-type = (%parse-argument-type argument-type spec)
     :when parsed-argument-type
     :do (return-from parse-argument-type parsed-argument-type))
  (error "Invalid argument type: ~A" spec))

(defgeneric parse-argument-value (value argument-type))
(defgeneric format-argument-value (value argument-type))
  
(defgeneric %parse-argument-type (argument-type spec))

(defmacro def-argument-type (name super-types slots &rest options)
  `(progn
     (defclass ,name ,super-types
       ,slots)
     ,@(loop 
	  :for option :in options
	  :collect
	  (case (first option)
	    (:parse 
	     (destructuring-bind (args &body body) (rest option)
	       `(defmethod %parse-argument-type ((argument-type (eql ',name)) ,@args)
		  ,@body)))
	    (:parse-value
	     (destructuring-bind (args &body body) (rest option)
	       `(defmethod parse-argument-value (,@args (argument-type ,name))
		  ,@body)))
	    (:format-value
	     (destructuring-bind (args &body body) (rest option)
	       `(defmethod format-argument-value (,@args (argument-type ,name))
		  ,@body)))))
     (pushnew ',name *argument-types*)))

(def-argument-type integer-argument-type ()
  ((junk-allowed :initarg :junk-allowed
		 :accessor junk-allowed
		 :initform nil)
   (radix :initarg :radix
	  :accessor radix
	  :initform 10))
  (:parse (spec)
	  (cond 
	    ((member spec (list :integer :int))
	     (make-instance 'integer-argument-type))
	    ((and (listp spec)
		  (member (first spec) (list :integer :int)))
	     (apply #'make-instance 'integer-argument-type (rest spec)))))
  (:parse-value (string)
		(parse-integer string 
			       :radix (radix argument-type)
			       :junk-allowed (junk-allowed argument-type)))
  (:format-value (value)
		 (check-type value integer)
		 (princ-to-string value)))

(def-argument-type string-argument-type ()
  ((max-length :initarg :max-length
	       :accessor max-length
	       :initform nil))
  (:parse (spec)
	  (when (member spec (list :string :str :text))
	    (make-instance 'string-argument-type)))
  (:parse-value (string)
		string)
  (:format-value (value)
		 (assert (stringp value))
		 value))

(def-argument-type boolean-argument-type ()
  ()
  (:parse (spec)
	  (when (member spec (list :boolean :bool))
	    (make-instance 'boolean-argument-type)))
  (:parse-value (string)
		(cond 
		  ((member string (list "true" "yes" "on") :test #'equalp)
		   t)
		  ((member string (list "false" "no" "off") :test #'equalp)
		   nil)
		  (t (error "Could not parse boolean value: ~A" string))))
  (:format-value (value)
		 (if value "true" "false")))

(def-argument-type list-argument-type ()
  ((elems-type-spec 
    :initarg :type
    :accessor elems-type-spec
    :initform :string))
  (:parse (spec)
	  (cond 
	    ((eql spec :list)
	     (make-instance 'list-argument-type))
	    ((and (listp spec)
		  (eql (first spec) :list))
	     (apply #'make-instance 'list-argument-type (rest spec)))))
  (:parse-value (string)
		(let ((elems-type (parse-argument-type (elems-type-spec argument-type))))
		  (mapcar 
		   (lambda (value)
		     (parse-argument-value value elems-type))
		   (split-sequence:split-sequence #\, string))))
  (:format-value (value)
		 (assert (listp value))
		 (format nil "~{~A~^,~}" value)))

(def-argument-type timestamp-argument-type ()
  ()
  (:parse (spec)
	  (when (member spec (list :timestamp :datetime))
	    (make-instance 'timestamp-argument-type)))
  (:parse-value (string)
		(or (chronicity:parse string)
		    (error "Could not parse timestamp: ~A" string)))
  (:format-value (value)
		 (cond
		   ((integerp value)
		    (local-time:format-timestring 
		     nil 
		     (local-time:universal-to-timestamp value)))
		   ((typep value 'local-time:timestamp)
		    (local-time:format-timestring
		     nil
		     value))
		   (t
		    (error "Not a timestamp: ~A" value)))))
