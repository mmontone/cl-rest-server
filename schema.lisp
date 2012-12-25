(in-package :rest-server)

;; Schemas

;; Schemas may be used either to serialize objects or validate input

(defgeneric serialize-with-schema (schema input &optional serializer stream)
  (:documentation "Serialize input using schema")
  (:method (schema input &optional (serializer *serializer*) (stream *serializer-output*))
    (%serialize-with-schema schema serializer input stream)))

(defmethod %serialize-with-schema (schema serializer input stream)
  (serialize-schema-element schema serializer input stream))

(defun serialize-schema-element (schema-element serializer input stream)
  (destructuring-bind (element-name attributes) schema-element
    (with-element ((or (and (stringp element-name)
			    element-name)
		       (symbol-name element-name))
		   :serializer serializer
		   :stream stream)
      (loop for attribute in attributes
	   do
	   (serialize-schema-attribute attribute serializer input stream)))))

(defun serialize-schema-attribute (schema-attribute serializer input stream)
  (destructuring-bind 
	(attribute-name attribute-type &rest options)
      schema-attribute
    (let* ((accessor (or (getf options :accessor)
			 (symbol-function attribute-name)))
	   (attribute-value (funcall accessor input)))
      (with-attribute (attribute-name :serializer serializer
				      :stream stream)
	(cond 
	  ((keywordp attribute-type)
	   (serialize attribute-value serializer stream))
	  ((symbolp attribute-type)
	   ;; It is a schema reference
	   (let ((attribute-schema (find-schema attribute-type)))
	     (%serialize-with-schema attribute-schema serializer attribute-value stream)))
	  ((listp attribute-type)
	   (if (equalp (first attribute-type) :schema)
	       (%serialize-with-schema (second attribute-type)
				       serializer
				       attribute-value
				       stream)
	       (serialize-schema-list attribute-type
				      serializer
				      attribute-value
				      stream))))))))

(defun serialize-schema-list (schema-list serializer input stream)
  (let ((list-type (first schema-list)))
    (with-elements-list ("list" :serializer serializer
				:stream stream)
      (cond 
	((keywordp list-type)
	 (loop for elem in input
	      do
	      (add-list-member "item" elem serializer stream)))
	((symbolp list-type)
	 (let ((schema (find-schema list-type)))
	   (loop for elem in input
		do
		(with-list-member ("item" :serializer serializer
					  :stream stream)
		  (%serialize-with-schema schema serializer elem stream)))))
	((listp list-type)
	 (let ((schema list-type))
	   (loop for elem in input
		do
		(with-list-member ("item" :serializer serializer
					  :stream stream)
		  (%serialize-with-schema schema serializer elem stream)))))))))

(defvar *schemas* (make-hash-table))

(defmacro define-schema (name schema)
  `(setf (gethash ',name *schemas*)
	 (schema ,schema)))

(defmacro schema (schema-def)
  `(quote ,schema-def))

(defun find-schema (name)
  (multiple-value-bind (schema foundp)
      (gethash name *schemas*)
    (if (not foundp)
	(error "Schema ~a not found" name)
	schema)))

(defgeneric validate-with-schema (schema input)
  (:documentation "Validate input using schema")
  (:method (schema input)
    (error "Not implemented")))