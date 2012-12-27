(in-package :rest-server)

;; Schemas

;; Schemas may be used either to serialize objects or validate input

(defgeneric serialize-with-schema (schema input &optional serializer stream)
  (:documentation "Serialize input using schema")
  (:method (schema input &optional (serializer *serializer*) (stream *serializer-output*))
    (%serialize-with-schema schema serializer input stream)))

(defmethod %serialize-with-schema (schema serializer input stream)
  (if (listp schema)
      (ecase (first schema)
	(:list (serialize-schema-list schema serializer input stream))
	(:element (serialize-schema-element schema serializer input stream)))))

(defun serialize-schema-element (schema-element serializer input stream)
  (destructuring-bind (_ element-name attributes) schema-element
    (declare (ignore _))
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
    (let* ((accessor (symbol-function (or (getf options :accessor) attribute-name)))
	   (attribute-value (funcall accessor input)))
      (when (not (and (getf options :optional) (not attribute-value)))
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
	     (%serialize-with-schema attribute-type
				     serializer
				     attribute-value
				     stream))))))))

(defun serialize-schema-list (schema-list serializer input stream)
  (destructuring-bind (_ list-type) schema-list
    (declare (ignore _))
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
    (schema-validate-with-element schema input)))

(define-condition validation-error (simple-error)
  ())

(defun validation-error (message &rest args)
  (error 'validation-error :format-control message
	 :format-arguments args))

(defun schema-validate-with-element (schema input)
  (destructuring-bind (input-element-name input-attributes)
      input
    (destructuring-bind (schema-element-name schema-attributes)
	schema
      (when (not (equalp input-element-name schema-element-name))
	(validation-error "Element schema doesn't match ~a with ~a" input schema))
      (loop
	 for schema-attribute in schema-attributes
	 for input-attribute = (find (first schema-attribute) input-attributes :key #'first
				     :test #'equalp)
	   (progn 
	     (when (not input-attribute)
	       (validation-error "Attribute ~a not found in ~a attributes" schema-attribute input))
	     (destructuring-bind (input-attribute-name input-attribute-value) input-attribute
	       (destructuring-bind (schema-attribute-name schema-attribute-type) schema-attribute
		 (schema-typep input-attribute-value schema-attribute-type))))))))

(defgeneric parse-api-input (format string)
  )

(defmethod parse-api-input ((format (eql :json)) string)
  (json:decode-json-from-string string)) 

(defmethod parse-api-input ((format (eql :xml)) string)
  (cxml:parse string (cxml-xmls:make-xmls-builder)))

(defmethod parse-api-input ((format (eql :sexp)) string)
  (read-from-string string))