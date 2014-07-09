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
	(:option (serialize-value serializer input stream))
	(:element (serialize-schema-element schema serializer input stream)))))

(defun serialize-schema-element (schema-element serializer input stream)
  (destructuring-bind (_ element-name attributes &rest options) schema-element
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
	     ;; It is a schema reference or a serializable class reference
	     (let ((attribute-schema (find-schema attribute-type nil)))
	       (if attribute-schema
		   (%serialize-with-schema attribute-schema serializer attribute-value stream)
		   ; else, try with a serializable class reference
		   (let ((serializable-class (find-class attribute-type nil)))
		     (if (and serializable-class
			      (typep serializable-class 'serializable-class))
			 (%serialize-with-schema (serializable-class-schema serializable-class)
						 serializer attribute-value stream)
			 ; else
			 (error "Could not resolve reference ~A when serializing" attribute-type))))))
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
  "Define a schema"
  `(setf (gethash ',name *schemas*)
	 (schema ,schema)))

(defmacro schema (schema-def)
  `(quote ,schema-def))

(defun find-schema (name &optional (errorp t))
  "Find a schema definition by name"
  (multiple-value-bind (schema foundp)
      (gethash name *schemas*)
    (if (not foundp)
	(if errorp
	    (error "Schema ~a not found" name)
	    nil)
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
	   do
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

(defun element-name (element)
  (second element))

(defun element-attributes (element)
  (third element))

(defun element-options (element)
  (cdddr element))

(defun element-option (option element)
  (find option (element-options element) :key #'car))

(defun attribute-name (attribute)
  (first attribute))

(defun attribute-type (attribute)
  (second attribute))

(defun attribute-type-name (attribute)
  (let ((attribute-type (attribute-type attribute)))
    (if (listp attribute-type)
	(first attribute-type)
	attribute-type)))  

(defun attribute-options (attribute)
  (cddr attribute))

(defun attribute-option (option attribute)
  (getf (attribute-options attribute) option))

(defun attribute-optional-p (attribute)
  (attribute-option :optional attribute))      

;; Unserialization

(defun element-class (element)
  "Returns the CLOS class associated with an element. May be null."
  (let ((element-class (element-option :class element)))
    (second element-class)))

(defun element-unserializer (element)
  "Returns the unserializer of the element if any"
  (let ((unserializer (element-option :unserializer element)))
    (second unserializer)))  

;; (element-unserializer '(:element user () (:unserializer unserialize-user)))

(defun unserialize-with-schema (schema string &optional (format :json))
  (let ((input (parse-api-input format string)))
    (unserialize-schema-element schema input)))  

(defun unserialize-schema-element (element input)
  "Unserializes an schema element

Args: - element (list) : An schema element
      - input (assoc-list) : An association list with values.
                             Probably obtained from parse-api-input.

See: parse-api-input (function)"
  
  (let ((unserializer (element-unserializer element))
	(element-class (element-class element)))
    (cond
      (unserializer (funcall unserializer input))
      (element-class (unserialize-schema-element-to-class element input element-class))
      (t input))))

(defun unserialize-schema-element-to-class (element input class)
  (let ((instance (allocate-instance (find-class class))))
    (loop for attribute in (element-attributes element)
       do (progn
	    (let* ((attribute-input (cdr (assoc (make-keyword (attribute-name attribute)) input
						)))
		   (attribute-value (unserialize-schema-attribute attribute attribute-input)))
	      
	      (setf (slot-value instance (or (attribute-option :slot attribute)
					     (attribute-name attribute)))
		    attribute-value))))
    (initialize-instance instance)
    instance))

(defun unserialize-schema-attribute (attribute input)
  (let ((unserializer (attribute-option :unserializer attribute)))
    (if unserializer
	(funcall unserializer)
	(if (null input)
	    (when (not (attribute-optional-p attribute))
	      (error "Attribute ~A is not optional but value was not provided" attribute))
	    ; else
	    (unserialize-schema-type (attribute-type attribute) input)))))

(defun unserialize-schema-type (type input)
  (%unserialize-schema-type (if (listp type)
				(first type)
				type)
			    type
			    input))

(defgeneric %unserialize-schema-type (type-name type input)
  (:method ((type-name (eql :integer)) attribute input)
    (if (integerp input)
	input
	(parse-integer input)))
  (:method ((type-name (eql :string)) type input)
    input)
  (:method ((type-name (eql :element)) type input)
    (unserialize-schema-element type input))
  (:method ((type-name (eql :list)) type input)
    (let ((list-type (second type)))
      (loop for elem in input
	 collect (unserialize-schema-type list-type elem))))
  (:method ((type-name (eql :option)) type input)
    input)
  (:method (type-name type input)
    (let ((schema (find-schema type-name nil)))
      (if (not schema)
	  (error "Invalid type ~A" type-name)
					; else
	  (unserialize-schema-type schema input)))))
