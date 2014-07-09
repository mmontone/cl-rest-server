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
    (with-elements-list ("LIST" :serializer serializer
				:stream stream)
      (cond 
	((keywordp list-type)
	 ;; It is a primitive type like :string, :boolean, etc
	 (loop for elem in input
	    do
	    (add-list-member "ITEM" elem
			     :serializer serializer
			     :stream stream)))
	((symbolp list-type)
	 ;; It is a reference to a schema like 'user-schema'
	 (let ((schema (find-schema list-type)))
	   (loop for elem in input
	      do
		(%serialize-with-schema schema serializer elem stream))))
	((listp list-type)
	 ;; It is some compound type, like :element, :list, or :option
	 (let ((schema list-type))
	   (loop for elem in input
	      do
		(%serialize-with-schema schema serializer elem stream))))))))

(defvar *schemas* (make-hash-table))

(defun register-schema (name definition)
  (setf (gethash name *schemas*)
	definition))

(defmacro define-schema (name schema)
  "Define a schema"
  `(register-schema ',name
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

(defun validate-with-schema (schema string &optional (format :json))
  "Validate input using schema"
  (let ((data (parse-api-input format string)))
    (schema-validate-with-element schema data)))

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
  (:documentation "Parses content depending on its format"))

(defmethod parse-api-input ((format (eql :json)) string)
  (json:decode-json-from-string string)) 

(defmethod parse-api-input ((format (eql :xml)) string)
  (cxml:parse string (cxml-xmls:make-xmls-builder)))

(defmethod parse-api-input ((format (eql :sexp)) string)
  (read-from-string string))

(defgeneric parse-with-schema (format schema string)
  (:documentation "Parses the string to an association list using the schema"))

(defmethod parse-with-schema (format schema string)
  (parse-api-input format string))

(defun schema-type (schema)
  (first schema))

(defmethod parse-with-schema ((format (eql :xml)) schema string)
  (parse-xml-with-schema schema (parse-api-input :xml string)))

(defun parse-xml-with-schema (schema-or-name input)
  (let ((schema (if (symbolp schema-or-name)
		    (find-schema schema-or-name)
		    schema-or-name)))
  
    (ecase (schema-type schema)
      (:list
       (let ((items (third input)))
	 (loop for item in items
	    collect
	      (parse-xml-with-schema
	       (second schema) ;; the list type
	       item))))
      (:element
       (assert (equalp (make-keyword (element-name schema))
		       (make-keyword (first input))) nil
		       "~A is not a ~A" input (element-name schema))
       (loop for attribute in (element-attributes schema)
	  appending (let ((input-attribute
			   (find (symbol-name (attribute-name attribute))
				 (cddr input)
				 :key #'first
				 :test #'equalp)))
		      (if input-attribute
			  ;; The attrbute is present
			  (list (cons (make-keyword (first input-attribute))
				      (cond
					((listp (attribute-type attribute))
					 ;; It is a compound type (:list, :element, etc)
					 (parse-xml-with-schema
					  (second (attribute-type attribute)) ;; The compound element type
					  (third input-attribute) ;; The attribute value
					  ))
					((keywordp (attribute-type attribute))
					 ;; the attribute type is simple, parse the attribute value
					 (unserialize-schema-attribute-value
					  (attribute-type attribute)
					  (third input-attribute)))
					((symbolp (attribute-type attribute))
					 ;; assume a schema reference
					 (let ((attribute-schema (find-schema (attribute-type attribute))))
					   (parse-xml-with-schema
					    attribute-schema
					    (third input-attribute) ;; The attribute value
					    )))))))))))))

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
	    (unserialize-schema-attribute-value (attribute-type attribute) input)))))

(defun unserialize-schema-attribute-value (type input)
  (%unserialize-schema-attribute-value
   (if (listp type)
       (first type)
       type)
   type
   input))

(defgeneric %unserialize-schema-attribute-value (type-name type input)
  (:method ((type-name (eql :integer)) attribute input)
    (if (integerp input)
	input
	(parse-integer input)))
  (:method ((type-name (eql :string)) type input)
    input)
  (:method ((type-name (eql :boolean)) type input)
    (if (stringp input)
	(let ((true-strings (list "true" "t" "yes" "on"))
	      (false-strings (list "false" "f" "no" "off")))
	  (assert (member input (append true-strings false-strings) :test #'equalp)
		  nil "Invalid boolean ~A" input)
	  (member input true-strings :test #'equalp))
	(not (null input))))
  (:method ((type-name (eql :element)) type input)
    (unserialize-schema-element type input))
  (:method ((type-name (eql :list)) type input)
    (let ((list-type (second type)))
      (loop for elem in input
	 collect (unserialize-schema-attribute-value list-type elem))))
  (:method ((type-name (eql :option)) type input)
    input)
  (:method (type-name type input)
    ;; Assume a schema reference
    (let ((schema (find-schema type-name nil)))
      (if (not schema)
	  (error "Invalid type ~A" type-name)
					; else
	  (unserialize-schema-attribute-value schema input)))))
