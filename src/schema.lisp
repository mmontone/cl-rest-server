(in-package :rest-server.schema)

;; Schemas

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

(defvar *collect-validation-errors* nil)
(defvar *signal-validation-errors* t)
(defvar *validation-errors-collection*)

(defun object-name (object)
  (second object))

(defun object-attributes (object)
  (third object))

(defun object-options (object)
  (cdddr object))

(defun object-option (option object)
  (cadr (find option (object-options object) :key 'car)))

(defun find-object-attribute (object attribute-name &key (error-p t))
  (loop for attribute in (object-attributes object)
     when (equalp (string (attribute-name attribute))
                  (string attribute-name))
     do (return-from find-object-attribute attribute))
  (when error-p
    (error "Attribute ~A not found in ~A" attribute-name object)))

(defun object-documentation (object)
  (object-option :documentation object))

(defun object-class (object)
  "Returns the CLOS class associated with an object. May be null."
  (object-option :class object))

(defun object-unserializer (object)
  "Returns the unserializer of the object if any"
  (object-option :unserializer object))


;; Schemas may be used either to serialize objects or validate input

(defun serialize-with-schema (schema input
                              &optional (serializer rs.serialize::*serializer*)
                                (stream rs.serialize::*serializer-output*))
  (%serialize-with-schema schema serializer input stream))

(defmethod %serialize-with-schema (schema serializer input stream)
  (%%serialize-with-schema (schema-type schema) schema serializer input stream))

(defmethod %%serialize-with-schema (schema-type schema serializer input stream)
  (serialize-value serializer input stream))

(defmethod %%serialize-with-schema ((schema-type (eql :list))
                                    schema serializer input stream)
  (serialize-schema-list schema serializer input stream))

(defmethod %%serialize-with-schema ((schema-type (eql :option))
                                    schema serializer input stream)
  (serialize-value serializer input stream))

(defmethod %%serialize-with-schema ((schema-type (eql :object))
                                    schema serializer input stream)
  (serialize-schema-object schema serializer input stream))

(defmethod %%serialize-with-schema ((schema-type (eql :integer))
                                    schema serializer input stream)
  (serialize-value serializer input stream))

(defmethod %%serialize-with-schema ((schema-type (eql :boolean))
                                    schema serializer input stream)
  (serialize-value serializer input stream))

(defmethod %%serialize-with-schema ((schema-type (eql :string))
                                    schema serializer input stream)
  (serialize-value serializer input stream))

(defmethod %%serialize-with-schema ((schema-type (eql :timestamp))
                                    schema serializer input stream)
  (serialize-value serializer input stream))

(defmethod %%serialize-with-schema ((schema-type (eql :keyword))
                                    schema serializer input stream)
  (serialize-value serializer (string-downcase (string input)) stream))

(defun serialize-schema-object (schema-object serializer input stream)
  (destructuring-bind (_ object-name attributes &rest options) schema-object
    (declare (ignore _ options))
    (with-object ((or (and (stringp object-name)
                           object-name)
                      (symbol-name object-name))
                  :serializer serializer
                  :stream stream)
      (loop for attribute in attributes
         do
           (serialize-schema-attribute attribute serializer input stream)))))

(defvar *null-values* (list nil)
  "The list of values considered null. Attribute with these values are not
serialized when optional. Useful for treatment of special values, like :null in Postmodern library")

(defun null-value (value)
  (member value *null-values*))

(defun serialize-schema-attribute (schema-attribute serializer input stream)
  (destructuring-bind
        (attribute-name attribute-type &rest options)
      schema-attribute
    (let* ((reader (symbol-function (or (getf options :reader)
                                        (getf options :accessor)
                                        attribute-name)))
           (attribute-value (funcall reader input)))
      (when (and
             ;; Booleans are always serialized. We don't have a way of
             ;; distinguishing between "false" and "unset" boolean attributes (both are nil)
             (not (eql attribute-type :boolean))
             (not (and (getf options :optional) (null-value attribute-value))))
        (with-attribute (attribute-name :serializer serializer
                                        :stream stream)
          (cond
            ((getf options :serializer)
             (funcall (getf options :serializer) attribute-value))
            ((keywordp attribute-type)
             (serialize-attribute-value attribute-type attribute-value stream serializer))
            ((symbolp attribute-type)
             ;; It is a schema reference or a serializable class reference
             (let ((attribute-schema (find-schema attribute-type nil)))
               (if attribute-schema
                   (%serialize-with-schema attribute-schema serializer attribute-value stream)
                                        ; else, try with a serializable class reference
                   (let ((serializable-class (find-class attribute-type nil)))
                     (if (and serializable-class
                              (typep serializable-class 'rs.mop::serializable-class))
                         (%serialize-with-schema (rs.mop::serializable-class-schema serializable-class)
                                                 serializer attribute-value stream)
                                        ; else
                         (error "Could not resolve reference ~A when serializing" attribute-type))))))
            ((listp attribute-type)
             (%serialize-with-schema attribute-type
                                     serializer
                                     attribute-value
                                     stream))))))))

(defmethod serialize-attribute-value (attribute-type attribute-value stream &optional (serializer rs.serialize::*serializer*))
  (serialize attribute-value serializer stream))

(defmethod serialize-attribute-value ((attribute-type (eql :timestamp)) attribute-value stream &optional (serializer rs.serialize::*serializer*))
  (declare (ignore serializer))
  (if (integerp attribute-value)
      ;; Assume a universal time number
      (write (net.telent.date:universal-time-to-rfc-date attribute-value) :stream stream)
      ;; else, serialize whatever it is
      (call-next-method)))

(defmethod serialize ((thing local-time:timestamp)
                      &optional (serializer rs.serialize::*serializer*)
                        (stream rs.serialize::*serializer-output*) &rest args)
  (declare (ignore serializer args))
  (local-time:format-rfc1123-timestring stream thing))

(defun serialize-schema-list (schema-list serializer input stream)
  (destructuring-bind (_ list-type) schema-list
    (declare (ignore _))
    (with-list ("LIST" :serializer serializer
                       :stream stream)
      (cond
        ((keywordp list-type)
         ;; It is a primitive type like :string, :boolean, etc
         (loop for elem in (coerce input 'list)
            do
              (add-list-member "ITEM" elem
                               :serializer serializer
                               :stream stream)))
        ((symbolp list-type)
         ;; It is a reference to a schema like 'user-schema'
         (let ((schema (find-schema list-type)))
           (loop for elem in (coerce input 'list)
              do
                (with-list-member ("ITEM" :serializer serializer
                                          :stream stream)
                  (%serialize-with-schema schema serializer elem stream)))))
        ((listp list-type)
         ;; It is some compound type, like :object, :list, or :option
         (let ((schema list-type))
           (loop for elem in (coerce input 'list)
              do
                (with-list-member ("ITEM" :serializer serializer
                                          :stream stream)
                  (%serialize-with-schema schema serializer elem stream)))))))))

(define-condition validation-error (simple-error)
  ())

(define-condition validation-error-collection (validation-error)
  ((validation-errors :initarg :validation-errors
                      :initform (error "Provide the validation errors")
                      :accessor validation-errors))
  (:report (lambda (c s)
             (format s "Validation errors: ~{~A~^, ~}"
                     (validation-errors c)))))

(defun validation-error (message &rest args)
  (cerror "Continue"
          'validation-error
          :format-control message
          :format-arguments args))

(defun simple-condition-message (condition)
  (apply #'format
         (simple-condition-format-control condition)
         (simple-condition-format-arguments condition)))

(defun validate-with-schema (schema string-or-data
                             &key
                               (format :json)
                               (collect-errors *collect-validation-errors*)
                               (error-p *signal-validation-errors*))
  "Validate input using schema.
Useful for validating resource operations posted content (for :post and :put methods).
Input can be a string or an association list.

Args:
  - schema (symbol or schema): The schema
  - string-or-data(string or list): The data to validate.
  - format (keyword): The data format
  - collect-errors (boolean): If true, collect all the validation errors. If false, return the first validation error found. Default: true.
  - error-p (boolean): If true, when validation errors are found, a validation error is signaled. If false, the validation errors are returned as the function result and no error is signaled."
  (let ((data (if (stringp string-or-data)
                  (rs::parse-api-input format string-or-data)
                  string-or-data))
        (*collect-validation-errors* collect-errors)
        (*signal-validation-errors* error-p)
        (*validation-errors-collection* nil))
    (let ((validation-error
           (handler-bind ((validation-error
                           (lambda (validation-error)
                             (cond
                               (collect-errors
                                (push validation-error *validation-errors-collection*)
                                (invoke-restart (find-restart 'continue)))
                               ((not error-p)
                                (return-from validate-with-schema validation-error))
                               (t
                                (error validation-error))))))
             (schema-validate schema data))))
      (if collect-errors
          *validation-errors-collection*
          validation-error))))

(defgeneric schema-validate (schema data &optional attribute)
  )

(defmethod schema-validate (schema data &optional attribute)
  ;; If present, the attribute-validator replaces completely the default schema validation. To avoid replacing it, but adding more validation use :add-validator
  (flet ((schema-validator ()
           (let ((schema (or (and (symbolp schema) (not (keywordp schema))
                                  (find-schema schema))
                             schema)))
             (%schema-validate (schema-type schema) schema data attribute))))
    (if (and attribute (attribute-validator attribute))

        ;; The validator function receives the data to validate and an "schema validator" function
        ;; it can use to validate the schema
        (funcall (attribute-validator attribute) data #'schema-validator)
        ;; else
        (schema-validator))))

(defmethod schema-validate :after (schema data &optional attribute)
  ;; After normal validation, :add-validator is evaluated if found
  (when (and attribute (attribute-add-validator attribute))
    (multiple-value-bind (valid-p error-message) (funcall (attribute-add-validator attribute) data)
      (when (not valid-p)
        (validation-error (or error-message
                              (format nil "~A: is invalid"
                                      (or (attribute-external-name attribute)
                                          (attribute-name attribute)))))))))

(defmethod %schema-validate ((schema-type (eql :object)) schema data &optional attribute)
  (declare (ignore attribute))
  "Validate data using schema object. "
  (loop
     :for schema-attribute :in (object-attributes schema)
     :for data-attribute := (assoc (string (attribute-name schema-attribute))
                                   data
                                   :test #'equalp
                                   :key #'string)
     :do
     (cond
       ((and (not data-attribute)
             (not (attribute-optional-p schema-attribute)))
        (let ((error-msg (or (attribute-option :attribute-required-message schema-attribute)
                             (format nil "Attribute required: ~a"
                                     (or (attribute-external-name schema-attribute)
                                         (attribute-name schema-attribute))))))
          (validation-error error-msg)))
       ((not data-attribute)
        ;; Nothing to validate
        )
       ((not (and (attribute-optional-p schema-attribute)
                     (null data-attribute)))
       (schema-validate (attribute-type schema-attribute)
                        (cdr data-attribute)
                        schema-attribute)))))

(defmethod %schema-validate ((schema-type (eql :list)) schema data &optional attribute)
  (when (not (listp data))
    (validation-error "~A: ~A is not of type ~A"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      attribute
                      (attribute-type attribute)))
  (every (lambda (val)
           (schema-validate (second schema) val))
         data))

(defmethod %schema-validate ((schema-type (eql :string)) schema data &optional attribute)
  (when (not (stringp data))
    (validation-error "~A: ~A is not a string"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      data)))

(defmethod %schema-validate ((schema-type (eql :boolean)) schema data &optional attribute)
  (when (not (typep data 'boolean))
    (validation-error "~A: ~A is not a boolean"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      data)))

(defmethod %schema-validate ((schema-type (eql :integer)) schema data &optional attribute)
  (when (not (integerp data))
    (validation-error "~A: ~A is not a number"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      data)))

(defmethod %schema-validate ((schema-type (eql :float)) schema data &optional attribute)
  (when (not (floatp data))
    (validation-error "~A: ~A is not a float"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      data)))

(defmethod %schema-validate ((schema-type (eql :timestamp)) schema data &optional attribute)
  (when (not
         (or (typep data 'local-time:timestamp)
             (and (stringp data)
                  (or (ignore-errors (local-time:parse-timestring data
                                                                  :allow-missing-timezone-part t))
                      (chronicity:parse data)))))
    (validation-error "~A: ~A is not a valid timestamp"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      data)))

(defmethod %schema-validate ((schema-type (eql :datetime)) schema data &optional attribute)
  (when (not
         (or (typep data 'local-time:timestamp)
             (and (stringp data)
                  (or (ignore-errors (local-time:parse-timestring data
                                                                  :allow-missing-timezone-part t))
                      (chronicity:parse data)))))
    (validation-error "~A: ~A is not a valid timestamp"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      data)))

(defmethod %schema-validate ((schema-type (eql :date)) schema data &optional attribute)
  (when (not
         (or (typep data 'local-time:timestamp)
             (and (stringp data)
                  (or (ignore-errors (local-time:parse-timestring data
                                                                  :allow-missing-time-part t
                                                                  :allow-missing-timezone-part t))
                      (chronicity:parse data)))))
    (validation-error "~A: ~A is not a valid date"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      data)))

(defmethod %schema-validate ((schema-type (eql :keyword)) schema data &optional attribute)
  (when (not (stringp data))
    (validation-error "~A: ~A is not a keyword"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      data)))

(defgeneric parse-with-schema (schema string-or-data &optional format)
  (:documentation "Parses the string to an association list using the schema"))

(defmethod parse-with-schema ((schema symbol) string-or-data &optional (format :json))
  (parse-with-schema (find-schema schema) string-or-data format))

(defmethod parse-with-schema (schema string-or-data &optional (format :json))
  (let ((data
         (if (stringp string-or-data)
             (rs::parse-api-input format string-or-data)
             string-or-data)))
    (%parse-with-schema (schema-type schema)
                        schema
                        data)))

(defmethod %parse-with-schema ((schema-type (eql :object))
                               schema data)
  (if (null data)
      data
      (loop
         for schema-attribute in (object-attributes schema)
         for data-attribute = (assoc (string (attribute-name schema-attribute))
                                     data
                                     :test #'equalp
                                     :key #'string)
         appending
           (cond
             ((and (not data-attribute)
                   (not (attribute-optional-p schema-attribute)))
              (validation-error "Attribute ~a not found in ~a"
                                (attribute-name schema-attribute)
                                data))
             ((not data-attribute)
              ;; don't add the attribute to the data in this case
              ;; idea to think of: we could use the attribute default value if specified, instead
              nil)
             ((or (equalp (attribute-type schema-attribute) :boolean)
                  (not (null (cdr data-attribute))))
              (list (cons (intern (string (attribute-name schema-attribute)) :keyword)
                          (parse-schema-attribute schema-attribute (cdr data-attribute)))))))))

(defun parse-schema-attribute (schema-attribute value)
  (let ((parsed-value (parse-schema-attribute-value (attribute-type schema-attribute) value)))
    (if (attribute-parser schema-attribute)
        (funcall (attribute-parser schema-attribute)
                 parsed-value)
        parsed-value)))

(defmethod %parse-with-schema ((schema-type (eql :list))
                               schema data)
  (let ((elem-schema (second schema)))
    (flet ((parse-elem (elem)
             (if (symbolp elem-schema)
                 (parse-schema-attribute-value elem-schema elem)
                 (parse-with-schema elem-schema elem))))
      (loop for elem in data
         collect (parse-elem elem)))))

(defmethod parse-schema-attribute-value ((type (eql :string)) data)
  (string data))

(defmethod parse-schema-attribute-value ((type (eql :boolean)) data)
  (cond
    ((or (eql data t)
         (eql data nil))
     data)
    ((stringp data)
     (cond
       ((member data (list "true" "t" "yes" "on") :test #'equalp)
        t)
       ((member data (list "false" "f" "no" "off") :test #'equalp)
        nil)
       (t (validation-error "~A is not a boolean" data))))
    (t (validation-error "~A is not a boolean" data))))

(defmethod parse-schema-attribute-value ((type (eql :integer)) data)
  (cond
    ((integerp data)
     data)
    ((stringp data)
     (parse-integer data))
    (t (validation-error "~A is not an integer" data))))

(defmethod parse-schema-attribute-value ((type (eql :float)) data)
  (cond
    ((floatp data)
     data)
    ((stringp data)
     (read-from-string data))
    (t (validation-error "~A is not a float" data))))

(defmethod parse-schema-attribute-value ((type (eql :timestamp)) data)
  (or (ignore-errors (local-time:parse-timestring data :allow-missing-timezone-part t))
      (chronicity:parse data)))

(defmethod parse-schema-attribute-value ((type (eql :datetime)) data)
  (or (ignore-errors (local-time:parse-timestring data :allow-missing-timezone-part t))
      (chronicity:parse data)))

(defmethod parse-schema-attribute-value ((type (eql :time)) data)
  (or
   (ignore-errors (local-time:parse-timestring
                   data
                   :allow-missing-date-part t
                   :allow-missing-timezone-part t))
   (chronicity:parse data)))

(defmethod parse-schema-attribute-value ((type (eql :date)) data)
  (or 
   (ignore-errors (local-time:parse-timestring
                   data
                   :allow-missing-time-part t
                   :allow-missing-timezone-part t))
   (chronicity:parse data)))

(defmethod parse-schema-attribute-value ((type (eql :keyword)) data)
  (intern (string-upcase data) :keyword))

(defmethod parse-schema-attribute-value ((type symbol) data)
  (let ((schema (find-schema type)))
    (parse-with-schema schema data)))

(defmethod parse-schema-attribute-value ((type cons) data)
  (parse-with-schema type data))

(defun schema-type (schema)
  (if (listp schema)
      (first schema)
      schema))

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
      (:object
       (assert (equalp (rs::make-keyword (object-name schema))
                       (rs::make-keyword (first input))) nil
                       "~A is not a ~A" input (object-name schema))
       (loop for attribute in (object-attributes schema)
          appending (let ((input-attribute
                           (find (symbol-name (attribute-name attribute))
                                 (cddr input)
                                 :key #'first
                                 :test #'equalp)))
                      (if input-attribute
                          ;; The attrbute is present
                          (list (cons (rs::make-keyword (first input-attribute))
                                      (cond
                                        ((listp (attribute-type attribute))
                                         ;; It is a compound type (:list, :object, etc)
                                         (parse-xml-with-schema
                                          (second (attribute-type attribute)) ;; The compound object type
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

(defun attribute-accessor (attribute)
  (attribute-option :accessor attribute))

(defun attribute-validator (attribute)
  (attribute-option :validator attribute))

(defun attribute-add-validator (attribute)
  (attribute-option :add-validator attribute))

(defun attribute-writer (attribute)
  (or (and (attribute-option :writer attribute)
           (alexandria:ensure-function (attribute-option :writer attribute)))
      (and (attribute-accessor attribute)
           (alexandria:ensure-function
            `(setf
              ,(attribute-accessor attribute))))
      (lambda (value obj)
        (setf (access:access obj (attribute-name attribute)) value))))

(defun attribute-reader (attribute)
  (or
   (and (attribute-option :reader attribute)
        (alexandria:ensure-function (attribute-option :reader attribute)))
   (and (attribute-accessor attribute)
        (alexandria:ensure-function (attribute-accessor attribute)))
   (lambda (obj)
     (access:access obj (attribute-name attribute)))))

(defun attribute-parser (attribute)
  (attribute-option :parser attribute))

(defun attribute-formatter (attribute)
  (attribute-option :formatter attribute))

(defun attribute-documentation (attribute)
  (attribute-option :documentation attribute))

(defun attribute-external-name (attribute)
  "Name of the field that is shown in error messages (and in serialization?)"
  (attribute-option :external-name attribute))

;; Unserialization

;; (object-unserializer '(:object user () (:unserializer unserialize-user)))

(defun unserialize-with-schema (schema string-or-data &optional (format :json))
  (let ((data (if (stringp string-or-data)
                  (rs::parse-api-input format string-or-data)
                  string-or-data)))
    (unserialize-schema-object schema data)))

(defun unserialize-schema-object (object input)
  "Unserializes an schema object

Args: - object (list) : An schema object
      - input (assoc-list) : An association list with values.
                             Probably obtained from parse-api-input.

See: parse-api-input (function)"

  (let ((unserializer (object-unserializer object))
        (object-class (object-class object)))
    (cond
      (unserializer (funcall unserializer input))
      (object-class (unserialize-schema-object-to-class object input object-class))
      (t input))))

(defun unserialize-schema-object-to-class (object input class)
  (let ((instance (allocate-instance (find-class class))))
    (loop for attribute in (object-attributes object)
       do (let ((attribute-input (assoc (string (attribute-name attribute))
                                        input
                                        :test #'equalp
                                        :key #'string)))
            (when (and (not attribute-input)
                       (not (attribute-optional-p attribute)))
              (validation-error "~A not provided" (attribute-name attribute)))
            (let ((attribute-value (unserialize-schema-attribute attribute (cdr attribute-input))))
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
              (validation-error
               "Attribute ~A is not optional but value was not provided"
               (attribute-name attribute)))
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
  (:method ((type-name (eql :object)) type input)
    (unserialize-schema-object type input))
  (:method ((type-name (eql :list)) type input)
    (let ((list-type (second type)))
      (loop for elem in input
         collect (unserialize-schema-attribute-value list-type elem))))
  (:method ((type-name (eql :option)) type input)
    input)
  (:method ((type-name symbol) type input)
    ;; Assume a schema reference
    (let ((schema (find-schema type-name nil)))
      (if (not schema)
          (error "Invalid type ~A" type-name)
                                        ; else
          (unserialize-schema-attribute-value schema input)))))

(defun populate-with-schema (schema object data &key exclude)
  "Populate CLOS objects from data + schema.
Attributes members of EXCLUDE parameter are not populated."
  (loop for attribute in (object-attributes schema)
     unless (member (attribute-name attribute) exclude)
     do
       (multiple-value-bind (value found)
           (access:access data (attribute-name attribute) :skip-call? t) ;; skip calls, as data should be plain data
         (when found
           (let ((attribute-writer (attribute-writer attribute)))
             (funcall attribute-writer
                      value
                      object))))))

;; Plugging

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
    (let ((valid-p (validate-with-schema (validation-schema decoration)
                                         posted-content
                                         :format (validation-format decoration))))
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
           (unserialize-with-schema (unserialization-schema decoration)
                                    posted-content
                                    (unserialization-format decoration))
           (rest args))))

(cl-annot:defannotation unserialization (args resource-operation-implementation)
    (:arity 2)
  `(rs::configure-resource-operation-implementation
    (rs::name (rs::resource-operation ,resource-operation-implementation))
    (list :unserialization ,@args)))

;; JSON-schema (WIP)

(defun json-schema (schema)
  (with-output-to-string (s)
    (let ((json:*json-output* s))
      (render-json-schema schema))))

(defun render-json-schema (schema &optional attribute)
  (ecase (schema-type schema)
    (:object (render-object-json-schema schema attribute))
    (:list (render-array-json-schema schema attribute))
    (t (render-type-json-schema schema attribute))))

(defun render-object-json-schema (schema attribute)
  (json:with-object ()
    (json:encode-object-member "type" "object")
    (json:as-object-member ("properties")
      (json:with-object ()
        (loop for attribute in (object-attributes schema)
           do
             (json:as-object-member ((attribute-name attribute))
               (render-json-schema (attribute-type attribute) attribute)))))
    (json:encode-object-member "description" (object-documentation schema))))


;; JSON schema parsing

(defun alist (x)
  (if (hash-table-p x)
      (alexandria:hash-table-alist x)
      x))

(defun parse-json-schema-ref (ref)
  (let ((schema-name (car (last (split-sequence:split-sequence #\/ ref)))))
    (intern (json::simplified-camel-case-to-lisp schema-name))))

(defun schema-from-json-schema (json-schema)
  (if (access:access json-schema "$ref")
      (parse-json-schema-ref (access:access json-schema "$ref"))
      (case (alexandria:make-keyword (string-upcase (access:access json-schema "type")))
        (:object (parse-json-schema-object json-schema))
        (:array (parse-json-schema-array json-schema))
        (:integer (parse-json-schema-integer json-schema))
        (:number (parse-json-schema-number json-schema))
        (:string (parse-json-schema-string json-schema))
        (:boolean (parse-json-schema-boolean json-schema))
        (t (error "Invalid JSON schema type: ~A" (access:access json-schema "type"))))))

(defun parse-json-schema-object (json-schema)
  (let ((required-props (access:access json-schema :required)))
    `(:object ,(access:access json-schema "title")
              ,(loop for prop in (alist (access:access json-schema "properties"))
                  collect (parse-json-schema-object-property prop (member (car prop) required-props :test 'equalp)))
              (:documentation ,(access:access json-schema :description)))))

(defun parse-json-schema-object-property (prop &optional (required-p t))
  `(,(intern (json:camel-case-to-lisp (car prop)))
     ,(schema-from-json-schema (cdr prop))
     :external-name ,(car prop)
     ,@(when (not required-p)
         (list :optional t))
     ,@(let ((default (access:access (cdr prop) "default")))
         (when default
           (list :default default)))
     ;; CUSTOM JSON SCHEMA PROPERTIES
     ;; These are not JSON schema properties, we parse some extra attributes
     ;; to fill-in REST-SERVER schema things not present in JSON schemas, like
     ;; accessors, readers, formatters, etc
     ;; Extension properties begin with an "x-" prefix
     ,@(when (access:access (cdr prop) "x-accessor")
         (list :accessor (read-from-string (access:access (cdr prop) "x-accessor"))))
     ,@(when (access:access (cdr prop) "x-reader")
         (list :reader (read-from-string (access:access (cdr prop) "x-reader"))))
     ,@(when (access:access (cdr prop) "x-writer")
         (list :writer (read-from-string (access:access (cdr prop) "x-writer"))))
     ,@(when (access:access (cdr prop) "x-validator")
         (list :validator (read-from-string (access:access (cdr prop) "x-validator"))))
     ,@(when (access:access (cdr prop) "x-add-validator")
         (list :add-validator (read-from-string (access:access (cdr prop) "x-add-validator"))))
     
     :documentation ,(access:access (cdr prop) "description")))

(defun parse-json-schema-boolean (json-schema)
  (declare (ignore json-schema))
  :boolean)

(defun parse-json-schema-integer (json-schema)
  (declare (ignore json-schema))
  :integer)

(defun parse-json-schema-string (json-schema)
  (cond
    ((equalp (access:access json-schema "format")
             "date")
     :date)
    ((equalp (access:access json-schema "format")
             "date-time")
     :datetime)
    (t :string)))

(defun parse-json-schema-number (json-schema)
  (alexandria:make-keyword (string-upcase (access:access json-schema :format))))

(defun parse-json-schema-array (json-schema)
  `(:list ,(schema-from-json-schema (access:accesses json-schema "items"))))
