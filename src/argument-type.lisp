(in-package :rest-server)

(defvar *argument-types* nil)

(defclass argument-type ()
  ())

(defmethod print-object ((argument-type argument-type) stream)
  (print-unreadable-object (argument-type stream :type t :identity t)
    (format stream "~S" (argument-type-spec argument-type))))

(defun parse-argument-type (spec)
  (loop
     :for argument-type in *argument-types*
     :for parsed-argument-type = (%parse-argument-type argument-type spec)
     :when parsed-argument-type
     :do (return-from parse-argument-type parsed-argument-type))
  (error "Invalid argument type: ~A" spec))

(defgeneric parse-argument-value (value argument-type &key error-p))
(defgeneric format-argument-value (value argument-type))
(defgeneric argument-type-spec (argument-type))

(defgeneric %parse-argument-type (argument-type spec))

(defmacro def-argument-type (name super-types slots &rest options)
  `(progn
     (defclass ,name ,(remove-duplicates (cons 'argument-type super-types))
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
               `(defmethod parse-argument-value (,@args (argument-type ,name) &key (error-p t))
                  (flet ((parse-error (message &rest args)
                           (if error-p
                               (cerror "Continue" 'rs.schema:validation-error
                                       :format-control message
                                       :format-arguments args)
                               (return-from parse-argument-value
                                 (values nil (apply #'format message args))))))
                    ,@body))))
            (:format-value
             (destructuring-bind (args &body body) (rest option)
               `(defmethod format-argument-value (,@args (argument-type ,name))
                  ,@body)))
            (:format-spec
             (destructuring-bind ((argument-type) &body body) (rest option)
               `(defmethod argument-type-spec ((,argument-type ,name))
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
                (or
                 (ignore-errors
                   (parse-integer string
                                  :radix (radix argument-type)
                                  :junk-allowed (junk-allowed argument-type)))
                 (parse-error "~A is not an integer" string)))
  (:format-value (value)
                 (check-type value integer)
                 (princ-to-string value))
  (:format-spec (argument-type)
                :integer))

(def-argument-type float-argument-type ()
  ()
  (:parse (spec)
          (make-instance 'float-argument-type))
  (:parse-value (string)
                (let ((thing (read-from-string string)))
                  (or (and (floatp thing)
                           thing)
                      (parse-error "~A is not a float" string))))
  (:format-value (value)
                 (check-type value float)
                 (princ-to-string value))
  (:format-spec (argument-type)
                :float))

(def-argument-type string-argument-type ()
  ((max-length :initarg :max-length
               :accessor max-length
               :initform nil))
  (:parse (spec)
          (cond
            ((member spec (list :string :str :text))
             (make-instance 'string-argument-type))
            ((and (listp spec)
                  (member (first spec) (list :string :str :text)))
             (apply #'make-instance 'string-argument-type (rest spec)))))
  (:parse-value (string)
                (or (and (stringp string)
                         string)
                    (parse-error "Not a string: ~A" string)))
  (:format-value (value)
                 (assert (stringp value))
                 value)
  (:format-spec (argument-type)
                :string))

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
                  (t (parse-error "Could not parse boolean value: ~A"
                                  string))))
  (:format-value (value)
                 (if value "true" "false"))
  (:format-spec (argument-type)
                :boolean))

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
                 (format nil "~{~A~^,~}" value))
  (:format-spec (argument-type)
                :list))

(def-argument-type timestamp-argument-type ()
  ()
  (:parse (spec)
          (when (member spec (list :timestamp :datetime))
            (make-instance 'timestamp-argument-type)))
  (:parse-value (string)
                (or (local-time:parse-timestring string)
                    (chronicity:parse string)
                    (parse-error "Could not parse timestamp: ~A"
                                 string)))
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
                    (error "Not a timestamp: ~A" value))))
  (:format-spec (argument-type)
                :timestamp))

(def-argument-type keyword-argument-type ()
  ()
  (:parse (spec)
          (when (member spec (list :keyword))
            (make-instance 'keyword-argument-type)))
  (:parse-value (string)
                (or (and string (intern (string-upcase string) :keyword))
                    (parse-error "Could not parse keyword: ~A"
                                 string)))
  (:format-value (value)
                 (princ-to-string value))
  (:format-spec (argument-type)
                :keyword))

(def-argument-type choice-argument-type ()
  ((choices :initarg :choices
            :accessor choices
            :initform (error "Provide the choices"))
   (elems-type-spec :initarg :type
                    :accessor elems-type-spec
                    :initform :string))
  (:parse (spec)
          (when (and (listp spec)
                     (member (first spec) (list :option :choice)))
            (make-instance 'choice-argument-type :values (rest spec))))
  (:parse-value (string)
                (let ((elem (parse-argument-value string (elems-type-spec argument-type))))
                  (or (member elem (choices argument-type)
                              :test #'equalp)
                      (parse-error "Could not parse choice ~A: ~A"
                                   argument-type elem))))
  (:format-value (value)
                 (format-argument-value value (elems-type-spec argument-type)))
  (:format-spec (argument-type)
                `(:choice ,@(choices argument-type))))

(def-argument-type or-argument-type ()
  ((types :initarg :types
          :accessor types
          :initform (error "Provide the types")))
  (:parse (spec)
          (when (and (listp spec)
                     (member (first spec) (list :or)))
            (make-instance 'or-argument-type
                           :types (mapcar #'parse-argument-type (rest spec)))))
  (:parse-value (string)
                (loop
                   :for type :in (types argument-type)
                   :do
                   (destructuring-bind (parse-result parse-error)
                       (parse-argument-value string type :error-p nil)
                     (when (not parse-error)
                       (return-from parse-argument-value parse-result))))
                (parse-error "Could not parse ~A: ~A" argument-type string))
  (:format-value (value)
                 (loop for type in (types argument-type)
                    for formatted = (ignore-errors (format-argument-value value type))
                    when formatted
                    return formatted)
                 (error "Could not format value: ~A" value))
  (:format-spec (argument-type)
                `(:or ,@(mapcar #'argument-type-spec (types argument-type)))))
