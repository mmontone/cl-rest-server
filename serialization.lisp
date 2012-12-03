(in-package :rest-server)

(defvar *serializers* nil)
(defvar *serializer* nil)
(defvar *serializer-output* t)

;; Serializer api

(defun call-with-serializer (serializer function)
  (let ((*serializer* serializer))
    (funcall function)))

(defmacro with-serializer (serializer &body body)
  `(call-with-serializer ,serializer (lambda () ,@body)))

(defun call-with-serializer-output (serializer-output function)
  (let ((*serializer-output* serializer-output))
    (funcall function)))

(defmacro with-serializer-output (serializer-output &body body)
  `(call-with-serializer-output ,serializer-output (lambda () ,@body)))

;; Intermediate representation

(defclass element ()
  ((name :initarg :name
         :accessor name
         :initform (error "Provide a name for the element"))
   (attributes :initarg :attributes
               :accessor attributes
               :initform nil)))

(defclass attribute ()
  ((name :initarg :name
         :accessor name
         :initform (error "Provide the attribute name"))
   (value :initarg :value
          :accessor value
          :initform (error "Provide the attribute value"))))

(defclass elements-list ()
  ((name :initarg :name
         :accessor name
         :initform (error "Provide the list name"))
   (elements :initarg :elements
             :accessor list-elements
             :initform nil)))

(defun element (name &rest attributes)
  (make-instance 'element
                 :name name
                 :attributes attributes))

(defun attribute (name value)
  (make-instance 'attribute
                 :name name
                 :value value))

(defun elements (name &rest elements)
  (make-instance 'elements-list
                 :name name
                 :elements elements))

(defmethod serialize-toplevel ((serializer t) stream function)
  (funcall function))

(defmethod serialize-toplevel ((serializer (eql :xml)) stream function)
  (cxml:with-xml-output (cxml:make-character-stream-sink stream :indentation nil :omit-xml-declaration-p t)
    (funcall function)))

;; Serializer format plug

(defmethod serialize ((element element) &optional (serializer *serializer*) (stream *serializer-output*))
  (serialize-element serializer element stream))

(defmethod serialize ((elements-list elements-list) &optional (serializer *serializer*) (stream *serializer-output*))
  (serialize-elements-list serializer elements-list stream))

(defmethod serialize ((attribute attribute) &optional (serializer *serializer*) (stream *serializer-output*))
  (serialize-attribute serializer attribute stream))

(defmethod serialize ((value t) &optional (serializer *serializer*) (stream *serializer-output*))
  (serialize-value serializer value stream))


;; Json serializer

(defmethod serialize-element ((serializer (eql :json)) element stream)
  (json:with-object (stream)
    (loop for attribute in (attributes element)
         do
         (serialize attribute serializer stream))))

(defmethod serialize-elements-list ((serializer (eql :json)) elements-list stream)
  (json:with-array (stream)
    (loop for element in (list-elements elements-list)
         do (serialize element serializer stream))))

(defmethod serialize-attribute ((serializer (eql :json)) attribute stream)
  (json:as-object-member ((name attribute) stream)
    (serialize (value attribute) serializer stream)))

(defmethod serialize-value ((serializer (eql :json)) value stream)
  (json:encode-json value stream))

;; XML serializer

(defmethod serialize-element ((serializer (eql :xml)) element stream)
  (cxml:with-element (name element)
    (loop for attribute in (attributes element)
         do (serialize attribute serializer stream))))

(defmethod serialize-elements-list ((serializer (eql :xml)) elements-list stream)
  (loop for element in (list-elements elements-list)
     do
       (serialize element serializer stream)))

(defmethod serialize-attribute ((serializer (eql :xml)) attribute stream)
  (cxml:with-element (name attribute)
    (serialize (value attribute) serializer stream)))

(defmethod serialize-value ((serializer (eql :xml)) value stream)
  (cxml:text (prin1-to-string value)))

;; SEXP serializer

(defmethod serialize-element ((serializer (eql :sexp)) element stream)
  (format stream "(~s (" (name element))
  (mapcar (lambda (attribute)
            (serialize attribute serializer stream))
          (attributes element))
  (format stream ")"))

(defmethod serialize-elements-list ((serializer (eql :sexp)) elements-list stream)
  (format stream "(")
  (mapcar (lambda (element)
            (serialize element serializer stream)
            (format stream " "))
          (list-elements elements-list))
  (format stream ")"))

(defmethod serialize-attribute ((serializer (eql :sexp)) attribute stream)
  (format stream "(~S . " (name attribute))
  (serialize (value attribute) serializer stream)
  (format stream ")"))

(defmethod serialize-value ((serializer (eql :sexp)) value stream)
  (format stream "~A" value))

(defvar *default-serializer* :json "The default api serializer")

(defmethod expand-api-function-wrapping ((option (eql :serialization)) enabled args)
  (declare (ignore args))
  (when enabled
    (let ((serializer (gensym "SERIALIZER-")))
      `(let ((,serializer (or (and (hunchentoot:header-in* "accept")
                                   (cond
                                     ((equalp (hunchentoot:header-in* "accept") "application/json") :json)
                                     ((member (hunchentoot:header-in* "accept") (list "text/xml" "application/xml")
                                              :test #'equalp)
                                      :xml)
                                     ((equalp (hunchentoot:header-in "accept") "text/html") :html)))
                              *default-serializer*)))
         (with-output-to-string (s)
           (with-serializer-output s
             (with-serializer ,serializer
               (serialize-toplevel ,serializer s
                                   (lambda ()
                                     (serialize (call-next-method)))))))))))