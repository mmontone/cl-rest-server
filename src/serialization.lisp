(in-package :rest-server.serialize)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *serializers* nil)
  (defvar *serializer-output* t)
  (defvar *default-serializer* :json "The default api serializer")
  (defvar *serializer* nil)

  ;; Serializer api

  (defun call-with-serializer (serializer function)
    (let ((*serializer* serializer))
      (serialize-toplevel serializer 
			  *serializer-output*
			  function)))

  (defmacro with-serializer (serializer &body body)
    "Execute body in serializer scope. Binds *serializer* to serializer.

     Example:
     (with-serializer :json
      (serialize user))"
    `(call-with-serializer ,serializer (lambda () ,@body)))

  (defun call-with-serializer-output (serializer-output function)
    (let ((*serializer-output* serializer-output))
      (funcall function)))

  (defmacro with-serializer-output (serializer-output &body body)
    "Defines the serializer output when executing body.
 
     Example: 
     (with-serializer-output s
        (with-serializer :json
           (serialize user)))"
  
    `(call-with-serializer-output ,serializer-output (lambda () ,@body)))

  ;; Generic streaming serialization api

  (defmacro with-element ((name
			   &key (serializer '*serializer*)
			   (stream '*serializer-output*))
			  &body body)
    "Serializes a serializing element."
    `(call-with-element ,serializer ,name (lambda () ,@body) ,stream))

  (defmacro with-attribute ((name &key (serializer '*serializer*)
				  (stream '*serializer-output*))
			    &body body)
    "Serializes an element attribute"
    `(call-with-attribute ,serializer
			  ,name
			  (lambda () ,@body)
			  ,stream))

  (defmacro with-list 
      ((name &key (serializer '*serializer*)
	     (stream '*serializer-output*)) 
       &body body)
    "Serializes an list of elements"
    `(call-with-list ,serializer ,name (lambda () ,@body) ,stream))

  (defmacro with-list-member ((name 
			       &key (serializer '*serializer*)
			       (stream '*serializer-output*))
			      &body body)
    "Serializes a list member"
    `(call-with-list-member ,serializer ,name (lambda () ,@body) ,stream))

  )


;; Intermediate representation

(defclass element ()
  ((name :initarg :name
         :accessor name
         :initform (error "Provide a name for the element"))
   (attributes :initarg :attributes
               :accessor attributes
               :initform nil))
  (:documentation "Serializer intermediate representation element class"))

(defclass attribute ()
  ((name :initarg :name
         :accessor name
         :initform (error "Provide the attribute name"))
   (value :initarg :value
          :accessor value
          :initform (error "Provide the attribute value"))
   (type :initarg :type
	 :accessor attr-type
	 :initform nil
	 :documentation "The attribute type")
   (formatter :initarg :formatter
	      :accessor attribute-formatter
	      :initform nil
	      :documentation "Attribute formatter"))   
  (:documentation "Serializer intermediate representation element attribute class"))

(defclass elements-list ()
  ((name :initarg :name
         :accessor name
         :initform (error "Provide the list name"))
   (elements :initarg :elements
             :accessor list-elements
             :initform nil))
  (:documentation "Serializer intermediate representation list of elements class"))

(defun element (name &rest attributes)
  "Build an element to be serialized"
  (make-instance 'element
                 :name name
                 :attributes attributes))

(defun attribute (name value &optional type formatter)
  "Build an element attribute to be serialized"
  (make-instance 'attribute
                 :name name
                 :value value
		 :type type
		 :formatter formatter))

(defun elements (name &rest elements)
  "Build a list of elements to be serialized"
  (make-instance 'elements-list
                 :name name
                 :elements elements))

(defmethod serialize-toplevel ((serializer t) stream function)
    (funcall function))

(defmethod serialize-toplevel ((serializer (eql :xml)) stream function)
  (cxml:with-xml-output (cxml:make-character-stream-sink
			 stream
			 :indentation nil
			 :omit-xml-declaration-p t)
    (funcall function)))

(defmethod serialize-toplevel ((serialize (eql :html)) stream function)
  (let ((cl-who::*html-mode* :html5))
    (format stream "<!DOCTYPE html>")
    (cl-who:with-html-output (html stream :prologue nil)
      (funcall function))))

;; Serializer format plug

(defgeneric serialize (element &optional serializer stream &rest args)
  (:documentation "Main serialization function. Takes the element to serialize, the serializer and the output stream"))

(defmethod serialize ((element element) &optional (serializer *serializer*) (stream *serializer-output*) &rest args)
  (serialize-element serializer element stream))

(defmethod serialize ((elements-list elements-list) &optional (serializer *serializer*) (stream *serializer-output*) &rest args)
  (serialize-elements-list serializer elements-list stream))

(defmethod serialize ((attribute attribute) &optional (serializer *serializer*) (stream *serializer-output*) &rest args)
  (serialize-attribute serializer attribute stream))

(defmethod serialize ((value t) &optional (serializer *serializer*) (stream *serializer-output*) &rest args)
  (apply #'serialize-value serializer value stream args))

(defun boolean-value (boolean &optional (serializer *serializer*)
				(stream *serializer-output*))
  (serialize-value serializer boolean stream :type :boolean))

(defun list-value (list &optional (serializer *serializer*)
			  (stream *serializer-output*))
  (assert (listp list) nil "Should be a list")
  (serialize-value serializer list stream :type :list))

(defmethod serializer-content-type ((serializer (eql :html)))
  "text/html")

;; Json serializer

(defmethod serializer-content-type ((serializer (eql :json)))
  "application/json")

(defmethod serialize-element ((serializer (eql :json)) element stream)
  (json:with-object (stream)
    (loop for attribute in (attributes element)
         do
         (serialize attribute serializer stream))))

(defmethod serialize-elements-list ((serializer (eql :json)) elements-list stream)
  (json:with-array (stream)
    (loop for element in (list-elements elements-list)
         do
	 (json:as-array-member (stream)
	   (serialize element serializer stream)))))

(defmethod serialize-attribute ((serializer (eql :json)) attribute stream)
  (json:as-object-member ((name attribute) stream)
    (serialize (value attribute) serializer stream
	       :type (attr-type attribute)
	       :formatter (attribute-formatter attribute))))

(defmethod serialize-value ((serializer (eql :json)) value stream &key type formatter)
  (let ((formatted-value (or (and formatter (funcall formatter value))
			     value)))
    (case type
      (:list (json:with-array (stream)
	       (loop for elem in value
		    do
		    (json:as-array-member (stream)
		      (serialize elem serializer stream)))))
      (:boolean (if value
		    (json:encode-json t stream)
		    (json:encode-json :false stream)))
      (t (json:encode-json formatted-value stream)))))

;; XML serializer

(defmethod serializer-content-type ((serializer (eql :xml)))
  "application/xml")

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
    (serialize (value attribute) serializer stream
	       :type (attr-type attribute)
	       :formatter (attribute-formatter attribute))))

(defmethod serialize-value ((serializer (eql :xml)) value stream &key type formatter)
  (cxml:text (prin1-to-string value)))

;; SEXP serializer

(defmethod serializer-content-type ((serializer (eql :sexp)))
  "text/lisp")

(defmethod serialize-element ((serializer (eql :sexp)) element stream)
  (format stream "(~s (" (name element))
  (loop for attribute in (attributes element)
       do (serialize attribute serializer stream))
  (format stream "))"))

(defmethod serialize-elements-list ((serializer (eql :sexp)) elements-list stream)
  (format stream "(")
  (loop for element in (list-elements elements-list)
       do
       (serialize element serializer stream)
       (format stream " "))       
  (format stream ")"))

(defmethod serialize-attribute ((serializer (eql :sexp)) attribute stream)
  (format stream "(~S . " (name attribute))
  (serialize (value attribute) serializer stream
	     :type (attr-type attribute)
	     :formatter (attribute-formatter attribute))
  (format stream ")"))

(defmethod serialize-value ((serializer (eql :sexp)) value stream &key type formatter)
  (prin1 value stream))

;; HTML serializer

(defmethod serialize-element ((serializer (eql :html)) element stream)
  (cl-who:with-html-output (html stream)
    (:div :class "element"
          (:h1 (cl-who:str (name element)))
          (:div :class "attributes"
                (mapcar (lambda (attribute)
                          (cl-who:htm
                           (serialize attribute serializer stream)))
                  (attributes element))))))

(defmethod serialize-elements-list ((serializer (eql :html)) elements-list stream)
  (cl-who:with-html-output (html stream)
    ;(:ol :class "elements"
    (format stream "<ol class=\"elements\">")
    (loop for element in (list-elements elements-list)
       do
	 (cl-who:htm
	  (:li (serialize element serializer stream))))
    (format stream "</ol>")))

(defmethod serialize-attribute ((serializer (eql :html)) attribute stream)
  (cl-who:with-html-output (html stream)
    (:div :class "attribute-name"
          (cl-who:str (name attribute)))
    (:div :class "attribute-value"
          (cl-who:str (serialize (value attribute) serializer stream
				 :type (attr-type attribute)
				 :formatter (attribute-formatter attribute))))))

(defmethod serialize-value ((serializer (eql :html)) value stream &key type formatter)
  (cl-who:with-html-output (html stream)
    (cl-who:fmt "~A" value)))

;; Streaming api implementation

(defgeneric call-with-element (serializer name body stream)
  (:method (serializer name body stream)
    (error "Unknown serializer: ~A. If NIL, remember to wrap with with-serializer."
	   serializer)))

(defmethod call-with-element ((serializer (eql :json)) name body stream)
  (declare (ignore name))
  (json:with-object (stream)
    (funcall body)))

(defmethod call-with-element ((serializer (eql :xml)) name body stream)
  (declare (ignore stream))
  (cxml:with-element name
      (funcall body)))

(defmethod call-with-element ((serializer (eql :html)) name body stream)
  (cl-who:with-html-output (html stream)
    (:div :class "element"
          (:h1 (cl-who:str name))
          (:div :class "attributes"
		(funcall body)))))

(defmethod call-with-element ((serializer (eql :sexp)) name body stream)
  (format stream "(~s (" name)
  (funcall body)
  (format stream "))"))

(defmethod call-with-attribute ((serializer (eql :json)) name body stream)
  (json:as-object-member (name stream)
    (funcall body)))

(defmethod call-with-attribute ((serializer (eql :xml)) name body stream)
  (declare (ignore stream))
  (cxml:with-element name
    (funcall body)))

(defmethod call-with-attribute ((serializer (eql :html)) name body stream)
  (cl-who:with-html-output (html stream)
    (:div :class "attribute-name"
          (cl-who:str name))
    (:div :class "attribute-value"
	  (funcall body))))

(defmethod call-with-attribute ((serializer (eql :sexp)) name body stream)
  (format stream "(~S . " name)
  (funcall body)
  (format stream ")"))

(defun set-attribute (name value &rest args
		      &key
			(serializer *serializer*)
			(stream *serializer-output*) &allow-other-keys)
  "Serializes an element attribute and value"
  (with-attribute (name :serializer serializer
			:stream stream)
    (apply #'serialize value serializer stream args)))

(defmethod call-with-list ((serializer (eql :json)) name body stream)
  (declare (ignore name))
  (json:with-array (stream)
    (funcall body)))

(defmethod call-with-list ((serializer (eql :xml)) name body stream)
  (declare (ignore name stream))
  (funcall body))

(defmethod call-with-list ((serializer (eql :html)) name body stream)
  (cl-who:with-html-output (html stream)
    (:ol (funcall body))))

(defmethod call-with-list ((serializer (eql :sexp)) name body stream)
  (format stream "(")
  (funcall body)
  (format stream ")"))

(defmethod call-with-list-member ((serializer (eql :json)) name body stream)
  (declare (ignore name))
  (json:as-array-member (stream)
    (funcall body)))

(defmethod call-with-list-member ((serializer (eql :xml)) name body stream)
  (with-element (name :serializer serializer
		      :stream stream)
    (funcall body)))

(defmethod call-with-list-member ((serializer (eql :html)) name body stream)
  (declare (ignore name))
  (cl-who:with-html-output (html stream)
    (:li 
     (funcall body))))

(defmethod call-with-list-member ((serializer (eql :sexp)) name body stream)
  (declare (ignore name))
  (funcall body)
  (format stream " "))

(defun add-list-member (name value &key (serializer *serializer*)
			(stream *serializer-output*))
  "Serializes a list member"
  (with-list-member (name :serializer serializer
				 :stream stream)
    (serialize value serializer stream)))

(defun accept-serializer ()
  (or (and (hunchentoot:header-in* "accept")
	   (let ((accepts (mimeparse:best-match
			   (list "text/lisp"
				 "text/xml"
				 "application/xml"
				 "text/html"
				 "application/json")
			   (hunchentoot:header-in* "accept"))))
	     (string-case:string-case (accepts :default *default-serializer*)
	       ("text/xml" :xml)
	       ("application/xml" :xml)
	       ("text/html" :html)
	       ("application/json" :json)
	       ("text/lisp" :lisp))))
      *default-serializer*))

;; Plugging

(defclass serialization-resource-operation-implementation-decoration
    (rs::resource-operation-implementation-decoration)
  ((streamed :initarg :streamed
	     :accessor streamed-p
	     :initform nil
	     :documentation "If the content is serialized with the streaming api"))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod rs::process-resource-operation-implementation-option
    ((option (eql :serialization))
     resource-operation-implementation
     &key enabled streamed)
  (if enabled
      (make-instance 'serialization-resource-operation-implementation-decoration
		     :decorates resource-operation-implementation
		     :streamed streamed)
      resource-operation-implementation))

(defmethod rs::execute :around ((decoration serialization-resource-operation-implementation-decoration)
			    &rest args)
  (let ((serializer (accept-serializer)))
    (set-reply-content-type (serializer-content-type serializer))
    (with-output-to-string (s)
      (with-serializer-output s
	(with-serializer serializer
	  (if (streamed-p decoration)
	      (call-next-method)
	      (serialize
	       (call-next-method)
	       *serializer*
	       s)))))))

(cl-annot:defannotation serialization (args resource-operation-implementation)
    (:arity 2)
  `(rs::configure-resource-operation-implementation
    (rs::name (rs::resource-operation ,resource-operation-implementation))
    (list :serialization ,@args)))
