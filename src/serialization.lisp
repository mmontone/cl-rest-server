(in-package :rest-server)

(defun accept-serializer ()
  (let ((content-type (rs::request-reply-content-type hunchentoot:*request*)))
    (or (and content-type
             (let ((accepts (mimeparse:best-match
                             (list "text/lisp"
                                   "text/xml"
                                   "application/xml"
                                   "text/html"
                                   "application/json")
                             content-type)))
               (string-case:string-case (accepts :default generic-serializer:*default-serializer*)
                 ("text/xml" :xml)
                 ("application/xml" :xml)
                 ("text/html" :html)
                 ("application/json" :json)
                 ("text/lisp" :sexp))))
        generic-serializer:*default-serializer*)))

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
  (declare (ignore args))
  (let ((serializer (accept-serializer)))
    (set-reply-content-type (generic-serializer::serializer-content-type serializer))
    (with-output-to-string (s)
      (generic-serializer:with-serializer-output s
        (generic-serializer:with-serializer serializer
          (if (streamed-p decoration)
              (call-next-method)
              (generic-serializer:serialize
               (call-next-method)
               generic-serializer::*serializer*
               s)))))))

(cl-annot:defannotation serialization (args resource-operation-implementation)
    (:arity 2)
  `(rs::configure-resource-operation-implementation
    (rs::name (rs::resource-operation ,resource-operation-implementation))
    (list :serialization ,@args)))
