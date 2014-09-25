(in-package :rest-server)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-documentation-toplevel (api &body body)
    `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
       (:html
        (:head
         (:title (cl-who:str (name ,api)))
         (:style :media "screen" :type "text/css"
                 (cl-who:str "body {  }")))
        (:body
         ,@body)))))

(defclass api-documentation-acceptor (hunchentoot:acceptor)
  ((api :initarg :api
        :accessor api
        :initform (error "Provide the api")))
  (:documentation "Acceptor for api documentation application"))

(defun start-api-documentation (api address port)
  "Start a web documentation application on the given api."
  (hunchentoot:start
   (make-instance 'api-documentation-acceptor
                  :address address
		  :port port
                  :api (if (symbolp api)
                           (find-api api)
                           api))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor api-documentation-acceptor) request)
  (loop for resource-operation being the hash-value of (functions (api acceptor))
     when (equalp (format nil "/~A" (name resource-operation)) (hunchentoot:request-uri*))
     return (with-documentation-toplevel (api acceptor)
              (document-resource-operation acceptor resource-operation))
     finally (return (api-toplevel-documentation acceptor))))

(defun api-toplevel-documentation (acceptor)
  (let ((api (api acceptor)))
    (with-documentation-toplevel api
      (:h1 :class "api-name" (cl-who:str (name api)))
      (:div :class "api-documentation"
            (cl-who:str (api-documentation api)))
      (:div :class "resource-operations"
            (loop for resource-operation being the hash-value of (functions api)
                 do
                 (document-resource-operation acceptor resource-operation))))))
      
(defun document-resource-operation (acceptor resource-operation)
  (cl-who:with-html-output (*standard-output*)
    (cl-who:htm
   (:div :class "resource-operation"
         (:div :class "name"
               (:a :href (format nil "http://~A:~A/~A"
                                 (hunchentoot:acceptor-address acceptor)
                                 (hunchentoot:acceptor-port acceptor)
                                 (name resource-operation))
                   (cl-who:str (name resource-operation))))
         (:div :class "method"
               (cl-who:str (request-method resource-operation)))
         (:div :class "documentation"
               (cl-who:str (api-documentation resource-operation)))
         (:div :class "signature"
               (cl-who:str (path resource-operation)))
         (:div :class "arguments"
               (:ul
                (loop for arg in (required-arguments resource-operation)
                   do
                   (cl-who:htm
                    (:li (cl-who:fmt "~a : ~a. ~a." (first arg) (second arg) (third arg)))))
                (loop for arg in (optional-arguments resource-operation)
                   do
                   (cl-who:htm
                    (:li (cl-who:fmt "~a : ~a. ~A. Default: ~a"
                                     (first arg)
                                     (second arg)
                                     (third arg)
                                     (nth 3 arg)))))))))))
