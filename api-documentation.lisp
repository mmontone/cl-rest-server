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
  (loop for api-function being the hash-value of (functions (api acceptor))
     when (equalp (format nil "/~A" (name api-function)) (hunchentoot:request-uri*))
     return (with-documentation-toplevel (api acceptor)
              (document-api-function acceptor api-function))
     finally (return (api-toplevel-documentation acceptor))))

(defun api-toplevel-documentation (acceptor)
  (let ((api (api acceptor)))
    (with-documentation-toplevel api
      (:h1 :class "api-name" (cl-who:str (name api)))
      (:div :class "api-documentation"
            (cl-who:str (api-documentation api)))
      (:div :class "api-functions"
            (loop for api-function being the hash-value of (functions api)
                 do
                 (document-api-function acceptor api-function))))))
      
(defun document-api-function (acceptor api-function)
  (cl-who:with-html-output (*standard-output*)
    (cl-who:htm
   (:div :class "api-function"
         (:div :class "name"
               (:a :href (format nil "http://~A:~A/~A"
                                 (hunchentoot:acceptor-address acceptor)
                                 (hunchentoot:acceptor-port acceptor)
                                 (name api-function))
                   (cl-who:str (name api-function))))
         (:div :class "method"
               (cl-who:str (request-method api-function)))
         (:div :class "documentation"
               (cl-who:str (api-documentation api-function)))
         (:div :class "signature"
               (cl-who:str (uri-prefix api-function)))
         (:div :class "arguments"
               (:ul
                (loop for arg in (required-arguments api-function)
                   do
                   (cl-who:htm
                    (:li (cl-who:fmt "~a : ~a. ~a." (first arg) (second arg) (third arg)))))
                (loop for arg in (optional-arguments api-function)
                   do
                   (cl-who:htm
                    (:li (cl-who:fmt "~a : ~a. ~A. Default: ~a"
                                     (first arg)
                                     (second arg)
                                     (third arg)
                                     (nth 3 arg)))))))))))