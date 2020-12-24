(in-package :rest-server)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-documentation-toplevel (api &body body)
    `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
       (:html
        (:head
         (:title (who:str (name ,api))))
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
  (loop for resource-operation in (resource-operations (api acceptor))
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
            (loop for resource-operation in (resource-operations api)
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
                        (:li (cl-who:fmt "~a : ~a. ~a" (rs::argument-name arg) (rs::argument-type arg) (argument-documentation arg))))
                       (loop for arg in (optional-arguments resource-operation)
                          do
                            (cl-who:htm
                             (:li (cl-who:fmt "~a : ~a. ~A. Default: ~a"
                                              (rs::argument-name arg)
                                              (rs::argument-type arg)
                                              (rs::argument-documentation arg)
                                              (rs::argument-default arg))))))))))))

;; API misin that displays a summary documentation of the api for the
;; indicated endpoints (default is root).

(defclass api-docs-mixin ()
  ((docs-path :accessor docs-path :initform "/"))
  (:documentation "Mixin for displaying API documentation at DOC-PATH"))

(defmethod api-dispatch-request :around ((api api-docs-mixin) request)
  (let* ((doc-types (list "text/html" "text/plain"))
         (content-type (mimeparse:best-match doc-types
                                             (hunchentoot:header-in* "accept"))))
    (if (and (member content-type doc-types :test 'string=)
             (ppcre:scan (parse-resource-path (docs-path api))
                         (request-uri request)))
        (print-api-docs api (parse-content-type content-type))
        (call-next-method))))

(defun print-api-docs (api content-type)
  (case content-type
    (:html (print-html-api-docs api))
    (t (print-text-api-docs api))))

(defvar *api-docs-html*)

(defun print-html-api-docs (api)
  (who:with-html-output-to-string (*api-docs-html*)
    (:html
     (:head
      (:title (who:fmt "~a api documentation" (title api)))
      (:style :media "screen" :type "text/css"
              (who:str "table.arguments th, td {
border: 1px solid black;
}")
              (who:str "th {
text-align:left;
}")
              (who:str ".resource {
border: 1px solid lightgray;
background-color: lightblue;
padding: 0px 10px;
margin-bottom: 30px;
box-shadow: 10px 10px 5px gray;
}")
              (who:str ".operation {
border: 1px solid lightgray;
background-color: lightyellow;
padding: 0px 10px;
margin-bottom: 20px;
box-shadow: 5px 5px 5px gray;
}")
              ))
     (:body
      (:h1 (who:str (title api)))
      (:p (who:str (api-documentation api)))
      (:h2 "Resources")
      (loop for resource being the hash-value in (resources api)
         do
           (print-resource-html resource *api-docs-html*))))))

(defun print-resource-html (resource html)
  (who:with-html-output (html)
    (:div :class "resource"
          (:h3 (who:str (resource-name resource)))
          (:p (who:str (resource-path resource)))
          (:p (who:str (resource-documentation resource)))
          (:h4 (who:str "Resource operations"))
          (loop for operation being the hash-value in (resource-operations resource)
             do (print-operation-html operation html)))))

(defun print-operation-html (operation html)
  (who:with-html-output (html)
    (:div :class "operation"
          (:h5 (who:str (name operation)))
          (:p (who:fmt "~a ~a"(request-method operation) (path operation)))
          (:p (who:str (api-documentation operation)))
          (:h6 "Arguments")
          (:table :class "arguments"
                  (:thead
                   (:tr
                    (:th (who:str "Name"))
                    (:th (who:str "Type"))
                    (:th (who:str "Required"))
                    (:th (who:str "Default"))
                    (:th (who:str "Description"))
                    (:tbody
                     (loop for arg in (append (required-arguments operation)
                                              (optional-arguments operation))
                        do (print-argument-html arg html)))))))))

(defun print-argument-html (arg html)
  (who:with-html-output (html)
    (:tr :class "arg"
         (:td (who:str (argument-name arg)))
         (:td (who:str (argument-type-spec (argument-type arg))))
         (:td (who:str (if (argument-required-p arg)
                           "True" "False")))
         (:td (when (argument-default arg)
                (who:fmt "~a" (argument-default arg))))
         (:td (who:str (argument-documentation arg)))
         )))

(defun print-text-api-docs (api)
  "TODO: print api text docs here")
