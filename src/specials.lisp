(in-package :rest-server)

(defvar *debug-mode* nil "If true, then error messages contain backtraces, etc")

(defvar *server-debug-mode*)

(defparameter *apis* (make-hash-table :test #'equalp)
  "Global hashtable containing the apis defined")

(defparameter *api* nil "The current api")

(defvar *rest-server-proxy* nil)

(defparameter *register-api-resource* t "Wether to register the created resource in the current API")
(defparameter *api-resource* nil "The current api resource")

(defvar *register-resource-operation* t
  "Whether to try to register the resource operation on creation. Bind to nil to prevent that")

(defparameter *text-content-types* (list :json :xml :lisp))

(defparameter *default-reply-content-type* "application/json")

(defparameter *extract-reply-content-type-from-url* nil "If true, extracts the request content-type from the url when available. For example: GET /users/1.json")
