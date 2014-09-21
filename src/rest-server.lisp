(in-package :rest-server)

(defparameter *api-function* nil "The current api function")

(log5:defcategory rest-server)

(defun make-keyword (string)
  (intern (string-upcase string) :keyword))
