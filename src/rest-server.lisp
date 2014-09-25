(in-package :rest-server)

(defparameter *resource-operation* nil "The current resource operation")

(log5:defcategory rest-server)

(defun make-keyword (string)
  (intern (string-upcase string) :keyword))
