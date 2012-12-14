(in-package :rest-server)

(log5:defcategory rest-server)

(defun make-keyword (string)
  (intern (string-upcase string) :keyword))