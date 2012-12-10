(require 'asdf)
#+sbcl
(require :sb-posix)
(require :rest-server)

(defpackage :rest-server.doc
  (:use :cl))

(in-package :rest-server.doc)

(defparameter +references-dir-path+ 
  (merge-pathnames 
   #p"doc/references/"
   (asdf:component-pathname (asdf:find-system 'rest-server))))

(defparameter +include-references-path+ 
  (merge-pathnames 
   #p"doc/references.texinfo"
   (asdf:component-pathname (asdf:find-system 'rest-server))))

(defparameter +docstrings-path+
  (merge-pathnames 
   #p"doc/docstrings.lisp"
   (asdf:component-pathname (asdf:find-system 'rest-server))))

(defparameter +docs-path+
  (merge-pathnames 
   #p"doc/"
   (asdf:component-pathname (asdf:find-system 'rest-server))))

(defparameter +texinfo-file+
  (merge-pathnames
   #p"rest-server.texinfo"
   +docs-path+))

(defparameter +info-dir-path+
  #p"/usr/share/info/")

(sb-posix:chdir +references-dir-path+)
(load +docstrings-path+)

(sb-texinfo:generate-includes +references-dir-path+
			      (find-package 'rest-server))

(with-open-file (f +include-references-path+
		   :if-does-not-exist :create
		   :if-exists :supersede
		   :direction :output)
  (loop for filepath in (directory (merge-pathnames +references-dir-path+
					 "*.texinfo"))
       do
       (format f "@include ~A/~A~%"
	       (car (last (pathname-directory +references-dir-path+)))
	       (file-namestring filepath))))

(sb-posix:chdir +docs-path+)
(sb-ext:run-program "/usr/bin/texi2html" (list "--css-include=rest-server.css"
					       (format nil "~A" +texinfo-file+))
		    :wait t)
(sb-ext:run-program "/usr/bin/texi2pdf" (list (format nil "~A" +texinfo-file+)))
(sb-ext:run-program "/usr/bin/makeinfo" (list "--plaintext rest-server.texinfo -o rest-server.txt"))
(sb-ext:run-program "/usr/bin/makeinfo" (list "rest-server.texinfo"))
(sb-ext:run-program "/usr/bin/ginstall-info" (list (format nil "--info-dir=~A" +info-dir-path+)
					  "rest-server.info"))
(cl-user::quit)