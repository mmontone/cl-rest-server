(in-package :cl-user)

(defpackage rest-server-system
  (:use :cl :asdf))
  
(in-package :rest-server-system)

(defsystem rest-server
  :name "rest-server"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :version "0.1"
  :maintainer "Mariano Montone <marianomontone@gmail.com>"
  :licence "
Copyright (c) 2012 Mariano Montone

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the \"Software\"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE."
  :description "REST APIs servers in Common Lisp."
  :long-description "REST APIs servers in Common Lisp"
  :components
  ((:module :src
	    :components
	    ((:file "package")
	     (:file "rest-server")
	     (:file "serialization")
	     (:file "mop")
	     (:file "xml")
	     (:file "schema")
	     (:file "error-handling")
	     (:file "authentication")
	     (:file "oauth2")
	     (:file "api")
	     (:file "resource")
	     (:file "api-function")
	     (:file "logging")
	     (:file "swagger")
	     (:file "api-documentation"))
	    :serial t))
  :serial t
  :depends-on (:hunchentoot 
	       :alexandria 
	       :log5 
	       :cl-json 
	       :cxml 
	       :local-time 
	       :split-sequence 
	       :drakma
	       :method-combination-utilities
               :cl-who
	       :ironclad
	       :babel
	       :closer-mop
	       :group-by
	       :anvil-connect
	       :chronicity
	       :net-telent-date))
