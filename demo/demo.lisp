(defpackage rest-server-demo.client
  (:use :cl))

(defpackage rest-server-demo
  (:use :cl :rest-server :generic-serializer)
  (:export :start-demo-api))

(in-package rest-server-demo)

(defun start-demo-api ()
  (rs:start-api 'users-demo-api :port 9090))
