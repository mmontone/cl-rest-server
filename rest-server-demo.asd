(defsystem rest-server-demo
  :author "Mariano Montone <marianomontone@gmail.com>"
  :version "0.1"
  :maintainer "Mariano Montone <marianomontone@gmail.com>"
  :licence "MIT"
  :description "rest-server demo application"
  :long-description "rest-server demo application"
  :components
  ((:module :demo
    :components
    ((:file "demo")
     (:file "schemas")
     (:file "api")
     (:file "model")
     (:file "implementation"))))
  :serial t
  :depends-on (:rest-server))
