(asdf:defsystem rest-server-openapi-demo
  :author "Mariano Montone <marianomontone@gmail.com>"
  :version "0.1"
  :maintainer "Mariano Montone <marianomontone@gmail.com>"
  :licence "MIT"
  :description "rest-server OpenAPI demo application"
  :long-description "rest-server OpenAPI demo application"
  :components
  ((:module :demo
    :components
    ((:module :openapi
      :components
      ((:file "openapi"))))))
  :depends-on (:rest-server))
