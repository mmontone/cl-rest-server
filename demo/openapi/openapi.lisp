;; Just load this file, then evaluate (rest-server/demo/open-api:start-petstore-api) for a demo of how OpenAPI support works.
;; Test with the url: http://localhost:3006/pet/55

(defpackage :rest-server/demo/open-api
  (:use
   :cl
   :rest-server
   :generic-serializer
   :rs.openapi)
  (:export :start-petstore-api))

(in-package :rest-server/demo/open-api)

(define-schemas-from-spec
    #.(asdf:system-relative-pathname :rest-server
                                     "demo/openapi/petstore.v3.json"))
(define-api-from-spec petstore () ()
    #.(asdf:system-relative-pathname :rest-server
                                     "demo/openapi/petstore.v3.json"))

(rs:implement-resource-operation petstore
    get-pet-by-id (pet-id)
  (format nil "Pet with id: ~A" pet-id)
  )

(rs:implement-resource-operation petstore
    get-user-by-name (username)
  (format nil "User: ~A" username)
  )

(rs:implement-resource-operation petstore
    login-user (username password)
  (format nil "User login with ~A and ~A" username password))

(defun start-petstore-api ()
  (rs:start-api 'petstore :port 3006))

(defun export-petstore-api ()
  (rs.openapi:export-api-spec (find-api 'petstore)))
