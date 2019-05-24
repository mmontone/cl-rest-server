(in-package :rs.openapi)

(%define-api-from-spec 'petstore
    (asdf:system-relative-pathname :rest-server
                                   "test/petstore.v3.json"))
(define-api-from-spec petstore
    (asdf:system-relative-pathname :rest-server
                                   "test/petstore.v3.json"))

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
  (rs:start-api 'petstore "localhost" 3006))

(defun export-petstore-api ()
  (rs.openapi:export-api-spec (find-api 'petstore)))
