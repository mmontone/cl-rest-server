(in-package :rs.openapi)

(%define-api-from-spec 'petstore
    (asdf:system-relative-pathname :rest-server
                                   "test/petstore.v3.json"))
(define-api-from-spec petstore
    (asdf:system-relative-pathname :rest-server
                                   "test/petstore.v3.json"))

(rs:implement-resource-operation petstore
    |getPetById| (|petId|)
  (format nil "Pet with id: ~A" |petId|)
  )

(rs:implement-resource-operation petstore
    |getUserByName| (username)
  (format nil "User: ~A" username)
  )

(rs:implement-resource-operation petstore
    |loginUser| (username password)
  (format nil "User login with ~A and ~A" username password))

(rs:start-api 'petstore "localhost" 3006)
    
