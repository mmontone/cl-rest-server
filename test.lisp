(in-package :rest-server)

(defparameter *element*
  (element "user"
           (attribute "id" 22)
           (attribute "realname" "Mike")
           (attribute "groups"
                      (elements "groups"
                                (element "group"
                                         (attribute "id" 33)
                                         (attribute "title" "My group"))))))

(with-serializer-output t
  (with-serializer :json
    (serialize *element*)))

(with-output-to-string (s)
  (with-serializer-output s
    (with-serializer :json
      (serialize *element*))))

(cxml:with-xml-output (cxml:make-character-stream-sink t :indentation nil :omit-xml-declaration-p t)
  (with-serializer-output t
    (with-serializer :xml
      (serialize *element*))))

(with-output-to-string (s)
  (with-serializer-output s
    (with-serializer :xml
      (cxml:with-xml-output (cxml:make-character-stream-sink s :indentation nil :omit-xml-declaration-p t)
        (serialize *element*)))))

(with-serializer-output t
  (with-serializer :sexp
    (serialize *element*)))

(defpackage :api-test
  (:use :rest-server :cl))

(in-package :api-test)

(define-api api-test
  (:documentation "This is an api test"
   :content-types (list :json :xml))
  (get-users (:method :get
              :content-types (list :json)
              :uri-prefix "/users"
              :documentation "Retrive the users list")       
             (&optional (expand-groups :boolean nil "Expand groups if true")))
  (get-user (:method :get
             :content-types (list :json)
             :uri-prefix "/users/{id}"
             :documentation "Retrive an user")
            ((id :string "The user id")
             &optional (expand-groups :boolean nil "Expand groups if true")))
  (create-user (:method :post
                :content-types (list :json)
                :uri-prefix "/users"
                :documentation "Create a user")
               ())
  (update-user (:method :put
                 :content-types (list :json)
                 :uri-prefix "/users/{id}"
                 :documentation "Update a user")
               ((id :string "The user id")))
  (delete-user (:method :delete
                 :content-types (list :json)
                 :uri-prefix "/users/{id}"
                 :documentation "Delete a user")
               ((id :string "The user id"))))

(defpackage :api-test-implementation
  (:use :cl :rest-server))

(in-package :api-test-implementation)

(defun get-users (&key (expand-groups nil))
  (list "user1" "user2" "user3" expand-groups))

(implement-api-function (get-user :serialization t)
    (id &key (expand-groups nil))
  (declare (ignore expand-groups))
  (element "user"
	   (attribute "id" id)
           (attribute "groups"
                      (elements "groups"
                                (element "group"
                                         (attribute "id" 22)
                                         (attribute "name" "Group 1"))
                                (element "group"
                                         (attribute "id" 33)
                                         (attribute "name" "Group 2"))))))

(defun create-user (posted-content)
  (format nil "Create user: ~A" posted-content))

(defun update-user (posted-content id)
  (format nil "Update user: ~A ~A" id posted-content))

(defun delete-user (id)
  (format nil "Delete user: ~A" id))