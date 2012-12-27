(defpackage :rest-server-tests
  (:use :cl :rest-server :fiveam)
  (:export :run-tests))

(in-package :rest-server-tests)

(def-suite rest-server-tests :description "rest-server system tests")

(defun run-tests ()
  (run 'rest-server-tests))

(in-suite rest-server-tests)

;; Serialization tests

(defparameter *element*
  (element "user"
	   (attribute "id" 22)
	   (attribute "realname" "Mike")
	   (attribute "groups"
		      (elements "groups"
				(element "group"
					 (attribute "id" 33)
					 (attribute "title" "My group"))))))

(test intermediate-representation-test
  (is (equalp (name *element*) "user"))
  (is (equalp (value (find "realname" (attributes *element*)  :key #'name :test #'equalp))
	      "Mike")))

(test json-serialization-test
  (let ((json-output
	 (with-output-to-string (s)
	   (with-serializer-output s
	     (with-serializer :json
	       (serialize *element*))))))
    (finishes (json:decode-json-from-string json-output))
    (let ((json (json:decode-json-from-string json-output)))
      (is (equalp (cdr (assoc :id json)) 22))
      (let ((groups (cdr (assoc :groups json))))
	(is (equalp (cdr (assoc :id (first groups)))
		    33))))))

(test xml-serialization-test
  (let ((xml-output
	 (with-output-to-string (s)
	   (cxml:with-xml-output (cxml:make-character-stream-sink s :indentation nil :omit-xml-declaration-p t)
	     (with-serializer-output s
	       (with-serializer :xml
		 (serialize *element*)))))))
    (finishes (cxml:parse xml-output (cxml-xmls:make-xmls-builder)))
    (let ((xml (cxml:parse xml-output (cxml-xmls:make-xmls-builder))))
      (destructuring-bind (element attributes &rest children)
	  xml
	(is (equalp element "user"))
	(is (null attributes))
	(let ((groups (find "groups" children :test #'equalp :key #'first)))
	  (destructuring-bind (element attributes &rest children)
	      groups
	    (declare (ignore attributes))
	    (is (equalp element "groups"))
	    (let ((title (find "title" children :test #'equalp :key #'first)))
	      (is (equalp (third title) "My group")))))))))

;; TODO: HTML and SEXP serialization tests

;; (with-output-to-string (s)
;;   (with-serializer-output s
;;     (with-serializer :xml
;;       (cxml:with-xml-output (cxml:make-character-stream-sink s :indentation nil :omit-xml-declaration-p t)
;;         (serialize *element*)))))

;; (with-serializer-output t
;;   (with-serializer :sexp
;;     (serialize *element*)))

;; Streaming api test

(defvar *streamed-element*
  (lambda ()
    (with-element ("user")
      (set-attribute "id" 22)
      (with-attribute ("realname")
	(serialize "Mike"))
      (with-attribute ("groups")
	(with-elements-list ("groups")
	  (with-element ("groups")
	    (set-attribute "id" 33)
	    (set-attribute "title" "My group")))))))

(test json-stream-serialization-test
  (let ((json-output
	 (with-output-to-string (s)
	   (with-serializer-output s
	     (with-serializer :json
	       (funcall *streamed-element*))))))
    (finishes (json:decode-json-from-string json-output))
    (let ((json (json:decode-json-from-string json-output)))
      (is (equalp (cdr (assoc :id json)) 22))
      (let ((groups (cdr (assoc :groups json))))
	(is (equalp (cdr (assoc :id (first groups)))
		    33))))))

(test xml-stream-serialization-test
  (let ((xml-output
	 (with-output-to-string (s)
	   (with-serializer-output s
	     (with-serializer :xml
	       (cxml:with-xml-output (cxml:make-character-stream-sink s :indentation nil :omit-xml-declaration-p t)
		 (funcall *streamed-element*)))))))
    (finishes (cxml:parse xml-output (cxml-xmls:make-xmls-builder)))
    (let ((xml (cxml:parse xml-output (cxml-xmls:make-xmls-builder))))
      (destructuring-bind (element attributes &rest children)
	  xml
	(is (equalp element "user"))
	(is (null attributes))
	(let ((groups (find "groups" children :test #'equalp :key #'first)))
	  (destructuring-bind (element attributes &rest children)
	      groups
	    (declare (ignore attributes))
	    (is (equalp element "groups"))
	    (let ((title (find "title" children :test #'equalp :key #'first)))
	      (is (equalp (third title) "My group")))))))))

;; TODO: HTML and SEXP streaming serialization tests

;; (with-serializer-output t
;;   (with-serializer :html
;;     (with-element ("user")
;;       (set-attribute "realname" "Hola"))))

;; (with-serializer-output t
;;   (with-serializer :sexp
;;     (with-element ("user")
;;       (set-attribute "realname" "Hola"))))

;; API tests

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

(defpackage :model-test
  (:use :cl)
  (:export :get-user
	   :all-users
	   :add-user
	   :update-user))

(in-package :model-test)

(defvar *users* nil)

(defun make-user (id realname)
  (list (cons :id id)
	(cons :realname realname)))

(defun add-user (user)
  (push (cons (cdr (assoc :id user))
	      user)
	*users*))

(defun update-user (user)
  (let ((user-id (cdr (assoc :id user))))
    (delete-user user-id)
    (add-user user)))

(defun get-user (id)
  (cdr (assoc id *users*)))

(defun delete-user (id)
  (setf *users* (delete id *users* :test #'equalp :key #'first)))

(defun all-users ()
  (mapcar #'cdr *users*))

(defpackage :api-test-implementation
  (:use :cl :rest-server))

(in-package :api-test-implementation)

(implement-api-function get-users (&key (expand-groups nil))
  (declare (ignore expand-groups))
  (with-output-to-string (s)
    (with-serializer-output s
      (with-serializer (rest-server::accept-serializer)
	(with-elements-list ("users")
	  (loop for user in (model-test:all-users)
	     do
	       (with-element ("user")
		 (set-attribute "id" (cdr (assoc :id user)))
		 (set-attribute "realname" (cdr (assoc :realname user))))))))))       

(implement-api-function (get-user :serialization t)
    (id &key (expand-groups nil))
  (declare (ignore expand-groups))
  (let ((user (model-test:get-user id)))
    (if (not user)
	(error 'http-not-found-error)
	; else
	(element "user"
		 (attribute "id" (cdr (assoc :id user)))
		 (attribute "realname" (cdr (assoc :realname user)))))))

(defun create-user (posted-content)
  (format nil "Create user: ~A" posted-content))

(defun update-user (posted-content id)
  (format nil "Update user: ~A ~A" id posted-content))

(defun delete-user (id)
  (format nil "Delete user: ~A" id))

(in-package :rest-server-tests)

(start-api 'api-test::api-test "localhost" 8181 (find-package :api-test-implementation))

(test api-setup-test
  (multiple-value-bind (result status-code)
      (drakma:http-request "http://localhost:8181/users" :method :get)
    (is (equalp status-code 200))
    (is (equalp (read-from-string result) (list "user1" "user2" "user3" nil)))))

(test basic-parameters-test
  (multiple-value-bind (result status-code)
      (drakma:http-request "http://localhost:8181/users?expand-groups=true" :method :get)
    (is (equalp status-code 200))
    (is (equalp (read-from-string result) (list "user1" "user2" "user3" t)))))

(test boolean-parameters-test
  (setf *development-mode* :production)
  (multiple-value-bind (result status-code)
      (drakma:http-request "http://localhost:8181/users?expand-groups=foo" :method :get)
    (declare (ignore result))
    (is (equalp status-code 500)))
  (multiple-value-bind (result status-code)
      (drakma:http-request "http://localhost:8181/users?expand-groups=true" :method :get)
    (is (equalp status-code 200))
    (is (equalp (read-from-string result) (list "user1" "user2" "user3" t)))))

(test error-handling-test
  (setf *development-mode* :production)
  (multiple-value-bind (result status-code)
      (drakma:http-request "http://localhost:8181/users?expand-groups=foo" :method :get)
    (declare (ignore result))
    (is (equalp status-code 500)))
  (setf *development-mode* :testing)
  (multiple-value-bind (result status-code)
      (drakma:http-request "http://localhost:8181/users?expand-groups=foo" :method :get)
    (is (equalp status-code 200))
    (let ((condition (json:decode-json-from-string result)))
      (is (equalp (cdr (assoc :condition condition)) "simpleError"))))
  ;; We can not test development mode like this. We are in a different thread.
  #+nil(setf *development-mode* :development)
  #+nil(signals simple-error
      (drakma:http-request "http://localhost:8181/users?expand-groups=foo" :method :get))
  )

(test accept-content-test
  (let ((result
	 (drakma:http-request "http://localhost:8181/users/22"
			      :method :get
			      :additional-headers '(("Accept" .  "application/json")))))
    (finishes (json:decode-json-from-string result)))
  (let ((result
	 (drakma:http-request "http://localhost:8181/users/22"
			      :method :get
			      :additional-headers '(("Accept" . "application/xml")))))
    (finishes (cxml:parse result (cxml-xmls:make-xmls-builder))))
  (let ((result
	 (drakma:http-request "http://localhost:8181/users/22"
			      :method :get
			      :additional-headers '(("Accept" . "text/html")))))
    (multiple-value-bind (html error)
	(html5-parser:parse-html5 result :strictp t)
      (is (null error))))
  (let ((result
	 (drakma:http-request "http://localhost:8181/users/22"
			      :method :get
			      :additional-headers '(("Accept" . "text/lisp")))))
    (finishes (read-from-string result))))

(test client-api-access-test
  (let ((response
	 (with-api-backend "http://localhost:8181"
	   (api-test::get-user 345 :expand-groups t))))
    (finishes (json:decode-json-from-string response)))
  (let ((response
	 (with-api-backend "http://localhost:8181"
	   (api-test::get-user 22))))
    (finishes (json:decode-json-from-string response))))

(test content-type-test
  "Specify the content type we are sending explicitly"
  (setf *development-mode* :production)
  (multiple-value-bind (result status-code)
      (drakma:http-request "http://localhost:8181/users" :method :post
			   :content "<user><realname>Mariano</realname></user>"
			   :content-type "text/xml"
			   :additional-headers '(("Accept" . "text/lisp")))
    (declare (ignore result))
    (is (equalp status-code 200))
    (drakma:http-request "http://localhost:8181/users/" :method :post
			   :content "<user><realname>Mariano</realname></user>"
			   :content-type "text/xml")
    )
  (multiple-value-bind (result status-code)
      (drakma:http-request "http://localhost:8181/users?expand-groups=true" :method :get)
    (is (equalp status-code 200))
    (is (equalp (read-from-string result) (list "user1" "user2" "user3" t)))))

(in-package :rest-server-tests)

(defparameter *schema* 
  (schema
   (:element user
    ((id :integer)
     (realname :string)
     (age :integer)
     (best-friend (:element user 
			    ((id :integer)
			     (realname :string))))
     (groups (:list (:element group
		     ((id :integer)
		      (name :string))))
	     :optional t)))))

(define-schema user-schema
    (:element user
     ((identity :integer :accessor id)
      (realname :string)
      (age :integer)
      (best-friend user-schema
		   :optional t)
      (groups (:list group-schema)
	      :optional t
	      :switch :include-user-groups))))

(define-schema minimal-user-schema
    (:element user
     ((id :integer)
      (realname :string))))

(define-schema group-schema
    (:element group
     ((id :integer)
      (name :string)
      (users (:list user-schema) 
	     :optional t
	     :switch :include-group-users))))

(defclass user ()
  ((id :initarg :id
       :accessor id
       :initform (error "Provide the id"))
   (realname :initarg :realname
	     :accessor realname
	     :initform (error "Provide the realname"))
   (age :initarg :age
	:accessor age
	:initform (error "Provide the age"))
   (groups :initarg :groups
	   :accessor groups
	   :initform nil)
   (best-friend :initarg :best-friend
		:accessor best-friend
		:initform nil)))		

(defclass group ()
  ((id :initarg :id
       :accessor id
       :initform (error "Provide the id"))
   (name :initarg :name
	 :accessor name
	 :initform (error "Provide the name"))
   (users :initarg :users
	  :accessor users
	  :initform nil)))

(defparameter *user* 
  (make-instance 'user
		 :realname "Mariano"
		 :id 2
		 :age 30
		 :groups (list (make-instance 'group
					      :name "My group"
					      :id 3))
		 :best-friend (make-instance 'user 
					     :id 3
					     :realname "Fernando"
					     :age 31
					     )))

(with-output-to-string (s)
  (with-serializer-output s
    (with-serializer :json
      (serialize-with-schema 
       *schema* *user*))))

(with-output-to-string (s)
  (with-serializer-output s
    (with-serializer :json
      (serialize-with-schema 
       (find-schema 'user-schema) *user*))))

(with-output-to-string (s)
  (with-serializer-output s
    (with-serializer :json
      (serialize-with-schema 
       (find-schema 'minimal-user-schema) *user*))))

(with-output-to-string (s)
  (with-serializer-output s
    (with-serializer :xml
      (serialize-with-schema 
       *schema* *user*))))

(with-output-to-string (s)
  (with-serializer-output s
    (with-serializer :xml
      (serialize-with-schema 
       (find-schema 'user-schema) *user*))))

(with-output-to-string (s)
  (with-serializer-output s
    (with-serializer :xml
      (serialize-with-schema 
       (find-schema 'minimal-user-schema) *user*))))

(test parse-api-input-test
  (let ((input-1 "{\"id\":2,\"realname\":\"Mariano\",\"age\":30,\"bestFriend\":{\"id\":3,\"realname\":\"Fernando\"},\"groups\":[{\"id\":3,\"name\":\"My group\"}]}")
	(input-2 "<user><id>2</id><realname>Mariano</realname><age>30</age><best-friend><id>3</id><realname>Fernando</realname></best-friend><groups><group><id>3</id><name>My group</name></group></groups></user>")
	(input-3 "(user ((id . 2) (realname . \"Mariano\") (age . 30) (best-friend . ((id . 3) (realname . \"Fernando\"))) (groups . ((group ((id . 3) (name . \"My group\")))))))"))
  (let ((parsed-input-1 (rest-server::parse-api-input :json input-1))
	(parsed-input-2 (rest-server::parse-api-input :xml input-2))
	(parsed-input-3 (rest-server::parse-api-input :sexp input-3)))
    (is (and
	 (equalp (prin1-to-string parsed-input-1)
		 (prin1-to-string parsed-input-2))
	 (equalp (prin1-to-string parsed-input-2)
		 (prin1-to-string parsed-input-3)))))))