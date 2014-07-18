(in-package :rest-server-tests)

(in-suite rest-server-tests)

;; API tests

(defpackage :api-test
  (:use :rest-server :cl))

(in-package :api-test)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (define-api api-test
      (:title "Api test"
	      :documentation "This is an api test")
    (users (:produces (:json :xml)
		      :consumes (:json)
		      :documentation "Users operations"
		      :path "/users")
	   (get-users (:request-method :get
				       :produces (:json)
				       :path "/users"
				       :documentation "Retrive the users list")       
		      (&optional (expand-groups :boolean nil "Expand groups if true")))
	   (get-user (:request-method :get
				      :produces (:json)
				      :path "/users/{id}"
				      :documentation "Retrive an user")
		     ((id :integer "The user id")
		      &optional (expand-groups :boolean nil "Expand groups if true")))
	   (create-user (:request-method :post
					 :consumes (:json)
					 :path "/users"
					 :documentation "Create a user")
			())
	   (update-user (:request-method :put
					 :consumes (:json)
					 :path "/users/{id}"
					 :documentation "Update a user")
			((id :integer "The user id")))
	   (delete-user (:request-method :delete
					 :consumes (:json)
					 :path "/users/{id}"
					 :documentation "Delete a user")
			((id :integer "The user id"))))))

;; Add Swagger resource
(rest-server::define-swagger-resource api-test)

(defpackage :model-test
  (:use :cl)
  (:export :get-user
	   :all-users
	   :add-user
	   :update-user))

(in-package :model-test)

(defparameter *user-id* 1)
(defvar *users* nil)

(defun make-user (id realname)
  (list (cons :id id)
	(cons :realname realname)))

(defun user-id (user)
  (cdr (assoc :id user)))

(defun user-realname (user)
  (cdr (assoc :realname user)))

(defun set-user-realname (user realname)
  (setf (cdr (assoc :realname user)) realname))

(defun add-user (realname)
  (let ((user (make-user (incf *user-id*)
		   realname)))
  (push user
	*users*)
  user))

(defun update-user (user)
  (delete-user (user-id user))
  (push user *users*))

(defun get-user (id)
  (find id *users* :key (lambda (user)
			  (cdr (assoc :id user)))))

(defun delete-user (id)
  (setf *users* (delete id *users* :test #'equalp :key #'first)))

(defun all-users ()
  *users*)

(defpackage :api-test-implementation
  (:use :cl :rest-server))

(in-package :api-test-implementation)

(implement-api-function api-test::api-test
    (api-test::get-users
     (:logging :enabled t)
     (:error-handling :enabled t))
    (&key (expand-groups nil))
  (declare (ignore expand-groups))
  (let ((serializer (rest-server:accept-serializer)))
    (set-reply-content-type (rest-server::serializer-content-type serializer))
    (with-output-to-string (s)
      (with-serializer-output s
	(with-serializer serializer
	  (with-elements-list ("users")
	    (loop for user in (model-test:all-users)
	       do
		 (with-list-member ("user")
		   (with-element ("user")
		     (set-attribute "id" (cdr (assoc :id user)))
		     (set-attribute "realname" (cdr (assoc :realname user))))))))))))

(implement-api-function api-test::api-test
    (api-test::get-user
     (:serialization :enabled t)
     (:logging :enabled nil))
    (id &key (expand-groups nil))
  (declare (ignore expand-groups))
  (let ((user (model-test:get-user id)))
    (if (not user)
	(error 'http-not-found-error)
	; else
	(element "user"
		 (attribute "id" (cdr (assoc :id user)))
		 (attribute "realname" (cdr (assoc :realname user)))))))

(implement-api-function api-test::api-test api-test::create-user (posted-content)
  (let ((user
	 (model-test:add-user (cdr (assoc :realname posted-content)))))
    (with-json-reply
      (json:encode-json-alist-to-string user))))

(implement-api-function
    api-test::api-test
    api-test::update-user (posted-content id)
    (let ((user (model-test::get-user id)))
      (if (not user)
	(error 'http-not-found-error)
	; else
	(progn
	  (model-test::set-user-realname user (cdr (assoc :realname posted-content)))
	  (model-test::update-user user)))))

(implement-api-function api-test::api-test
    api-test::delete-user (id)
  (model-test::delete-user id))

(in-package :rest-server-tests)

(defparameter *api-acceptor*
  (start-api 'api-test::api-test "localhost" 8181))

(push (cons "application" "json") drakma:*text-content-types*)
(push (cons "application" "xml") drakma:*text-content-types*)

(test api-post-test
  (drakma:http-request
   "http://localhost:8181/users"
   :method :post
   :content-type "application/json"
   :accept "application/json"
   :content (json:encode-json-plist-to-string '(:realname "user1")))
  (drakma:http-request
   "http://localhost:8181/users"
   :method :post
   :content-type "application/json"
   :accept "application/json"
   :content (json:encode-json-plist-to-string '(:realname "user2")))
  (drakma:http-request
   "http://localhost:8181/users"
   :method :post
   :content-type "application/json"
   :accept "application/json"
   :content (json:encode-json-plist-to-string '(:realname "user3"))))   
  
(test api-setup-test
  (multiple-value-bind (result status-code headers)
      (drakma:http-request "http://localhost:8181/users" :method :get)
    (is (equalp status-code 200))
    (is (equalp (cdr (assoc :content-type headers)) "application/json"))
    (let ((users (json:decode-json-from-string result)))
      (is (alexandria:set-equal
	   (mapcar (lambda (user)
		     (cdr (assoc :realname user)))
		   users)
	   (list "user1" "user2" "user3")
	   :test #'equalp)))))

#+nil(test basic-parameters-test
  (multiple-value-bind (result status-code)
      (drakma:http-request "http://localhost:8181/users?expand-groups=true" :method :get)
    (is (equalp status-code 200))
    (is (equalp (read-from-string result) (list "user1" "user2" "user3")))))

#+nil(test boolean-parameters-test
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
  (let ((development-mode (rest-server::development-mode *api-acceptor*)))
    (setf (rest-server::development-mode *api-acceptor*) :production)
    (multiple-value-bind (result status-code)
	(drakma:http-request "http://localhost:8181/users?expand-groups=foo" :method :get)
      (declare (ignore result))
      (is (equalp status-code 500)))
    (setf (rest-server::development-mode *api-acceptor*) :testing)
    (multiple-value-bind (result status-code)
	(drakma:http-request "http://localhost:8181/users?expand-groups=foo" :method :get)
      (is (equalp status-code 200))
      (let ((condition (json:decode-json-from-string result)))
	(is (equalp (cdr (assoc :condition condition)) "simpleError"))))
    ;; We can not test development mode like this. We are in a different thread.
    #+nil(setf *development-mode* :development)
    #+nil(signals simple-error
	   (drakma:http-request "http://localhost:8181/users?expand-groups=foo" :method :get))
    (setf (rest-server::development-mode *api-acceptor*) development-mode)))

(test accept-content-test
  (multiple-value-bind (result status headers)
      (drakma:http-request "http://localhost:8181/users/2"
			   :method :get
			   :additional-headers '(("Accept" . "application/json")))
    (finishes (json:decode-json-from-string result))
    (is (equalp (cdr (assoc :content-type headers)) "application/json")))
  (multiple-value-bind (result status headers)
      (drakma:http-request "http://localhost:8181/users/2"
			   :method :get
			   :additional-headers '(("Accept" . "application/xml")))
    (finishes (cxml:parse result (cxml-xmls:make-xmls-builder)))
    (is (equalp (cdr (assoc :content-type headers)) "application/xml")))
  (multiple-value-bind (result status headers)
      (drakma:http-request "http://localhost:8181/users/2"
			   :method :get
			   :additional-headers '(("Accept" . "text/html")))
    (multiple-value-bind (html error)
	(html5-parser:parse-html5 result :strictp t)
      (is (null error)))
    (is (equalp (cdr (assoc :content-type headers)) "text/html")))
  (let ((result
	 (drakma:http-request "http://localhost:8181/users/22"
			      :method :get
			      :additional-headers '(("Accept" . "text/lisp")))))
    (finishes (read-from-string result))))

(defparameter *api-url* "http://localhost:8181")

(test client-api-access-test
  ;; Create a new user
  (multiple-value-bind (result status)
      (with-api-backend *api-url*
	(api-test::create-user
	 (json:encode-json-plist-to-string
	  (list :realname "Felipe"))))
    (is (equalp status 200))
    (let ((created-user (json:decode-json-from-string result)))
      ;; Retrieve the list of users
      (multiple-value-bind (users status)
	  (with-api-backend *api-url*
	    (api-test::get-users))
	(finishes (json:decode-json-from-string users))
	(equalp status 200))
      ;; Fetch the created user
      (multiple-value-bind (result status)
	  (with-api-backend *api-url*
	    (api-test::get-user (cdr (assoc :id created-user))))
	(finishes (json:decode-json-from-string result))
	(is (equalp status 200)))
      ;; Fetch an unexisting user
      (multiple-value-bind (result status)
	 (with-api-backend *api-url*
	   (api-test::get-user 123456))
	(is (equalp status 404))))))

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
