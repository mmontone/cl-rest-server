(in-package :rest-server-tests)

(in-suite rest-server-tests)

;; OAuth2 API tests

(defpackage :oauth2-test
  (:use :rest-server 
	:rest-server.schema
	:rest-server.serialize
	:rest-server.logging
	:rest-server.mop
	:cl))

(defparameter *resource-server-id* "a7c87ae0-dabe-4b8a-8a45-ee9708696794")
(defparameter *resource-server-secret* "443c188d-60ce-40e2-b268-69c0c78ad055")
(defparameter *oauth2-server-url* "http://localhost:8080")
(defparameter *client-id* "rest-sever-user-client")
(defparameter *client-secret* "d869c320-e8a0-4bc2-aebb-5feb0035be08")

(in-package :oauth2-test)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (define-api oauth2-api-test
      (:title "Api test"
	      :documentation "This is an api test")
    (users (:produces (:json :xml)
		      :consumes (:json)
		      :documentation "Users operations"
		      :path "/users"
		      :authorizations (:token (:oauth2 :scopes (:user))))
	   (get-users (:request-method :get
				       :produces (:json)
				       :path "/users"
				       :documentation "Retrive the users list"
				       :authorizations (:token (:oauth2 :scopes (:user))))
		      (&optional (expand-groups :boolean nil "Expand groups if true")))
	   (get-user (:request-method :get
				      :produces (:json)
				      :path "/users/{id}"
				      :documentation "Retrive an user"
				      :authorizations (:token (:oauth2 :scopes (:user))))
		     ((id :integer "The user id")
		      &optional (expand-groups :boolean nil "Expand groups if true")))
	   (create-user (:request-method :post
					 :consumes (:json)
					 :path "/users"
					 :documentation "Create a user"
					 :authorizations (:token (:oauth2 :scopes (:user :admin))))
			())
	   (update-user (:request-method :put
					 :consumes (:json)
					 :path "/users/{id}"
					 :documentation "Update a user"
					 :authorizations (:token (:oauth2 :scope (:user :admin))))
			((id :integer "The user id")))
	   (delete-user (:request-method :delete
					 :consumes (:json)
					 :path "/users/{id}"
					 :documentation "Delete a user"
					 :authorizations (:token (:oauth2 :scopes (:profile :admin))))
			((id :integer "The user id"))))))

(implement-resource-operation oauth2-api-test
    (get-users
     (:logging :enabled t)
     (:error-handling :enabled t))
    (&key (expand-groups nil))
  (declare (ignore expand-groups))
  (with-output-to-string (s)
    (with-serializer-output s
      (with-serializer (rest-server.serialize::accept-serializer)
	(with-list ("users")
	   (loop for user in (model-test:all-users)
	      do
		(with-list-member ("user")
		  (with-element ("user")
		    (set-attribute "id" (cdr (assoc :id user)))
		    (set-attribute "realname" (cdr (assoc :realname user)))))))))))

(implement-resource-operation oauth2-api-test
    (get-user
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

(implement-resource-operation oauth2-api-test create-user (posted-content)
  (let ((user
	 (model-test:add-user (cdr (assoc :realname posted-content)))))
    (json:encode-json-alist-to-string user)))

(implement-resource-operation
    oauth2-api-test
    update-user (posted-content id)
    (let ((user (model-test::get-user id)))
      (if (not user)
	(error 'http-not-found-error)
	; else
	(progn
	  (model-test::set-user-realname user (cdr (assoc :realname posted-content)))
	  (model-test::update-user user)))))

(implement-resource-operation oauth2-api-test
    delete-user (id)
  (model-test::delete-user id))

(in-package :rest-server-tests)

(start-api 'oauth2-test::oauth2-api-test "localhost" 8183)

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
				  :port 8184))

(hunchentoot:define-easy-handler (login :uri "/login")
    ()
  (hunchentoot:redirect
   (format nil 
	   "~A/oauth2/authorize?response_type=code&client_id=~A&redirect_uri=~A&scope=user&state=test"
	   *oauth2-server-url*
	   *client-id*
	   (drakma:url-encode "http://localhost:8184/callback" :utf-8))))

(hunchentoot:define-easy-handler (callback :uri "/callback")
    (code error (error-description :real-name "error_description"))
  (if error
      (format nil"<h1>~A</h1>" error-description)
      (let ((tokens
	     (rest-server::exchange-authorization-code
	      code
	      "http://localhost:8184/callback"
	      *client-id*
	      *client-secret*)))
	(setf (hunchentoot:session-value 'tokens) tokens)
	(hunchentoot:redirect "http://localhost:8184"))))

(hunchentoot:define-easy-handler (root :uri "/")
    ()
  (when (not (hunchentoot:session-value 'tokens))
    (hunchentoot:redirect "http://localhost:8184/login"))
  (let* ((tokens (hunchentoot:session-value 'tokens))
	 (token
	  (format nil "~A ~A" 
		  (getf tokens :token-type)
		  (getf tokens :access-token))))
    (multiple-value-bind (result status-code)
	(drakma:http-request "http://localhost:8183/users"
			     :method :get
			     :additional-headers `(("Accept" .  "application/json")
						   ("Authentication" . ,token)))
      (with-output-to-string (s)
	(format s "Status: ~A" status-code)
	(format s "Result: ~A" result)))))

(test basic-oauth2-authentication-test
  ;; Unauthentication access doesn't work
  (multiple-value-bind (result status-code)
      (drakma:http-request "http://localhost:8183/users"
			   :method :get
			   :additional-headers '(("Accept" .  "application/json")))
    (is (equalp status-code 401)))

  ;; Invalid token access
  (multiple-value-bind (result status-code)
      (drakma:http-request "http://localhost:8183/users"
			   :method :get
			   :additional-headers '(("Accept" .  "application/json")
						 ("Authentication" . "Bearer abc123")))
    (is (equalp status-code 401))))
