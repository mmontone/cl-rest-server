(in-package :rest-server-tests)

(in-suite rest-server-tests)

;; API tests

(eval-when (:compile-toplevel :load-toplevel :execute)

  (define-api auth-api-test
      (:title "Api test"
	      :documentation "This is an api test")
    (authentication (:produces (:json)
			       :consumes (:json)
			       :documentation "Authentication operations"
			       :path "auth")
		    (login (:request-method :post
					    :produces (:json)
					    :consumes (:json)
					    :documentation "Login"
					    :path "/login")
			   ()))
    (users (:produces (:json :xml)
		      :consumes (:json)
		      :documentation "Users operations"
		      :path "/users"
		      :authorizations (api-test-token-authentication))
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

(defparameter *username* "test")
(defparameter *password* "test")

(defclass api-test-token-authentication
    (rest-server::token-authentication)
  ())

(defmethod rest-server::authenticate-token ((authentication api-test-token-authentication) token)
  (let ((decoded-token (rest-server::decode-token token)))
    (equalp decoded-token *username*)))

(implement-api-function auth-api-test
    (login
     (:error-handling :enabled t))
    (posted-content)
  (if (and (equalp (cdr (assoc :username posted-content))
		     *username*)
	     (equalp (cdr (assoc :password posted-content))
		     *password*))
	;; Return the token
	(rest-server::encode-string (format nil "~S" *username*))
	;; else, authentication error
	(error 'rest-server::http-authorization-required-error
	       :format-control "Authentication error")))
  
(implement-api-function auth-api-test
    (get-users
     (:logging :enabled t)
     (:error-handling :enabled t))
    (&key (expand-groups nil))
  (declare (ignore expand-groups))
  (with-output-to-string (s)
    (with-serializer-output s
      (with-serializer (rest-server::accept-serializer)
	(with-list ("users")
	   (loop for user in (model-test:all-users)
	      do
		(with-list-member ("user")
		  (with-element ("user")
		    (set-attribute "id" (cdr (assoc :id user)))
		    (set-attribute "realname" (cdr (assoc :realname user)))))))))))

(implement-api-function auth-api-test
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

(implement-api-function auth-api-test
    create-user
    (posted-content)
  (let ((user
	 (model-test:add-user (cdr (assoc :realname posted-content)))))
    (json:encode-json-alist-to-string user)))

(implement-api-function auth-api-test
    update-user
    (posted-content id)
    (let ((user (model-test::get-user id)))
      (if (not user)
	(error 'http-not-found-error)
	; else
	(progn
	  (model-test::set-user-realname user (cdr (assoc :realname posted-content)))
	  (model-test::update-user user)))))

(implement-api-function auth-api-test
    delete-user (id)
  (model-test::delete-user id))

(start-api 'auth-api-test "localhost" 8182)

(test basic-token-authentication-test
  ;; Unauthentication access doesn't work
  (multiple-value-bind (result status-code)
      (drakma:http-request "http://localhost:8182/users"
			   :method :get
			   :additional-headers '(("Accept" .  "application/json")))
    (is (equalp status-code 401)))

  ;; Failed login
  (multiple-value-bind (token status-code)
      (drakma:http-request "http://localhost:8182/login"
			   :method :post
			   :content-type "application/json"
			   :content (json:encode-json-plist-to-string
				     (list :username "test"
					   :password "bad"))
			   :additional-headers '(("Accept" .  "application/json")))
    (is (equalp status-code 401)))

    ;; Invalid token access
  (multiple-value-bind (result status-code)
      (drakma:http-request "http://localhost:8182/users"
			   :method :get
			   :additional-headers '(("Accept" .  "application/json")
						 ("Authentication" . "Invalid token")))
    (is (equalp status-code 401)))
  
  ;; Successful login
  (multiple-value-bind (token status-code)
      (drakma:http-request "http://localhost:8182/login"
			   :method :post
			   :content-type "application/json"
			   :content (json:encode-json-plist-to-string
				     (list :username "test"
					   :password "test"))
			   :additional-headers '(("Accept" .  "application/json")))
    (is (stringp token))
    (is (equalp status-code 200))

    ;; Valid token access
    (multiple-value-bind (result status-code)
      (drakma:http-request "http://localhost:8182/users"
			   :method :get
			   :additional-headers `(("Accept" .  "application/json")
						 ("Authentication" . ,token)))
      (finishes (json:decode-json-from-string result))
      (is (equalp status-code 200)))))
