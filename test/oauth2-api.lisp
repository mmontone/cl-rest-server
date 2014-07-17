(in-package :rest-server-tests)

(in-suite rest-server-tests)

;; OAuth2 API tests

(defpackage :oauth2-test
  (:use :rest-server :cl))

;;(anvil-connect::register-client (list "http://localhost:8183"))

(defparameter *client-id* "2026b7c0-374d-4ba6-b841-0d1b1fcdf02d")
(defparameter *client-secret* "234eb2440ab2b6d9f03a")
(defparameter *client-access-token* "eyJhbGciOiJSUzI1NiJ9.eyJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjMwMDAiLCJzdWIiOiIyMDI2YjdjMC0zNzRkLTRiYTYtYjg0MS0wZDFiMWZjZGYwMmQiLCJhdWQiOiIyMDI2YjdjMC0zNzRkLTRiYTYtYjg0MS0wZDFiMWZjZGYwMmQiLCJpYXQiOjE0MDU1NDk3MDU4NzUsInNjb3BlIjoiY2xpZW50In0.RGE2a1pOTTJ6N0JROVBlbG1MTWJmYXBQekZsYjFhZXgzZE41d1NTbkZUQWhhcnd4ZmxjaURwRi1GcXJDRWh3V0Z1VVJlVmp6SmVDU25NZ3BmRWdlY2lNdnhUcUxXbjA2dFZyOTNEdGYtSHU2WEpwZlVudzVDMjlVQVRqZ0xycVh4YVVVall1U0t5cFVEYjM1U1hOSVNmdDkzRHdBc3ZUZDlPU2RIcVNwRlJfbEQzV1RuRHhIXzRrT3BkeU9sd0JkRzd5RWlfbjREbENpOHpnY0RjbTRwS2lkMVBwTnFjUjNRaHduNU5aUVFnc21RVlo0UGtRUHNYM2EtOHFlMmFkY1d3SENnemR2b1lGUG0zQ015bjhmTWU2bkVVa0g5aDc3cjNjMG1iREdkRWE4a0hlcWRHY1dXRVY4N3RfNHdjdldMVE1uSzVhNk5NQ1JpZFlXTDhlLXln")

(in-package :oauth2-test)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (define-api oauth2-api-test
      (:title "Api test"
	      :documentation "This is an api test")
    (users (:produces (:json :xml)
		      :consumes (:json)
		      :documentation "Users operations"
		      :path "/users"
		      :authorizations (:token (:oauth2 :scope (:profile :openid))))
	   (get-users (:request-method :get
				       :produces (:json)
				       :path "/users"
				       :documentation "Retrive the users list"
				       :authorizations (:token (:oauth2 :scope (:profile))))       
		      (&optional (expand-groups :boolean nil "Expand groups if true")))
	   (get-user (:request-method :get
				      :produces (:json)
				      :path "/users/{id}"
				      :documentation "Retrive an user"
				      :authorizations (:token (:oauth2 :scope (:profile))))
		     ((id :integer "The user id")
		      &optional (expand-groups :boolean nil "Expand groups if true")))
	   (create-user (:request-method :post
					 :consumes (:json)
					 :path "/users"
					 :documentation "Create a user"
					 :authorizations (:token (:oauth2 :scope (:profile :admin))))
			())
	   (update-user (:request-method :put
					 :consumes (:json)
					 :path "/users/{id}"
					 :documentation "Update a user"
					 :authorizations (:token (:oauth2 :scope (:profile :admin))))
			((id :integer "The user id")))
	   (delete-user (:request-method :delete
					 :consumes (:json)
					 :path "/users/{id}"
					 :documentation "Delete a user"
					 :authorizations (:token (:oauth2 :scope (:profile :admin))))
			((id :integer "The user id"))))))

(implement-api-function oauth2-api-test
    (get-users
     (:logging :enabled t)
     (:error-handling :enabled t))
    (&key (expand-groups nil))
  (declare (ignore expand-groups))
  (with-output-to-string (s)
    (with-serializer-output s
      (with-serializer (rest-server::accept-serializer)
	(with-elements-list ("users")
	   (loop for user in (model-test:all-users)
	      do
		(with-list-member ("user")
		  (with-element ("user")
		    (set-attribute "id" (cdr (assoc :id user)))
		    (set-attribute "realname" (cdr (assoc :realname user)))))))))))

(implement-api-function oauth2-api-test
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

(implement-api-function oauth2-api-test create-user (posted-content)
  (let ((user
	 (model-test:add-user (cdr (assoc :realname posted-content)))))
    (json:encode-json-alist-to-string user)))

(implement-api-function
    oauth2-api-test
    update-user (posted-content id)
    (let ((user (model-test::get-user id)))
      (if (not user)
	(error 'http-not-found-error)
	; else
	(progn
	  (model-test::set-user-realname user (cdr (assoc :realname posted-content)))
	  (model-test::update-user user)))))

(implement-api-function oauth2-api-test
    delete-user (id)
  (model-test::delete-user id))

(in-package :rest-server-tests)

(start-api 'oauth2-test::oauth2-api-test "localhost" 8183)

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
				  :port 8184))

(hunchentoot:define-easy-handler (login :uri "/login")
    ()
  (hunchentoot:redirect
   (format nil "http://localhost:3000/authorize?client_id=~A&redirect_uri=~A&response_type=code&scope=openid+profile"
	   *client-id*
	   (drakma:url-encode "http://localhost:8184/callback" :utf-8))))

(hunchentoot:define-easy-handler (callback :uri "/callback")
    (code)
  (let ((tokens
	 (anvil-connect::exchange-authorization-code
	  code
	  "http://localhost:8184/callback"
	  *client-id*
	  *client-secret*)))
    (setf (hunchentoot:session-value 'tokens) tokens)
    (hunchentoot:redirect "http://localhost:8184")))

(hunchentoot:define-easy-handler (root :uri "/")
    ()
  (when (not (hunchentoot:session-value 'tokens))
    (hunchentoot:redirect "http://localhost:8184/login"))
  (let ((tokens (hunchentoot:session-value 'tokens)))
    (multiple-value-bind (result status-code)
	(drakma:http-request "http://localhost:8183/users"
			     :method :get
			     :additional-headers `(("Accept" .  "application/json")
						   ("Authentication" . ,(format nil "~A ~A" (getf tokens :token-type)
										(getf tokens :access-token)))))
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
