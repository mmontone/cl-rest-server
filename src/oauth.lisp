(in-package :rest-server)

(defclass oauth-authentication ()
  ())

(defmethod authenticate ((authentication oauth-authentication))
  (handler-case (oauth:validate-access-token)
    (error (e)
      (princ-to-string e))))

(defmacro define-oauth-resource (api-name)
  ;; OAuth resource
  `(progn
     (with-api ,api-name
       (define-api-resource oauth
	   (:documentation "OAuth authentication resource"
			   :path "/oauth")
	 (register-oauth-consumer
	  (:request-method :post
			   :path "/oauth/register"
			   :produces (:json)
			   :documentation "Register a new OAuth consumer")
	  ())
	 (get-oauth-request-token
	  (:request-method :post
			   :path "/oauth/token"
			   :produces (:json)
			   :documentation "Hand out OAuth request tokens")
	  (&optional
	   (scope :list "OAuth scope list")))
	 (get-oauth-user-authorization
	  (:request-method :post
			   :path "/oauth/authorize"
			   :produces (:json)
			   :documentation "Let the user authorize the access token. [6.2.1].")
	  ())
	 (get-oauth-access-token
	  (:request-method :post
			   :path "/oauth/access"
			   :produces (:json)
			   :documentation "Get an access token from a previously issued and authorized request token.")
	  ())))

     ;; Api functions implementation
     (implement-api-function ,api-name register-oauth-consumer (posted-content)
       (let ((token
	      (oauth:register-token
	       (oauth:make-consumer-token))))
       (with-output-to-string (json:*json-output*)
	 (json:with-object ()
	   (json:encode-object-member :key (oauth:token-key token))
	   (json:encode-object-member :secret (oauth:token-secret token))
	   (json:encode-object-member :user-data (oauth:token-user-data token))
	   (json:encode-object-member :last-timestamp (oauth::consumer-token-last-timestamp token))))))

     (implement-api-function ,api-name get-oauth-request-token (posted-content &key scope)
       (let ((request-token (oauth:validate-request-token-request)))
	 (oauth:request-token-response request-token)))

     (implement-api-function ,api-name get-oauth-user-authorization (posted-content)
       (oauth:protocol-assert (eq (oauth:request-method) :get)) ; [6.2.1]
       (let ((request-token (oauth:get-supplied-request-token)))
	 (when t		     ; XXX obtain user permission here
	   (setf (oauth:request-token-authorized-p request-token) t)
	   ;; now notify the Consumer that the request token has been authorized.
	   (let ((callback-uri (oauth:request-token-callback-uri request-token)))
	     (cond
	       ((eq oauth:*protocol-version* :1.0)
		;; callback uri is optional in 1.0; you might want to employ
		;; some other means to construct it.
		(hunchentoot:abort-request-handler "Authorization complete."))
	       (t
		(oauth:protocol-assert callback-uri)
		(hunchentoot:redirect (princ-to-string (oauth:finalize-callback-uri request-token)))))))
	 ;; only reached when authorization failed
	 ;; NOTE: optionally notify the Consumer if the user refused authorization.
	 ))

     (implement-api-function ,api-name get-oauth-access-token (posted-content)
       (let ((access-token (oauth:validate-access-token-request)))
	 (princ-to-string access-token)))))
