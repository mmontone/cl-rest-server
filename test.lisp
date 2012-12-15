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