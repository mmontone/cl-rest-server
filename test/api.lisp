(in-package :rest-server-tests)

(in-suite rest-server-tests)

;; API tests

(in-package :api-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (schemata:define-schema user
      (schemata:object user
               ((:id integer :documentation "The user id")
                (:realname string :documentation "The user realname")))))

(define-api api-test (rs::api-docs-mixin)
    (:title "Api test"
            :documentation "This is an api test")
  (parameters (:produces (:json)
                         :consumes (:json)
                         :documentation "Parameters test"
                         :path "/parameters")
              (parameters (:produces (:json)
                                     :consumes (:json)
                                     :documentation "Parameters test"
                                     :path "/parameters")
                          (&optional (boolean :boolean nil "A boolean parameter")
                                     (integer :integer nil "An integer parameter")
                                     (string :string nil "A string parameter")
                                     (list :list nil "A list parameter"))))
  (users (:produces (:json :xml)
                    :consumes (:json)
                    :documentation "Users operations"
                    :models (user)
                    :path "/users")
         (get-users (:request-method :get
                                     :produces (:json)
                                     :path "/users"
                                     :documentation "Retrive the users list")
                    (&optional (page :integer 1 "The page")
                               (expand :list nil "Attributes to expand")))
         (get-user (:request-method :get
                                    :produces (:json)
                                    :path "/users/{id}"
                                    :documentation "Retrive an user")
                   ((id :integer "The user id")
                    &optional
                    (expand :list nil "Attributes to expand")))
         (cached-get-user (:request-method :get
                                           :produces (:json)
                                           :path "/users/{id}/cached"
                                           :documentation "Retrive an user")
                          ((id :integer "The user id")
                           &optional
                           (expand :list nil "Attributes to expand")))
         (create-user (:request-method :post
                                       :consumes (:json)
                                       :path "/users"
                                       :documentation "Create a user"
                                       :body-schema user)
                      ())
         (update-user (:request-method :put
                                       :consumes (:json)
                                       :path "/users/{id}"
                                       :documentation "Update a user"
                                       :body-schema user)
                      ((id :integer "The user id")))
         (delete-user (:request-method :delete
                                       :consumes (:json)
                                       :path "/users/{id}"
                                       :documentation "Delete a user")
                      ((id :integer "The user id"))))
  (conditional-dispatch (:produces (:json :xml :html)
                                   :consumes (:json :xml :html)
                                   :documentation "Conditional dispatch test"
                                   :path "/conditional-dispatch")
                        (conditional-dispatch (:produces (:json :xml :html)
                                                         :consumes (:json :xml :html)
                                                         :documentation "Parameters test"
                                                         :path "/conditional-dispatch")
                                              ()))
  (decorations (:produces (:json)
                          :consumes (:json)
                          :documentation "Decorations tests"
                          :path "/decorations")
               (logging-decoration (:produces (:json)
                                              :consumes (:json)
                                              :documentation "Logging decoration test"
                                              :path "/decorations/logging")
                                   ())
               (error-handling-decoration (:produces (:json)
                                                     :consumes (:json)
                                                     :documentation "Error handling decoration test"
                                                     :path "/decorations/error-handling")
                                          ())
               (multiple-decorations (:produces (:json)
                                                :consumes (:json)
                                                :documentation "Multiple decorations test"
                                                :path "/decorations/multiple")
                                     ())))

;; Add Swagger resource
;;(rs.swagger::define-swagger-resource api-test)

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

(defun all-users (&optional offset segment)
  (let ((users (copy-list *users*)))
    (if offset
        (apply #'subseq users (cons (min offset (length users))
                                    (and segment (list (min (+ offset segment)
                                                            (length users))))))
        users)))

(defpackage :api-test-implementation
  (:use :cl
        :rest-server
        :generic-serializer
        :rest-server.logging
        :rest-server.error
        ))

(in-package :api-test-implementation)

(implement-resource-operation api-test::api-test
    api-test::parameters (&key boolean integer string list)
  (json:encode-json-plist-to-string
   (list :boolean boolean
         :integer integer
         :string string
         :list list)))

(implement-resource-operation api-test::api-test
    (api-test::get-users
     (:logging :enabled t)
     (:error-handling :enabled t))
    (&rest args &key expand (page 1))
  (declare (ignorable args))
  (let ((serializer (rest-server::accept-serializer)))
    (set-reply-content-type (generic-serializer::serializer-content-type serializer))
    (with-output-to-string (s)
      (with-serializer-output s
        (with-serializer serializer
          (with-pagination (:page page :expand expand)
            (with-list ("users")
              (loop for user in (model-test:all-users (* 10 (1- page)) 10)
                 do
                   (with-list-member ("user")
                     (with-object ("user")
                       (set-attribute "id" (cdr (assoc :id user)))
                       (set-attribute "realname" (cdr (assoc :realname user)))))))))))))

(implement-resource-operation api-test::api-test
    (api-test::get-user
     (:logging :enabled nil)
     (:serialization :enabled t))
    (id &key expand)
  (declare (ignore expand))
  (let ((user (model-test:get-user id)))
    (if (not user)
        (error 'http-not-found-error)
                                        ; else
        (object "user"
                 (attribute "href"
                            (format-absolute-resource-operation-url rest-server::*resource-operation* :id id))
                 (attribute "id" id)
                 (attribute "realname" (cdr (assoc :realname user)))))))

(implement-resource-operation api-test::api-test
    (api-test::cached-get-user
     (:logging :enabled nil)
     (:caching :type :etag
               :content-id :id)
     (:serialization :enabled t))
    (id &key expand)
  (declare (ignore expand))
  (let ((user (model-test:get-user id)))
    (if (not user)
        (error 'http-not-found-error)
                                        ; else
        (object "user"
                 (attribute "href"
                            (format-absolute-resource-operation-url rest-server::*resource-operation* :id id))
                 (attribute "id" id)
                 (attribute "realname" (cdr (assoc :realname user)))))))

(implement-resource-operation
    api-test::api-test
    api-test::create-user (&posted-content posted-content)
  (let ((user
         (model-test:add-user (cdr (assoc :realname posted-content)))))
    (with-json-reply
      (json:encode-json-alist-to-string user))))

(implement-resource-operation
    api-test::api-test
    api-test::update-user (&posted-content posted-content id)
  (let ((user (model-test::get-user id)))
    (if (not user)
        (error 'http-not-found-error)
                                        ; else
        (progn
          (clear-cache 'api-test::cached-get-user id)
          (model-test::set-user-realname user (cdr (assoc :realname posted-content)))
          (model-test::update-user user)))))

(implement-resource-operation api-test::api-test
    api-test::delete-user (id)
  (model-test::delete-user id))

;; Conditional dispatch
;; (implement-resource-operation api-test::api-test
;;     api-test::conditional-dispatch ()
;;   (error 'http-not-acceptable-error))

;; (implement-resource-operation-case
;;     api-test::conditional-dispatch "text/html"
;;     ()
;;   "<p>Hello</p>")

;; (implement-resource-operation-case
;;     api-test::conditional-dispatch "application/json"
;;     ()
;;   "\"hello\"")

;; (implement-resource-operation-case
;;     api-test::conditional-dispatch "application/xml"
;;     ()
;;   "<p>Hello</p>")

(implement-resource-operation api-test::api-test
	api-test::conditional-dispatch ()
  (error 'http-not-acceptable-error))

(implement-resource-operation api-test::api-test
	(api-test::conditional-dispatch
	 (:produces :content-types (list :html)))
	()
  "<p>Hello</p>")

(implement-resource-operation api-test::api-test
	(api-test::conditional-dispatch
	 (:produces :content-types (list :json)))
	()
  "\"hello\"")

(implement-resource-operation api-test::api-test
	(api-test::conditional-dispatch
	 (:produces :content-types (list :xml)))
	()
  "<p>Hello</p>")

;; Decorations

(cl-annot:enable-annot-syntax)

@rs.log:logging ()
(implement-resource-operation api-test::api-test
    api-test::logging-decoration ()
  "Hello")

@error-handling ()
(implement-resource-operation api-test::api-test
    api-test::error-handling-decoration ()
  (error 'http-not-found-error))

@rs.log:logging ()
@error-handling ()
(implement-resource-operation api-test::api-test
    api-test::multiple-decorations ()
  (error 'http-not-found-error))

(in-package :rest-server-tests)

(deftest api-post-test
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

(deftest api-setup-test
  (multiple-value-bind (result status-code headers)
      (drakma:http-request "http://localhost:8181/users" :method :get)
    (is (equalp status-code 200))
    (is (equalp (cdr (assoc :content-type headers)) "application/json"))
    (finishes (json:decode-json-from-string result))
    #+nil(let ((users (json:decode-json-from-string result)))
           (is (alexandria:set-equal
                (mapcar (lambda (user)
                          (cdr (assoc :realname user)))
                        users)
                (list "user1" "user2" "user3")
                :test #'equalp)))))

#+nil(deftest basic-parameters-test
       (multiple-value-bind (result status-code)
           (drakma:http-request "http://localhost:8181/users?expand-groups=true" :method :get)
         (is (equalp status-code 200))
         (is (equalp (read-from-string result) (list "user1" "user2" "user3")))))

#+nil(deftest boolean-parameters-test
       (setf *catch-errors* t)
       (multiple-value-bind (result status-code)
           (drakma:http-request "http://localhost:8181/users?expand-groups=foo" :method :get)
         (declare (ignore result))
         (is (equalp status-code 500)))
       (multiple-value-bind (result status-code)
           (drakma:http-request "http://localhost:8181/users?expand-groups=true" :method :get)
         (is (equalp status-code 200))
         (is (equalp (read-from-string result) (list "user1" "user2" "user3" t)))))

(deftest error-handling-test
  (let ((catch-errors (rest-server::catch-errors *api-acceptor*)))
    (setf (rest-server::catch-errors *api-acceptor*) t)
    (multiple-value-bind (result status-code)
        (drakma:http-request "http://localhost:8181/users?expand-groups=foo" :method :get)
      (declare (ignore result))
      (is (equalp status-code 500)))
    ;; We can not test development mode like this. We are in a different thread.
    #+nil(setf *catch-errors* nil)
    #+nil(signals simple-error
           (drakma:http-request "http://localhost:8181/users?expand-groups=foo" :method :get))
    (setf (rest-server::catch-errors *api-acceptor*) catch-errors)))

(deftest accept-content-test
  (multiple-value-bind (result status headers)
      (drakma:http-request "http://localhost:8181/users/2"
                           :method :get
                           :additional-headers '(("Accept" . "application/json")))
    (is (equal status 200))
    (finishes (json:decode-json-from-string result))
    (is (equalp (cdr (assoc :content-type headers)) "application/json")))
  (multiple-value-bind (result status headers)
      (drakma:http-request "http://localhost:8181/users/2"
                           :method :get
                           :additional-headers '(("Accept" . "application/xml")))
    (is (equal status 200))
    (finishes (cxml:parse result (cxml-xmls:make-xmls-builder)))
    (is (equalp (cdr (assoc :content-type headers)) "application/xml")))
  (multiple-value-bind (result status headers)
      (drakma:http-request "http://localhost:8181/users/2"
                           :method :get
                           :additional-headers '(("Accept" . "text/html")))
    (declare (ignorable headers))
    (is (eql status 200))
    (multiple-value-bind (html error)
        (html5-parser:parse-html5 result :strictp t)
      (declare (ignorable error html))
                                        ;(is (null error))
      )
    (is (cl-ppcre:scan  "text/html" (cdr (assoc :content-type headers)))))
  (let ((result
         (drakma:http-request "http://localhost:8181/users/2"
                              :method :get
                              :additional-headers '(("Accept" . "text/lisp")))))
    (finishes (read-from-string result))))

(defparameter *api-url* "http://localhost:8181")

(deftest client-api-access-test
  ;; Create a new user
  (multiple-value-bind (result status)
      (with-api-backend *api-url*
        (api-test::create-user
         (json:encode-json-plist-to-string
          (list :realname "Felipe"))))
    (declare (ignorable result))
    (is (equalp status 200))
    (let ((created-user result))
      ;; Retrieve the list of users
      (multiple-value-bind (users status)
          (with-api-backend *api-url*
            (api-test::get-users))
        (is (equalp status 200))
        (is (listp users)))
      ;; Fetch the created user
      (multiple-value-bind (result status)
          (with-api-backend *api-url*
            (api-test::get-user (cdr (assoc :id created-user))))
        (declare (ignorable result))
        (is (equalp status 200)))
      ;; Fetch an unexisting user
      (multiple-value-bind (result status)
          (with-api-backend *api-url*
            (api-test::get-user 123456))
        (declare (ignorable result))
        (is (equalp status 404))))))

#+fails(deftest content-type-test
         "Specify the content type we are sending explicitly"
         (setf *catch-errors* t)
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
             (drakma:http-request "http://localhost:8181/users?expand=true" :method :get)
           (is (equalp status-code 200))
           (is (equalp (read-from-string result) (list "user1" "user2" "user3" t)))))

(deftest resource-operations-parameters-test
  (macrolet ((check (key value)
               `(progn
                  (is (equalp status 200))
                  (is (equalp (cdr (assoc ,key
                                          result))
                              ,value)))))
    ;; No parameters
    (multiple-value-bind (result status)
        (with-api-backend *api-url*
          (api-test::parameters))
      (declare (ignorable result))
      (is (equalp status 200)))

    ;; Wrong parameter
    (multiple-value-bind (result status)
        (drakma:http-request "http://localhost:8181/parameters?foo=foo" :method :get)
      (declare (ignorable result))
      (is (equalp status 500)))

    ;; Boolean
    (multiple-value-bind (result status)
        (with-api-backend *api-url*
          (api-test::parameters :boolean t))
      (declare (ignorable result))
      (is (equalp status 200))
      (is (cdr (assoc :boolean
                      result))))

    (multiple-value-bind (result status)
        (with-api-backend *api-url*
          (api-test::parameters :boolean "t" :encode-request-arguments nil))
      (declare (ignorable result))
      (is (equalp status hunchentoot:+http-bad-request+)))

    (multiple-value-bind (result status)
        (with-api-backend *api-url*
          (api-test::parameters :boolean nil))
      (declare (ignorable result))
      (is (equalp status 200))
      (is (not (cdr (assoc :boolean
                           result)))))

    (multiple-value-bind (result status)
        (with-api-backend *api-url*
          (api-test::parameters :boolean 44))
      (declare (ignorable result))
      (is (equalp status 200)))

    (multiple-value-bind (result status)
        (with-api-backend *api-url*
          (api-test::parameters :boolean "44" :encode-request-arguments nil))
      (declare (ignorable result))
      (is (equalp status hunchentoot:+http-bad-request+)))

    ;; String

    (multiple-value-bind (result status)
        (with-api-backend *api-url*
          (api-test::parameters :string "asd"))
      (declare (ignorable result))
      (is (equalp status 200))
      (is (equalp (cdr (assoc :string
                              result))
                  "asd")))

    ;; Integer
    (multiple-value-bind (result status)
        (with-api-backend *api-url*
          (api-test::parameters :integer 34))
      (declare (ignorable result))
      (check :integer 34))

    (multiple-value-bind (result status)
        (with-api-backend *api-url*
          (api-test::parameters :integer "34" :encode-request-arguments nil))
      (declare (ignorable result))
      (check :integer 34))

    (multiple-value-bind (result status)
        (with-api-backend *api-url*
          (api-test::parameters :integer "t" :encode-request-arguments nil))
      (declare (ignorable result))
      (is (equalp status hunchentoot:+http-bad-request+)))

    (multiple-value-bind (result status)
        (with-api-backend *api-url*
          (api-test::parameters :integer "nil" :encode-request-arguments nil))
      (declare (ignorable result))
      (is (equalp status hunchentoot:+http-bad-request+)))

    ;; Lists
    #+fails(multiple-value-bind (result status)
               (with-api-backend *api-url*
                 (api-test::parameters :list (list "foo" "bar")))
             (check :list (list "foo" "bar")))

    (multiple-value-bind (result status)
        (with-api-backend *api-url*
          (api-test::parameters :list "foo,bar" :encode-request-arguments nil))
      (declare (ignorable result))
      (check :list (list "foo" "bar")))

    (multiple-value-bind (result status)
        (with-api-backend *api-url*
          (api-test::parameters :list (list "foo" "bar")))
      (check :list (list "foo" "bar")))

    (multiple-value-bind (result status)
        (drakma:http-request "http://localhost:8181/parameters?list=foo,bar" :method :get)
      (is (equalp status 200))
      (is (equalp (cdr (assoc :list (json:decode-json-from-string result)))
                  (list "foo" "bar"))))))

(deftest caching-test
  ;; Create a new user
  (multiple-value-bind (result status)
      (with-api-backend *api-url*
        (api-test::create-user
         (json:encode-json-plist-to-string
          (list :realname "Cached user"))))
    (is (equalp status 200))
    ;; Fetch the created user
    (let ((created-user result))
      (let ((user-id (cdr (assoc :id created-user))))
        (multiple-value-bind (result status headers)
            (drakma:http-request
             (format nil "http://localhost:8181/users/~A/cached" user-id)
             :method :get
             :additional-headers '(("Accept" . "application/json")))
          (declare (ignorable result status headers))
          ;; Test there's an ETag
          (let ((etag (cdr (assoc :etag headers))))
            (is (not (null etag)))
            ;; When using the etag, we obtain a content not modified response
            (multiple-value-bind (result status)
                (drakma:http-request
                 (format nil "http://localhost:8181/users/~A/cached" user-id)
                 :method :get
                 :additional-headers `(("Accept" . "application/json")
                                       ("If-None-Match" . ,etag)))
              (declare (ignorable result))
              (is (equalp status 304)))
            ;; If we use an invalid etag, we get the user and an etag
            (multiple-value-bind (result status headers)
                (drakma:http-request
                 (format nil "http://localhost:8181/users/~A/cached" user-id)
                 :method :get
                 :additional-headers '(("Accept" . "application/json")
                                       ("If-None-Match" . "foo")))
              (declare (ignorable result headers))
              (is (equalp status 200))
              (finishes (json:decode-json-from-string result)))

            ;; If we use the etag again, we obtain a content not modified response
            (multiple-value-bind (result status)
                (drakma:http-request
                 (format nil "http://localhost:8181/users/~A/cached" user-id)
                 :method :get
                 :additional-headers `(("Accept" . "application/json")
                                       ("If-None-Match" . ,etag)))
              (declare (ignorable result))
              (is (equalp status 304)))

            ;; Update the user
            (with-api-backend *api-url*
              (api-test::update-user
               (json:encode-json-plist-to-string
                (list :realname "Felipe"))
               user-id))

            ;; If we try to use the old etag, we get another and the new user info
            (multiple-value-bind (result status)
                (drakma:http-request
                 (format nil "http://localhost:8181/users/~A/cached" user-id)
                 :method :get
                 :additional-headers `(("Accept" . "application/json")
                                       ("If-None-Match" . ,etag)))
              (is (equalp status 200))
              (is (cdr (assoc :etag headers)))
              (finishes (json:decode-json-from-string result)))))))))

(deftest conditional-dispatch-test
  ;; No accept content-type
  (multiple-value-bind (result status)
      (drakma:http-request
       (format nil "http://localhost:8181/conditional-dispatch")
       :method :get)
    (declare (ignorable result))
    (is (equalp status hunchentoot:+http-not-acceptable+)))

  ;; Html accept
  (multiple-value-bind (result status headers)
      (drakma:http-request
       (format nil "http://localhost:8181/conditional-dispatch")
       :method :get
       :additional-headers `(("Accept" . "text/html"))
	   :accept "text/html")
    (declare (ignorable result))
    (is (equalp status 200))
    (is (ppcre:scan "text/html" (cdr (assoc :content-type headers)))))

  ;; Json accept
  (multiple-value-bind (result status headers)
      (drakma:http-request
       (format nil "http://localhost:8181/conditional-dispatch")
       :method :get
       :additional-headers `(("Accept" . "application/json"))
	   :accept "application/json")
    (declare (ignorable result))
    (is (equalp status 200))
    (is (ppcre:scan "application/json" (cdr (assoc :content-type headers)))))

  (multiple-value-bind (result status headers)
      (drakma:http-request
       (format nil "http://localhost:8181/conditional-dispatch")
       :method :get
       :additional-headers `(("Accept" . "application/xml"))
	   :accept "application/xml")
    (declare (ignorable result))
    (is (equalp status 200))
    (is (ppcre:scan "application/xml" (cdr (assoc :content-type headers))))))

(deftest not-found-test
  "Test that http-not-found is obtained on non existant resources"
  (multiple-value-bind (result status)
      (drakma:http-request
       (format nil "http://localhost:8181/foo")
       :method :get)
    (declare (ignorable result))
    (is (equalp status hunchentoot:+http-not-found+))))
