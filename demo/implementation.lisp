(in-package :rest-server-demo)

(implement-resource-operation users-demo-api
    (list-users
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
              (loop for user in (simple-users-model:all-users (* 10 (1- page)) 10)
                    do
                       (with-list-member ("user")
                         (with-object ("user")
                           (set-attribute "id" (cdr (assoc :id user)))
                           (set-attribute "realname" (cdr (assoc :realname user)))))))))))))

(implement-resource-operation users-demo-api
    (fetch-user
     (:logging :enabled nil)
     (:serialization :enabled t))
    (id &key expand)
  (declare (ignore expand))
  (let ((user (simple-users-model:get-user id)))
    (if (not user)
        (error 'http-not-found-error)
                                        ; else
        (object "user"
                (attribute "href"
                           (format-absolute-resource-operation-url rest-server::*resource-operation* :id id))
                (attribute "id" id)
                (attribute "realname" (cdr (assoc :realname user)))))))

(implement-resource-operation
    users-demo-api
    create-user (&posted-content posted-content)
  (let ((user
          (simple-users-model:add-user (cdr (assoc :realname posted-content)))))
    (with-json-reply
      (json:encode-json-alist-to-string user))))

(implement-resource-operation
    users-demo-api
    update-user (&posted-content posted-content id)
  (let ((user (simple-users-model:get-user id)))
    (if (not user)
        (error 'http-not-found-error)
                                        ; else
        (progn
          (simple-users-model::set-user-realname user (cdr (assoc :realname posted-content)))
          (simple-users-model:update-user user)))))

(implement-resource-operation users-demo-api
    delete-user (id)
  (simple-users-model:delete-user id))
