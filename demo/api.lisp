(in-package :rest-server-demo)

(define-api users-demo-api ()
    (:title "Users demo api"
     :documentation "An api demo with a simple users CRUD"
     :client-package :rest-server-demo.client
     :export-client-functions t)
  (users (:produces (:json :xml)
          :consumes (:json)
          :documentation "Users operations"
          :models (user)
          :path "/users")
         (list-users (:request-method :get
                      :produces (:json)
                      :path "/users"
                      :documentation "Retrive the users list")
                     (&optional (page :integer 1 "The page")
                                (expand :list nil "Attributes to expand")))
         (fetch-user (:request-method :get
                      :produces (:json)
                      :path "/users/{id}"
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
                      ((id :integer "The user id")))))
