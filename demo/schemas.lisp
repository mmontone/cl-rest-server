(in-package :rest-server-demo)

(define-schema user
    (:object user
             ((:id :integer :documentation "The user id")
              (:realname :string :documentation "The user realname"))))

