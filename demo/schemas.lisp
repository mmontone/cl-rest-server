(in-package :rest-server-demo)

(schemata:define-schema user
    (schemata:object user
             ((:id integer :documentation "The user id")
              (:realname string :documentation "The user realname"))))

