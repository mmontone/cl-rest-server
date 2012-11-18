(in-package :rest-server)

(defparameter *element*
  (element "user"
           (attribute "id" 22)
           (attribute "realname" "Mike")
           (attribute "groups"
                      (elements "groups"
                                (element "group"
                                         (attribute "id" 33)
                                         (attribute "title" "My group"))))))

(with-serializer-output t
  (with-serializer :json
    (serialize *element*)))

(cxml:with-xml-output (cxml:make-character-stream-sink t :indentation nil :omit-xml-declaration-p t)
  (with-serializer-output t
    (with-serializer :xml
      (serialize *element*))))