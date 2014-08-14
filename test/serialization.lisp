(in-package :rest-server-tests)

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

#+fails(test xml-serialization-test
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

(defparameter *streamed-element*
  (lambda ()
    (with-element ("user")
      (set-attribute "id" 22)
      (with-attribute ("realname")
	(serialize "Mike"))
      (with-attribute ("groups")
	(with-elements-list ("groups")
	  (with-list-member ("group")
	    (with-element ("group")
	      (set-attribute "id" 33)
	      (set-attribute "title" "My group"))))))))

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

#+fails(test xml-stream-serialization-test
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
