(in-package :rest-server-tests)

(in-suite rest-server-tests)

;; Serialization tests

(defparameter *object*
  (object "user"
	   (attribute "id" 22)
	   (attribute "realname" "Mike")
	   (attribute "groups"
		      (objects "groups"
				(object "group"
					 (attribute "id" 33)
					 (attribute "title" "My group"))))))

(deftest intermediate-representation-test
  (is (equalp (name *object*) "user"))
  (is (equalp (value (find "realname" (attributes *object*)  :key #'name :test #'equalp))
	      "Mike")))

(deftest json-serialization-test
  (let ((json-output
	 (with-output-to-string (s)
	   (with-serializer-output s
	     (with-serializer :json
	       (serialize *object*))))))
    (finishes (json:decode-json-from-string json-output))
    (let ((json (json:decode-json-from-string json-output)))
      (is (equalp (cdr (assoc :id json)) 22))
      (let ((groups (cdr (assoc :groups json))))
	(is (equalp (cdr (assoc :id (first groups)))
		    33))))))

#+fails(deftest xml-serialization-test
  (let ((xml-output
	 (with-output-to-string (s)
	   (cxml:with-xml-output (cxml:make-character-stream-sink s :indentation nil :omit-xml-declaration-p t)
	     (with-serializer-output s
	       (with-serializer :xml
		 (serialize *object*)))))))
    (finishes (cxml:parse xml-output (cxml-xmls:make-xmls-builder)))
    (let ((xml (cxml:parse xml-output (cxml-xmls:make-xmls-builder))))
      (destructuring-bind (object attributes &rest children)
	  xml
	(is (equalp object "user"))
	(is (null attributes))
	(let ((groups (find "groups" children :test #'equalp :key #'first)))
	  (destructuring-bind (object attributes &rest children)
	      groups
	    (declare (ignore attributes))
	    (is (equalp object "groups"))
	    (let ((title (find "title" children :test #'equalp :key #'first)))
	      (is (equalp (third title) "My group")))))))))

;; TODO: HTML and SEXP serialization tests

;; (with-output-to-string (s)
;;   (with-serializer-output s
;;     (with-serializer :xml
;;       (cxml:with-xml-output (cxml:make-character-stream-sink s :indentation nil :omit-xml-declaration-p t)
;;         (serialize *object*)))))

;; (with-serializer-output t
;;   (with-serializer :sexp
;;     (serialize *object*)))

;; Streaming api test

(defparameter *streamed-object*
  (lambda ()
    (with-object ("user")
      (set-attribute "id" 22)
      (with-attribute ("realname")
	(serialize "Mike"))
      (with-attribute ("groups")
	(with-list ("groups")
	  (with-list-member ("group")
	    (with-object ("group")
	      (set-attribute "id" 33)
	      (set-attribute "title" "My group"))))))))

(deftest json-stream-serialization-test
  (let ((json-output
	 (with-output-to-string (s)
	   (with-serializer-output s
	     (with-serializer :json
	       (funcall *streamed-object*))))))
    (finishes (json:decode-json-from-string json-output))
    (let ((json (json:decode-json-from-string json-output)))
      (is (equalp (cdr (assoc :id json)) 22))
      (let ((groups (cdr (assoc :groups json))))
	(is (equalp (cdr (assoc :id (first groups)))
		    33))))))

#+fails(deftest xml-stream-serialization-test
  (let ((xml-output
	 (with-output-to-string (s)
	   (with-serializer-output s
	     (with-serializer :xml
	       (cxml:with-xml-output (cxml:make-character-stream-sink s :indentation nil :omit-xml-declaration-p t)
		 (funcall *streamed-object*)))))))
    (finishes (cxml:parse xml-output (cxml-xmls:make-xmls-builder)))
    (let ((xml (cxml:parse xml-output (cxml-xmls:make-xmls-builder))))
      (destructuring-bind (object attributes &rest children)
	  xml
	(is (equalp object "user"))
	(is (null attributes))
	(let ((groups (find "groups" children :test #'equalp :key #'first)))
	  (destructuring-bind (object attributes &rest children)
	      groups
	    (declare (ignore attributes))
	    (is (equalp object "groups"))
	    (let ((title (find "title" children :test #'equalp :key #'first)))
	      (is (equalp (third title) "My group")))))))))

(deftest html-serialization-test
  (multiple-value-bind (doc errors)
      (let ((html
	     (with-output-to-string (s)
	       (with-serializer-output s
		 (with-serializer :html
		   (with-object ("user")
		     (set-attribute "realname" "Hola")))))))
	(html5-parser:parse-html5 html))
    (is (and doc (not errors))))
  (multiple-value-bind (doc errors)
      (let ((html
	     (with-output-to-string (s)
	       (with-serializer-output s
		 (with-serializer :html
		   (serialize *object*))))))
	(html5-parser:parse-html5 html))
    (is (and doc (not errors)))))

(deftest sexp-serialization-test
  (let ((str
	 (with-output-to-string (s)
	   (with-serializer-output s
	     (with-serializer :sexp
	       (with-object ("user")
		 (set-attribute "realname" "Hola")
		 (set-attribute "age" 33)))))))
    (finishes
      (read-from-string str)))
  (let ((str (with-output-to-string (s)
	       (with-serializer-output s
		 (with-serializer :sexp
		   (serialize *object*))))))
    (finishes
      (read-from-string str))))

(defparameter *typed-object*
  (object "user"
	   (attribute "false" nil :boolean)
	   (attribute "true" t :boolean)
	   (attribute "empty" nil :list)
	   (attribute "not-empty" (list 1 2 3) :list)))

(deftest typed-serialization-test
  (let ((result (with-output-to-string (s)
		  (with-serializer-output s
		    (with-serializer :json
		      (serialize *typed-object*))))))
    (is
     (equalp result
	     "{\"false\":false,\"true\":true,\"empty\":[],\"not-empty\":[1,2,3]}")))

  (let ((result
	 (with-output-to-string (s)
	   (with-serializer-output s
	     (with-serializer :json
	       (with-object ("my-object")
		 (with-attribute ("false")
		   (boolean-value nil))
		 (with-attribute ("true")
		   (boolean-value t))
		 (with-attribute ("empty")
		   (list-value nil))
		 (with-attribute ("not-empty")
		   (list-value (list 1 2 3)))))))))
    (is (equalp result
		"{\"false\":false,\"true\":true,\"empty\":[],\"not-empty\":[1,2,3]}")))

  (let ((result
	 (with-output-to-string (s)
	   (with-serializer-output s
	     (with-serializer :json
	       (with-object ("my-object")
		 (set-attribute "false" nil :type :boolean)
		 (set-attribute "true" t :type :boolean)
		 (set-attribute "empty" nil :type :list)
		 (set-attribute "not-empty" (list 1 2 3) :type :list)))))))
    (is (equalp result
		"{\"false\":false,\"true\":true,\"empty\":[],\"not-empty\":[1,2,3]}")))

  )
