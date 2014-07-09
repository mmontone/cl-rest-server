(in-package :rest-server-tests)

(in-suite rest-server-tests)

(defparameter *schema* 
  (schema
   (:element user
    ((id :integer)
     (realname :string)
     (age :integer :optional t)
     (sex (:option :male :female))
     (best-friend (:element user 
			    ((id :integer)
			     (realname :string))))
     (groups (:list (:element group
		     ((id :integer)
		      (name :string))))
	     :optional t)))))

(define-schema user-schema
    (:element user
	      ((id :integer :accessor id)
	       (realname :string)
	       (age :integer)
	       (best-friend user-schema
			    :optional t)
	       (groups (:list group-schema)
		       :optional t
		       :switch :include-user-groups))
	      (:class user)))

(define-schema minimal-user-schema
    (:element user
     ((id :integer)
      (realname :string))))

(define-schema group-schema
    (:element group
	      ((id :integer)
	       (name :string)
	       (users (:list user-schema) 
		      :optional t
		      :switch :include-group-users))
	      (:class group)))  

(defclass user ()
  ((id :initarg :id
       :accessor id
       :initform (error "Provide the id"))
   (realname :initarg :realname
	     :accessor realname
	     :initform (error "Provide the realname"))
   (age :initarg :age
	:accessor age
	:initform (error "Provide the age"))
   (groups :initarg :groups
	   :accessor groups
	   :initform nil)
   (best-friend :initarg :best-friend
		:accessor best-friend
		:initform nil)))		

(defclass group ()
  ((id :initarg :id
       :accessor id
       :initform (error "Provide the id"))
   (name :initarg :name
	 :accessor name
	 :initform (error "Provide the name"))
   (users :initarg :users
	  :accessor users
	  :initform nil)))

(defparameter *user* 
  (make-instance 'user
		 :realname "Mariano"
		 :id 2
		 :age 30
		 :groups (list (make-instance 'group
					      :name "My group"
					      :id 3))
		 :best-friend (make-instance 'user 
					     :id 3
					     :realname "Fernando"
					     :age 31
					     )))

(test basic-json-schema-serialization-test
  (let ((user (make-instance 'user
			     :realname "Mariano"
			     :id 2
			     :age 30
			     :groups (list (make-instance 'group
							  :name "My group"
							  :id 3))
			     :best-friend (make-instance 'user 
							 :id 3
							 :realname "Fernando"
							 :age 31))))
    (let ((json
	   (with-output-to-string (s)
	     (with-serializer-output s
	       (with-serializer :json
		 (serialize-with-schema 
		  *schema* user))))))
      (finishes (json:decode-json-from-string json)))))      

(with-output-to-string (s)
  (with-serializer-output s
    (with-serializer :json
      (serialize-with-schema 
       (find-schema 'user-schema) *user*))))

(with-output-to-string (s)
  (with-serializer-output s
    (with-serializer :json
      (serialize-with-schema 
       (find-schema 'minimal-user-schema) *user*))))

(with-output-to-string (s)
  (with-serializer-output s
    (with-serializer :xml
      (serialize-with-schema 
       *schema* *user*))))

#+fails(with-output-to-string (s)
  (with-serializer-output s
    (with-serializer :xml
      (serialize-with-schema 
       (find-schema 'user-schema) *user*))))

(with-output-to-string (s)
  (with-serializer-output s
    (with-serializer :xml
      (serialize-with-schema 
       (find-schema 'minimal-user-schema) *user*))))

(test parse-api-input-test
  (let ((input-1 "{\"id\":2,\"realname\":\"Mariano\",\"age\":30,\"bestFriend\":{\"id\":3,\"realname\":\"Fernando\"},\"groups\":[{\"id\":3,\"name\":\"My group\"}]}")
	(input-2 "<user><id>2</id><realname>Mariano</realname><age>30</age><best-friend><id>3</id><realname>Fernando</realname></best-friend><groups><group><id>3</id><name>My group</name></group></groups></user>")
	(input-3 "(user ((id . 2) (realname . \"Mariano\") (age . 30) (best-friend . ((id . 3) (realname . \"Fernando\"))) (groups . ((group ((id . 3) (name . \"My group\")))))))"))
  (let ((parsed-input-1 (rest-server::parse-api-input :json input-1))
	(parsed-input-2 (rest-server::parse-api-input :xml input-2))
	(parsed-input-3 (rest-server::parse-api-input :sexp input-3)))
    (is (and
	 (equalp (prin1-to-string parsed-input-1)
		 (prin1-to-string parsed-input-2))
	 (equalp (prin1-to-string parsed-input-2)
		 (prin1-to-string parsed-input-3)))))))

;; MOP

(defclass serializable-user ()
  ((id :initarg :id
       :accessor id
       :serialize t
       :serialization-type :integer)
   (realname :initarg :realname
	     :accessor realname
	     :initform (error "Provide the realname")
	     :serialization-type :string)
   (age :initarg :age
	:accessor age
	:initform (error "Provide the age")
	:serialization-type :integer)
   (groups :initarg :groups
	   :accessor groups
	   :initform nil
	   :serialization-type (:list group-schema))
   (best-friend :initarg :best-friend
		:accessor best-friend
		:initform nil
		:serialization-type user-schema
		:serialization-optional t)
   (another-friend :initarg :another-friend
		   :accessor another-friend
		   :initform nil
		   :serialization-type serializable-user
		   :serialization-optional t))
  (:metaclass serializable-class)
  (:serialization-name user))

(closer-mop:finalize-inheritance (find-class 'serializable-user))

(serializable-class-schema (find-class 'serializable-user))

(defparameter *serializable-user* 
  (make-instance 'serializable-user
		 :realname "Mariano"
		 :id 2
		 :age 30
		 :groups (list (make-instance 'group
					      :name "My group"
					      :id 3))
		 :best-friend (make-instance 'serializable-user 
					     :id 3
					     :realname "Fernando"
					     :age 31
					     )
		 :another-friend (make-instance 'serializable-user 
					     :id 3
					     :realname "Julio"
					     :age 31
					     )))

(with-output-to-string (s)
  (with-serializer-output s
    (with-serializer :json
      (serialize-with-schema 
       (serializable-class-schema 
	(find-class 'serializable-user))
       *serializable-user*))))

(with-output-to-string (s)
  (with-serializer-output s
    (with-serializer :json
      (serialize *serializable-user*))))

;; Unserialization

(let ((data
       (with-output-to-string (s)
	 (with-serializer-output s
	   (with-serializer :json
	     (serialize-with-schema 
	      (find-schema 'user-schema) *user*))))))
  (rest-server::unserialize-with-schema
   (find-schema 'user-schema)
   data))
