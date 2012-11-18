(in-package :rest-server)

(defclass user ()
  ((realname :initarg :realname
             :accessor realname)
   (age :initarg :age
        :accessor age)
   (groups :initarg :groups
           :accessor groups))
  (:documentation "System user"))

(defclass group ()
  ((name :initarg :name
         :accessor name)
   (users :initarg :users
          :accessor users))
  (:documentation "System group"))

(defclass issue ()
  ((title :initarg :title
          :accessor title)
   (description :initarg :description
                :accessor description)))

(define-serialization-model user
    ((id :integer)
     (email :string)
     (realname :string)
     (age :integer)
     (groups (:list group))
     (follows (:list issue) :accessor #'following-issues)
     (participates-in (:list issue) :accessor #'issues)
     (activity (:list t))))

(define-serialization-model group
    ((id :integer)
     (name :string)
     (description :string)
     (users (:list user))
     (projects (:list project))))

(defmacro define-serialization-template (name args &body body)
  `(defun ,name (,@args &rest overides)
     ,@body))

(define-serialization-template user-output-template (options)
  (apply #'scaffold-template-from-model 'user
         options
         (list :groups (group-template :ignore-users))))

(define-materialization-function materialize-user-1 (arguments)
  ;; Assumes groups are list of ids
  (with-object-validation
      (apply #'make-instance 'user
                     (materialize-arguments arguments
                                            :groups (lambda (id)
                                                      (mapcar #'fetch-group group))))))

