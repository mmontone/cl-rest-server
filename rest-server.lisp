(in-package :rest-server)

(defvar *apis* (make-hash-table :test #'equalp) "Hashtable holding the defined apis")

(log5:defcategory rest-server)

(defmacro define-api (name &rest options)
  "Define a Web API"
  )

(defmacro define-serialization-model ())

(defmacro define-serialization-type (type documentation))

(defmacro define-type-serializer ((object type) &body body))
(defmacro define-type-unserializer ((object type) &body body))

(define-serialization-type :string "String type")
(define-serialization-type :integer "Integer type")
(define-serialization-type :keyword "Keyword type")
(define-serialization-type :symbol "Symbol type")
(define-serialization-type :list "List type")
(define-serialization-type :timestamp "Timestamp type")

(define-type-serializer (string :string)
  string)

(define-type-unserializer (string :string)
  string)

(define-type-serializer (integer :integer)
  integer)

(define-type-unserializer (string :integer)
  (parse-integer :integer))

(define-type-serializer (symbol :symbol)
  (format nil "~a:~a"
          (symbol-package symbol)
          (symbol-name symbol)))

(defun make-keyword (string)
  (intern (string-upcase string) :keyword))

(define-type-unserializer (string :symbol)
  (let ((split (split-sequence:split-sequence #\: string)))
    (destructuring-bind (symbol-name symbol-package)
        (intern (string-upcase symbol-name) (make-keyword symbol-package)))))

(define-type-serializer (keyword :keyword)
  (symbol-name keyword))

(define-type-unserializer (string :keyword)
  (make-keyword string))