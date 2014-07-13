(in-package :rest-server)

(defvar *key* nil)

(defun register-key (key)
  (setf *key*
        (let* ((octets (babel:string-to-octets key))
               (length (length octets)))
          (and (> length 0)
               (flet ((pad-to (n)
                        "Is that a good idea, or should I just 0-pad?"
                        (apply 'concatenate
                               '(vector (unsigned-byte 8))
                               (subseq octets 0 (rem n length))
                               (make-list (floor n length)
                                          :initial-element octets))))
                 (ironclad:make-cipher :aes
                                       :key  (cond ((<= length 16) (pad-to 16))
                                                   ((<= length 24) (pad-to 23))
                                                   ((<= length 32) (pad-to 32))
                                                   (t (error "Maximum key size for AES: 32 bytes")))
                                       :mode :ecb))))))

(defun compress (string)
  (babel:string-to-octets string))

(defun inflate (ub8s)
  (babel:octets-to-string ub8s))

(defun word-to-octets (x)
  (declare (type (unsigned-byte 32) x))
  (make-array 4
              :element-type '(unsigned-byte 8)
              :initial-contents (list (ldb (byte 8 0) x)
                                      (ldb (byte 8 8) x)
                                      (ldb (byte 8 16) x)
                                      (ldb (byte 8 24) x))))

(defun octets-to-word (octets)
  (+ (* (expt 2 0)  (aref octets 0))
     (* (expt 2 8)  (aref octets 1))
     (* (expt 2 16) (aref octets 2))
     (* (expt 2 24) (aref octets 3))))

(defun encrypt (octets)
  (if (null *key*)
      octets
      (let* ((octets (concatenate '(vector (unsigned-byte 8))
                                  (word-to-octets (length octets))
                                  octets))
             (octets (concatenate '(vector (unsigned-byte 8))
                                  octets
                                  (make-array (- (* 16 (ceiling (length octets) 16))
                                                 (length octets))
                                              :element-type '(unsigned-byte 8)
                                              :initial-element 0)))
             (out    (make-array (* 2 (length octets))
                                 :element-type '(unsigned-byte 8)))
             (length (nth-value 1
                                (ironclad:encrypt *key*
                                                  octets
                                                  out))))
        (subseq out 0 length))))

(defun decrypt (octets)
  (if (null *key*)
      octets
      (let* ((out (make-array (* 2 (length octets))
                              :element-type '(unsigned-byte 8)
                              :initial-element 0))
             (length (nth-value 1
                                (ironclad:decrypt *key* octets out)))
             (out    (subseq out 0 length)))
        (subseq out 4 (+ 4 (octets-to-word out))))))

(defun encode-string (str)
  (cl-base64:usb8-array-to-base64-string
   (encrypt
    (compress
     (prin1-to-string str)))
   :uri t))

(defun decode-string (b64-str)
  (read-from-string
   (inflate
    (decrypt
     (cl-base64:base64-string-to-usb8-array b64-str
                                            :uri t)))
   nil))

(register-key "zgpaZoegGAbg3G480jn340fnsS3ia")

(defun user-authentication-token (id username)
  (encode-string (format nil "(~A ~A)" id username)))

(defun decode-token (token)
  (ignore-errors (read-from-string (decode-string token))))

#+nil(defun authentication-token-user (token)
  (let ((username-and-pass (decode-token token)))
    (when username-and-pass
      (destructuring-bind (username password) username-and-pass
        (login username password)))))

#+nil(defmacro with-authenticated-user ((token &optional (user (gensym))) &body body)
  `(let ((,user (or (and ,token
                         (authentication-token-user ,token))
                    (authentication-error))))
     (let ((b2-model::*user* ,user))
       ,@body)))

;; Plugging

(defclass authentication-api-function-implementation-decoration
    (api-function-implementation-decoration)
  ()
  (:metaclass closer-mop:funcallable-standard-class))

(defclass token-authentication-api-function-implementation-decoration
    (authentication-api-function-implementation-decoration)
  ((authentication-function
    :initarg :authentication-function
    :accessor authentication-function
    :initform (lambda (token)
		(error "Not implemented"))))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod authenticate-token ((decoration token-authentication-api-function-implementation-decoration)
			       token)
  (funcall (authentication-function decoration) token))

(defmethod process-api-function-implementation-option
    ((option (eql :authentication))
     api-function-implementation
     &rest args
     &key
       (enabled t)
       (type :token))
  (if enabled
      (apply #'make-authentication-decoration
	     type
	     api-function-implementation
	     args)
      api-function-implementation))

(defgeneric make-authentication-decoration (type api-function-implementation &rest args)
  (:documentation "Create an authentication-decoration of specific type"))

(defmethod make-authentication-decoration ((type (eql :token))
					   api-function-implementation
					   &rest args)
  (apply #'make-instance
	 'token-authentication-api-function-implementation-decoration
	 :decorates api-function-implementation
	 args))

(defmethod make-authentication-decoration (type
					   api-function-implementation
					   &rest args)
    (apply #'make-instance type
	   :decorates api-function-implementation
	   :allow-other-keys t args))

(defmethod execute :around
    ((decoration token-authentication-api-function-implementation-decoration)
     &rest args)
  (with-condition-handling
    (let ((token (hunchentoot:header-in* "Authentication")))
      (if (not token)
	  (error 'http-authorization-required-error
		 :format-control "Provide the token")
	  (if (not (authenticate-token decoration token))
	      (error 'http-authorization-required-error
		     :format-control "Authentication error")
	      (call-next-method))))))
