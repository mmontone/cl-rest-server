(in-package :rest-server)

(defvar *authorization-enabled-p* t "Globally enable/disable API authorization")

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

(defun parse-authentications (auths)
  (loop for auth-spec in auths
       collect
       (make-authentication auth-spec)))

(defun make-authentication (auth-spec)
  "Make an authentication object from the spec"
  (cond
    ((keywordp auth-spec)
     (make-instance (intern
		     (format nil "~A-AUTHENTICATION" (symbol-name auth-spec))
		     :rest-server)))
    ((symbolp auth-spec)
     (make-instance auth-spec))
    ((listp auth-spec)
     (destructuring-bind (auth-type &rest args) auth-spec
       (let ((class-name (ecase (type-of auth-type)
			   (keyword
			    (intern
			     (format nil "~A-AUTHENTICATION" (symbol-name auth-type))
			     :rest-server))
			   (symbol auth-type))))
	 (apply #'make-instance class-name args))))
    (t (error "Invalid authentication spec ~A" auth-spec))))

(defclass token-authentication ()
  ((authentication-function
    :initarg :authentication-function
    :accessor authentication-function
    :initform (lambda (token)
		#+nil(error "Not implemented")))))

(defmethod authenticate-token ((authentication token-authentication)
			       token)
  (funcall (authentication-function authentication) token))

(defmethod authenticate
    ((authentication token-authentication))
  (let ((token (hunchentoot:header-in* "Authorization")))
    (if (not token)
	"Provide the token"
	(if (not (authenticate-token authentication token))
	    "Invalid token"))))

(defun resource-operation-authorizations (resource-operation)
  "Authorizations that apply to an resource-operation. Merges resources authorizations and
   resource-operation authorizations, giving priority to resource-operation authorizations (overwrites)"
  (let ((authorizations (copy-list (authorizations resource-operation))))
    (flet ((authorization-exists-p (auth)
	     (cond
	       ((symbolp auth)
		(member auth authorizations :test #'equalp))
	       ((listp auth)
		(member (first auth) (remove-if-not #'listp authorizations)
			:key #'first
			:test #'equalp))
	       (t (error "Invalid authorization ~A" auth)))))
      (loop for resource-authorization in (resource-authorizations (resource resource-operation))
	 do (when (not (authorization-exists-p resource-authorization))
	      (push resource-authorization authorizations))))
    (parse-authentications authorizations)))

(defun verify-authentication (resource-operation)
  (when (and *authorization-enabled-p*
	     (authorization-enabled *api*))
    (let ((authentications (resource-operation-authorizations resource-operation)))
      (when (and (plusp (length authentications))
		 (every #'authenticate authentications))
	(signal 'http-authorization-required-error))))
  t)

(defmethod process-api-option ((option (eql :authorization)) api
			       &key (enabled t))			 
  (setf (authorization-enabled api) enabled))

