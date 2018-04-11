(in-package :rs.auth)

(defvar *authorization-enabled-p* t "Globally enable/disable API authorization")
(defvar *auth* nil "The succesful authorization. Bound for resource-operations (the resource operation can tell how it was authorized)")

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

(defun user-authorization-token (id username)
  (encode-string (format nil "(~A ~A)" id username)))

(defun decode-token (token)
  (ignore-errors (read-from-string (decode-string token))))

#+nil(defun authorization-token-user (token)
       (let ((username-and-pass (decode-token token)))
         (when username-and-pass
           (destructuring-bind (username password) username-and-pass
             (login username password)))))

#+nil(defmacro with-authorized-user ((token &optional (user (gensym))) &body body)
       `(let ((,user (or (and ,token
                              (authorization-token-user ,token))
                         (authorization-error))))
          (let ((b2-model::*user* ,user))
            ,@body)))

(adt:defdata auth-result
  (auth-success t)
  (auth-not-present string)
  (auth-fail (or string list)))

(defgeneric authorize (authorization resource-operation)
  (:documentation "Authorizes a RESOURCE-OPERATION access using AUTHORIZATION.
Implementations are expected to return an AUTH-RESULT value."))

;; For development. Possible to declare a generic funciton return type instead?
(defmethod authorize :around (authorization resource-operation)
  (let ((result (call-next-method)))
    (check-type result auth-result)
    result))

;; Plugging

(defun parse-authorizations (auths)
  (loop for auth-spec in auths
     collect
       (make-authorization auth-spec)))

(defun make-authorization (auth-spec)
  "Make an authorization object from the spec"
  (cond
    ((keywordp auth-spec)
     (make-instance (intern
                     (format nil "~A-AUTHORIZATION" (symbol-name auth-spec))
                     :rest-server)))
    ((symbolp auth-spec)
     (make-instance auth-spec))
    ((listp auth-spec)
     (destructuring-bind (auth-type &rest args) auth-spec
       (let ((class-name (ecase (type-of auth-type)
                           (keyword
                            (intern
                             (format nil "~A-AUTHORIZATION" (symbol-name auth-type))
                             :rest-server))
                           (symbol auth-type))))
         (apply #'make-instance class-name args))))
    (t (error "Invalid authorization spec ~A" auth-spec))))

(defclass token-authorization ()
  ((authorization-function
    :initarg :authorization-function
    :accessor authorization-function
    :initform (lambda (token)
                (error "Don't know how to authorize ~A" token)))))

(defmethod authorize-token ((authorization token-authorization)
                            token)
  (let ((authorized-token
         (funcall (authorization-function authorization) token)))
    (when authorized-token
      (list :authorization authorization
            :token authorized-token))))

(defmethod authorize
    ((authorization token-authorization) resource-operation)
  (flet ((auth-failed (message)
           (log5:log-for (rs::rest-server) "Token authorization failed: ~A" message)
           (return-from authorize (auth-fail message)))
         (auth-succeeded (authorized-token)
           (log5:log-for (rs::rest-server) "Token authorization: ~A" authorized-token)
           (return-from authorize
             (auth-success authorized-token))))
    (let* ((token (hunchentoot:header-in* "Authorization")))
      (if (not token)
          (progn
            (log5:log-for (rs::rest-server) "Token not present")
            (auth-not-present "Provide the token"))
          ;; else
          (let ((authorized-token (authorize-token authorization token)))
            (if (not authorized-token)
                (auth-failed "Invalid token")
                (auth-succeeded authorized-token)))))))

(defclass public-authorization ()
  ()
  (:documentation "Authorization for no authorization. That is, it always passes. Useful for building an optionally authorized end-point."))

(defmethod authorize
    ((authorization public-authorization) resource-operation)
  (auth-success nil))

(defun resource-operation-authorizations (resource-operation)
  "Authorizations that apply to an resource-operation. Merges resources authorizations and
   resource-operation authorizations, giving priority to resource-operation authorizations (overwrites)"
  (let ((authorizations (copy-list (rs::authorizations resource-operation))))
    (flet ((authorization-exists-p (auth)
             (cond
               ((symbolp auth)
                (member auth authorizations :test #'equalp))
               ((listp auth)
                (member (first auth) (remove-if-not #'listp authorizations)
                        :key #'first
                        :test #'equalp))
               (t (error "Invalid authorization ~A" auth)))))
      (loop for resource-authorization in (rs::resource-authorizations (resource resource-operation))
         do (when (not (authorization-exists-p resource-authorization))
              (push resource-authorization authorizations))))
    (parse-authorizations authorizations)))

(defun call-verifying-authorization (resource-operation function)
  (if (not (and *authorization-enabled-p*
                (rs::authorization-enabled rs::*api*)))
      (funcall function)
      ;; else
      (let ((authorizations
             (resource-operation-authorizations resource-operation)))
        (if (not (plusp (length authorizations)))
            (funcall function)
            ;; else
            (progn
              (log5:log-for (rs::rest-server) "API: Authorizing request...")
              (loop for auth in authorizations
                 do
                   (log5:log-for (rs::rest-server) "Trying with: ~A" auth)
                   (adt:match auth-result (authorize auth resource-operation)
                     ((auth-success auth-data)
                      (log5:log-for (rs::rest-server) "Request authorization successful.")
                      (return-from call-verifying-authorization
                        (let ((*auth* (cons auth auth-data)))
                          (funcall function))))
                     ((auth-fail error)
                      ;; Fail immediatly, don't try other authorizations, since
                      ;; the parameters for this authorization were present (the user tried to authorize via this auth)
                      (log5:log-for (rs::rest-server) "Authorization failed")
                      (error 'rs.error:http-forbidden-error
                             :format-arguments error))
                     ((auth-not-present _)
                      
                      ;; otherwise, try with the rest of authorizations
                      )))
              ;; here, all the authorizations have failed
              ;; auth required error
              (log5:log-for (rs::rest-server) "Authorization required")
              (error 'rs.error:http-authorization-required-error)
              )))))

(defmethod rs::process-api-option ((option (eql :authorization)) api
                               &key (enabled t))
  (setf (rs::authorization-enabled api) enabled))
