(in-package :rest-server)

(defun make-keyword (string)
  (intern (string-upcase string) :keyword))

(defun special-arg-indicator-p (symbol)
  (and (symbolp symbol)
	   (eql (elt (symbol-name symbol) 0) #\&)))

(defun extract-special-arguments (lambda-list)
  "Extracts a lambda-list 'special' arguments. Special arguments are those starting with & character. For example, &posted-content posted.
Special arguments are useful for injecting arguments into functions.

Returns a lambda list with the special arguments removed, and the removed special arguments as second value."

  (let ((clean-lambda-list ())
		(special-args ()))
	(loop
	   :for arg := (pop lambda-list)
	   :while arg
	   :do
	   (cond
		 ((member arg '(&key &optional &body &rest &aux &allow-other-keys &whole &environment))
		  ;; In this case, it is one of CL special arguments
		  (push arg clean-lambda-list))
		 ((special-arg-indicator-p arg)
		  ;; It is an special argument
		  (let ((arg-value (pop lambda-list)))
			(push (cons (make-keyword (string arg)) arg-value) special-args)))
		 (t
		  ;; Otherwise, it's a normal argument
		  (push arg clean-lambda-list))))
	(values (nreverse clean-lambda-list)
			(nreverse special-args))))

(defun insert-after (lst index newelt)
  (push newelt (cdr (nthcdr index lst))) 
  lst)

(defun add-keyword-argument (arg lambda-list)
  "Add a keyword argument ARG to LAMBDA-LIST"
  (let ((key-pos (position '&key lambda-list)))
	(if key-pos
		(insert-after lambda-list key-pos
					  arg)
		(if (consp lambda-list)
			(setf (cdr (last lambda-list))
				  (list '&key arg))
			(setf lambda-list (list '&key arg))))
	lambda-list))

(defun encode-file-data-uri-scheme (filename &optional charset)
  "Encode file data as URI Data scheme: https://en.wikipedia.org/wiki/Data_URI_scheme"
  (declare (ignore charset))
  (let ((file-contents 
         (alexandria:read-file-into-byte-vector filename))
        (file-mime
         (mimes:mime filename)))
    (format nil "~A;base64,~A" file-mime 
            (base64:usb8-array-to-base64-string
             file-contents :uri t))))

(defun lisp-to-underscores (string)
  (format nil "~{~A~^_~}"
          (mapcar #'string-downcase
                  (split-sequence:split-sequence #\- string))))

(defun underscores-to-lisp (string)
  (format nil "~{~A~^-~}"
          (mapcar #'string-upcase
                  (split-sequence:split-sequence #\_ string))))

(defun call-with-lisp-json (function)
  (let ((cl-json:*lisp-identifier-name-to-json*
         #'lisp-to-underscores)
        (cl-json:*json-identifier-name-to-lisp*
         #'underscores-to-lisp))
    (funcall function)))

(defmacro with-lisp-json (&body body)
  `(call-with-lisp-json (lambda () ,@body)))

;;--------------
;; Basic crypto
;;--------------

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
