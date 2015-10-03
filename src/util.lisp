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
