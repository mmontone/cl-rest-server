(in-package :rest-server)

(defparameter +python-script+ (asdf:system-relative-pathname
			       :rest-server
			       "lib/jwtcmd.py"))

(defun jwt-encode (json)
  (string-trim (list #\space #\newline)
	       (trivial-shell:shell-command (format nil "python ~A encode '~A'" 
						    +python-script+
						    json))))

(defun jwt-decode (json)
  (json:decode-json-from-string
   (trivial-shell:shell-command (format nil "python ~A decode '~A'"
					+python-script+
					json))))
