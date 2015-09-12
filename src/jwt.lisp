(in-package :rest-server)

(defparameter +python-script+ (asdf:system-relative-pathname
			       :rest-server
			       "lib/jwtcmd.py"))

#-abcl
(defun jwt-encode (json)
  (string-trim (list #\space #\newline)
	       (trivial-shell:shell-command (format nil "python ~A encode '~A'" 
						    +python-script+
						    json))))

#-abcl
(defun jwt-decode (json &optional (format :alist))
  (let ((decoded-list
	 (json:decode-json-from-string
	  (trivial-shell:shell-command (format nil "python ~A decode '~A'"
					       +python-script+
					       json)))))
    (ecase format
      (:plist (alexandria:alist-plist decoded-list))
      (:alist decoded-list))))

#+abcl
(defun jwt-encode (json)
  json)

#+abcl
(defun jwt-decode (json &optional (format :alist))
  (let ((decoded-list (json:decode-json-from-string json)))
	(ecase format
	  (:plist (alexandria:alist-plist decoded-list))
	  (:alist decoded-list))))
