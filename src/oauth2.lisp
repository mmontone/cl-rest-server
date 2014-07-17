(in-package :rest-server)

(defparameter *client-id* "2026b7c0-374d-4ba6-b841-0d1b1fcdf02d")
(defparameter *client-secret* "234eb2440ab2b6d9f03a")
(defparameter *client-access-token* "eyJhbGciOiJSUzI1NiJ9.eyJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjMwMDAiLCJzdWIiOiIyNzVhOTE0ZC0zODEyLTQ5MGMtODkzYy01YjBkZTgyZGI0YjQiLCJhdWQiOiIyNzVhOTE0ZC0zODEyLTQ5MGMtODkzYy01YjBkZTgyZGI0YjQiLCJpYXQiOjE0MDQ2MDk5ODgxOTcsInNjb3BlIjoiY2xpZW50In0.aE9Pd21yZmk0eWlucmlTLVdPR2MyWmk2UzUtbU5GMkNVMDFLWFBObVhJUjJvdHd1NXhJcFBXVkQ5WVpJU2R1Q2J6ZWNuTzdpQW9xb0pxYmxRSUhaeWdTa1ZGUGU1dlJLZXF0VzRqN1h3WGFKUmtITHpPay1Jb054aVhoRjlvOEJGblkySDJCZ1F6bWROb25IeWxWSE5qTWZFZnNWNENOd3NhQ1ZLRXY2bDJReEF2U0N5eW9PRXNRRll5QkJmVGRfMm9mcG1KZ1psR0dDLVBldHVLS1k4dUVEMDNLZjF5cnl5bnZRNm1RZEJLcThpeVBTTkR5MnZ0LWlOa3RhU2tqWlpnd3U3alJQVlRKMlExbEZORWFyenhaN2dJZUh3OXVITF9GZXlHd0labHM5SHRMWTNyNjExNEwzdUdTQ2t6QmxPVWZNQkxvLUtBTDczdU94RkU4WktR")

(defclass oauth2-authentication ()
  ((scope :initarg :scope
	  :initform nil
	  :documentation "Auth scopes")))

(defmethod scope ((authentication oauth2-authentication))
  (mapcar (lambda (symbol-or-string)
	    (if (stringp symbol-or-string)
		symbol-or-string
		(string-downcase (symbol-name symbol-or-string))))
	  (slot-value authentication 'scope)))

(defmethod authenticate ((authentication oauth2-authentication))
  (let ((access-token (hunchentoot:header-in* "Authentication")))
    ;; if there's no access token, error
    (if (not access-token)
	"Provide the token"
	;; else, verify the access token
	(destructuring-bind (token-type token-string)
	    (split-sequence:split-sequence #\  access-token)
	  ;; Check it is a Bearer token
	  (if (not (equalp token-type "Bearer"))
	      "Not a Bearer token"
	      (multiple-value-bind (result status)
		  (anvil-connect::verify-access-token token-string *client-access-token*)
		;; if not valid, error
		(if (not (equalp status 200))
		    "Invalid token"
		    ;; else, check that the scopes are ok
		    (let ((token-scopes (split-sequence:split-sequence #\ 
								       (getf result :scope))))
		      (if (not (every (lambda (scope)
					(member scope token-scopes :test #'equalp))
				      (scope authentication)))
			  "Scope not sufficient")))))))))
