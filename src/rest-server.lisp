(in-package :rest-server)

(log5:defcategory rest-server)

(defvar *api-logging-output* *standard-output* "Where the API logging message go")

(defun start-api-logging ()
  (log5:start-sender 'api-info  
		     (log5:stream-sender :location *api-logging-output*)
		     :category-spec 'rest-server
		     :output-spec '(log5::time 
				    log5::message 
				    log5::context)))

(defun make-keyword (string)
  (intern (string-upcase string) :keyword))