(in-package :rest-server)

;; flow from https://raw.githubusercontent.com/wiki/basho/webmachine/images/http-headers-status-v3.png

;; compile flow.dia diagram with this: http://wizard4j.org/pc?action=languageDia
;; and write the output to flow.xml

(defparameter *flow-chart-file* (asdf:system-relative-pathname :rest-server "src/flow.xml"))

(defparameter *flow*
  (cxml:parse-file
   *flow-chart-file*
   (cxml-dom:make-dom-builder)))

(defun flow-document-parser (document)
  `(defun dipatch-api-request (api-acceptor request)
     ,(flow-parse-element (dom:document-element document))))

(defun child-nodes (element)
  (coerce (dom:child-nodes element) 'list))

(defun yes-branch (element)
  (loop for child in (remove-if-not #'dom:element-p (child-nodes element))
     when (and (equalp (dom:tag-name child) "branch")
	       (equalp (dom:get-attribute child "name")
		       "yes"))
     do (return-from yes-branch (first (remove-if-not #'dom:element-p (child-nodes child)))))
  (error "Yes branch not found ~A" element))

(defun no-branch (element)
  (loop for child in (remove-if-not #'dom:element-p (child-nodes element))
     when (and (equalp (dom:tag-name child) "branch")
	       (equalp (dom:get-attribute child "name")
		       "no"))
     do (return-from no-branch (first (remove-if-not #'dom:element-p (child-nodes child)))))
  (error "No branch not found ~A" element))

(defun flow-parse-element (element)
  (format t "parsing ~A~%" element)
  (let ((element-name (dom:tag-name element)))
    (cond
      ((equalp element-name "switch")
       (list 'if (list (switch-symbol element))
	     (flow-parse-element (yes-branch element))
	     (flow-parse-element (no-branch element))))
      ((equalp element-name "info")
       (list (info-symbol element)))
      ((or (equalp element-name "flowchart")
	   (equalp element-name "root"))
       (flow-parse-element
	(first
	 (remove-if-not #'dom:element-p (child-nodes element)))))
      (t (error "~A" element)))))

(defun info-symbol (element)
  (intern (format nil "FLOW-~A"
		  (json:camel-case-to-lisp (dom:get-attribute element "name")))))

(defun switch-symbol (element)
  (intern (format nil "FLOW-~A-P"
		  (json:camel-case-to-lisp (dom:get-attribute element "name")))))

(eval (flow-document-parser *flow*))
