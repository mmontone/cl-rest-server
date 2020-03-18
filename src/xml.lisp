(in-package :rest-server)

(defun make-xmls-builder (&key (include-default-values t)
		               (include-namespace-uri t))
  "Make a XMLS style builder.  When 'include-namespace-uri is true a modified
  XMLS tree is generated that includes the element namespace URI rather than
  the qualified name prefix and also includes the namespace URI for attributes."
  (make-instance 'xmls-builder
		 :include-default-values include-default-values
		 :include-namespace-uri include-namespace-uri))


(defclass xmls-builder (cxml-xmls::xmls-builder)
  ())

(defun make-node (&key name ns attrs children)
  (declare (ignore ns attrs))
  (cons name
	children))

(defun node-children (node)
  (cdr node))

(defun (setf node-children) (newval node)
  (setf (cdr node) newval))

(defmethod sax:start-element
    ((handler xmls-builder) namespace-uri local-name qname attributes)
  (let* ((include-default-values (cxml-xmls::include-default-values handler))
	 (include-namespace-uri (cxml-xmls::include-namespace-uri handler))
	 (attributes
          (loop
              for attr in attributes
	      for attr-namespace-uri = (sax:attribute-namespace-uri attr)
	      for attr-local-name = (sax:attribute-local-name attr)
              when (and (or (sax:attribute-specified-p attr)
			    include-default-values)
			#+(or)
			(or (not include-namespace-uri)
			    (not attr-namespace-uri)
			    attr-local-name))
              collect
                (list (cond (include-namespace-uri
			     (cond (attr-namespace-uri
				    (cons attr-local-name attr-namespace-uri))
				   (t
				    (sax:attribute-qname attr))))
                            (t
                             (sax:attribute-qname attr)))
                      (sax:attribute-value attr))))
	 (namespace (when include-namespace-uri namespace-uri))
         (node (make-node :name local-name
                          :ns namespace
                          :attrs attributes))
         (parent (car (cxml-xmls::element-stack handler))))
    (if parent
        (push node (node-children parent))
        (setf (cxml-xmls::root handler) node))
    (push node (cxml-xmls::element-stack handler))))

(defmethod sax:end-element
    ((handler xmls-builder) namespace-uri local-name qname)
  (declare (ignore namespace-uri local-name qname))
  (let ((node (pop (cxml-xmls::element-stack handler))))
    (setf (node-children node) (reverse (node-children node)))))

(defmethod sax:characters ((handler xmls-builder) data)
  (let* ((parent (car (cxml-xmls::element-stack handler)))
         (prev (car (node-children parent))))
    ;; Be careful to accept both rods and strings here, so that xmls can be
    ;; used with strings even if cxml is configured to use octet string rods.
    (if (typep prev '(or cxml-xmls::rod string))
        ;; um entities herum wird SAX:CHARACTERS mehrfach aufgerufen fuer
        ;; den gleichen Textknoten.  Hier muessen wir den bestehenden Knoten
        ;; erweitern, sonst ist das Dokument nicht normalisiert.
        ;; (XXX Oder sollte man besser den Parser entsprechend aendern?)
        (setf (car (node-children parent))
              (concatenate `(vector ,(array-element-type prev))
                           prev
                           data))
        (push data (node-children parent)))))
