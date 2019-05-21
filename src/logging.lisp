(in-package :rest-server.logging)

;; Defaults and api

(defparameter *default-logging-output* *standard-output*)

(defun %log-for (category-spec message &rest args)
  (if (log5::%log-p category-spec)
      (let ((category (log5::update-category-spec nil category-spec)))
        (log5::%handle-message
         (log5::category-id category)
         message
         (lambda () args)))
      (values)))

(defun api-log-for (api message &rest args)
  (apply #'%log-for (log-category-spec api) message args))

(defmethod start-api-logging ((api symbol))
  (start-api-logging (find-api api)))

(defmethod start-api-logging ((api rs::api-definition))
  (apply #'log5::start-sender-fn
         (rs::name api)
         (log-category-spec api)
         (log-output-spec api)
         (first (log-sender-type api))
         (rest (log-sender-type api))))

(defmethod stop-api-logging ((api symbol))
  (stop-api-logging (find-api api)))

(defmethod stop-api-logging ((api rs::api-definition))
  (log5:stop-sender (rs::name api)))

;; Api function logging

(defclass logging-resource-operation-implementation-decoration
    (rs::resource-operation-implementation-decoration)
  ((log-category-spec :initarg :category-spec
                      :accessor log-category-spec
                      :initform '(rs::rest-server))
   (log-output-spec :initarg :output-spec
                    :accessor log-output-spec
                    :initform '(log5::time
                                log5::category
                                log5::message
                                log5::context))
   (log-sender-type :initarg :sender-type
                    :accessor log-sender-type
                    :initform `(log5:stream-sender :location *default-logging-output*)))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod rs::process-resource-operation-implementation-option
    ((option (eql :logging))
     resource-operation-implementation
     &key
       (enabled t)
       (category-spec '(rs::rest-server))
       (output-spec '(log5::time
                      log5::message))
       (sender-type `(log5:stream-sender :location ,*default-logging-output*)))
  (if enabled
      (make-instance 'logging-resource-operation-implementation-decoration
                     :decorates resource-operation-implementation
                     :category-spec category-spec
                     :output-spec output-spec
                     :sender-type sender-type)
      resource-operation-implementation))

(defmethod rs::execute :around ((decoration logging-resource-operation-implementation-decoration)
                                &rest args)
  (api-log-for decoration
               "API: Handling ~A ~A by ~A"
               (hunchentoot:request-method*)
               (hunchentoot:request-uri*)
               (rs::name (rest-server::resource-operation decoration)))
  (let ((posted-content (rs::get-posted-content)))
    (when posted-content
      (api-log-for decoration "Posted content: ~A" posted-content)))
  (let ((result (call-next-method)))
    (api-log-for decoration "Response: (HTTP STATUS: ~A) ~A"
                 (hunchentoot:return-code*) result)
    result))

(cl-annot:defannotation logging (args resource-operation-implementation)
    (:arity 2)
  `(rs::configure-resource-operation-implementation
    (rs::name (rs::resource-operation ,resource-operation-implementation))
    (list :logging ,@args)))

;; Api logging

(defclass logging-api ()
  ((logging-enabled :initarg :logging-enabled
                    :initform t
                    :accessor logging-enabled)
   (log-category-spec :initarg :category-spec
                      :accessor log-category-spec
                      :initform '(rs::rest-server))
   (log-output-spec :initarg :output-spec
                    :accessor log-output-spec
                    :initform '(log5::time
                                log5::message))
   (log-sender-type :initarg :sender-type
                    :accessor log-sender-type
                    :initform `(log5:stream-sender :location ,*default-logging-output*))))

(defmethod rest-server::process-api-option ((option (eql :logging)) api
                                            &key (enabled t)
                                              category-spec
                                              output-spec
                                              sender-type)
  (dynamic-mixins:ensure-mix api 'logging-api)
  (setf (logging-enabled api) enabled)
  (when category-spec
    (setf (log-category-spec api) category-spec))
  (when output-spec
    (setf (log-output-spec api) output-spec))
  (when sender-type
    (setf (log-sender-type api) sender-type)))

(defmethod rest-server::api-execute-function-implementation :around ((api logging-api) resource-operation-implementation resource request)
  (api-log-for rs::*api*
               "API: Handling ~A ~A by ~A"
               (hunchentoot:request-method*)
               (hunchentoot:request-uri*)
               (rs::name (rs::resource-operation resource-operation-implementation)))
  (let ((posted-content (rs::get-posted-content)))
    (when posted-content
      (api-log-for rs::*api* "Posted content: ~A" posted-content)))
  (let ((result (call-next-method)))
    (api-log-for rs::*api* "Response: (HTTP STATUS ~A) ~A" (hunchentoot:return-code*) result)
    result))

(defun enable-api-logging (api-name &optional (start t))
  (dynamic-mixins:ensure-mix (find-api api-name) 'logging-api)
  (when start
    (start-api-logging api-name)))

(defun disable-api-logging (api-name &optional (stop t))
  (dynamic-mixins:delete-from-mix (find-api api-name) 'logging-api)
  (when stop
    (stop-api-logging api-name)))
