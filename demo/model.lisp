(defpackage :simple-users-model
  (:use :cl)
  (:export
   :get-user
   :all-users
   :add-user
   :update-user
   :delete-user))

(in-package :simple-users-model)

(defparameter *user-id* 1)
(defvar *users* nil)

(defun make-user (id realname)
  (list (cons :id id)
        (cons :realname realname)))

(defun user-id (user)
  (cdr (assoc :id user)))

(defun user-realname (user)
  (cdr (assoc :realname user)))

(defun set-user-realname (user realname)
  (setf (cdr (assoc :realname user)) realname))

(defun add-user (realname)
  (let ((user (make-user (incf *user-id*)
                         realname)))
    (push user
          *users*)
    user))

(defun update-user (user)
  (delete-user (user-id user))
  (push user *users*))

(defun get-user (id)
  (find id *users* :key (lambda (user)
                          (cdr (assoc :id user)))))

(defun delete-user (id)
  (setf *users* (delete id *users* :test #'equalp :key #'first)))

(defun all-users (&optional offset segment)
  (let ((users (copy-list *users*)))
    (if offset
        (apply #'subseq users (cons (min offset (length users))
                                    (and segment (list (min (+ offset segment)
                                                            (length users))))))
        users)))
