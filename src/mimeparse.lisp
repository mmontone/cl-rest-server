;;;; mimeparse.lisp -- MIME-type parsing and matching

;;; In the spirit of http://code.google.com/p/mimeparse/, with a Common
;;; Lisp flavor.
;;;
;;; Copyright 2008 Nathan Froyd
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation files
;;; (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(cl:defpackage :mimeparse
  (:use :cl)
  ;; Types
  (:export #:media-range)
  ;; Functions
  (:export #:parse-media-range
           #:quality
           #:best-match))
(cl:in-package :mimeparse)

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (require :cl-ppcre)
;;   (require :parse-number)
;;   (require :rt))

(defun strip (string)
  (string-trim " 	" string))

(defclass media-range ()
  ((type :initarg :type :reader media-type)
   (subtype :initarg :subtype :reader media-subtype)
   (params :initarg :params :accessor media-params)))

(defmethod initialize-instance :after ((range media-range) &rest initargs
                                       &key type subtype params &allow-other-keys)
  (declare (ignore initargs type subtype))
  (let* ((param (getparam "q" params))
         (q-param (second param)))
    (when (or (not q-param)
              (string= q-param "")
              (let ((n (handler-case (parse-number:parse-number q-param :radix 10)
                         ;; Always export your conditions...
                         (parse-number::invalid-number ()
                           nil)
                         ;; Try not to let the implementation do work for you...
                         #+sbcl
                         (sb-int:simple-parse-error ()
                           nil))))
                (not (and (typep n '(or float integer)) (<= 0 n 1)))))
      (if param
          (setf (second param) "1")
          (push (list "q" "1") (media-params range))))
    range))

(defmethod print-object ((range media-range) stream)
  (format stream "~A/~A;~:{~A=~A~^~}"
          (media-type range)
          (media-subtype range)
          (media-params range)))

(defun getparam (p params)
  (assoc p params :test #'string=))

(defun parse-media-range (media-range-string)
  "Parses MEDIA-RANGE-STRING and returns a media-range object.  The returned
media-range always has a value for the `q' parameter; a proper default is
suppplied if one is not provided in MEDIA-RANGE-STRING."
  (let* ((parts (ppcre:split ";" media-range-string))
         (params (mapcar (lambda (p)
                           (mapcar #'strip (ppcre:split "=" p :limit 2)))
                         (rest parts)))
         (full-type (first parts)))
    (when (string= full-type "*")
      (setf full-type "*/*"))
    (let ((type/subtype (ppcre:split "/" full-type)))
      (make-instance 'media-range
                     :type (strip (first type/subtype))
                     :subtype (strip (second type/subtype))
                     :params params))))

(defun fitness-and-quality-parsed (mime-type parsed-ranges)
  "Find the best match for MIME-TYPE against PARSED-RANGES, which is a
list of media-ranges that have already been parsed as if by
PARSE-MEDIA-RANGE.  Returns two values: the fitness value and the value
of the `q' quality parameter of the best match.  -1 and 0 are returned
if no match was found."
  (flet ((wild-string= (s1 s2)
           (or (string= s1 "*") (string= s2 "*") (string= s1 s2)))
         (score (s1 s2 v)
           (if (string= s1 s2)
               v
               0)))
    (loop with target = (parse-media-range mime-type)
       with best-fitness = -1
       with best-q = 0
       for range in parsed-ranges
       when (and (wild-string= (media-type target) (media-type range))
                 (wild-string= (media-subtype target)
                               (media-subtype range)))
       do (let* ((params (media-params range))
                 (n-param-matches
                  (count-if (lambda (tp)
                              (let ((key (first tp)))
                                (and (string/= key "q")
                                     (let ((p (getparam key params)))
                                       (and p
                                            (string= (second tp)
                                                     (second p)))))))
                            (media-params target)))
                 (fitness (+ (score (media-type target)
                                    (media-type range) 100)
                             (score (media-subtype target)
                                    (media-subtype range) 10)
                             n-param-matches)))
            (when (> fitness best-fitness)
              (setf best-fitness fitness
                    best-q (second (getparam "q" params)))))
       finally (return (values best-fitness
                               (if (stringp best-q)
                                   (parse-number:parse-number best-q)
                                   best-q))))))

(defun split-accept-header (header)
  (mapcar #'parse-media-range (ppcre:split "," header)))

(defgeneric quality (mime-type ranges)
  (:method (mime-type (ranges string))
    (quality mime-type (split-accept-header ranges)))
  (:method (mime-type (ranges list))
    (nth-value 1 (fitness-and-quality-parsed mime-type ranges)))
  (:documentation "Returns the quality `q' of MIME-TYPE when compared
against the media-ranges in RANGES.  RANGES can be either a list or a
string.  If RANGES is a list, it should be a list of media-ranges as
returned by PARSE-MEDIA-RANGE.  If RANGES is a string, it should be a
string that conforms to the format of the HTTP Accept: header."))

(defun best-match (supported header)
  "Return the best match for all media-ranges in HEADER.  SUPPORTED is a
list of supported MIME types.  The value of HEADER must be a string that
conforms to the format of the HTTP Accept: header."
  (loop with parsed-header = (split-accept-header header)
     with candidate = ""
     with best-fitness = -1
     with best-q = 0
     for mime-type in supported
     do (multiple-value-bind (fitness quality)
            (fitness-and-quality-parsed mime-type parsed-header)
          ;; The original code requires that we take the last of
          ;; equivalently-fit mime-types.
          (when (and (plusp quality)
                     (>= quality best-q)
                     (>= fitness best-fitness))
            (setf best-fitness fitness
                  best-q quality
                  candidate mime-type)))
     finally (return candidate)))

(cl:defpackage :mimeparse-tests
  (:use :cl))

(cl:in-package :mimeparse-tests)

;;; rfc2616 example

(defvar *accept*
  "text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5")

(defvar *rfc2616-tests*
  '(("text/html;level=1" 1)
    ("text/html" 0.7)
    ("text/plain" 0.3)
    ("image/jpeg" 0.5)
    ("text/html;level=2" 0.4)
    ("text/html;level=3" 0.7)))

;; (rtest:deftest :rfc2616
;;   (loop for (mime-type expected-q) in *rfc2616-tests*
;;      do (let ((q (mimeparse:quality mime-type *accept*)))
;;           (unless (eql q expected-q)
;;             (error "Expected ~A for mime-type ~A, got ~A"
;;                    expected-q mime-type q)))
;;      finally (return t))
;;   t)

;;; best match testing

;; (defvar *best-match-tests*
;;   (list (list "application/xbel+xml" "application/xml")
;;         '(("application/xbel+xml" "application/xbel+xml") ; direct match
;;           ("application/xbel+xml; q=1" "application/xbel+xml") ; direct match with q
;;           ("application/xml; q=1" "application/xml")           ; direct match of #2 with q
;;           ("application/*; q=1" "application/xml")             ; subtype wildcard
;;           ("*/*" "application/xml"))                           ; type wildcard
;;         (list "application/xbel+xml" "text/xml")
;;         '(("text/*;q=0.5,*/*; q=0.1" "text/xml") ; type versus lower-weighted subtype
;;           ("text/html,application/atom+xml; q=0.9" "")) ; match failure
;;         (list "application/json" "text/html")
;;         '(("application/json, text/javascript, */*" "application/json")
;;           ("application/json, text/html;q=0.9" "application/json"))
;;         (list "image/*" "application/xml")
;;         '(("image/png" "image/*")       ; type wildcard
;;           ("image/*" "image/*")))) ; wildcard for requested and supported

;; (rtest:deftest :best-match
;;   (loop for (supported-types tests . rest) on *best-match-tests* by #'cddr
;;      do (loop for (requested expected) in tests
;;            do (let ((actual (mimeparse:best-match supported-types requested)))
;;                 (unless (string= actual expected)
;;                   (error "Requested ~A from supported types ~A and received ~A"
;;                          requested supported-types actual))))
;;      finally (return t))
;;   t)
