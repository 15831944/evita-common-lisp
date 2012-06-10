;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 19 Filenames - Logical File System
;;; lisp/runtime/r19-logical.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r19-logical.lisp#5 $
;;;
;;; Description:
;;;  This fils contains pathname functions for logical-pathname.
;;;     make-logical-host
;;;     logical-match-directory-p
;;;     logical-match-component-p
;;;     logical-parse-component
;;;     logical-translate-component
;;;     logical-unparse-directory
;;;     logical-unparse-element
;;;
;;; Methods:
;;;     make-pathname-using-host
;;;     internal-pathname-device-match-p
;;;     internal-pathname-directory-match-p
;;;     pathname-equal-using-host-p
;;;     internal-pathname-name-match-p
;;;     internal-pathname-type-match-p
;;;     internal-pathname-version-match-p
;;;     truename-using-host
;;;     wild-pathname-p-using-host
;;;
;;; Public Functions:
;;;     load-logical-pathname-translations  19.4.7
;;;     logical-pathname                    19.4.9
;;;     logical-pathname-translations       19.4.8
;;;     translate-logical-pathname          19.4.15
;
(in-package :si)

;;; device    string, nil or :unspecific  (no wild)
;;; directory cons, nil, or :unspecific. This isn't (:relative).
;;; name      string, cons, nil, :wild, or :unspecific
;;; type      string, cons, nil, :wild, or :unspecific
;;; version   nil, :newest, or :unspecific

(declaim (ftype (function (string) logical-host)
  intern-logical-pathname-host ) )

(declaim (ftype (function (string) logical-host)
  make-logical-host ) )

(declaim (ftype (function (t t) (values t list))
  logical-match-component-p ) )

(declaim (ftype (function (t t) (values t list))
  logical-match-directory-p ) )

(declaim (ftype (function (logical-host t t) (values t list))
  logical-parse-component ) )

(declaim (ftype (function (cons list) simple-string)
  logical-translate-component ) )

(declaim (ftype (function (t stream) unspecified)
  logical-unparse-directory ) )

(declaim (ftype (function (t stream) unspecified)
  logical-unparse-element ) )


;;;; intern-logical-pathname-host
(defun intern-logical-pathname-host (name)
    (declare (type string name))
    (declare (values logical-host))
  (let ((host (intern-pathname-host name #'make-logical-host)))
    (check-type host logical-host)
    host ) )


;;;; make-logical-host
;;;
;;; Note: We need to push host object into *pathname-hosts* to allow
;;; find-pathname-host recognizes newly created host.
;
(defun make-logical-host (name)
    (declare (type string name))
    (declare (values logical-host))
  (let ((host (make-instance 'logical-host)))
    (setf (ref basic-host name host) (string-upcase name))
    host ) )


;;;; logical-match-component-p
;;;
;;; Syntax:
;;;   logical-match-component-p component pattern => boolean, matched-strings
;;;
;;; Arguments and Values:
;;;   component a string, list, :wild, nil, :unspecific
;;;   pattern   a string, list, :wild, nil, :unspecific
;;;
;;; Called by:
;;;   pathname-match-p-using-host
;
(defun logical-match-component-p (component pattern)
    (declare (values t list))
  (let ((mstrings '())
        (mstart   nil) )
    (labels (
      ;; end-wild-match
      (end-wild-match (string mend)
        (when mstart
          (push (subseq string mstart mend) mstrings) )
        (setq mstart nil) )

      ;; match
      (match (string pattern index end)
        (loop
          (when (null pattern)
            (when (= index end)
              (end-wild-match string index)
              (return t) )
            (return nil) )

          (let ((pat (pop pattern)))
            (etypecase pat
              (string
                (end-wild-match string index)
                (let ((nindex (string-not-equal string pat :start1 index)))
                  (cond
                    ((not nindex)
                      (setq index end) )
                    ((/= nindex (+ index (length pat)))
                      (return-from match nil) )
                    (t
                      (setq index nindex) )) ) )

              ((eql :wild)
                (end-wild-match string index)
                (when (= index end)
                  (push "" mstrings)
                  (return t) )
                (let ((aindex         index)
                      (saved-mstrings mstrings) )
                  (setq index end)
                  (loop
                    (when (< index aindex)
                      (return-from match nil) )

                    (setq mstart   aindex)
                    (setq mstrings saved-mstrings)

                    (when (match string pattern index end)
                      (return-from match t) )

                    (decf index) ) ) )) )) )
    )
    ;;
    ;; logical-match-component-p
    ;;
    (etypecase pattern
      ((or null (eql :wild))
        (values t (list component)) )

      ((eql :unspecific)
        (values (eq component :unspecific) '()) )

      (string
        (values (and (stringp component)
                     (string-equal component pattern) )
                '() ) )

      (cons
        (if (and (stringp component)
                 (match component pattern 0 (length component)) )
            (values t (nreverse mstrings))
          (values nil '()) ) ))) ) )


;;;; logical-match-directory-p
;;;
;;; Syntax:
;;;   logical-match-directory-p component pattern => boolean, matched-strings
;;;
;;; Arguments and Values:
;;;   component a string, list, :wild, nil, :unspecific
;;;   pattern   a string, list, :wild, nil, :unspecific
;;;
;;; Called by:
;;;  pathname-match-p-using-host
;
(defun logical-match-directory-p (component pattern)
  (let ((mstrings '())
        (mstart   nil) )
    (labels (
      ;; end-wild-match
      (end-wild-match (string mend)
        (when mstart
          (push (subseq string mstart mend) mstrings) )
        (setq mstart nil) )

      ;; match
      (match (string pattern index end)
        (loop
          (when (null pattern)
            (when (= index end)
              (end-wild-match string index)
              (return t) )
            (return nil) )

          (let ((pat (pop pattern)))
            (etypecase pat
              (string
                (when (= index end)
                  (return nil) )

                (end-wild-match string index)
                (unless (logical-match-component-p (nth index string) pat)
                  (return nil) )
                (incf index) )

              (cons
                (when (= index end)
                  (return nil) )

                (end-wild-match string index)
                (multiple-value-bind (match-p matched)
                    (logical-match-component-p (nth index string) pat)
                  (unless match-p
                    (return nil) )
                  (push matched mstrings)
                  (incf index) ) )

              ((eql :wild)
                (when (= index end)
                  (return nil) )

                (end-wild-match string index)
                (push (subseq string index (1+ index)) mstrings)
                (incf index) )

              ((eql :wild-inferiors)
                (end-wild-match string index)
                (when (= index end)
                  (push '() mstrings)
                  (return t) )
                (let ((aindex         index)
                      (saved-mstrings mstrings) )
                  (setq index end)
                  (loop
                    (when (< index aindex)
                      (return-from match nil) )

                    (setq mstart   aindex)
                    (setq mstrings saved-mstrings)

                    (when (match string pattern index end)
                      (return-from match t) )

                    (decf index) ) ) )) )) )
    )
    ;;
    ;; logical-match-directory-p
    ;;
    (if (not (consp component))
        (values (eq component pattern) '())
      (etypecase pattern
        ((or null (eql :wild) (eql :unspecific))
          (values t (list (rest component))) )

        (string
          (values (string-equal component pattern) '()) )

        (cons
          (cond
            ((not (eq (pop component) (pop pattern)))
              (values nil '()) )
            ((match component pattern 0 (length component))
              (values t (nreverse mstrings)) )
            (t
              (values nil '()) )) ))) ) ) )


;;;; logical-parse-component
;;;
;;; Called by:
;;;  parse-directory-component
;;;  parse-name-component
;;;  parse-type-component
;;;
;;; Description:
;;;  Parses pathanme component for logical-pathname.
;
(defun logical-parse-component (host name component)
  (cond
    ((eq nil         name) name)
    ((eq :wild       name) name)
    ((eq :unspecific name) name)
    ((not (stringp name))
      (error 'bad-pathname-component
             :host      host
             :component component
             :datum     name ) )
    ((string= "*" name) :wild)
    (t
      (let ((elts   '())
            (last   0)
            (length (length name)) )
        (dotimes (index length)
          (let ((char (char name index)))
            (cond
              ((or (< (char-code char) #x20)
                   (position char +logical-reserved-chars+) )
                (error 'bad-pathname-character
                       :host        host
                       :component   component
                       :datum       char ) )

              ((eql #\* char)
                (unless (= last index)
                  (push (subseq name last index) elts)
                (push ':wild-any elts)
                (setq last (1+ index)) ) )) ) )

        (if (null elts)
            name
          (progn
            (unless (= last length)
              (push (subseq name last length) elts) )
            (nreverse elts) )) ) )) )


;;;; logical-translate-component
;;;
;;; Syntax:
;;;   translate-component-using-host host elts parts => component
;;;
;;; Arguments and Values:
;;;   host      a logical-host.
;;;   elts      a list of string, or :wild.
;;;   parts     a list of string.
;;;   component a string.
;;;
;;; Called by:
;;;  translate-component-using-host
;;;  translate-directory-using-host
;
(defun logical-translate-component (pats parts)
    (declare (type cons pats))
    (declare (type list parts))
  (let ((elts '()))
    (dolist (pat pats)
      (when (eq :wild pat)
        (setq pat (pop parts)) )
      (push pat elts) )
    (setq elts (nreverse elts))
    (with-output-to-string (stream)
      (dolist (pat elts)
        (when pat (write-string pat stream)) )) ) )


;;;; logical-unparse-directory
;;;
;;; Called by:
;;;   internal-directory-namestring
;;;   internal-enough-namestring
;
(defun logical-unparse-directory (directory stream)
    (declare (values ext:unspecified))
    (declare (type stream stream))
  (when (consp directory)
    (ecase (pop directory)
      (:absolute)
      (:relative  (write-char #\; stream)) )
    (dolist (elt directory)
      (case elt
       (:wild-inferiors
         (write-string "**" stream) )
       (otherwise
         (logical-unparse-element elt stream) ))
       (write-char #\; stream) )) )


;;;; logical-unparse-element
;;;
;;; Called by:
;;;     internal-directory-namestring
;;;     internal-file-namestring
;
(defun logical-unparse-element (elt stream)
    (declare (values ext:unspecified))
    (declare (type stream stream))
  (cond
    ((stringp elt)    (write-string elt stream))
    ((eq :wild elt)   (write-char #\* stream))
    ((consp elt)
      (dolist (elt-2 elt)
        (cond
          ((stringp elt-2) (write-string elt-2 stream))
          ((eq :wild  elt-2)  (write-char #\* stream)) ) ))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Methods
;;;;

;;;; internal-directory-namestring
;
(defmethod internal-directory-namestring ((pathname logical-pathname) stream)
    (declare (type logical-pathname pathname))
    (declare (type stream stream))
  (logical-unparse-directory (ref pathname directory pathname) stream) )


;;;; internal-enough-namestring
;;;
;;; Note: type name must be followed by name and dot(.).
;
(defmethod internal-enough-namestring ((pathname logical-pathname)
                                       defaults stream )
    (declare (type logical-pathname pathname))
    (declare (type pathname defaults))
    (declare (type stream stream))

  (setq defaults (translate-logical-pathname defaults))

  (unless (eq (ref pathname host pathname) (ref pathname host defaults))
    (return-from internal-enough-namestring (namestring pathname)) )

  ;; directory
  ;;
  (let ((dir-1 (ref pathname directory pathname)))
    (when (consp dir-1)
      (let ((dir-2 (ref pathname directory defaults)))
        (if (not (consp dir-2))
            (logical-unparse-directory dir-1 stream)
          (let ((scan-1 dir-1)
                (scan-2 dir-2) )
            (loop
              (unless (equal (car scan-1) (car scan-2))
                (cond
                  ((not (null scan-2))
                    ;; dir-2 is longer than dir-1.
                    (setq scan-1 dir-1) )
                  ((not (eq scan-1 dir-1))
                    (setq scan-1 (cons :relative scan-1)) ))
                (logical-unparse-directory scan-1 stream)
                (return) )

              ;; dir-1 and dir-2 are equal.
              (when (null scan-1)
                (return) )

              ;; go next element
              (setq scan-1 (cdr scan-1))
              (setq scan-2 (cdr scan-2)) ) )) )) )

  ;; name and type
  ;;
  (let* ((name-1      (ref pathname name pathname))
         (name-2      (ref pathname name defaults))
         (same-name-p (equal name-1 name-2)) )
    (unless same-name-p
      (logical-unparse-element name-1 stream) )

    (let ((type-1 (ref pathname type pathname)))
      (unless (or (eq nil type-1) (eq :unspecific type-1))
        (let ((type-2 (ref pathname type defaults)))
          (unless (equal type-1 type-2)
            (when same-name-p
              (logical-unparse-element name-1 stream) )
            (write-char #\. stream)
            (logical-unparse-element type-1 stream)) )) ) ) )


;;;; internal-file-namestring
;
(defmethod internal-file-namestring ((pathname logical-pathname) stream)
    (declare (type stream stream))
  (logical-unparse-element (ref pathname name pathname) stream)

  (let ((type (ref pathname type pathname)))
    (when type
      (write-char #\. stream)
      (logical-unparse-element type stream) ) )

  (let ((version (ref pathname version pathname)))
    (cond
      ((eq :wild version)
        (write-char #\. stream)
        (write-char #\* stream) )

      ((eq :newest version))

      ((integerp version)
        (write-char #\. stream)
        (print-object version stream) )) ) )


;;;; internal-namestring
;;;
;;; Called by:
;;;     print-pathname
;
(defmethod internal-namestring ((pathname logical-pathname) stream)
    (check-type stream stream)
  (let ((host (ref pathname host pathname)))
    (write-string (ref basic-host name host) stream)
    (write-char #\: stream) )
  (internal-directory-namestring pathname stream)
  (internal-file-namestring pathname stream) )


;;;; make-pathname-using-host
;
(defmethod make-pathname-using-host ((host logical-host)
                                     device directory name type version )
    (declare (ignore device))

  (labels (
    (upcase (elt)
      (typecase elt
        (string
          (string-upcase elt) )
        (cons
          (mapcar #'upcase elt) )
        (otherwise
          elt )) )
    )
    ;;
    ;; make-pathname-using-host
    ;;
    (let ((pathname (make-instance 'logical-pathname)))
      (setf (ref pathname host pathname)      host)
      (setf (ref pathname device pathname)    :unspecific)
      (setf (ref pathname directory pathname) (upcase directory))
      (setf (ref pathname name pathname)      (upcase name))
      (setf (ref pathname type pathname)      (upcase type))
      (setf (ref pathname version pathname)   version)

      pathname ) ) )


;;;; parse-device-component
;;;
;;; Called by:
;;;  make-pathname
;;;
;;; Description:
;;;  Parses wild name.
;
(defmethod parse-device-component ((host logical-host) device)
  (unless device
    (setq device :unspecific) )

  (unless (eq :unspecific device)
    (error 'bad-pathname-component
           :host      host
           :component :device
           :datum     device ))
  device )


;;;; parse-directory-component
;;;
;;; Called by:
;;;  make-pathname
;;;
;;; Description:
;;;  Parses wild name.
;
(defmethod parse-directory-component ((host logical-host) directory)
  (cond
    ((not directory) directory)
    ((eq :wild directory)
      (list :absolute :wild-inferiors) )
    ((not (consp directory))
      (error 'bad-pathname-component
             :host      host
             :component :directory
             :datum     directory ) )
    ((or (eq :absolute (first directory))
         (eq :relative (first directory)) )
      (let ((elts '()))
        (dolist (elt (rest directory))
          (typecase elt
            (string
              (push (logical-parse-component host elt :directory) elts) )

            ((eql :wild-inferiors)
              (unless elts
                (error 'bad-pathname-component
                       :host        host
                       :component   :directory
                       :datum       directory ))
              (push elt elts) )

            ((member :back :up :wild)
              (push elt elts) )

            (otherwise
              (error 'bad-pathname-component
                     :host          :host
                     :component     :directory
                     :datum         directory ) )) )
        (cons (first directory) (nreverse elts)) ) )
    (t
      (error 'bad-pathname-component
             :host      host
             :component :directory
             :datum     directory ) )) )


;;;; parse-name-component
;;;
;;; Called by:
;;;     make-pathname
;;;
;;; Description:
;;;  Parses wild name.
;
(defmethod parse-name-component ((host logical-host) name)
  (logical-parse-component host name :name) )


;;;; parse-namestring-using-host
;;;
;;; Syntax:
;;;     parse-namestring-using-host hsot string start end => pathname, index
;;;
;;; Called by:
;;;     parse-namestring
;;;
;
(defmethod parse-namestring-using-host ((host logical-host) string start end)
    (declare (type string string))
    (declare (type sequence-index start))
    (declare (type sequence-end end))
    (declare (values logical-pathname sequence-index))
  (multiple-value-bind (bstring offset end) (string-data string start end)
    (let* ((pathname nil)
           (index offset) )

      (block parse
        (let ((directory '())
              (name      nil)
              (type      nil)
              (version   nil) )

          ;; Parse directories
          ;;
          (let* ((dir-top-elt
                   (cond
                     ((= end index) (return-from parse))
                     ((eql (schar bstring index) #\;) (incf index) :relative)
                     (t :absolute) ) )
                 (end
                   (position #\; bstring
                             :start    index
                             :end      end
                             :from-end t )))
            (loop
              for jndex from index below (or end index)
              for char = (schar bstring jndex)
              with dir = nil
              with last = index
              finally
                (unless (= last jndex)
                  (let ((str (subseq bstring last jndex)))
                       (if dir
                           (push str dir)
                         (setq dir str) ) ))
                (when dir (push dir directory))
                (when end (setq index (1+ end)))
              do
                (case char
                  ((#\*)
                    (case (first dir)
                      (:wild
                        (unless (and (null (rest dir)) (= last jndex))
                          (setq index jndex)
                          (return-from parse) )
                        (setf (first dir) :wild-inferiors) )

                      (:wild-inferiors
                        (setq index jndex)
                        (return-from parse) )

                      (otherwise
                        (unless (= last jndex)
                          (push (subseq bstring last jndex) dir) )
                        (push :wild dir) ))
                    (setq last (1+ jndex)) )

                  ((#\;)
                    (cond
                      ((null dir)
                        (when (= last jndex)
                          (setq index jndex)
                          (return-from parse) )
                        (setq dir (subseq bstring last jndex)) )

                      ((/= last index)
                        (push (subseq bstring last jndex) dir) ))

                    (push dir directory)
                    (setq dir nil)
                    (setq last (1+ jndex)) )

                  (otherwise
                    (when (eq :wild-inferiors (first dir))
                      (setq index jndex)
                      (return-from parse) )
                    (unless (or (alphanumericp char) (eql #\- char))
                      (setq index jndex)
                      (return-from parse) ) )))

            (loop for scan on directory 
                  for dir = (first scan) do
              (when (consp dir)
                (setf (first scan)
                      (if (null (rest dir))
                          (first dir)
                        (nreverse dir)) )))

            (when directory
              (setq directory (cons dir-top-elt (nreverse directory))) ) )

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;
          ;; Parser name
          ;;
          (loop
            for jndex from index below end
            for char   = (schar bstring jndex)
            with last  = index
            finally
              (unless (= last jndex)
                (let ((str (subseq bstring last jndex)))
                  (if name
                      (push str name)
                    (setq name str) ) ))
              (setq index jndex)
            do
              (case char
                (#\-)
                ((#\*)
                  (unless (= last jndex)
                    (push (subseq bstring last jndex) name) )

                  (when (eq :wild (first name))
                    (setq index jndex)
                    (return-from parse) )

                  (push :wild name)
                  (setq last (1+ jndex)) )

                ((#\.)
                  (if (= last jndex)
                      (when (null name)
                        (setq index jndex)
                        (return-from parse) )
                    (let ((str (subseq bstring last jndex)))
                      (if name
                          (push str name)
                        (setq name str) ) ))

                  (setq index (1+ jndex))
                  (return) )

                (otherwise
                  (unless (alphanumericp char)
                    (setq index jndex)
                    (return-from parse)) )) )
          (cond
            ((not (consp name)))
            ((null (rest name))
              (setq name (first name)) )
            (t
              (setq name (nreverse name)) ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;
          ;; Parser type
          ;;
          (loop
            for jndex from index below end
            for char   = (schar bstring jndex)
            with last  = index
            finally
              (unless (= last jndex)
                (let ((str (subseq bstring last jndex)))
                  (if type
                      (push str type)
                    (setq type str) ) ))
              (setq index jndex)
            do
              (case char
                (#\-)
                ((#\*)
                  (unless (= last jndex)
                    (push (subseq bstring last jndex) type) )

                  (when (eq :wild (first type))
                    (setq index jndex)
                    (return-from parse) )

                  (push :wild type)
                  (setq last (1+ jndex)) )

                ((#\.)
                  (if (= last jndex)
                      (when (null type)
                        (setq index jndex)
                        (return-from parse) )
                    (let ((str (subseq bstring last jndex)))
                      (if type
                          (push str type)
                        (setq type str) ) ))

                  (setq index (1+ jndex))
                  (return) )

                (otherwise
                  (unless (alphanumericp char)
                    (setq index jndex)
                    (return-from parse)) )) )
          (cond
            ((not (consp type)))
            ((null (rest type))
              (setq type (first type)) )
            (t
              (setq type (nreverse type)) ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;
          ;; version
          ;;
          (cond
            ((= end index))   ; no version

            ((and (= end (1+ index)) (eql #\* (schar bstring index)))
              (setq version :wild) )

            ((string-equal "newest" bstring :start2 index)
              (setq version :newest) )

            (t
              (setq version 0)

              (loop for jndex from index below end
                    for char = (schar bstring jndex) do

                (unless (digit-char-p char)
                  (setq index jndex)
                  (return-from parse) )

                (setq version (+ (digit-char-p char) (* 10 version))) )

              (unless (plusp version)
                (setq index end)
                (return-from parse) ) ))

          (setq index end)

          ;; Make pathname
          ;;
          (setq pathname
            (make-pathname-using-host
              host :unspecific directory name type version )) ))

      ;; Return values
      ;;
      (values pathname (+ (- index offset) start)) ) ) )


;;;; parse-type-component
;;;
;;; Called by:
;;;  make-pathname
;;;
;;; Description:
;;;  Parses wild name.
;
(defmethod parse-type-component ((host logical-host) type)
  (logical-parse-component host type :type) )


;;;; parse-version-component
;;;
;;; Called by:
;;;  make-pathname
;;;
;;; Description:
;;;  Parses wild name.
;
(defmethod parse-version-component ((host logical-host) version)
  (cond
    ((not version))
    ((and (integerp version) (plusp version)))
    ((eq :newest version))
    (t
      (error 'bad-pathname-component
             :host      host
             :component :version
             :datum     version )) )
  version )


;;;; pathname-equal-using-host
;;;
;;; Called by:
;;;  pathname-equal
;
(defmethod pathname-equal-using-host ((host logical-host) x y)
  (and (eq    (ref pathname host     x) (ref pathname host     y))
       (equal (pathname-directory x) (pathname-directory y))
       (equal (pathname-name      x) (pathname-name      y))
       (equal (pathname-type      x) (pathname-type      y))
       (eql   (pathname-version   x) (pathname-version   y)) ) )


;;;; pathname-match-p-using-host
;;;
;;; Called by:
;;;  pathname-match-p
;
(defmethod pathname-match-p-using-host ((host logical-host) pathname wildcard)
    (check-type wildcard logical-pathname)
  (and
    (eq host (ref pathname host wildcard))

    (logical-match-directory-p (ref pathname directory pathname)
                               (ref pathname directory wildcard) )

    (logical-match-component-p (ref pathname name pathname)
                               (ref pathname name wildcard) )

    (logical-match-component-p (ref pathname type pathname)
                               (ref pathname type wildcard) )) )


;;;; translate-component-using-host
;;;
;;; Syntax:
;;;   translate-component-using-host host elts parts => component
;;;
;;; Arguments and Values:
;;;   host      a logical-host.
;;;   elts      a list of string, or :wild.
;;;   parts     a list of string.
;;;   component a string.
;;;
;;; Called by:
;;;  translate-pathname-using-host
;
(defmethod translate-component-using-host ((host logical-host) elts parts)
  (logical-translate-component elts parts) )


;;;; translate-directory-using-host
;;;
;;; Called by:
;;;   translate-pathname-using-host
;;;
;;; Description:
;;;  Replaces wildcard elements with parts.
;;;
;;; Note: When replaces :wild with part from :wild-inferiors, the result
;;; takes only the first element only.
;
(defmethod translate-directory-using-host ((host logical-host) pat-dir parts)
  (let ((dir '()))
    (dolist (pat pat-dir)
      (cond
        ((consp pat)
          (push (logical-translate-component pat (pop parts)) dir) )

        ((eq :wild-inferiors pat)
          (setq dir (nreconc (pop parts) dir)) )

        ((eq :wild pat)
          (let ((part (pop parts)))
            (when (consp part)
              (setq part (first part)) )
            (push part dir) ) )

        (t
          (push pat dir) )) )
    (nreverse dir) ) )


;;;; translate-pathname-using-host
;;;
;;; Note: signal pathname-not-matched when pathname doesn't match to
;;; from-wildcard.
;
(defmethod translate-pathname-using-host
        ((host-1 logical-host) pathname from-wildcard to-wildcard)
    (declare (type pathname from-wildcard))
    (declare (type pathname to-wildcard))
  (let ((dir-1     (ref pathname directory pathname))
        (name-1    (ref pathname name      pathname))
        (type-1    (ref pathname type      pathname))
        (version-1 (ref pathname version   pathname))
        dir-parts
        name-parts
        type-parts )

    ;; host
    ;;
    (unless (eq host-1 (ref pathname host from-wildcard))
      (pathname-not-matched pathname from-wildcard) )

    ;; directory
    ;;
    (multiple-value-bind (match-p matched)
        (logical-match-directory-p dir-1 (ref pathname directory from-wildcard))
      (unless match-p
        (pathname-not-matched pathname from-wildcard) )

      (setq dir-parts matched) )

    ;; name
    ;;
    (multiple-value-bind (match-p matched)
        (logical-match-component-p name-1 (ref pathname name from-wildcard))
      (unless match-p
        (pathname-not-matched pathname from-wildcard) )

      (setq name-parts matched) )

    ;; type
    ;;
    (multiple-value-bind (match-p matched)
        (logical-match-component-p type-1 (ref pathname type from-wildcard))
      (unless match-p
        (pathname-not-matched pathname from-wildcard) )

      (setq type-parts matched) )

    ;; Translate
    ;;
    (let* ((host-2    (ref pathname host      to-wildcard))
           (device-2  (ref pathname device    to-wildcard))
           (dir-2     (ref pathname directory to-wildcard))
           (name-2    (ref pathname name      to-wildcard))
           (type-2    (ref pathname type      to-wildcard))
           (version-2 (ref pathname version   to-wildcard))
           (convert-p (not (eq (ref basic-host customary-case host-1)
                               (ref basic-host customary-case host-2) ))) )
      ;; directory
      ;;
      (cond
        ((or (eq :wild dir-2) (eq nil dir-2))
          (when convert-p
            (setq dir-1 (host-convert-case host-1 dir-1))
            (setq dir-1 (host-convert-case host-2 dir-1)) )

          (setq dir-2 dir-1) )

        ((consp dir-2)
          (when convert-p
            (setq dir-parts (host-convert-case host-1 dir-parts))
            (setq dir-parts (host-convert-case host-2 dir-parts)) )

          (setq dir-2
            (translate-directory-using-host host-2 dir-2 dir-parts) ) ))

      ;; name
      ;;
      (cond
        ((or (eq :wild name-2) (eq nil name-2))
          (when convert-p
            (setq name-1 (host-convert-case host-1 name-1))
            (setq name-1 (host-convert-case host-2 name-1)) )

          (setq name-2 name-1) )

        ((consp name-2)
          (when convert-p
            (setq name-parts (host-convert-case host-1 name-parts))
            (setq name-parts (host-convert-case host-2 name-parts)) )

          (setq name-2
            (translate-component-using-host host-2 name-2 name-parts) ) ))

      ;; type
      ;;
      (cond
        ((or (eq :wild type-2) (eq nil type-2))
          (when convert-p
            (setq type-1 (host-convert-case host-1 type-1))
            (setq type-1 (host-convert-case host-2 type-1)) )

          (setq type-2 type-1) )

        ((consp type-2)
          (when convert-p
            (setq type-parts (host-convert-case host-1 type-parts))
            (setq type-parts (host-convert-case host-2 type-parts)) )

          (setq type-2
            (translate-component-using-host host-2 type-2 type-parts) ) ))

      ;; version
      ;;
      (when (or (eq :wild version-2) (eq nil version-2))
        (setq version-2 version-1) )

      ;; Result
      ;;
      (make-pathname-using-host host-2
                                device-2
                                dir-2
                                name-2
                                type-2
                                version-2 ) ) ) )


;;;; truename-using-host
;;;
;;; Called by:
;;;  truename
;
(defmethod truename-using-host ((host logical-host) pathname)
  (truename (translate-logical-pathname pathname)) )


;;;; wild-pathname-p-using-host
;;;
;;; Called by:
;;;  wild-pathname-p
;
(defmethod wild-pathname-p-using-host ((host logical-host) pathname field-key)
  (ecase field-key
    ((:type)
      (or (eq :wild   (ref pathname type pathname))
          (consp (ref pathname type pathname)) ) )

    ((:name)
      (or (eq :wild   (ref pathname name pathname))
          (consp (ref pathname name pathname)) ) )

    ((:device) nil)
    ((:version) nil)

    ((:directory)
       (let ((directory (ref pathname directory pathname)))
         (and (consp directory)
              (dolist (elt directory nil)
                (when (or (eq :wild elt)
                          (eq :wild-inferiors elt)
                          (consp elt) )
                  (return t) ) )) ) )) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;;

;;;; 19.4.7 load-logical-pathname-translations
;
(defun cl:load-logical-pathname-translations (hoststr)
    (check-type hoststr string)
  (let* ((host (find-pathname-host hoststr nil))
          (xlations
            (etypecase host
              (null '())
              (logical-host (ref logical-host translations host)) ) ))

    (when (load (format nil "SYS:SITE;~A.TRANSLATIONS" hoststr)
               :if-does-not-exist nil )
      (if (not host)
          (find-pathname-host host nil)
        (not (eq xlations (ref logical-host translations host))) )) ) )


;;;; 19.4.9 logical-pathname
;
(defun cl:logical-pathname (pathspec)
  (let ((pathname (pathname pathspec)))
    (unless (typep pathname 'logical-pathname)
      (error 'type-error
             :datum             (ref pathname host pathname)
             ::expected-type    'logical-host ))
    pathname ) )


;;;; 19.4.8 logical-pathname-translations
;
(defun cl:logical-pathname-translations (hostspec)
  (let ((host (if (stringp hostspec)
                  (find-pathname-host hostspec)
                hostspec )) )
        (check-type host logical-host)
    (ref logical-host translations host) ) )


;;;; 19.4.8 (setf logical-pathname-translations)
(defun (setf cl:logical-pathname-translations) (translations hostspec)
  (let ((host
          (etypecase hostspec
            (logical-host hostspec)
            (string (intern-logical-pathname-host hostspec)) ) ))

    ;; Validates translations
    ;;
    (loop for xlation in translations do
      (unless (eql (safe-list-length xlation) 2)
        (error 'invalid-logical-pathname-translation
               :host            host
               :translation     xlation ))

      collect
        (let ((from (first  xlation))
              (to   (second xlation)) )
          (etypecase from
            (logical-pathname)
            (string
              (setq from (parse-namestring from host)) ))

          (unless (eq (ref pathname host from) host)
            (error 'invalid-logical-pathname-translation
                   :host            host
                   :translation     xlation ))

          (etypecase to
            (pathname)
            (string (setq to (pathname to))) )

          (list from to) ) into xlations

       finally
        (setf (ref logical-host translations host) xlations) )

    translations ) )


;;;; 19.4.15 translate-logical-pathname
(defun cl:translate-logical-pathname (pathname &key)
  (labels (
    (translate-logical-pathname-aux (pathname count)
      (when (>= count 20)
         (error 'translation-limits-exceeds
                :pathname pathname ) )

      (unless (typep pathname 'logical-pathname)
         (return-from translate-logical-pathname-aux pathname) )

       (incf count)
       (let* ((host    (ref pathname host pathname))
              (xlation (assoc pathname 
                             (logical-pathname-translations host)
                             :test #'pathname-match-p )) )

            ;; No translation matches.
            ;;
           (unless xlation
             (error 'no-translation-matches
                    :pathname pathname ))

       (translate-logical-pathname-aux
          (translate-pathname pathname (first xlation) (second xlation))
          count ) ))
    )
    ;;
    ;; translate-logical-pathname
    ;;
    (translate-logical-pathname-aux (pathname pathname) 0) ) )
