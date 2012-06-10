;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 23 Reader - Readtable
;;; runtime/r23-readtable.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r23-readtable.lisp#4 $
;;;
;;; Description:
;;;  This file implements following functions:
;;;     conform-to-case                 internal
;;;     copy-dispatch-table             internal
;;;     ensure-readtable                internal
;;;     error-read-invalid-char         internal
;;;     funcall-with-standard-io-syntax internal
;;;     get-character-syntax            internal
;;;     read-token                      internal
;;;     reader-eof-error                internal
;;;     simple-reader-error             interanl
;;;
;;; Public Functions:
;;;     copy-readtable                  23.2.2
;;;     get-dispatch-macro-character    23.2.9
;;;     get-macro-character             23.2.10
;;;     make-dispatch-macro-character   23.2.3
;;;     read                            23.2.4
;;;     read-delimited-list             23.2.5
;;;     read-from-string                23.2.6
;;;     read-preserving-whitespace      23.2.4
;;;     readtable-case                  23.2.7  boot
;;;     readtablep                      23.2.8  boot
;;;     set-dispatch-macro-character    23.2.9
;;;     set-macro-character             23.2.10
;;;     set-syntax-from-char            23.2.11
;;;
;;;
;;; Reatable Object:
;;;  A readtable object has three slots:
;;;     case
;;;         A kewyord which denotes case sensivity mode: :upcase, :downcase,
;;;         :preserve, and :invert.
;;;     vector
;;;         Mapping from character (0 to 127) to lexical attributes.
;;;     char-table
;;;         A hash-table for mapping character to lexical attributes.
;;;           1. fixnum                     -- for non-macro character
;;;           2. (cons fixnum function)     -- for terminating-macro char.
;;;           3. (cons fixnum hash-table)   -- for non-terminating-macro char.
;;;
;;;  A hash-table of non-terminating-macro character maps character to
;;;  dispatch macro function which takes three arguments, stream, subchar and
;;;  arg.
;
(in-package :si)

;; BUGBUG: How to handle type and trait values


;;;; copy-dispatch-table
;;;
;;; Called by:
;;;     copy-readtable
;;;     set-syntax-from-char
;;;
;;; Description:
;;;  Makes copy of specified dispatch table and returns it.
;
(defun copy-dispatch-table (src)
  (loop
    with new = (make-hash-table :size (hash-table-size src)
                            :test #'eq )
    for char being each hash-key of src using (hash-value fn) do
      (setf (gethash/eq char new) fn)
    finally (return new) ) )


;;;; dispatch-char-reader
;;;
;;; For: #
;;;
;;; Called by:
;;;  reader
;;;  get-macro-character
;;;
;;; Description:
;;;  A reader macro function for dispatching character, e.g. Sharpsign(#).
;
(defun dispatch-char-reader (stream disp-char readtable)
    (declare (type stream stream))
    (declare (type character disp-char))
  (multiple-value-bind (sub-char arg)
      (loop for char = (fast-read-char stream t)
            with ndigits = 0
            with n = 0
            do (let ((digit (digit-char-p char 10)))
                 (unless digit
                   (when (zerop ndigits) (setq n nil))
                   (return (values char n)) )
                 (incf ndigits)
                 (setq n (+ (* n 10) digit)) ))
    (let ((reader (get-dispatch-macro-character disp-char sub-char readtable)))
      (unless reader
        (simple-reader-error stream
          "No dispatch function is defined for ~C~C"
          disp-char sub-char ))
      (funcall reader stream sub-char arg) ) ) )


;;;; ensure-readtable
;
(defun ensure-readtable (readtable)
  (when (not readtable) (setq readtable *standard-readtable*))
  (check-type readtable readtable)
  readtable )


;;;; Call Function With Standard I/O Syntax
;;;
;;; Called by:
;;;     with-standard-io-syntax
;;;
;
(defun funcall-with-standard-io-syntax (function)
  (let ((*package*                      (find-package :cl-user))
        (*print-array*                  t)
        (*print-base*                   10)
        (*print-case*                   :upcase)
        (*print-circle*                 nil)
        (*print-escape*                 t)
        (*print-gensym*                 t)
        (*print-length*                 nil)
        (*print-level*                  nil)
        (*print-lines*                  nil)
        (*print-miser-width*            nil)
        (*print-pprint-dispatch*        nil)
        (*print-pretty*                 nil)
        (*print-radix*                  nil)
        (*print-readably*               nil)
        (*print-right-margin*           nil)
        ;;
        (*read-base*                    10)
        (*read-default-float-format*    'single-float)
        (*read-eval*                    t)
        (*read-suppress*                nil)
        (*readtable*                    *standard-readtable*) )
    (funcall function) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Function
;;;;

;;;; 23.2.2 copy-readtable
(defun cl:copy-readtable (&optional from to)
  (labels (
    ;; make-readtable
    (make-readtable ()
      (let ((to (.allocate-record #.(class-description 'readtable))))
        (setf (ref readtable vector to) (make-array 128))
        (setf (ref readtable table to)  (make-hash-table :test #'eq))
        to ) )

    ;; copy
    (copy (to from)
      (setf (ref readtable case to) (ref readtable case from))

      (loop
        with fr-vec = (ref readtable vector from)
        with to-vec = (ref readtable vector to)
        for i below 128
        for info = (svref fr-vec i) do
          (setf (svref to-vec i)
             (if (consp info)
                 (cons (car info) (cdr info))
               info )))

      (let ((from-char-table  (ref readtable table from))
            (to-char-table    (ref readtable table to)) )
        (clrhash to-char-table)
        (loop for char being each hash-key of from-char-table
                       using (hash-value info)
              do  (when (consp info)
                    (setq info (cons (car info)
                                     (if (hash-table-p (cdr info))
                                         (copy-dispatch-table (cdr info))
                                     (cdr info) ))))
                  (setf (gethash char to-char-table) info) ) ) )
    )
    ;;
    (let ((from (ensure-readtable from))
          (to   (or to (make-readtable))) )
        (check-type to readtable)
      (unless (eq from to) (copy to from))
      to ) ) )


;;;; 23.2.9 get-dispatch-macro-character
(defun cl:get-dispatch-macro-character
            (disp-char sub-char &optional readtable)
    (declare (values (or symbol function)))
  (let ((readtable (ensure-readtable readtable))
        (disp-char (char-upcase disp-char))
        (sub-char  (char-upcase sub-char)) )
    (let ((info (get-char-info disp-char readtable)))
      (if (and (consp info) (hash-table-p (cdr info)))
          (values (gethash/eq sub-char (cdr info)))
        (error 'not-dispatch-macro-character
               :readtable readtable
               :character disp-char ) ) ) ) )


;;;; 23.2.10 get-macro-character
(defun cl:get-macro-character (char &optional readtable)
    (declare (values function-designator t))
    (check-type char character)
  (let* ((readtable (ensure-readtable readtable))
         (char (char-upcase char))
         (info (get-char-info char readtable)) )
    (cond
      ((not (consp info))
        (values nil nil) )

      ((hash-table-p (cdr info))
        (values #'(lambda (stream char)
                    (funcall #'dispatch-char-reader stream char readtable) )
                (eql (logand (car info) #xF) 2) ) )
      (t
        (values (cdr info) (eql (logand (car info) #xF) 2)) )) ) )


;;;; 23.2.3 make-dispatch-macro-character
(defun cl:make-dispatch-macro-character
            (disp-char &optional non-terminating-p readtable)
  (let* ((readtable (ensure-readtable readtable))
         (info (get-char-info disp-char readtable)) )
    (cond
      ((not (consp info))
        (let ((info (cons (if non-terminating-p 2 3)
                         (make-hash-table :test 'eq) )))
          (setf (get-char-info (char-upcase   disp-char) readtable) info)
          (setf (get-char-info (char-downcase disp-char) readtable) info) ) )
      ((not (hash-table-p (cdr info)))
        (setf (cdr info) (make-hash-table :test #'eq)) )) )
  t )


;;;; 23.2.9 set-dispatch-macro-character
(defun cl:set-dispatch-macro-character
            (disp-char sub-char function &optional readtable)
    (declare (type character disp-char sub-char))
    (declare (type function-designator function))
    (declare (values (eql t)))
  (let ((readtable (ensure-readtable readtable)))
    (when (char<= #\0 sub-char #\9)
      (error 'invalid-dispatch-sub-char :character sub-char) )

    (let ((info (get-char-info disp-char readtable)))
      (unless (and (consp info) (hash-table-p (cdr info)))
        (error 'not-dispatch-macro-character
               :readtable readtable
               :character disp-char ) )
      (if function
          (progn
            (setf (gethash (char-upcase   sub-char) (cdr info)) function)
            (setf (gethash (char-downcase sub-char) (cdr info)) function) )
        (progn
          (remhash (char-upcase   sub-char) (cdr info))
          (remhash (char-downcase sub-char) (cdr info)) )) ) )
   t )


;;;; 23.2.7 readtable-case
(defun cl:readtable-case (rdt)
    (check-type rdt readtable)
  (ref readtable case rdt) )


(defun (setf cl:readtable-case) (case rdt)
    (check-type case (member :upcase :downcase :preserve :invert))
    (check-type rdt readtable)
  (setf (ref readtable case rdt) case) )


;;;; 23.2.10 set-macro-character, get-macro-character
(defun cl:set-macro-character
            (char new-function &optional non-terminating-p readtable)
    (check-type char character)
    (check-type new-function (or symbol function))
  (let* ((readtable (ensure-readtable readtable))
         (info (get-char-info char readtable)) )
      (if (consp info)
          ;; BUGBUG: TYPE_NMacro = 2
          ;; BUGBUG: TYPE_TMacro = 3
          (setf (car info) (if non-terminating-p 2 3)
                (cdr info) new-function )
        (let ((info (cons (if non-terminating-p 2 3) new-function)))
          (setf (get-char-info (char-upcase   char) readtable) info)
          (setf (get-char-info (char-downcase char) readtable) info) ))
      t ) )


;;;; 23.2.11 set-syntax-from-char
(defun cl:set-syntax-from-char
            (to-char from-char &optional to from)
    (check-type to-char   character)
    (check-type from-char character)
    (check-type to readtable)
  (let ((info (get-char-info from-char (ensure-readtable from))))
    (cond
      ((not (consp info))
        ;; We don't copy constituent traits.
        (let ((to-info (get-char-info to-char to)))
          (when (consp to-info) (setq to-info (car to-info)))
          (setf (get-char-info to-char to)
              (logior (logand to-info #x1FFF00) (logand info #xF)) ) ) )
      ((not (hash-table-p (cdr info)))
        ;; Macro character
        (setf (get-char-info to-char to)
                    (cons (car info) (cdr info)) ) )
      (t
        ;; Dispatch macro character
        (setf (get-char-info to-char to)
                (cons (car info) (copy-dispatch-table (cdr info))) ) ))
    t ) )
