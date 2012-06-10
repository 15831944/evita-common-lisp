;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Runtime - 22 Printers
;;; runtime/r22-defs-printer.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r22-defs-printer.lisp#5 $
;;;
;;; Internal API:
;;;     float-marker
;;;     float-to-digits
;;;     ieee-decone-float
;;;     print-unreadable-object-function
;;;     write-chars
;;;     write-object
;;;
;
(in-package :si)

;;;; *printer-level*
;;;
;;; Description:
;;;  Current recursive level.
;
(deftlv *printer-level* 0)

;;;; *printer-label-table*
;;;
;;; Description:
;;;  Hash table for detecting circular and shared object.
;;;     nil                 -- not appeared yet
;;;     t                   -- the first time
;;;     0                   -- the second time
;;;     positive integer    -- label number.
;;;
;
(deftlv *printer-label-table* nil)

;;;; *printer-label*
;;;
;;; Description:
;;;  Contains label number or nil.
;
(deftlv *printer-label* nil)


;;;; *printer-stream*
;;;
;;; Description:
;;;  Contains current printer stream to detect toplevel or recursive
;;;  printer call.
;
(deftlv *printer-stream* nil)


;;;; *bignum-divisor-vector*
;;;; *fixnum-ndigits-vector*
;;;
;;; Description:
;;;  For print-bignum-aux.
;;;  Conatins max(base ^ n) < most-positive-fixnum.
;
(defparameter *bignum-divisor-vector* (make-array 37 :initial-element nil))
(defparameter *fixnum-ndigits-vector* (make-array 37 :initial-element nil))

(loop for base from 2 to 36  do
  (loop for power-1 = -1 then (1+ power-1)
        and new-divisor = base then (* new-divisor base)
        and divisor = 1 then new-divisor
        while (typep new-divisor 'fixnum)
        do (setf (aref *bignum-divisor-vector* base) divisor)
           (setf (aref *fixnum-ndigits-vector* base) (1+ power-1)) ))

;;; BUGBUG: *char-name-table* and *name-char-table* should be defined in
;;; r13-char.lisp instead of r22-printer.lisp.
(defvar *char-name-table* (make-hash-table :test 'eq))
(defvar *name-char-table* (make-hash-table :test 'equalp))

(dolist (code.name '(
    (#x08 . "Backspace")
    (#x09 . "Tab")
    (#x0A . "Linefeed")
    (#x0A . "Newline")
    (#x0C . "Page")
    (#x0D . "Return")
    (#x1B . "Escape")
    (#x20 . "Space")
    (#x7F . "Delete")
    (#x7F . "Rubout" )) )
  (let ((char (code-char (car code.name)))
        (name (cdr code.name)) )
    (setf (gethash char *char-name-table*) name)
    (setf (gethash name *name-char-table*) char) ) )


(declaim (ftype (function (float) (member #\E #\S #\F #\D #\L))
  float-marker ) )

(declaim (ftype (function
    (integer integer integer integer)
    (values list fixnum) )
  float-to-digits ) )

(declaim (ftype (function
    (float)
    (values integer integer integer
        (member :normal :infinity :zero :nan :snan :subnormal) ))
  ieee-integer-decode-float ) )

(declaim (ftype (function (array stream) t) print-array-unreadably))

(declaim (ftype (function
    (bignum stream (integer 2 36) fixnum fixnum)
    unspecified )
  print-bignum-aux ) )

(declaim (ftype (function (fixnum stream (integer 2 36)) unspecified)
  print-fixnum-aux ) )

(declaim (ftype (function (stream list integer character) unspecified)
  print-float-digits-E ) )

(declaim (ftype (function (stream list fixnum) unspecified)
  print-float-digits-F ) )

(declaim (ftype (function
    (simple-string stream &optional sequence-index sequence-end) unspecified)
  print-simple-string ) )

(declaim (ftype (function (structure-object stream) unspecified)
  print-structure print-structure-unreadably ) )

(declaim (ftype (function (vector stream) unspecified)
  print-vector-unreadably ) )

(declaim (ftype (function (t stream t t (or function null)) null)
  print-unreadable-object-function ) )

(declaim (ftype (function (character fixnum stream) unspecified)
  write-chars ) )

(declaim (ftype (function (t stream) unspecified)
  write-object ) )

