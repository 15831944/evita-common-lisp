;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Runtime - 22 Printer
;;; runtime/r22-printer.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r22-printer-api.lisp#2 $
;;;
;;; Note:
;;;  You should load "r22-setup" when loading this file, "r22-print" into
;;;  running environment. This file includes function which replaces
;;;  default printers.
;;;
;;; Description:
;;;  This file implements following public API functions:
;;;     pprint                      22.4.14
;;;     prin1                       22.4.14
;;;     prin1-to-string             22.4.15
;;;     princ                       22.4.14
;;;     princ-to-string             22.4.15
;;;     print                       22.4.14
;;;     print-not-readable-object   22.4.30
;;;     write                       22.4.14
;;;     write-string                22.4.15f
;
(in-package :si)

;;;; 22.4.14 pprint
(defun cl:pprint (object &optional stream)
  (setq stream (ensure-output-stream stream))
  (terpri stream)
  (let ((*print-pretty* t))
    (write-object object stream) )
  (values) )


;;;; 22.4.14 prin1
(defun cl:prin1 (object &optional stream)
  (setq stream (ensure-output-stream stream))
  (let ((*print-escape* t))
    (write-object object stream) )
  object )


;;;; 22.4.15 prin1-to-string
(defun cl:prin1-to-string (object)
  (with-output-to-string (stream)
    (prin1 object stream) ) )


;;;; 22.4.14 princ
(defun cl:princ (object &optional stream)
  (setq stream (ensure-output-stream stream))
  (let ((*print-escape* nil))
    (write-object object stream) )
  object )


;;;; 22.4.15 princ-to-string
(defun cl:princ-to-string (object)
  (with-output-to-string (stream)
    (princ object stream) ) )


;;;; 22.4.14 print
(defun cl:print (object &optional stream)
  (setq stream (ensure-output-stream stream))
  (let ((*print-escape* t))
    (write-char #\Newline stream)
    (write-object object stream)
    (write-char #\Space stream) )
  object )


;;;; 22.4.30 print-not-readable-object
(defun cl:print-not-readable-object (condition)
  (declare (type print-not-readable condition))
  (slot-value condition 'object) )


;;;; 22.4.14 write
(defun cl:write (object &key
                     (stream            *standard-output*)
                     (escape            *print-escape*)
                     (radix             *print-radix*)
                     (base              *print-base*)
                     (circle            *print-circle*)
                     (pretty            *print-pretty*)
                     (level             *print-level*)
                     (length            *print-length*)
                     (case              *print-case*)
                     (gensym            *print-gensym*)
                     (array             *print-array*)
                     (readably          *print-readably*)
                     (right-margin      *print-right-margin*)
                     (miser-width       *print-miser-width*)
                     (lines             *print-lines*)
                     (pprint-dispatch   *print-pprint-dispatch*) )
  (let ((*print-escape*          escape)
        (*print-radix*           radix)
        (*print-base*            base)
        (*print-circle*          circle)
        (*print-pretty*          pretty)
        (*print-level*           level)
        (*print-length*          length)
        (*print-case*            case)
        (*print-gensym*          gensym)
        (*print-array*           array)
        (*print-readably*        readably)
        (*print-right-margin*    right-margin)
        (*print-miser-width*     miser-width)
        (*print-lines*           lines)
        (*print-pprint-dispatch* pprint-dispatch) )
    (write-object object stream) ) )


;;;; 22.4.15 write-to-string
;;; BUGBUG: This implementation accepts keyword argument ":stream".
(defun cl:write-to-string (object &rest args)
    (declare (dynamic-extent args))
  (with-output-to-string (stream)
    (apply #'write object :stream stream args) ) )
