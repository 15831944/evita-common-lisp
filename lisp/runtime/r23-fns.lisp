;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 2 Syntax
;;; runtime/r02-syntax.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r23-fns.lisp#2 $
;;;
;;; Description:
;;;  TBD
;
(in-package :si)

;;;; get-char-info
(defun get-char-info (char readtable)
    (declare (values t))
    (declare (type character char))
    (declare (type readtable readtable))
  (let ((charvec (ref readtable vector readtable))
        (code    (char-code char)) )
    (if (< code (length charvec))
        (svref charvec code)
      ;; #x101 = TYPE_cons | TRAIT_alphabetic
      (gethash/eq char (ref readtable table readtable) #x101) ) ) )


;;;; (setf get-char-info)
(defun (setf get-char-info) (info char readtable)
    (declare (type (or fixnum cons) info))
    (declare (type character char))
    (declare (type readtable readtable))
  (let ((charvec (ref readtable vector readtable))
        (code    (char-code char)) )
    (cond
      ((< code (length charvec))
        (setf (svref charvec code) info) )
      ((eql info #x101)
        (remhash char (ref readtable table readtable)) )
      (t
        (setf (gethash/eq char (ref readtable table readtable)) info) )) ) )


;;;; get-char-syntax
(defun get-char-syntax (ch readtable)
    (declare (values fixnum))
  (let ((info (get-char-info ch readtable)))
    (if (consp info)
        (logand (car info) #xF)
      (logand info #xF) ) ) )


;;;; fast-read-char
(defun fast-read-char (stream eof-error-p)
  (if (not eof-error-p)
      (let ((ch (stream-read-char stream)))
         (if (eq ch :eof) nil ch) )
    (let ((ch (stream-read-char stream)))
       (when (eq ch :eof) (error 'end-of-file :stream stream))
       ch )) )


;;;; free-pooled-token
(defun free-pooled-token (token)
  (push token *read-pooled-token*) )


;;;; make-pooled-token
(defun make-pooled-token ()
  (let ((token (pop *read-pooled-token*)))
    (if token
        (setf (ref reader-token length token) 0)
      (setq token (make-reader-token (make-string 100) (make-array 100))) )
    token ) )


;;;; reader-eof-error
;;;
;;; BUGBUG: NYI: use another error for eof occured in middle of object.
;
(defun reader-eof-error (stream)
  (error 'reader-eof-error :stream stream :start *read-start-line-number*) )


;;;; Report Extract Infix Parameter For Dispatch Macro Character
;
(defun reader-error-extra-infix-arg
    (stream sub-char arg &optional (disp-char #\#))
  (simple-reader-error stream
      "~C~C doesn't accept infix argument: ~S" disp-char sub-char arg ) )


;;;; Report Missing Infix Parameter
;
(defun reader-error-missing-infix-arg
    (stream sub-char &optional (disp-char #\#))
  (simple-reader-error stream
    "~C~C requires infix argument." disp-char sub-char ) )


;;;; simple-reader-error
;
(defun simple-reader-error (stream control &rest args)
  (error 'simple-reader-error
         :stream stream
         :format-control   control
         :format-arguments args ) )
