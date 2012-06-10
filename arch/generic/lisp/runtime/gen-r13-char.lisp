;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 13 Characters - Low
;;; lisp/runtime/r13-low.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r13-char.lisp#2 $
;;;
;;; Description:
;;;  This fils contains declarations for type system.
;
(in-package :si)

(macrolet (
  (define (suffix op)
   `(defun ,(intern (format nil "CHAR-~A/2" suffix)) (ch1 ch2)
        (declare (type character ch1 ch2))
        (declare (values t))
      (,(intern (format nil "CHAR~A/2" op))
            (char-downcase ch1)
            (char-downcase ch2) ) ) )
  )
  ;;
  (define equal =)
  (define not-equal /=)
  (define lessp <)
  (define not-lessp >=)
  (define greaterp >)
  (define not-greaterp <=) )
