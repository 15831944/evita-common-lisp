;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 Numbers
;;; lisp/runtime/r12-number.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime3/r12-number.lisp#3 $
;;;
;
(in-package :si)

;;;; 12.2.15 =
(defun cl:= (z1 &rest z*)
    (declare (dynamic-extent z*))
  (if (null z*)
      (=/2 z1 z1)
    (dolist (z2 z* t) (unless (=/2 z1 z2) (return nil))) ) )


;;;; 12.2.15 /=
(defun cl:/= (z1 &rest z*)
    (declare (dynamic-extent z*))
  (loop
    (when (null z*) (return t))
    (dolist (z2 z*)
      (when (=/2 z1 z2) (return-from /= nil)) )
    (setq z1 (pop z*)) ) )


;;;; 12.2.25 +
;;;; 12.2.25 *
;;;; 12.2.32 gcd
;;;; 12.2.62 logand, logior, logeqv, logxor
(macrolet (
  (define (name identity)
   `(defun ,name (&rest z*)
      (let ((z1 ,identity))
        (dolist (z2 z* z1)
          (setq z1 (,(intern (format nil "~A/2" name)) z1 z2)) ) ) ) )
   )
   ;;
   (define cl:+ 0)
   (define cl:* 1)

   (define cl:gcd 0)

   (define cl:logand -1)
   (define cl:logeqv -1)
   (define cl:logior  0)
   (define cl:logxor  0) )


;;; 12.2.62 logand, logandc1, logandc2, lognand, lognor, logorc1, logorc2
(defun logandc1 (x y) (logand (lognot x) y))
(defun logandc2 (x y) (logand x (lognot y)))
(defun lognand  (x y) (lognot (logand x y)))
(defun lognor   (x y) (lognot (logand x y)))
(defun logorc1  (x y) (logior (lognot x) y))
(defun logorc2  (x y) (logior x (lognot y)))


;;;; 12.2.16 max, min
(macrolet (
  (max/2 (x y) `(if (> ,x ,y) ,x ,y))
  (min/2 (x y) `(if (< ,x ,y) ,x ,y))
  (define (name)
   `(defun ,name (x &rest y*)
      (if (null y*)
          (progn (check-type x real) x)
        (dolist (y y* x)
          (setq x (,(intern (format nil "~A/2" name)) x y)) )) ) )
  )
  (define cl:max)
  (define cl:min) )
