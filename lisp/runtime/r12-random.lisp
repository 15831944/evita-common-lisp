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
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r12-random.lisp#2 $
;;;
;;; Description:
;;;  This file contains following functions:
;;;     make-random-state       12.2.40
;;;     random                  12.2.41
;;;     random-state-p          12.2.42
;;;     *random-state*          12.2.43
;;;
;;; Note:
;;; We use 32-bit linear congruential generator called "randq1" describe in
;;; Chapter 7, p.284, "An Even Quicker Generator" of
;;;     Numerical recipes in C, William H.Press, Saul A.Teukolsky, Willam T.
;;      Vertterling, Brian P Flannery, Cambridge University Press, 1992.
;
(in-package :si)

;;;; 12.2.42 randome-state-p
;
(defstruct (random-state (:constructor %make-random-state))
  (idum  0   :type integer)
  ;(fdum  nil :type (vector single-float 1))
  fdum )


;;;; 12.2.43 *random-state*
;;;
;;; Note:
;;;  This is per-thread variable.
;
(defvar cl:*random-state*)


;;;; 12.2.40 make-random-state
;
(defun cl:make-random-state (&optional state)
  (etypecase state
    (null
      (setq state (copy-random-state *random-state*)) )
    ((eql t)
      (setq state (%make-random-state :idum (get-internal-real-time))) )
    (random-state
      (setq state (copy-random-state state)) ) )
   (setf (random-state-fdum state) (make-array 1 :element-type 'single-float))
   state )


;;;; 12.2.41 random
;;;
;;; Syntax:
;;;     random real &optional state
;;;
;;; Note: This function works only for IEEE floating-point if argument
;;; is float.
;
(defun cl:random (x &optional (state *random-state*))
  (check-type x (real (0) *))
  (let ((idum (+ (* 1664525 (random-state-idum state)) 1013904223)))
    (setq idum (logand idum #.(1- (ash 1 32))))
    (setf (random-state-idum state) idum)
    (if (not (floatp x))
        (rem idum x)
      (let ((fdum (random-state-fdum state)))
        (setf (ubyte32 fdum 2)
              (logior #x3f800000 (logand idum #x007fffff)) )
        (* (- (aref fdum 0) 1) x) )) ) )
