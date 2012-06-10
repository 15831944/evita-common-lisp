;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 12 Numbers - Integer
;;; arch/generic/lisp/runtime/gen-r12-integer.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r12-integer.lisp#1 $
;
(in-package :si)

;;;; 12.2.57 integer-length
;;;     2^(n-1) <= x < 2^n      where x > 0
;;;     -2^(n-1) > x >= -2^n    where x < 0
;;;                0            where x = 0
;;;
;;; BUGBUG: integer-length: We should NOT loop for bignum. integer-length
;;; of bignum x is
;;;     (+ (* (1- (bignum-length x)) size-of-bigit)
;;;        (integer-length (bignum-ref x (1- (bignum-length x)))) )
;
(defun cl:integer-length (x)
  (cond
    ((plusp x)
      (let ((n 0))
        (loop until (eql x 0) do
          (incf n)
          (setq x (ash x -1)) )
        n ) )
    ((minusp x)
      (let ((n 0))
        (loop until (eql x -1) do
          (incf n)
          (setq x (ash x -1)) )
        n ) )
    (t 0) ) )
