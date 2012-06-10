;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64-rationalize
;;; arch/generic/lisp/math/gen-math-f64-rationalize.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-rationalize.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of float64-rationalize.
;
(in-package :si)

;;; The algorithm here is the method described in CLISP.  Bruno Haible has
;;; graciously given permission to use this algorithm.  He says, "You can use
;;; it, if you present the following explanation of the algorithm."
;;;
;;; Algorithm (recursively presented):
;;;   If x is a rational number, return x.
;;;   If x = 0.0, return 0.
;;;   If x < 0.0, return (- (rationalize (- x))).
;;;   If x > 0.0:
;;;     Call (integer-decode-float x). It returns a m,e,s=1 (mantissa,
;;;     exponent, sign).
;;;     If m = 0 or e >= 0: return x = m*2^e.
;;;     Search a rational number between a = (m-1/2)*2^e and b = (m+1/2)*2^e
;;;     with smallest possible numerator and denominator.
;;;     Note 1: If m is a power of 2, we ought to take a = (m-1/4)*2^e.
;;;       But in this case the result will be x itself anyway, regardless of
;;;       the choice of a. Therefore we can simply ignore this case.
;;;     Note 2: At first, we need to consider the closed interval [a,b].
;;;       but since a and b have the denominator 2^(|e|+1) whereas x itself
;;;       has a denominator <= 2^|e|, we can restrict the seach to the open
;;;       interval (a,b).
;;;     So, for given a and b (0 < a < b) we are searching a rational number
;;;     y with a <= y <= b.
;;;     Recursive algorithm fraction_between(a,b):
;;;       c := (ceiling a)
;;;       if c < b
;;;         then return c       ; because a <= c < b, c integer
;;;         else
;;;           ; a is not integer (otherwise we would have had c = a < b)
;;;           k := c-1          ; k = floor(a), k < a < b <= k+1
;;;           return y = k + 1/fraction_between(1/(b-k), 1/(a-k))
;;;                             ; note 1 <= 1/(b-k) < 1/(a-k)
;;;
;;; You can see that we are actually computing a continued fraction expansion.
;;;
;;; Algorithm (iterative):
;;;   If x is rational, return x.
;;;   Call (integer-decode-float x). It returns a m,e,s (mantissa,
;;;     exponent, sign).
;;;   If m = 0 or e >= 0, return m*2^e*s. (This includes the case x = 0.0.)
;;;   Create rational numbers a := (2*m-1)*2^(e-1) and b := (2*m+1)*2^(e-1)
;;;   (positive and already in lowest terms because the denominator is a
;;;   power of two and the numerator is odd).
;;;   Start a continued fraction expansion
;;;     p[-1] := 0, p[0] := 1, q[-1] := 1, q[0] := 0, i := 0.
;;;   Loop
;;;     c := (ceiling a)
;;;     if c >= b
;;;       then k := c-1, partial_quotient(k), (a,b) := (1/(b-k),1/(a-k)),
;;;            goto Loop
;;;   finally partial_quotient(c).
;;;   Here partial_quotient(c) denotes the iteration
;;;     i := i+1, p[i] := c*p[i-1]+p[i-2], q[i] := c*q[i-1]+q[i-2].
;;;   At the end, return s * (p[i]/q[i]).
;;;   This rational number is already in lowest terms because
;;;   p[i]*q[i-1]-p[i-1]*q[i] = (-1)^i.
;;;
;;; This is a fairly straigtforward implementation of the iterative
;;; algorithm above.
(defun float64-rationalize (x)
    (declare (values rational))
    (declare (type double-float x))
  (multiple-value-bind (hx lx)  (decode-float64 x)
  (let ((ix   (logand hx #x7fffffff)))
  (multiple-value-bind (f exp)
      (cond
        ((eql ix 0)
          (values 0 0) )
        ((<= ix #x000fffff) ; Subnormal
          (values ix  (+ 1023 52 -1)) )
        ((>= ix #x7ff00000) ; Inf or NaN
          (error 'arithmetic-error
                 :operation 'rationalize
                 :operands (list x) ) )
        (t ; Normal
          (values (logior (ash 1 52)
                          (ash (logand ix #x000fffff) 32)
                          (logand lx #xffffffff) )
                  (- (ash ix -20) 1023 52) ) ))
    (if (or (zerop f) (>= exp 0))
         (if (>= hx 0) (ash f exp) (- (ash f exp)))
       ;; exp < 0 and (2*m-1) and (2*m+1) are coprime to 2^(1-e),
       ;; so build the ftion up immediately, without having to do
       ;; a gcd.
       (let ((a (/ (- (* 2 f) 1) (ash 1 (- 1 exp))))
             (b (/ (+ (* 2 f) 1) (ash 1 (- 1 exp))))
             (p0 0)
             (q0 1)
             (p1 1)
             (q1 0))
          (do ((c (ceiling a) (ceiling a)))
              ((< c b)
                (let ((top (+ (* c p1) p0))
                      (bot (+ (* c q1) q0)) )
                  (/ (if (>= hx 0) top (- top)) bot)) )
            (let* ((k (- c 1))
                   (p2 (+ (* k p1) p0))
                   (q2 (+ (* k q1) q0)) )
              (psetq a (/ (- b k))
                     b (/ (- a k)))
              (setq p0 p1
                    q0 q1
                    p1 p2
                    q1 q2 ) ) ) ) ) ) ) ) )
