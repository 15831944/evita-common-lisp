;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32 atan2
;;; arch/generic/lisp/macth/gen-math-f32-atan2.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-atan2.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-atan
;
(in-package :si)

#|
 * From fdlibm (http://www.netlib.org/fdlibm/)
 *
/* ef_atan2.c -- float version of e_atan2.c.
 * Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
 */

/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 *
 */
|#
(defun float32-atan/2 (x y)
    (declare (values single-float))
    (declare (type single-float x y))
  (prog* (
    (tiny  1.0f-30)
    (zero  0f0)
    (pi_o_4     #+nil 7.8539818525e-01
                #.(encode-float32 #x3f490fdb) )
    (pi_o_2     #+nil 1.5707963705e+00
                #.(encode-float32 #x3fc90fdb) )
    (fpi        #+nil 3.1415927410e+00
                #.(encode-float32 #x40490fdb) )
    (pi_lo      #+nil -8.7422776573e-08
                #.(encode-float32 #xb3bbbd2e) )
    ;;
    (hx (decode-float32 x))
    (ix (logand hx #x7fffffff))
    (hy (decode-float32 y))
    (iy (logand hy #x7fffffff))
    )
    ;;

    ;; x or y is NaN
    (when (or (> ix #x7f80000) (> iy #x7f800000))
      (return (+ x y)) )

    ;; x=1.0
    (when (eql hx #x3f800000)
      (return (float32-atan/1 y)) )

    ;; m = 2*sign(x)+sign(y)
    (let ((m (logior (logand (ash hy -31) 1)
                     (logand (ash hx -30) 2) ) ))
                     

    ;; when y = 0
    (when (eql iy 0)
      (case m
        (1
          ;; atan(+-0,+anything)=+-0
          (return y) )
        (2
          ;; atan(+0,-anything) = pi
          (return (+ fpi tiny)) )
        (3
          ;; atan(-0,-anything) =-pi
          (return (- (- fpi) tiny))) ))

    ;; when x = 0
    (when (eql ix 0)
      (return (if (< hy 0) (- (+ pi_o_2 tiny)) (+ pi_o_2 tiny))) )
            
    ;; when x is INF
    (when (eql ix #x7F800000)
      (if (eql iy #x7F800000)
          (ecase m
            (0
              ;; atan(+INF,+INF)
              (return  (+ pi_o_4 tiny)) )
            (1
              ;; atan(-INF,+INF)
              (return (- (+ pi_o_4 tiny))) )
            (2
              ;; atan(+INF,-INF)
              (return (+ (* 3 pi_o_4) tiny)) )
            (3
              ;; atan(-INF,-INF)
              (return (- (+ (* 3 pi_o_4) tiny)) ) ))
          (ecase m
            (0
              ;; atan(+...,+INF)
               (return zero) )
            (1
              ;; atan(-...,+INF)
              (return (- zero)) )
            (2
              ;; atan(+...,-INF)
              (return (+ fpi tiny)) )
            (3
              ;; atan(-...,-INF)
              (return (- (+ fpi tiny))) ))))

    ;; when y is INF
    (when (eql iy #x7F800000)
      (return (if (< hy 0) (- (+ pi_o_2 tiny)) (+ pi_o_2 tiny))) )

    ;; compute y/x
    (let* ((k (ash (- iy ix) -23))
           (z
              (cond
                ((> k 60)
                  ;; |y/x| >  2**60
                  (+ pi_o_2 (* 0.5 pi_lo)) )
                ((and (< hx 0) (< k -60))
                  ;; |y|/x < -2**60
                  0f0 )
                (t
                  ;; safe to do y/x 
                  (float32-atan/1 (float32-abs (/ y x))) )) ))
        (ecase m
          (0 ;; atan(+,+)
            (return z) )
          (1 ;; atan(-,+)
            (return (encode-float32
                (logxor (decode-float32 z) #x80000000) )) )
          (2 ;; atan(+,-)
            (return  (- fpi (- z pi_lo))) )
          (3 ;; atan(-,-)
            (return (- (- z pi_lo) fpi)) )) ) ) ) )
