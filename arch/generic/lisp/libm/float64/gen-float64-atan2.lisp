;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - asin
;;; arch/generic/lisp/macth/gen-r12-acosh64.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-atan2.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     fasin64
;
(in-package :si)

#|
 * From fdlibm (http://www.netlib.org/fdlibm/)
 *
 * @(#)e_atan2.c 5.1 93/09/24 
 *
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 *
 

; __ieee754_atan2(y,x)
 * Method :
 *      1. Reduce y to positive by atan2(y,x)=-atan2(-y,x).
 *      2. Reduce x to positive by (if x and y are unexceptional): 
 *              ARG (x+iy) = arctan(y/x)              ... if x > 0,
 *              ARG (x+iy) = pi - arctan[y/(-x)]   ... if x < 0,
 *
 * Special cases:
 *
 *      ATAN2((anything), NaN ) is NaN 
 *      ATAN2(NAN , (anything) ) is NaN 
 *      ATAN2(+-0, +(anything but NaN)) is +-0   
 *      ATAN2(+-0, -(anything but NaN)) is +-pi  
 *      ATAN2(+-(anything but 0 and NaN), 0) is +-pi/2 
 *      ATAN2(+-(anything but INF and NaN), +INF) is +-0  
 *      ATAN2(+-(anything but INF and NaN), -INF) is +-pi 
 *      ATAN2(+-INF,+INF ) is +-pi/4  
 *      ATAN2(+-INF,-INF ) is +-3pi/4 
 *      ATAN2(+-INF, (anything but,0,NaN, and INF)) is +-pi/2 
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following 
 * constants. The decimal values may be used, provided that the 
 * compiler will convert from decimal to binary accurately enough 
 * to produce the hexadecimal values shown.
|#
(defun float64-atan/2 (y x)
    (declare (values double-float))
    (declare (type double-float y x))
  (prog* (
        (tiny  1.0d-300)
        (zero  0d0)
        (pi_o_4  #+nil 7.8539816339744827900E-01
                 #.(encode-float64 #x3FE921FB #x54442D18) )
        (pi_o_2  #+nil 1.5707963267948965580E+00
                 #.(encode-float64 #x3FF921FB #x54442D18) )
        #+nil (pi      #+nil 3.1415926535897931160E+00
                 #.(encode-float64 #x400921FB #x54442D18) ) 
        (pi_lo   #+nil 1.2246467991473531772E-16
                 #.(encode-float64 #x3CA1A626 #x33145C07) )) 
    (multiple-value-bind (hx lx) (decode-float64 x)
    (let ((ix (logand hx #x7fffffff)))
    (multiple-value-bind (hy ly) (decode-float64 y)
    (let ((iy (logand hy #x7fffffff)))
      ;; x or y is NaN
      (when (or (> (logior ix (ash (logior lx (- lx)) -31)) #x7ff00000)
                (> (logior iy (ash (logior ly (- ly)) -31)) #x7ff00000))
        (return (+ x y)) )

      ;; x=1.0
      (when (eql (logior (- hx #x3ff00000) lx) 0)
        (return  (float64-atan/1 y)) )

            ;; 2*sign(x)+sign(y) 
      (let ((m (logior (logand (ash hy -31) 1) (logand (ash hx -30) 2))))
          (declare (type (integer 0 3) m))
        ; when y = 0 
        (when (eql (logior iy ly) 0)
          (case m
            (0 (return y))
            (1 (return y))   ; atan(+-0,+anything)=+-0 
            (2 (return (+ pi tiny))) ; atan(+0,-anything) = pi 
            (3 (return (- 0 pi tiny))) )); atan(-0,-anything) =-pi 
        ;; when x = 0 
        (when (eql (logior ix lx) 0)
          (return
              (if (< hy 0) (- 0 pi_o_2 tiny) (+ pi_o_2 tiny)) )) 

        ;; when x is INF
        (when (eql ix #x7ff00000)
          (if (eql iy #x7ff00000)
            (case m
              (0 (return (+ pi_o_4 tiny))) ; atan(+INF,+INF) 
              (1 (return (- 0 pi_o_4 tiny))) ; atan(-INF,+INF) 
              (2 (return (+ (* 3 pi_o_4) tiny))) ;atan(+INF,-INF)
              (3 (return (- (* -3 pi_o_4) tiny))) ) ;atan(-INF,-INF)
            (case m
              (0 (return zero))      ; atan(+...,+INF) 
              (1 (return (- zero)))  ; atan(-...,+INF) 
              (2 (return (+ pi tiny))); atan(+...,-INF) 
              (3 (return (- 0 pi tiny))) ))) ; atan(-...,-INF) 

        ;; when y is INF 
        (when (eql iy #x7ff00000)
          (return
            (if (< hy 0) (- 0 pi_o_2 tiny) (+ pi_o_2 tiny)) )) 

        ;; compute y/x
        (let* ((k  (ash (- iy ix) -20))
               (z
                 (cond
                   ((> k 60)
                     (+ pi_o_2 (* 0.5 pi_lo)) )  ; |y/x| >  2**60 
                   ((and (< hx 0) (< k -60))
                     zero )                      ; |y|/x < -2**60 
                  (t  ; safe to do y/x
                    (float64-atan/1 (float64-abs (/ y x))) )) ))
            (declare (type double-float z))
          (case m
            (0  ; atan(+,+)
               (return z)) 
            (1  ; atan(-,+)
              (multiple-value-bind (zh zl) (decode-float64 z)
                (return (encode-float64 (logxor zh #x80000000) zl)) ) )
            (2  ; atan(+,-)
               (return (- pi (- z pi_lo))) )
            (3  ; atan(-,-)
               (return (- (- z pi_lo) pi)) )) ) ) ) ) ) ) ) )
