;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - atan
;;; arch/generic/lisp/macth/gen-math-f32-atan.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-atan.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-atan
;
(in-package :si)

#|
 * From fdlibm (http://www.netlib.org/fdlibm/)
 *
/* sf_atan.c -- float version of s_atan.c.
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
(defun float32-atan/1 (x)
    (declare (values single-float))
    (declare (type single-float x))
  (prog* (
    (atanhi0    #+nil 4.6364760399e-01  ;  atan(0.5)hi
                    #.(encode-float32 #x3eed6338) )
    (atanhi1    #+nil 7.8539812565e-01  ;  atan(1.0)hi
                    #.(encode-float32 #x3f490fda) )
    (atanhi2    #+nil 9.8279368877e-01  ;  atan(1.5)hi
                    #.(encode-float32 #x3f7b985e) )
    (atanhi3    #+nil 1.5707962513e+00  ;  atan(inf)hi
                    #.(encode-float32 #x3fc90fda) )
    ;;
    (atanlo0    #+nil 5.0121582440e-09  ;  atan(0.5)lo
                    #.(encode-float32 #x31ac3769) )
    (atanlo1    #+nil 3.7748947079e-08  ;  atan(1.0)lo
                    #.(encode-float32 #x33222168) )
    (atanlo2    #+nil 3.4473217170e-08  ;  atan(1.5)lo
                    #.(encode-float32 #x33140fb4) )
    (atanlo3    #+nil 7.5497894159e-08  ;  atan(inf)lo
                    #.(encode-float32 #x33a22168) )
    ;;
    (aT0        #+nil 3.3333334327e-01
                    #.(encode-float32 #x3eaaaaaa) )
    (aT1        #+nil -2.0000000298e-01
                    #.(encode-float32 #xbe4ccccd) )
    (aT2        #+nil 1.4285714924e-01
                    #.(encode-float32 #x3e124925) )
    (aT3        #+nil -1.1111110449e-01
                    #.(encode-float32 #xbde38e38) )
    (aT4        #+nil 9.0908870101e-02
                    #.(encode-float32 #x3dba2e6e) )
    (aT5        #+nil -7.6918758452e-02
                    #.(encode-float32 #xbd9d8795) )
    (aT6        #+nil 6.6610731184e-02
                    #.(encode-float32 #x3d886b35) )
    (aT7        #+nil -5.8335702866e-02
                    #.(encode-float32 #xbd6ef16b) )
    (aT8        #+nil 4.9768779427e-02
                    #.(encode-float32 #x3d4bda59) )
    (aT9        #+nil -3.6531571299e-02
                    #.(encode-float32 #xbd15a221) )
    (aT10       #+nil 1.6285819933e-02
                    #.(encode-float32 #x3c8569d7) )
    ;;
    (zero       #+nil 0.0
                    #.(encode-float32  0) )
    (one        #+nil  1.0000000000e+00
                    #.(encode-float32 #x3F800000) )
    (huge       #+nil 1.0e30
                    #.(encode-float32 #x7149F2C9) )
    ;;
    (hx (decode-float32 x))
    (ix (logand hx #x7fffffff))
    (atanhi zero)
    (atanlo zero)
    )

    ;; if |x| >= 2^34
    (when (>= ix #x50800000)
      (cond
        ((> ix #x7f800000)  ; is NaN
          (return (+ x x)) )
        ((> hx 0)
          (return (+ atanhi3 atanlo3)) )
        (t
          (return (- (+ atanhi3 atanlo3))) )))

    (cond
      ;; |x| >= 0.4375
      ((>= ix #x3ee00000)
        (setq x (float32-abs x))
        (cond
          ((< ix #x3f300000)  ; 7/16 <=|x|<11/16
              (setq atanhi atanhi0)
              (setq atanlo atanlo0)
              (setq x  (/ (- (* 2 x) one) (+ 2 x))) )
           ((< ix #x3f980000) ; |x| < 1.1875
              ;; 11/16<=|x|< 19/16
              (setq atanhi atanhi1)
              (setq atanlo atanlo1)
              (setq x  (/ (- x one) (+ x one))) )
           ((< ix #x401c0000) ; |x| < 2.4375
              (setq atanhi atanhi2)
              (setq atanlo atanlo2)
              (setq x (/ (- x 1.5) (+ one (* 1.5 x)))) )
           (t
              ;; 2.4375 <= |x| < 2^66
              (setq atanhi atanhi3)
              (setq atanlo atanlo3)
              (setq x (/ -1.0 x)) )) )
      ((< ix #x31000000)
        ;; |x| < 2^-29
        (when (> (+ huge x) one)
          ;; raise inexact
          (return x) ) ))

    ;; end of argument reduction
    (let* ((z  (* x x))
           (w  (* z z))
           ;; break sum from i=0 to 10 aT[i]z**(i+1) into odd and even poly
           (s1 (* z (+ at0
               (* w (+ at2
               (* w (+ at4
               (* w (+ at6
               (* w (+ at8
               (* w at10))))))))))) )
           (s2 (* w (+ at1
               (* w (+ at3
               (* w (+ at5
               (* w (+ at7
               (* w at9))))))))) ))
      (when (eql atanhi zero) (return (- x (* x (+ s1 s2)))))
      (let ((z (- atanhi (- (- (* x (+ s1 s2)) atanlo) x))))
        (return (if (< hx 0) (- z) z)) ) ) ) )
