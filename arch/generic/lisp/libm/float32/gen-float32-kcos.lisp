;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - Kernel cos
;;; arch/generic/lisp/math/gen-math-f32-kcos.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-kcos.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-kernel-cos
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* kf_cos.c -- float version of k_cos.c
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
 */
|#
(defun float32-kernel-cos (x y)
    (declare (values single-float))
    (declare (type single-float x y))
  (prog* (
    (one    #+nil 1.0000000000e+00  #.(encode-float32 #x3f800000))
    (C1     #+nil 4.1666667908e-02  #.(encode-float32 #x3d2aaaab))
    (C2     #+nil -1.3888889225e-03 #.(encode-float32 #xbab60b61))
    (C3     #+nil 2.4801587642e-05  #.(encode-float32 #x37d00d01))
    (C4     #+nil -2.7557314297e-07 #.(encode-float32 #xb493f27c))
    (C5     #+nil 2.0875723372e-09  #.(encode-float32 #x310f74f6))
    (C6     #+nil -1.1359647598e-11 #.(encode-float32 #xad47d74e))
    ;;
    (hx (decode-float32 x))
    (ix (logand hx #x7fffffff))
    )
    ;; if x < 2**27
    (when (< ix #x32000000)
      ;; generate inexact
      (when (eql (truncate x) 0) (return one)) )

    (let* (
      (z  (* x x))
      (r  (* z (+ c1
          (* z (+ c2
          (* z (+ c3
          (* z (+ c4
          (* z (+ c5
          (* z c6))))))))))) ))
      ;; if |x| < 0.3 */
      (if (< ix  #x3e99999a)
          ;; if |x| < 0.3
          (return (- one (- (* 0.5f0 z) (- (* z r) (* x y)))))
        (let* (
          (qx 
            (if (> ix #x3f480000)   ; x > 0.78125
                0.28125f0
              (encode-float32 (- ix #x01000000)) ) )   ; x/4
          (hz (- (* 0.5f0 z) qx))
          (a  (- one qx))
          )
          (return (- a (- hz (- (* z r) (* x y))))) )) ) ) )

