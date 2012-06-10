;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64 - ceiling
;;; arch/generic/lisp/macth/gen-float64-ceiling.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-fceiling.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-ceiling
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* @(#)s_ceil.c 5.1 93/09/24 */
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

/*
 * ceil(x)
 * Return x rounded toward -inf to integral value
 * Method:
 *      Bit twiddling.
 * Exception:
 *      Inexact flag raised if x not equal to ceil(x).
 */
|#
(defun float64-fceiling (x)
    (declare (values double-float))
    (declare (type double-float x))
  (multiple-value-bind (i0 i1) (decode-float64 x)
  (prog* (
    (huge 1.0d300)
    (j0 (- (logand (ash i0 -20) #x7ff) #x3ff))
    )
    (cond
      ((< j0 0)
        ;; raise inexact if x != 0
        (when (> (+ huge x) 0d0)
          ;; return 0*sign(x) if |x|<1
          (cond
            ((< i0 0) (setq i0 #x80000000 i1 0))
            ((not (eql (logior i0 i1) 0)) (setq i0 #x3ff00000 i1 0)) )) )
      ((< j0 20)
        (let ((i (ash #x000fffff (- j0))))
          ;; x is integral
          (when (eql (logior (logand i0 i) i1) 0)
             (return x) ) 

          ;; raise inexact flag
          (when (> (+ huge x) 0d0)
            (when (> i0 0) (incf i0 (ash #x00100000 (- j0))))
            (setq i0 (logand i0 (lognot i)))
            (setq i1 0) ) ) )
      ((> j0 51)
        ;; inf or NaN
        ;; x is integral
        (return (if (eql j0 #x400) (+ x x) x)) )
      (t
        (let ((i (ash #xffffffff (- (- j0 20)))))

          (when (eql (logand i1 i) 0)
            ;; x is integral
            (return x) )

          (when (> (+ huge x) 0d0)
            ;; raise inexact flag
            (when (> i0 0)
              (if (eql j0 20)
                  (incf i0)
                (let ((j (+ i1 (ash 1 (- 52 j0)))))
                  (when (< j i1) (incf i0)) ; got a carry
                  (setq i1 j) )))
            (setq i1 (logand i1 (lognot i))) ) ) ))
    (return (encode-float64 i0 i1)) ) ) )
