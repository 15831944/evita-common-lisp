;;;; -*- Hypote: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32 - hypot
;;; arch/generic/lisp/macth/gen-float32-hypot.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-hypot.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-hypot
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* ef_hypot.c -- float version of e_hypot.c.
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
(defun float32-hypot (x y)
    (declare (values single-float))
    (declare (type single-float x y))
  (prog* (
    (ha (decode-float32 x))
    (ha (logand ha #x7fffffff))
    (a  (encode-float32 ha))
    (hb (decode-float32 y))
    (hb (logand hb #x7fffffff))
    (b  (encode-float32 hb))
    (k  0)
    )
    ;;
    (when (> hb ha)
      (rotatef a b)
      (rotatef ha hb) )

    ;; x/y > 2**30
    (when (> (- ha hb) #xf000000)
      (return (+ a b)) )

    ;; a>2**50
    (when (> ha #x58800000)
      ;; Inf or NaN
      (when (>= ha #x7f800000)
        (let ((w (+ a b)))  ; for sNaN
          (when (eql ha #x7f80000) (setq w a))
          (when (eql hb #x7f80000) (setq w b))
          (return w) ))

       ;; scale a and b by 2**-68
       (decf ha #x22000000)
       (decf hb #x22000000)
       (decf k 68)
       (setq a (encode-float32 ha))
       (setq b (encode-float32 hb)) )

    ;; b < 2**-50
    (when (< hb #x26800000)
      (cond
        ((eql hb 0)
          (return a) )
        ((< hb #x007fffff)
          (let ((t1 #.(encode-float32 #x7e800000)))    ; t1=2^126
            (setq b (* b t1))
            (setq a (* a t1))
            (decf k 126) ) )
        (t
          ;; scale a and b by 2^68
          (decf ha #x22000000)  ; a *= 2^68
          (decf hb #x22000000)  ; b *= 2^68
          (decf k 68)
          (setq a (encode-float32 ha))
          (setq b (encode-float32 hb)) )))

    ;; medium size a and b
    (let ((w (- a b)))
      (if (> w b)
          (let* ((t1 (encode-float32 (logand ha #xfffff000)))
                 (t2 (- a t1)) )
            (setq w (float32-sqrt
                (- (* t1 t1) (- (* b (- b)) (* t2 (+ a t1)))) )) )
          (let* ((a  (+ a a))
                 (y1 (encode-float32 (logand hb #xfffff000)))
                 (y2 (- b y1))
                 (t1 (encode-float32 (+ ha #x00800000)))
                 (t2 (- a t1)) )
          (setq w (float32-sqrt
                    (- (* t1 y1) (- (* w (- w)) (+ (* t1 y2) (* t2 b)))) )) ))

      (when (eql k 0)
        (return w) )

      (let ((t1 (encode-float32 (+ #x3f800000 (ash k 23)))))
        (return (* t1 w)) ) ) ) )
