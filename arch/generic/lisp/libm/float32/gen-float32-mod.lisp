;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32 - mod
;;; arch/generic/lisp/macth/gen-float32-mod.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-mod.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-mod
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* ef_fmod.c -- float version of e_fmod.c.
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

/*
 * __ieee754_fmodf(x,y)
 * Return x mod y in exact arithmetic
 * Method: shift and subtract
 */
|#
(defun float32-mod (x y)
    (declare (values single-float))
    (declare (type single-float x y))
  (prog* (
    (one    1f0)
    (pzero  +0f0)
    (mzero  -0f0)
    ;;
    (hx (decode-float32 x))
    (hy (decode-float32 y))
    (sx (logand hx #x80000000))
    (hx (logand hx #x7fffffff))
    (hy (logand hy #x7fffffff))
    (ix 0)
    (iy 0)
    )

    ;; purge off exception values
    (when (or (eql hy 0)
              (>= hx #x7F800000)
              (>  hy #x7F800000) )
      (return (/ (* x y) (* x y))) )

    ;; |x|<|y| return x
    (when (< hx hy)
      (return x) )

    ;; |x|=|y| return x*0
    (when (eql hx hy)
      (return (if (eql sx 0) pzero mzero)) )

    ;; Note: y cannot be zero if we reach here.

    ;; determine ix = ilogb(x)
    (if (> hx #x007FFFFF)
        (setq ix (- (ash hx -23) 127))
        ;; subnormal x
        (progn
          (setq ix -126)
          (loop for i = (ash hx -8) then (ash i -1) until (eql i 0) do
            (decf ix) )))

    ;; determine iy = ilogb(y)
    (if (> hy #x007FFFFF)
        (setq iy (- (ash hy -23) 127))
        ;; subnormal x
        (progn
          (setq iy -126)
          (loop for i = (ash hy -8) then (ash i -1) until (eql i 0) do
            (decf iy) )))

    ;; set up {hx,lx}, {hy,ly} and align y to x
    (if (>= ix -126)
        (setq hx (logior #x00800000 (logand #x007fffff hx)))
        ;; subnormal x, shift x to normal
        (setq hx (ash hx (- -126 ix))) )

    (if (>= iy -126)
        (setq hy (logior #x00800000 (logand #x007fffff hy)))
        ;; subnormal x, shift x to normal
        (setq hy (ash hy (- -126 iy))) )

    ;; fix point fmod
    (loop named noname
      with hz = 0
      for n = (- ix iy) then (1- n)
      until (eql n 0) do
        (setq hz (- hx hy))
        (cond
          ((< hz 0)
            (setq hx (+ hx hx)) )
          ((eql hz 0)
            ;; return sign(x)*0
            (return (if (eql sx 0) pzero mzero)) )
          (t
            (setq hx (+ hz hz)) ))
      finally
        (setq hz (- hx hy))
        (when (>= hz 0) (setq hx hz)) )

    ;; convert back to floating value and restore the sign
    (when (eql hx 0)
      ;; return sign(x)*0
      (return (if (eql sx 0) pzero mzero)) )

    ;; normalize x
    (loop while (< hx #x00800000) do
      (setq hx (+ hx hx))
      (decf iy) )

    ;; normalize output
    (when (>= iy -126)
      (let ((hx (logior (- hx #x00800000) (ash (+ iy 127) 23))))
        (return (encode-float32 (logior hx sx))) ))

    ;; subnormal output
    ;; If denormals are not supported, this code will generate a
    ;; zero representation.
    (let* ((n  (- -126 iy))
           (hx (ash hx n))
           (x  (encode-float32 (logior hx sx)))
           ;; create necessary signal
           (y  (* x one)) )
      ;; exact output
      (return y) ) ) )
