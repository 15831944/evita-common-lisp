;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64 - mod
;;; arch/generic/lisp/macth/gen-float64-mod.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-mod.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-mod
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* @(#)e_fmod.c 5.1 93/09/24 */
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
 * __ieee754_fmod(x,y)
 * Return x mod y in exact arithmetic
 * Method: shift and subtract
 */
|#
(defun float64-mod (x y)
    (declare (values double-float))
    (declare (type double-float x y))
  (multiple-value-bind (hx lx) (decode-float64 x)
  (multiple-value-bind (hy ly) (decode-float64 y)
  (prog* (
    (one    +1d0)
    (pzero  +0d0)
    (mzero  -0d0)
    ;;
    (sx (logand hx #x80000000))
    (hx (logand hx #x7fffffff))
    (hy (logand hy #x7fffffff))
    (ix 0)
    (iy 0)
    )

    ;; purge off exception values
    ;; y=0,or x not finite or y is NaN
    (when (or (eql (logior hy ly) 0)
              (>= hx #x7ff00000)
              (> (logior hy (ash (logior ly (- ly)) -31)) #x7ff00000) )
      (return (/ (* x y) (* x y))) )

    (when (<= hx hy)
      ;; |x|<|y| return x
      (when (or (< hx hy) (< lx ly)) (return x))

      ;; |x|=|y| return x*0
      (when (eql lx ly)
        (return (if (eql sx 0) pzero mzero)) ))

    ;; determine ix = ilogb(x)
    (setq ix
      (cond
        ((>= hx #x00100000)
          (- (ash hx -20) 1023) )
        ;; subnormal x
        ((eql hx 0)
          (setq ix -1043)
          (loop for i = lx then (ash i -1) until (eql i 0) do
            (decf ix) ) )
        (t
          (setq ix -1022)
          (loop for i = (ash hx -11) then (ash i -1) until (eql i 0) do
            (decf ix) ) )))

    ;; determine iy = ilogb(y)
    (setq iy
      (cond
        ((>= hy #x00100000)
          (- (ash hy -20) 1023) )
        ;; subnormal y
        ((eql hy 0)
          (setq iy -1043)
          (loop for i = ly then (ash i -1) until (eql i 0) do
            (decf iy) ) )
        (t
          (setq iy -1022)
          (loop for i = (ash hy -11) then (ash i -1) until (eql i 0) do
            (decf iy) ) )))

    ;; set up {hx,lx}, {hy,ly} and align y to x
    (if (>= ix -1022)
        (setq hx (logior #x00100000 (logand #x000fffff hx)))
      ;; subnormal x, shift x to normal
      (let ((n (- -1022 ix)))
        (if (<= n 31)
            (setq hx (logior (ash hx n) (ash lx (- 32 n)))
                  lx (ash lx n) )
            (setq hx (ash lx (- n 32))
                  lx  0 )) ))

    (if (>= iy -1022)
        (setq hy (logior #x00100000 (logand #x000fffff hy)))
      ;; subnormal y, shift y to normal
      (let ((n (- -1022 iy)))
        (if (<= n 31)
            (setq hy (logior (ash hy n) (ash ly (- (- 32 n))))
                  ly (ash ly n) )
            (setq hy (ash ly (- n -32))
                  ly  0 )) ))

    ;; fix point fmod
    (loop named noname
      with hz = 0
      with lz = 0
      for n = (- ix iy) then (1- n)
      until (eql n 0) do
        (setq hz (- hx hy))
        (setq lz (- lx ly))

        (when (< lx ly) (decf hz 1))

        (cond
          ((< hz 0)
            (setq hx (+ hx hx (ash lx -31)))
            (setq lx (+ lx lx)) )
           ((eql (logior hz lz) 0)
            ;; return sign(x)*0
            (return (if (eql sx 0) pzero mzero)) )
           (t
            (setq hx (+ hz hz (ash lz -31)))
            (setq lx (+ lz lz)) ))
      finally
        (setq hz (- hx hy))
        (setq lz (- lx ly))
        (when (< lx ly) (decf hz))
        (when (>= hz 0) (setq hx hz lx lz)) )

    ;; convert back to floating value and restore the sign
    (when (eql (logior hx lx) 0)
        ;; return sign(x)*0
      (return (if (eql sx 0) pzero mzero)) )

    ;; normalize x
    (loop while (< hx #x00100000) do
      (setq hx (+ hx hx (ash lx -31)))
      (setq lx  (+ lx lx))
      (decf iy) )

    ;; normalize output
    (when (>= iy -1022)
      (return (encode-float64
                (logior sx (- hx #x00100000) (ash (+ iy 1023) 20))
                lx )))

    ;; subnormal output
    (let ((n (- -1022 iy)))
      (cond
        ((<= n 20)
          (setq lx (logior (ash lx (- n)) (ash hx (- 32 n))))
          (setq hx (logior (ash hx (- n)) sx)) )
        ((<= n 31)
          (setq lx (logior (ash hx (- 32 n)) (ash lx (- n))))
          (setq hx sx) )
        (t
          (setq lx (ash hx (- (- n 32))))
          (setq hx sx) ))
      (let* ((x (encode-float64 hx lx))
             ;; create necessary signal
             (y (* one x)) )
        ;; exact output
        (return y) ) ) ) ) ) )
