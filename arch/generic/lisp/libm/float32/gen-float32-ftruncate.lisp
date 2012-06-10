;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32 - truncate
;;; arch/generic/lisp/macth/gen-float32-truncate.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-ftruncate.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-truncate
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

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
(defun float32-ftruncate (x)
    (declare (values single-float))
    (declare (type single-float x))
  (prog* (
    (w (decode-float32 x))
    (signbit (logand w #x80000000))
    (exponent_less_127 (- (ash (logand w #x7f800000) -23) 127))
    )
    (cond
      ((< exponent_less_127 0)
        ;; -1 < x < 1, so result is +0 or -0.
        (return (encode-float32 signbit)) )
      ((< exponent_less_127 23)
        (return (encode-float32
            (logior signbit
                (logand (lognot (ash #x007fffff (- exponent_less_127)))
                        w )))) )
      ((eql exponent_less_127 128)
        ;; x is NaN or infinite.
        (return (+ x x)) )
      (t
        ;; All bits in the fraction field are relevant.
        (return x) )) ) )
