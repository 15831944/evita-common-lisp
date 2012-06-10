;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32 - round
;;; arch/generic/lisp/macth/gen-float32-round.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-fround.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-round
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
(defun float32-fround (x)
    (declare (values single-float))
    (declare (type single-float x))
  (prog* (
    (w (decode-float32 x))
    (exponent_less_127 (- (ash (logand w #x7f800000) -23) 127))
    )
    (cond
      ((< exponent_less_127 0)
        (setq w (logand w #x80000000))
        (when (eql exponent_less_127 -1)
          ;; Result is +1.0 or -1.0.
          (setq w (logior w (ash 127 23))) )
        (return (encode-float32 w)) )
      ((< exponent_less_127 23)
        (let ((exponent_mask (ash #x007fffff (- exponent_less_127))))
          (when (eql (logand w exponent_mask) 0)
            ;; x has an integral value.
            (return x) )

          (incf w (ash #x00400000 (- exponent_less_127)))
          (setq w (logand w (lognot exponent_mask)))
          (return (encode-float32 w)) ) )
      ((eql exponent_less_127 128)
        ;; x is NaN or infinite.
        (return (+ x x)) )
      (t
        (return x) )) ) )
