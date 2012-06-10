;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64 - round
;;; arch/generic/lisp/macth/gen-float64-round.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-fround.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-round
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
(defun float64-fround (x)
    (declare (values double-float))
    (declare (type double-float x))
  (multiple-value-bind (msw lsw) (decode-float64 x)
  (prog* (
    (exponent_less_1023 (- (ash (logand msw #x7ff00000) -20) 1023))
    )
    (cond
      ((< exponent_less_1023 0)
          (let ((msw (logand msw #x80000000)))
            ;; Result is +1.0 or -1.0.
            (if (eql exponent_less_1023 -1)
                (return (encode-float64 (logior msw (ash 1023 20)) lsw))
              (return x) ) ) )
      ((< exponent_less_1023 20)
        (let ((exponent_mask (ash #x000fffff (- exponent_less_1023))))
          (when (and (eql (logand msw exponent_mask) 0) (eql lsw 0))
            ;; x in an integral value.
            (return x) )
          (incf msw (ash #x00080000 (- exponent_less_1023)))
          (setq msw (logand msw (lognot exponent_mask)))
          (return (encode-float64 msw 0)) ) )
      ((> exponent_less_1023 51)
        (if (eql exponent_less_1023 1024)
            ;; x is NaN or infinite.
            (return (+ x x))
          (return x) ) )
      (t
        (let ((exponent_mask (ash #xffffffff (- (- exponent_less_1023 20)))))
          (when (eql (logand lsw exponent_mask) 0)
            ;; x is an integral value.
            (return x) )

          (let ((tmp (+ lsw (ash 1 (- 51 exponent_less_1023)))))
            (when (< tmp lsw) (incf msw))
            (setq lsw tmp) )

          (setq lsw (logand lsw (lognot exponent_mask)))
          (return (encode-float64 msw lsw)) ) )) ) ) )
