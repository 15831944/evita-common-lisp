;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64 - truncate
;;; arch/generic/lisp/macth/gen-float64-truncate.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-ftruncate.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-truncate
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
(defun float64-ftruncate (x)
    (declare (values double-float))
    (declare (type double-float x))
  (multiple-value-bind (msw lsw) (decode-float64 x)
  (prog* (
    (signbit (logand msw #x80000000))
    ;; Extract exponent field.
    (exponent_less_1023 (- (ash (logand msw #x7ff00000) -20) 1023))
    )
    (declare (type (integer -1023 1024) exponent_less_1023))
  (cond
    ((< exponent_less_1023 20)
      ;; All significant digits are in msw.
      (if (< exponent_less_1023 0)
          ;; -1 < x < 1, so result is +0 or -0.
          (return (encode-float64 signbit 0))
          ;; All relevant fraction bits are in msw, so lsw of the result is 0.
          (return (encode-float64
            (logior signbit
                    (logand msw
                        (lognot (ash #x000fffff (- exponent_less_1023))) ))
            0 ))) )
    ((> exponent_less_1023 51)
      (if (eql exponent_less_1023 1024)
          ;; x is infinite, or not a number, so trigger an exception.
          (return (+ x x))
          ;; All bits in the fraction fields of the msw and lsw are needed in
          ;; the result.
          (return x) ) )
    (t  ; (<= 20 exponent_less_1023 51)
      ;; All fraction bits in msw are relevant.  Truncate irrelevant
      ;; bits from lsw.
      (return (encode-float64 msw
        (logand (1- (ash 1 (- exponent_less_1023 20))) lsw)
        #+nil
        (logand (lognot (ash #xffffffff (- (- exponent_less_1023 20))))
                lsw ))) )) ) ) )
