;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - Declarations
;;; arch/generic/lisp/math/gen-math-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d03-proclaim.lisp#1 $
;;;
;;; Description:
;;; TBD
;
(in-package :si)

(declaim (ftype (function (double-float) double-float)
 float64-acos   float64-asin    float64-atan   #+nil "see float64-atan/2"
 float64-acosh  float64-asinh   float64-atanh
 float64-cos    float64-sin     float64-tan
 float64-cosh   float64-sinh    float64-tanh

 float64-exp float64-expm1
 float64-log float64-log1p

 float64-fceiling float64-ffloor float64-fround float64-ftruncate ) )


(declaim (ftype (function (double-float double-float) double-float)
 float64-atan/2
 float64-expt
 float64-mod
 float64-rem ) )


(declaim (ftype (ftunction (double-float double-float) double-float)
  float64-kernel-cos ) )

(declaim (ftype (ftunction (double-float double-float fixnum) double-float)
  float64-kernel-sin
  float64-kernel-tan ) )

(declaim (ftype (function (double-float)
            (values fixnum double-float double-float) )
 float64-kernel-rem-pio2 ) )
