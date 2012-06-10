;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 12 Numbers
;;; arch/generic/lisp/runtime/gen-r12-number.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r12-number.lisp#7 $
;
(in-package :si)


;;;; 12.2.28 1+, 1-
(defun cl:1+ (z) (+ z 1))
(defun cl:1- (z) (- z 1))


;;;; 12.2.46 complex
(defun cl:complex (realpart &optional (imagpart 0))
  (labels (
    (expect-real (x)
      (error 'type-error :datum x :expected-type 'real) )
    )
    ;;
  (typecase realpart
    (rational
      (typecase imagpart
        (rational
          (if (eql imagpart 0)
              realpart
            (let ((cpx (.allocate-record
                            #.(class-description 'rational-complex) ) ))
              (setf (ref rational-complex realpart cpx) realpart)
              (setf (ref rational-complex imagpart cpx) imagpart)
              cpx )) )
        (double-float
          (complex (float realpart imagpart) imagpart) )
        (single-float
          (complex (float realpart imagpart) imagpart) )
        (otherwise (expect-real imagpart)) ) )
    (double-float
      (let ((cpx (.allocate-binobj
                    #.(class-description 'double-float-complex) ) ))
        (setf (ref double-float-complex realpart cpx) realpart)
        (setf (ref double-float-complex imagpart cpx)
            (float imagpart realpart) )
        cpx ) )
    (single-float
      (if (typep imagpart 'double-float)
          (complex (float realpart imagpart) imagpart)
        (let ((cpx (.allocate-binobj
                        #.(class-description 'single-float-complex) ) ))
          (setf (ref single-float-complex realpart cpx) realpart)
          (setf (ref single-float-complex imagpart cpx)
              (float imagpart realpart) )
          cpx ) ) )
    (otherwise (expect-real realpart)) ) ) )


;;;; 12.2.53 denominator - rational => integer
(defun cl:denominator (x)
  (etypecase x
    (integer x)
    (ratio   (ref ratio denominator x)) ) )


;;;; 12.2.50 imagpart
(defun cl:imagpart (x)
  (typecase x
    (integer 0)
    (double-float 0d0)
    (single-float 0s0)
    ((complex rational)     (ref rational-complex imagpart x))
    ((complex double-float) (ref double-float-complex imagpart x))
    ((complex single-float) (ref single-float-complex imagpart x))
    (otherwise (error 'type-error :datum x :expected-type 'number)) ) )


;;;; 12.2.53 numerator
(defun cl:numerator (x)
  (etypecase x
    (integer x)
    (ratio   (ref ratio numerator x)) ) )


;;;; 12.2.50 realpart - rational => integer
(defun cl:realpart (x)
  (typecase x
    (real x)
    ((complex rational)     (ref rational-complex realpart x))
    ((complex double-float) (ref double-float-complex realpart x))
    ((complex single-float) (ref single-float-complex realpart x))
    (otherwise (error 'type-error :datum x :expected-type 'number)) ) )


;;;; logeqv/2
(defun logeqv/2 (x y)
  (lognot (logxor/2 x y)) )

;;;; lognot
(defun lognot (x)
  (logxor/2 x -1) )


;;;; 12.2.51  cl:upgraded-complex-part-type
(defun cl:upgraded-complex-part-type (ty &optional env)
    (declare (type type-specifier typespec))
  (cond
    ((subtypep ty 'rational env) 'rational)
    ((subtypep ty 'single-float env) 'single-float)
    ((subtypep ty 'double-float env) 'double-float)
    ((subtypep ty 'float env)        'double-float)
    (t (error "Type ~S must be subtype of real." ty)) ) )
