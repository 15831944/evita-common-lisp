;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - Type System - 4 Types and Classes
;;; lisp/runtime/r04-coerce.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r04-coerce.lisp#2 $
;;;
;;; Description:
;;;  This fils contains implementation of coerce functions.
;
(in-package :si)


;;;; coerce-error
;
(defun coerce-error (object typespec)
  (runtime-type-error
    typespec
    object
    "Can't coerce ~S to type ~S."
    object
    typespec ) )


;;;; coerce-to-character
;
(defgeneric coerce-to-character (x)
  (:method (x) x)
  (:method ((x string)) (when (eql (length x) 1) (char x 0)))
  (:method ((x symbol)) (coerce-to-character (symbol-name x))) )


;;;; coerce-to-complex
;
(defgeneric coerce-to-complex (x)
  (:method (x) x)
  (:method ((x float)) (complex x)) )


;;; coerce-to-float
;
(defgeneric coerce-to-float (x)
  (:method (x) x)
  (:method ((x float)) x)
  (:method ((x real)) (float x 1f0)) )


(defgeneric coerce-to-list (x)
  (:method (x) x)
  (:method ((x list)) x)
  (:method ((x vector)) (loop for elt across x collect elt)) )


;;; coerce-to-double-float
;
(defgeneric coerce-to-double-float (x)
  (:method (x) x)
  (:method ((x double-float)) x)
  (:method ((x real)) (float x 1d0)) )


;;;; coerce-to-double-float-complex
;
(defgeneric coerce-to-double-float-complex (x)
  (:method (x) x)
  (:method ((x real)) (complex (float x 0d0) 0d0))
  (:method ((x complex))
    (complex (float (realpart x) 0d0) (float (imagpart x) 0d0)) ) )


;;;; coerce-to-function symbol
;
(defgeneric coerce-to-function (x)
  (:method (x) x)
  (:method ((x symbol))
    (if (and (fboundp x)
             (null (macro-function x))
             (not (special-operator-p x)) )
        (symbol-function x)
      (call-next-method) ) )
  (:method ((x cons))
    (cond
      ((and (function-name-p x) (fboundp x))
        (fdefinition x) )
      ((and (>= (safe-list-length x) 2) (eq (first x) 'lambda))
        (values (compile nil x)) )
      (t
        (call-next-method) )) ) )


;;; coerce-to-single-float
;
(defgeneric coerce-to-single-float (x)
  (:method (x) x)
  (:method ((x single-float)) x)
  (:method ((x real)) (float x 1f0)) )


;;;; coerce-to-single-float-complex
;
(defgeneric coerce-to-single-float-complex (x)
  (:method (x) x)
  (:method ((x real)) (complex (float x 0f0) 0f0))
  (:method ((x complex))
    (complex (float (realpart x) 0f0) (float (imagpart x) 0f0)) ) )


;;;; coerce-to-vector
;
(defgeneric coerce-to-vector (x elttype size)
  (:method (x e s) (declare (ignore e s)) x)
  (:method ((x sequence) elttype size)
    (let ((length (length x)))
      (cond
        ((eq size '*)
          (make-array length :initial-contents x :element-type elttype) )
        ((eq size length)
          (make-array size   :initial-contents x :element-type elttype) )
        (t x) ) ) ) )


;;;; 4.4.25 coerce
;
(defun cl:coerce (object type)
    (declare (type t object))
    (declare (type ext:type-specifier type))
    (declare (values t))
  (let ((tyo (parse-type type)))
    (tyo-coerce object tyo) ) )
