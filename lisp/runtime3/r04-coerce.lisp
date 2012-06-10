;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - 4 Types and Classes - coerce
;;; lisp/runtime3/r04-coerce.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime3/r04-coerce.lisp#2 $
;;;
;;; Description:
;;;  This fils contains declarations for type system.
;
(in-package :si)

;;;; 4.4.24 coerce
(defun cl:coerce (object result-type)
  (labels (
    ;; coerce-aux
    (coerce-aux ()
      (cond
        ((subtypep result-type 'list)
          (coerce-to-list) )
        ((subtypep result-type 'vector)
          (coerce-to-vector) )
        ((subtypep result-type 'character)
          (coerce-to-character) )
        ((subtypep result-type 'complex)
          (coerce-to-complex) )
        ((subtypep result-type 'double-float)
          (float object 0d0) )
        ((subtypep result-type 'single-float)
          (float object 0f0) )
        ((subtypep result-type 'function)
          (coerce-to-function) )
        (t
          (unsupported) )) )

    ;; coerce-to-character
    (coerce-to-character ()
      (let ((string (string object)))
        (if (eql (length string) 1)
          (char string 0)
        (unsupported) ) ) )

    ;; coerce-to-complex
    (coerce-to-complex ()
      (complex object) )

    ;; coerce-to-function
    (coerce-to-function ()
      (cond
        ((function-name-p object) (fdefinition object))
        ((and (consp object) (eq (first object) 'lambda))
            (compile nil object) )
        (t (unsupported)) ) )

    ;; coerce-to-list
    (coerce-to-list ()
      (let* ((head (list 0))
             (tail head) )
        (dotimes (i (length object) (cdr head))
          (setq tail (setf (cdr tail) (list (row-major-aref object i)))) ) ) )

    ;; coerce-to-vector
    (coerce-to-vector ()
      (let ((result
              (multiple-value-bind (elty size) (parse-vecty result-type)
                 (make-array (or size (length object)) :element-type elty) ) ))
        (if (listp object)
            (let ((i 0) (n (length result)))
              (dolist (elt object)
                (when (>= i n) (return))
                (setf (row-major-aref result i) elt)
                (incf i) )
              result )
          (dotimes (i (min (length result) (length object)) result)
            (setf (row-major-aref result i)
                    (row-major-aref object i) ) )) ) )

    ;; parse-vecty
    (parse-vecty (ty)
      (case (if (consp ty) (first ty) ty)
        ((string simple-string)
          (values 'character (ty/second ty)) )
        ((bit-vector simple-bit-vector)
          (values 'bit (ty/second ty)) )
        ((vector)
          (values (or (ty/second ty) 't) (ty/third ty)) )
        ((simple-vector)
          (values 't (ty/second ty)) )
        (t (unsupported)) ) )

    ;; ty/second
    (ty/second (ty)
      (when (consp ty)
        (let ((x (second ty))) (if (eq x '*) nil x)) ) )

    ;; ty/third
    (ty/third (ty)
      (when (consp ty)
        (let ((x (third ty))) (if (eq x '*) nil x)) ) )

    ;; unsupported
    (unsupported ()
      (error 'coerce-error :datum object :expected-type result-type) )
    )
    ;;
    (if (typep object result-type)
        object
      (let ((result (coerce-aux)))
        (cond
          ((typep result result-type) result)
          ((and (rationalp result) (subtypep result-type '(complex rational)))
            result )
          (t (error 'coerce-error :datum result
                :expected-type result-type ) )) )) ) )
