;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Dev - 25 Environment - Inspect
;;; dev/d25-inspect.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d25-inspect.lisp#2 $
;;;
;;; Description:
;;;  This file contains functions for development:
;;;     inspect     25.2.18
;
(in-package :devel)

;;;; 25.2.18 inspect
;;;
;;; Note:
;;;  DO NOT STORE OBJECT-HEADER INTO MEMORY!
;
(defun cl:inspect (object)
  (let ((cells '()))
    (typecase object
      ((or fixnum character)
        (setq cells (list object)) )

      (bignum
        (push (si:record-ref object 0) cells) ; header
        (push (si:record-ref object 1)  cells) ; length
        (dotimes (i (si:record-ref object 1))
          (push (si:ubyte32 object (+ i 2)) cells) ) )

      (double-float
        (push (si:record-ref object 0) cells)
        (push (si:ubyte32 object 1) cells)
        (push (si:ubyte32 object 2) cells)
        (push (si:ubyte32 object 3) cells) )

      (list
        (push (car object) cells)
        (push (cdr object) cells) )

      (si:marker
        (push (ash (si:address-of object) -2) cells) )

      ((or simple-bit-vector
           si:double-float-vector
           si:signed-byte-8-vector
           si:signed-byte-16-vector
           si:signed-byte-32-vector
           si:single-float-vector
           si:unsigned-byte-8-vector
           si:unsigned-byte-16-vector
           si:unsigned-byte-32-vector )
        (push (si:record-ref object 0) cells)
        (push (si:record-ref object 1) cells)
        (dotimes (i (si:record-ref object 1))
          (push (si:ubyte32 object (+ i 2)) cells) ) )

      (simple-string
        (push (si:record-ref object 0) cells)
        (push (si:record-ref object 1) cells)
        (dotimes (i (si:record-ref object 1))
          (push (schar object i) cells) ) )

      (single-float
        (push (si:record-ref object 0) cells)
        (push (si:ubyte32 object 1) cells) )

      (otherwise
        (setq cells (inspect-object object)) ))

  (let ((index 0)
        (*print-array*  nil)
        (*print-pretty* t)
        (*print-circle* nil)
        (*print-lines*  1) )
    (format t "; Slots of ~S" object)
    (terpri)

    (dolist (cell (nreverse cells))
      (if (eq #.(si:unbound-marker) cell)
          (format t ";  [~D] .. Unbound .." index)
        (format t ";  [~D] ~S" index cell) )
      (terpri)
      (incf index) ) ) )

  object )


;;;; inspect-object
;
(defmethod inspect-object (object)
    (declare (values list))
  (labels (
    ;; list-cells
    (list-cells (object length)
      (loop for i from (1- length) downto 0
        collect (si:record-ref object i) ) )
    )
    ;;
    (let ((classd (si::classd-of object)))
      (case (si::.classd-detail classd)
        ((#.si::|ClassD.InstanceFormat.Fixed|)
          (list-cells object (si::.classd-format-hint classd)) )
        (otherwise
          (list object) )) ) ) )


(defmethod inspect-object ((object structure-object))
  (let ((cells '()))
    (push (si::.structure-classd object) cells)
    (loop for index from 1 below (si::record-length object) do
      (push (si:record-ref object index) cells) )
    cells ) )


(defmethod inspect-object ((object standard-object))
  (let ((cells '()))
    (push (si::classd-of object) cells)
    (loop for index from 1 below (si::instance-length object) do
      (push (si:instance-ref object index) cells) )
    cells ) )


(defmethod inspect-object ((object clos:funcallable-standard-object))
  (let ((cells '()))
    (push (si::.structure-classd object) cells)
    (loop for index from 1 below (si::record-length object) do
      (push (si:record-ref object index) cells) )
    cells ) )


;;;; inspect-object ext:pointer
;
(defmethod inspect-object ((object ext:pointer))
    (declare (values list))
  (list (si::.pointer-base object)
        (si::.pointer-type object)
        (si::.pointer-address object)
        (si:record-ref object 0) ) )


(defmethod inspect-object ((array array))
    (declare (values list))
  (loop
    with cells = '()
    for index below (si::.array-length array) do
      (push (si:record-ref array index) cells)
    finally (return cells) ) )


;;; inspect-object symbol
;
(defmethod inspect-object ((symbol symbol))
  (let ((cells '()))
    (push (si::symbol-hash-code symbol) cells)
    (push (symbol-name symbol) cells)
    (push (symbol-package symbol) cells)
    (push (and (fboundp symbol) (symbol-function symbol)) cells)
    cells ) )
