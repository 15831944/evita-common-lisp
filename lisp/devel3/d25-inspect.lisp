;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - devel - 25 Environment - Inspect
;;; lisp/devel3/d25-inspect.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel3/d25-inspect.lisp#1 $
;;;
;;; Description:
;;;  This file constains implemenation of
;;;     inspect
;
(in-package :si)

(defgeneric inspect-object (object))

(deftlv *inspect* nil)

;;;; 25.2.18 inspect
(defun cl:inspect (object)
  (labels (
    (dump (object)
      (loop
        with list = (sort (inspect-object object) 'string< :key 'car)
        for nth from 0
        for (slot-name boundp value) in list do
          (format t "; [~2D] ~S ~30T" nth slot-name)
          (if (not boundp)
              (format t "... Unbound ...")
            (let ((*print-lines* 1))
              (format t "~:W" value) ))
          (terpri)
        finally
          (when list (setq *inspect* list)) ) )
    )
    (let ((object (if (and (integerp object) (not (minusp object)))
                      (third (nth object *inspect*))
                    object )))
      (dump object)
      object ) ) )


;;;; inspect-object t
(defmethod inspect-object (o)
  (list (list 'value t o)) )


;;;; inspect-object array
(defmethod inspect-object ((o array))
  (multiple-value-bind (displaced-to offset) (array-displacement o)
    (list
      (list 'adjustable             t (adjustable-array-p o))
      (list 'dimensions             t (array-dimensions o))
      (list 'element-type           t (array-element-type o))
      (list 'rank                   t (array-rank o))
      (list 'displaced-to           t displaced-to)
      (list 'displaced-index-offset t offset) ) ) )


;;;; inspect-object array
(defmethod inspect-object ((o vector))
  (nconc
    (call-next-method)
    (list
       (list 'fill-pointer
          t
          (and (array-has-fill-pointer-p o) (fill-pointer o)) ))) )


;;;; inspect-object class-description
(defmethod inspect-object ((o class-description))
  (list
    (list 'class          t (ref class-description class o))
    (list 'hash-code      t (ref class-description hash-code o))
    (list 'slots          t (ref class-description slots o))
    (list 'format         t (ref class-description format o))
    (list 'format-param   t (ref class-description format-param o))
    (list 'format-misc    t (ref class-description format-misc o))
    (list 'tag-code       t (ref class-description tag-code o))
    (list 'typespec       t (ref class-description typespec o))
    (list 'element-type   t (ref class-description element-type o)) ) )

;;;; inspect-object cons
(defmethod inspect-object ((o cons))
  (list (list 'car t (car o)) (list 'cdr t (cdr o))) )


;;;; inspect-object float
(defmethod inspect-object ((o float))
  (multiple-value-bind (f e s) (integer-decode-float o)
    (list (list 'significand t f)
          (list 'exponent    t e)
          (list 'sign        t s) ) ) )


;;;; inspect-object hash-table
(defmethod inspect-object ((o hash-table))
  (list (list 'count            t (hash-table-count o))
        (list 'rehash-size      t (hash-table-rehash-size o))
        (list 'rehash-threshold t (hash-table-rehash-threshold o))
        (list 'size             t (hash-table-size o))
        (list 'test             t (hash-table-test o)) ) )


;;;; inspect-object package
(defmethod inspect-object ((o package))
  (list (list 'name                 t (package-name o))
        (list 'nicknames            t (package-nicknames o))
        (list 'shadowing-symbols    t (package-shadowing-symbols o))
        (list 'used-by-list         t (package-used-by-list o))
        (list 'use-list             t (package-use-list o)) ) )


;;;; inspect-object standard-object
(defmethod inspect-object ((o standard-object))
  (loop
    with class = (class-of o)
    for eslotd in (class-slots class)
    for boundp = (slot-boundp-using-class class o eslotd)
     collect
        (list (slot-definition-name eslotd)
              boundp
              (and boundp (slot-value-using-class class o eslotd)) )) )


;;;; inspect-object structure-object
(defmethod inspect-object ((o structure-object))
  (loop
    with class = (class-of o)
    for eslotd in (class-slots class)
    for loc = (slot-definition-location eslotd)
     collect (list (slot-definition-name eslotd)
                   t
                   (structure-instance-access o loc) ) ) )


;;;; inspect-object symbol
(defmethod inspect-object ((o symbol))
  (list (list 'name     t (symbol-name o))
        (list 'package  t (symbol-package o))
        (list 'plist    t (symbol-plist o))
        (list 'value    (boundp o) (and (boundp o) (symbol-value o))) ) )
