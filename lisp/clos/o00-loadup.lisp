;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 7 Objects - BOOT - Load CLOS modules
;;; lisp/clos/o00-loadup.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/o00-loadup.lisp#2 $
;;;
;;; Description:
;;;  This file contains forms for creating slot accessors.
;;;
;
;(in-package :cl-user)
;(load "o01-defpackage")

(in-package :si)

(load "../lisp/clos/o02-defs.lisp")
(load "../lisp/clos/o03-boot.lisp")
(load "../arch/generic/lisp/clos/gen-o04-emf.lisp")
(load "../lisp/clos/o04-emf.lisp")
(load "../lisp/clos/o05-dfun.lisp")
(load "../lisp/clos/o06-cdfun.lisp")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Set param-info slot of generic-functions that created by BOOT CLOS.
;
(labels (
  (patch (class)
    (dolist (method (specializer-direct-methods class))
      (patch-1 method) )
    (dolist (subclass (class-direct-subclasses class))
      (patch subclass) ) )

  ;; patch-1
  (patch-1 (method)
    (let ((gf (method-generic-function method)))
      (when (slot-value gf 'param-info)
        (return-from patch-1) )
      (format t "; Set lambda-list ~S~%" (slot-value gf 'name))
      (set-generic-function-lambda-list gf (method-lambda-list method) nil) ) )
  )
  ;;
  (patch (find-class 't))
 ) ; labels


;; Load real subst-in-function-1
(load "../arch/evm/lisp/clos/evm-o04-emf.lisp")

;;; Until here, we can redefine ordinal function by generic-function.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load methods
;;;

(load "../lisp/clos/o11-method.lisp")
(load "../lisp/clos/o12-class.lisp")
(load "../lisp/clos/o13-slot.lisp")
(load "../lisp/clos/o14-gf.lisp")
(load "../arch/generic/lisp/clos/gen-o14-gf.lisp")
(load "../lisp/clos/o15-defgeneric.lisp")
(load "../lisp/clos/o19-forward-referenced-class.lisp")
(load "../lisp/clos/o20-struct.lisp")
(load "../lisp/clos/o21-built-in-class.lisp")


;;; Convert early generic-function to real general-funciton.
(labels (
  (convert-to-gf (fname slambda-list)
    (when (typep (fdefinition fname) 'generic-function)
      (return-from convert-to-gf) )

    (let* ((lambda-list  (extract-lambda-list slambda-list))
           (specializers (extract-specializer-names slambda-list))
           (gf (make-instance
                 'standard-generic-function
                 :name fname ) )
           (mt (make-instance
                 'standard-method
                 :specializers (mapcar 'find-class specializers)
                 :lambda-list  lambda-list
                 :function     (fdefinition fname) ) ))
      (format t "; Convert to generic-function ~S~%" fname)
      (add-method gf mt)
      ;(set-funcallable-instance-function gf (compute-discriminator gf))
      (setf (fdefinition fname) gf) ) )
  )
  ;;
  (convert-to-gf 'make-method-lambda
                 '((gf standard-generic-function)
                   (mt standard-method)
                   form-lambda
                   env ))
  (convert-to-gf 'compute-discriminating-function
                 '((gf standard-generic-function)) )
 ) ; labels


;;;
;;; Enable Generic Functions
;;;
;;; Note: Since discriminator uses slot-value, slot-value must handle
;;  instances of standard-class, standard-generic-function and
;;; standard-method without calling slot-value-using-class.
;;;
(loop for (fname . gf) in *early-generic-function-alist* do
  (format t "; Fix early generic-function ~S~%" fname)

  ;; For compiler in make-discriminator/*
  (when (eq 'print-object fname)
    (set-funcallable-instance-function gf (compute-discriminator gf)) )

  (setf (fdefinition fname) gf) )

(unintern 'si::*early-generic-function-alist*)
