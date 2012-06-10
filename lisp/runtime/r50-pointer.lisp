;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 50 Extensions
;;; runtime/r50-extension.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r50-pointer.lisp#2 $
;;;
;;; Description:
;;;  This function contains pointer object function.
;;;
;
(in-package :si)

;;;; *pointer-type-alist*
;;;
;;; Description:
;;;  The alist of pointer data-type and pointer type. LSB 3 bit contains
;;;  shift amount for increment and decrement.
;
(defconstant *pointer-type-alist* '(
    ((signed-byte 8)        .  #.|Pointer.ElementType.SByte8|)
    ((unsigned-byte 8)      .  #.|Pointer.ElementType.UByte8|)
    ((signed-byte 16)       .  #.|Pointer.ElementType.SByte16|)
    ((unsigned-byte 16)     .  #.|Pointer.ElementType.UByte16|)
    ((signed-byte 32)       .  #.|Pointer.ElementType.SByte32|)
    ((unsigned-byte 32)     .  #.|Pointer.ElementType.UByte32|)
    ((signed-byte 64)       .  #.|Pointer.ElementType.SByte64|)
    ((unsigned-byte 64)     .  #.|Pointer.ElementType.UByte64|)
    (single-float           .  #.|Pointer.ElementType.SingleFloat|)
    (double-float           .  #.|Pointer.ElementType.DoubleFloat|)
    ((complex single-float) .  #.|Pointer.ElementType.SingleFloatComplex|)
    ((complex double-float) .  #.|Pointer.ElementType.DoubleFloatComplex|)
    (character              .  #.|Pointer.ElementType.Character|)
    (t                      .  #.|Pointer.ElementType.LispVal|)
  ) ) ; *pointer-type-alist*


;;;; pointer+
;
(defun ext:pointer+ (pointer delta)
    (declare (type pointer pointer))
    (declare (type fixnum delta))
    (declare (values pointer))
  (let ((new-pointer
          (.make-pointer (.pointer-base pointer) (.pointer-type pointer)) ))
    (pointer-inc new-pointer (+ (pointer-diff pointer new-pointer) delta)) ) )


;;;; pointer-
;
(defun ext:pointer- (pointer delta)
    (declare (type pointer pointer))
    (declare (type fixnum delta))
    (declare (values pointer))
  (let ((new-pointer
          (.make-pointer (.pointer-base pointer) (.pointer-type pointer)) ))
    (pointer-inc new-pointer (- (pointer-diff pointer new-pointer) delta)) ) )

(macrolet (
  (define (op)
    (let ((fname (intern (format nil "POINTER~A" op))))
      `(defun ,fname (pointer-1 pointer-2)
           (declare (type pointer-1 pointer-2))
           (declare (values (or nil fixnum)))
          (,op (.pointer-diff pointer-1 pointer-2) 0) ) ) )
  )

  (define =)
  (define /=)
  (define <)
  (define <=)
  (define >)
  (define >=) )


(labels (
  (data-type-to-pointer-type (type)
      (declare (type ext:type-specifier type))
      (declare (type (unsigned-byte 4)))
     (cdr (assoc (upgraded-pointer-data-type type)
                 *pointer-type-alist*
                 :test #'equal )) )
  )

  ;;  ext:copy-pointer
  (defun ext:copy-pointer (pointer &optional (data-type nil data-type-p))
      (declare (type ext:pointer pointer))
      (declare (type ext:type-specifier data-type))
      (declare (values ext:pointer))
    (let ((new-pointer
            (.make-pointer (.pointer-base pointer) (.pointer-type pointer)) ))
      (pointer-inc new-pointer (pointer-diff pointer new-pointer))
      (when data-type-p
        (setf (.pointer-type new-pointer)
          (data-type-to-pointer-type data-type) ))
      new-pointer ) )

  ;; ext:make-pointer
  (defun ext:make-pointer (base &key (data-type 't) (offset 0) raw)
      (declare (type t base))
      (declare (type ext:type-specifier data-type))
      (declare (type fixnum offset))
      (declare (values ext:pointer))
    (let ((pointer 
            (if raw
                (let ((pointer
                        (.make-pointer base |Pointer.ElementType.UByte8|) ))
                  (setf (object-ref pointer 1) base)
                  pointer )
              (let ((type (data-type-to-pointer-type data-type)))
                (.make-pointer base type) )) ))
      (unless (zerop offset) (pointer-inc pointer offset))
      pointer ) )
 ) ; lables


;;;; pointer-data-type
;;;
;;; For: print-object
;
(defun ext:pointer-data-type (pointer)
    (declare (type ext:pointer pointer))
    (declare (type ext:type-specifier pointer))
  (car (rassoc (.pointer-type pointer) *pointer-type-alist*)) )


;;;; ext:pointer-dec
;
(defun ext:pointer-dec (pointer &optional (delta 1))
    (declare (type ext:pointer pointer))
    (declare (type fixnum delta))
    (declare (values ext:pointer))
  (.pointer-inc pointer (ash (- delta) (logand (.pointer-type pointer) 7))) )


;;;; ext:pointer-diff
;
(defun ext:pointer-diff (pointer-1 pointer-2)
    (declare (type ext:pointer pointer))
    (declare (values fixnum))
  (let ((type (.pointer-type pointer-1)))
    (unless (eql (.pointer-type pointer-2) type)
      (error 'pointer-type-mismatch
             :pointer1 pointer-1
             :pointer2 pointer-2 ) )
    (ash (.pointer-diff pointer-1 pointer-2) (- (logand type 7))) ) )


;;;; ext:pointer-inc
;
(defun ext:pointer-inc (pointer &optional (delta 1))
    (declare (type ext:pointer pointer))
    (declare (type fixnum delta))
    (declare (values ext:pointer))
  (.pointer-inc pointer (ash delta (logand (.pointer-type pointer) 7))) )


;;;; ext:upgraded-pointer-data-type
;
(defun ext:upgraded-pointer-data-type (type)
    (declare (type ext:type-specifier))
    (declare (values ext:type-specifier))
  (loop for (present) in *pointer-type-alist* do
    (when (subtypep type present)
      (return present) )
    finally (return 't) ) )
