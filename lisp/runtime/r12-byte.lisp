;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 12 Numbers - Byte
;;; lisp/runtime/r12-byte.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r12-byte.lisp#2 $
;;;
;;; See Also:
;;;  macro/m12-number.lisp
;;;
;;; Description:
;;;  This file contains following functions:
;;;
;;; Public Functions:
;;;     byte                    12.2.66
;;;     byte-size               12.2.66
;;;     byte-position           12.2.66
;;;     deposit-field           12.2.70
;;;     dpb                     12.2.68
;;;     ldb                     12.2.69     accessor
;;;     ldb-test                12.2.70
;;;     mask-field              12.2.71     accessor
;;;
;;; Writer of ldb and mask-field are defined by define-setf-expander.
;
(in-package :si)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;;

;;;; 12.2.66 byte
;;;
;;; Syntax:
;;;     (byte size position) => bytespec
;
(defun cl:byte (size position)
  ; (check-type size unsigned-byte)
  ; (check-type position unsigned-byte)
  (cons size position) )


;;;; 12.2.66 byte-position
;;; Syntax:
;;;     (byte-position bytespec) => position
;
(defun cl:byte-position (bytespec)
  ; (check-type bytespec bytespec)
  (cdr bytespec) )


;;;; 12.2.66 byte-size
;;; Syntax:
;;;     (byte-size BYTESPEC) => size
;
(defun cl:byte-size (bytespec)
  ; (check-type bytespec bytespec)
  (car bytespec) )


;;;; 12.2.70 deposite-field
;
(defun cl:deposit-field (newbyte bytespec integer)
  (let ((mask (ash (ldb (byte (byte-size bytespec) 0) -1)
                   (byte-position bytespec) )) )
    (logior (logand newbyte mask)
            (logand integer (lognot mask)) ) ) )


;;;; 12.2.68 dpb
;
(defun cl:dpb (newbyte bytespec integer)
  (let ((mask (1- (ash 1 (byte-size bytespec)))))
    (logior (logand integer (lognot (ash mask (byte-position bytespec))))
            (ash (logand newbyte mask) (byte-position bytespec)) ) ) )


;;;; 12.2.69 ldb
;;;
;;; Syntax:
;;;     ldb bytespec integer => result-integer
;;;
;;; Arguments and Values:
;;;     bytespec       - a byte specifier.
;;;     integer        - an integer.
;;;     result-integer - an integer.
;;;
;;; Description:
;;;  Loads byte.
;;;
(defun cl:ldb (bytespec integer)
    (declare (type bytespec bytespec))
    (declare (type integer integer))
  (logand (ash integer (- (byte-position bytespec)))
          (1- (ash 1 (byte-size bytespec))) ) )


;;;; 12.2.70 ldb-test
;;;
;;; Syntax:
;;;     ldb-test bytespec integer => boolean
;;;
;;; Arguments and Values:
;;;     bytespec - a byte specifier.
;;;     integer  - an integer.
;;;     boolean  - a generalized boolean.
;;;
;;; Description:
;;;  Tests byte.
;;;
(defun cl:ldb-test (bytespec integer)
    (declare (type bytespec bytespec))
    (declare (type integer integer))
  (not (zerop (ldb bytespec integer))) )


;;;; 12.2.71 mask-field
;
(defun cl:mask-field (bytespec integer)
    (declare (type bytespec bytespec))
  (logand integer (dpb -1 bytespec 0)) )
