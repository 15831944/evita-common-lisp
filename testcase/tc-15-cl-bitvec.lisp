;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; evcl - test - 9 Conditions.
;;; lisp/test/t09-condition
;;;
;;; This file is NOT part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2004 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-15-cl-bitvec.lisp#1 $
;;;
;;; Description:
;;;  This file contains test cases of conditions.
;
(in-package :tc-user)

;;; 15.2.33  bit, sbit 
;;    01234567890123456789012345678901
;; b1 -----1----1---11---1----1-------
;; b2      0123456789    |    |
;; b3           |    0123456789
;; b4           0123456789
(deftest cl-15-34-002 ()
  (let* ((b1 (make-array 32 :element-type 'bit :initial-element 0))
         (b2 (make-array 10 :element-type 'bit :displaced-to b1
                 :displaced-index-offset 5 ))
         (b3 (make-array 10 :element-type 'bit :displaced-to b1
                 :displaced-index-offset 15 ))
         (b4 (make-array 10 :element-type 'bit :displaced-to b1
                 :displaced-index-offset 10 ) ))
    (setf (bit b2 0) 1)
    (setf (bit b2 (1- (length b2))) 1)

    (setf (bit b3 0) 1)
    (setf (bit b3 (1- (length b2))) 1)

    (setf (bit b4 0) 1)
    (setf (bit b4 (1- (length b2))) 1)

    (values (bit-to-int b1)
            (bit-to-int b2)
            (bit-to-int b3)
            (bit-to-int b4) ) )
  (values #x108C420 #x221 #x211 #x231) )


;;; 15.2.34 bit-and, bit-andc1, bit-andc2, bit-eqv, bit-ior, bit-nand,
;;;         bit-nor, bit-not, bit-orc1, bit-orc2, bit-xor

(deftest cl-15-34-001 ()
    "Bound conditions"
  (loop
    for n from 2 to 16
    for base = (* n 32)
      nconc
        (loop
          for delta from -3 to 3
          for length = (+ base delta)
          for bv = (make-array length :element-type 'bit :initial-element 1)
            unless (eql (count 1 bv) length)
              collect (list 'make-array length)
            unless (eql (count 0 (bit-not bv)) length)
              collect (list 'bit-not length)
            unless (eql (count 0 (bit-xor bv bv)) length)
              collect (list 'bit-xor length) ))
    nil )
