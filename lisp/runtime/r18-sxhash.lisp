;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 18 Hash Tables
;;; lisp/runtime/r18-hash-table.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r18-sxhash.lisp#8 $
;;;
;;; Description:
;;;  This fils contains following functions:
;;;     sxhash/eq                   internal    intrinsic
;;;     sxhash/eql                  internal    arch
;;;     sxhash/equalp               internal
;;;     sxhash                      18.2.14
;
(in-package :si)

;;;; sxhash/equalp
;;;
;;;     Characters
;;;      True if they are char-equal.
;;;     Numbers
;;;      True if they are =.
;;;     Conses
;;;      True if cars are equalp and cdrs are equalp.
;;;     Arrays
;;;      True if they are same number of dimensions and all elements are equalp
;;;     Records
;;;      True if they are same class and all slots are equalp.
;;;     Hash Tables
;;;      True if count, test and values are equalp.
;
(defun sxhash/equalp (object)
    (declare (values ext:sequence-index))
  (labels (
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; hash
    ;;
    (hash (x y)
        (declare (type ext:sequence-index x y))
        (declare (values ext:sequence-index))
      (let* ((y (logand y #xFFFFFF))
             (x (logxor x y)) )
        (logand (logior (ash (logand x #xFFFF) 8) (ash x -16))
                #.(1- (ash 1 27)) ) ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; sxhash-aux
    ;;
    (sxhash-aux (object depth hash-code)
      (when (>= depth 10)
        (return-from sxhash-aux (hash hash-code 12345)) )

      (incf depth)

      (typecase object
        (character
          (setq hash-code (sxhash/eq (char-downcase object))) )

        (cons
          (dotimes (i 10)
            (setq hash-code (sxhash-aux (car object) depth hash-code))
            (setq object (cdr object))
            (unless (consp object)
              (setq hash-code (sxhash-aux object depth hash-code))
              (return) )) )

        (string
          (multiple-value-bind (string start end) (string-data object 0 nil)
            (loop
              for index from start below end
              for char = (char-downcase (schar string index)) do
                (setq hash-code (hash hash-code (char-code char))) ) ) )

        (array
          (dotimes (i (array-total-size object))
            (setq hash-code
              (sxhash-aux (row-major-aref object i) depth hash-code) ) ) )

        (structure-object
          (loop
            for eslotd in (class-slots (class-of object))
            for location = (slot-value eslotd 'location)
            for val = (structure-instance-access object location) do
              (setq hash-code (sxhash-aux val depth hash-code)) ) )

        (hash-table
          (setq hash-code
            (sxhash-aux (hash-table-test object) depth hash-code) )
          (with-hash-table-iterator (next object)
            (loop
              (multiple-value-bind (more? key val) (next)
                (unless more? (return))
                (setq hash-code (sxhash-aux key depth hash-code))
                (setq hash-code (sxhash-aux val depth hash-code)) )) ) )

        (single-float
          (setq hash-code
            (sxhash-aux (float object 1d0) depth hash-code)) )

        (complex
          (setq hash-code
            (sxhash-aux (realpart object) depth hash-code) )

          (setq hash-code
            (sxhash-aux (imagpart object) depth hash-code) ) )

        (otherwise
          (setq hash-code (hash hash-code (sxhash object))) ))
      hash-code )
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; sxhash/equalp
    ;;
    (sxhash-aux object 0 0) ) )


;;;; 18.2.14 sxhash
;;;
;;; Syntax:
;;;     sxhash object => hash-code (non-negative fixnum)
;;;
;;; Note:
;;;     sxhash = sxhash/equal
;;;
;;;     Symbol
;;;         True if they are eq.
;;;     Numbers
;;;         True if they are eql.
;;;     Characters
;;;         True if they are eql.
;;;     Conses
;;;         True if two cars begin equal and two cdrs being eqaul recursivley.
;;;     Strings
;;;         True if all elements are eql.
;;;     Bit-Vectors
;;;         True if all elements are eql.
;;;     Arrays
;;;         True if two arrays are eq.
;;;     Pathnames
;;;         True if all the corresponding components are equal.
(defun cl:sxhash (object)
    (declare (values ext:sequence-index))
  (labels (
    ;; hash
    (hash (x y)
        (declare (type ext:sequence-index x y))
        (declare (values ext:sequence-index))
      (let* ((y (logand y #xFFFFFF))
             (x (logxor x y)) )
        (logand (logior (ash (logand x #xFFFF) 8) (ash x -16))
                #.(1- (ash 1 27)) ) ) )
    ;; sxhash-aux
    (sxhash-aux (object depth hash-code)
      (when (>= depth 10)
        (return-from sxhash-aux (hash hash-code 12345)) )

      (incf depth)

      (typecase object
        (cons
          (dotimes (i 10)
            (setq hash-code (sxhash-aux (car object) depth hash-code))
            (setq object (cdr object))
            (unless (consp object)
              (setq hash-code (sxhash-aux object depth hash-code))
              (return) )) )

        (symbol
          (setq hash-code (hash hash-code (sxhash/eq object))) )

        (string
          (multiple-value-bind (string start end) (string-data object 0 nil)
            (loop
              for index from start below end
              for char = (schar string index) do
                (setq hash-code (hash hash-code (char-code char))) ) ) )

        (bit-vector
          (multiple-value-bind (string start end) (vector-data object 0 nil)
            (loop
              for index from start below end
              for bit = (sbit string index) do
                (setq hash-code (hash hash-code bit)) ) ) )

        (pathname
          (setq hash-code (hash hash-code (pathname-sxhash object))) )

        (otherwise
          (setq hash-code (hash hash-code (sxhash/eql object))) ))
      hash-code )
    )
    ;;
    ;; sxhash
    ;;
    (sxhash-aux object 0 0) ) )
