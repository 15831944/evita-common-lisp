;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 15 Arrays - Bit Vector
;;; arch/generic/lisp/runtime/gen-r15-bitvec.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl2/mainline/arch/generic/lisp/runtime/gen-r15-array.lisp#3 $
;
(in-package :si)

(macrolet (
  (define (name)
    (let ((bit-op (intern (format nil "~A/3" name))))
     `(defun ,name (v1 v2 &optional v3)
        (multiple-value-bind (b1 s1 e1) (bit-data v1) (let ((n (- e1 s1)))
        (multiple-value-bind (b2 s2 e2) (bit-data v2)
        (multiple-value-bind (b3 s3 e3)
            (cond
              ((eq v3 nil) (values (make-array n :element-type 'bit) 0 n))
              ((eq v3 't)  (values b1 s1 e1))
              (t (bit-data v3)) )
          (unless (eql (- e2 s2) n)
            (error 'type-error :datum v2 :expected-type `(array bit ,n)) )
          (unless (eql (- e3 s3) n)
            (error 'type-error :datum v3 :expected-type `(array bit ,n)) )
          (,bit-op b3 b1 b2 s3 s1 s2 n) )))) ) ) )
  )
  ;;
  (labels (
    ;; bit-data
    (bit-data (a)
        (declare (values simple-bit-vector sequence-index sequence-index))
      (typecase a
        (simple-bit-vector (values a 0 (length a)))
        (bit-vector-object (follow a 0 (length a)))
        (simple-bit-array-object (follow a 0 (array-total-size a)))
        (bit-array-object        (follow a 0 (array-total-size a)))
        (otherwise
          (error 'type-error :datum a :expected-type '(array bit)) )) )

    ;; follow
    (follow (a s n)
      (etypecase a
        (simple-bit-vector
          (values a s n) )
        (bit-vector-object
          (follow (ref bit-vector-object displaced-to a)
                  (+ (ref bit-vector-object offset a) s) n) )
        (simple-bit-array-object
          (follow (ref simple-bit-array-object displaced-to a)
                  (+ (ref simple-bit-array-object offset a) s) n) )
        (bit-array-object
          (follow (ref bit-array-object displaced-to a)
                  (+ (ref bit-array-object offset a) s) n) )) )
    ;;
    )
    ;;
    (define cl:bit-and)

    (define cl:bit-andc1)
    (define cl:bit-andc2)

    (define cl:bit-ior)
    (define cl:bit-orc1)
    (define cl:bit-orc2)

    (define cl:bit-nand)
    (define cl:bit-nor)
    (define cl:bit-eqv)
    (define cl:bit-xor)

    ;; cl:bit-not
    (defun cl:bit-not (v1 &optional v2)
      (multiple-value-bind (b1 s1 e1) (bit-data v1) (let ((n (- e1 s1)))
      (multiple-value-bind (b2 s2 e2)
          (cond
            ((eq v2 nil) (values (make-array n :element-type 'bit) 0 n))
            ((eq v2 't)  (values b1 s1 e1))
            (t (bit-data v2)) )
        (unless (eql (- e2 s2) n)
          (error 'type-error :datum v2 :expect-type `(array bit ,n)) )
        (bit-not/2 b2 b1 s1 s2 n) ))) )
 ) ) ; macrolet
