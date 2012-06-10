;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 5 Data and Control Flow
;;; lisp/runtime/r05-control.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r05-control.lisp#5 $
;;;
;;; Description:
;;;  Implements functions defined in section "5 Data and Control Flow".
;;;
;;; Public Functions:
;;;     apply                               5.3.1   boot
;;;     compiled-function-p                 5.3.11  boot
;;;     complement                          5.3.38
;;;     constantly                          5.3.39
;;;     eq                                  5.3.33  intrinsic
;;;     eql                                 5.3.34  intrinsic
;;;     equal                               5.3.35
;;;     equalp                              5.3.36
;;;     every                               5.3.40
;;;     fboundp                             5.3.4
;;;     fdefinition                         5.3.3
;;;     (setf fdefinition)                  5.3.3
;;;     fmakunbound                         5.3.5
;;;     funcall                             5.3.7
;;;     function-lambda-expression          dev
;;;     functionp                           5.3.10  boot
;;;     get-setf-expansion                  5.3.63  devel
;;;     identity                            5.3.37
;;;     not                                 5.3.31  compiler
;;;     notany                              5.3.40
;;;     notevery                            5.3.40
;;;     some                                5.3.40
;;;     values                              5.3.53  boot, compiler
;;;     values-list                         5.3.53  boot
;
(in-package :si)

;;;; 5.3.38 complement
;;; We use default name for returned function instead of explict name for
;;; reducing memory usage.
;
(defun cl:complement (fn)
  (lambda (&rest args)
      (declare (lambda-name (complement otherwise)))
    (not (apply fn args))) )


;;;; 5.3.39 constantly
;;; Note:
;;; We use default name for returned function instead of explict name for
;;; reducing memory usage.
;
(defun cl:constantly (object)
  (case object
    ((nil)
      (lambda (&rest args)
          (declare (lambda-name (constantly nil)))
          (declare (ignore args))
        nil ) )
    ((0)
      (lambda (&rest args)
          (declare (lambda-name (constantly 0)))
          (declare (ignore args))
        0 ) )
    (otherwise
      (lambda (&rest args)
          (declare (lambda-name (constantly otherwise)))
          (declare (ignore args))
        object ) )) )


;;;; 5.3.35 equal
;;;
;;; Syntax:
;;;     equal x y => eq-p
;;;
;;; Returns:
;;;     x    - an object.
;;;     y    - an object.
;;;     eq-p - a generalized boolean.
;;;
;;; Description:
;;;  Returns true if x and y are structually similar (isomorphic) objects.
;;;  Equal treats objects as follows:
;;;     number          use eql
;;;     character       use eql
;;;     cons            descends
;;;     bit vector      descends
;;;     string          descends
;;;     pathanme        functionally equivalent
;;;     structure       use eq
;;;     other array     use eq
;;;     hash-table      use eq
;;;     other objects   use eq
;;;
;;; Note: We use intrinsic version for performance.
;
(defun cl:equal (x y)
  (cond
    ((eq x y)
     t )

    ((consp x)
      (and (consp y) (equal (car x) (car y)) (equal (cdr x) (cdr y))) )

    ((symbolp x) nil)
    ((characterp x) nil)

    ((numberp x)
      (eql x y) )

    ((stringp x)
      (and (stringp y) (string= x y)) )

    ((bit-vector-p x)
      (and (bit-vector-p y)
           (eql (length x) (length y))
           (dotimes (i (length x) t)
            (unless (eql (bit x i) (bit y i))
              (return nil) ) )) )

    ((pathnamep x)
     (and (pathnamep y) (pathname-equal x y)) )

    (t
      nil )) )


;;;; 5.3.36 equalp
;;;
;;; Syntax:
;;;     equalp x y => eq-p
;;;
;;; Returns:
;;;     x    - an object.
;;;     y    - an object.
;;;     eq-p - a generalized boolean.
;;;
;;; Description:
;;;  Returns true if tyey are equal, or if they have components that are not
;;;  the same type as each other and if those components are equalp;
;;;  specifically, equalp returns true in the following cases:
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
;;;
(defun cl:equalp (x y)
  (cond
    ((eq x y)
      t )

    ((symbolp x) nil)

    ((characterp x)
      (and (characterp y) (char-equal x y)) )

    ((numberp x)
      (and (numberp y) (= x y)) )

    ((consp x)
      (and (consp y)
           (equalp (car x) (car y))
           (equalp (cdr x) (cdr y)) ) )

    ((vectorp x)
      (and (vectorp y)
           (eql (length x) (length y))
           (dotimes (i (length x) t)
             (unless (equalp (row-major-aref x i) (row-major-aref y i))
               (return nil) ) )) )

    ((arrayp x)
      (and (arrayp y)
           (equal (array-dimensions x) (array-dimensions y))
           (dotimes (i (array-total-size x) t)
             (unless (equalp (row-major-aref x i) (row-major-aref y i))
               (return nil) ) )) )

    ((pathnamep x)
      (and (pathnamep y) (pathname-equal x y)) )

    ((typep x 'structure-object)
      (and (typep y 'structure-object)
           (eq (class-of x) (class-of y))
           (dolist (eslotd (class-slots (class-of x)))
             (let* ((location (slot-value eslotd 'location))
                    (xval (structure-instance-access x location))
                    (yval (structure-instance-access y location)) )
               (unless (equalp xval yval) (return nil)) ) )) )

    ((hash-table-p x)
      (and (hash-table-p y)
           (eq (hash-table-count x) (hash-table-count y))
           (eq (hash-table-test  x) (hash-table-test  y))
           (with-hash-table-iterator (next-fn x)
             (loop
               (multiple-value-bind (more? key value) (next-fn)
                 (unless more? (return t))
                 (unless (equalp value (gethash key y))
                   (return nil) ) )) )) )

    (t
      nil )) )


;;;; 5.3.40 every
;;;; 5.3.40 notany
;;;; 5.3.40 notevery
;;;; 5.3.40 some
(macrolet (
  (define-predicate (name end-value term-form term-value)
    `(progn
       (defun ,name (predicate sequence &rest more-sequences)
           (declare (type (or symbol function) predicate))
           (declare (type sequence sequence))

         (let ((length    (length sequence))
               (sequences (cons sequence more-sequences)) )
           (dolist (sequence more-sequences)
             (setq length (min length (length sequence))) )

           (dotimes (index length ,end-value)
             (let ((args '()))
               (do ((scan sequences (cdr scan)))
                   ((null scan))
                 (let ((sequence (car scan)))
                   (if (vectorp sequence)
                       (push (row-major-aref sequence index) args)
                     (progn
                       (push (first sequence) args)
                       (setf (car scan) (rest sequence)) )) ) )
               (setq args (nreverse args))

               (let ((value (apply predicate args)))
                 (when ,term-form
                   (return ,term-value) ) ) ) ) ) )

        (defun ,(intern (format nil "~A/1" name)) (predicate sequence)
          (etypecase sequence
            (list
              (dolist (elt sequence ,end-value)
                (let ((value (funcall predicate elt)))
                  (when ,term-form
                    (return ,term-value) ) ) ) )
            (vector
              (let ((length (length sequence)))
                (dotimes (index length ,end-value)
                  (let* ((elt (row-major-aref sequence index))
                         (value (funcall predicate elt)) )
                    (when ,term-form
                      (return ,term-value) ) ) ) ) )) ) ) )
    )
    ;;
    (define-predicate cl:every    t   (not value) nil)
    (define-predicate cl:notany   t   value       nil)  ; not some
    (define-predicate cl:notevery nil (not value) t)    ; not every
    (define-predicate cl:some     nil value       value)
  ) ; macrolet


;;;; 5.3.9 function-lambda-expression
;;;
;;; See: dvel;d05-control.lisp


;;;; 5.3.10 functionp
;;;
;;; BOOT


;;;; 5.3.63 get-setf-expansion
;;;
;;; See: devel;d05-control.lisp


;;;; 5.3.37 identity
;;;
;;; Syntax:
;;;     identity object => object
;
(defun cl:identity (object)
  object )


;;;; 5.3.31 not
;;;
;;; Syntax:
;;;     not x => boolean
;
(defun cl:not (x)
  (if x nil t) )


; 5.3.53 values         boot and m05-setf.lisp
; 5.3.54 values-list    boot
