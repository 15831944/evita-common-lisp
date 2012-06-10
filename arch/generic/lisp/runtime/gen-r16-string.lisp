;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 16 Strings - Low
;;; lisp/runtime/r16-string.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r16-string.lisp#4 $
;
(in-package :si)

;;;; string-ci-compare
;;;; string-cs-compare
(macrolet (
  (define (name op)
   `(defun ,name (v1 v2 s1 e1 s2 e2)
        (declare (values fixnum sequence-index))
        (declare (type string v1 v2))
        (declare (type sequence-index s1 s2))
        (declare (type sequence-end   e1 e2))
      (labels (
        ;; compare
        (compare (v1 v2 s1 e1 s2 e2)
            (declare (values fixnum sequence-index))
          (let ((i1 s1) (i2 s2))
              (declare (type sequence-index i1 i2))
            (loop
              (when (eql i1 e1)
                (return (values (- (- e1 s1) (- e2 s2)) i1)) )
              (when (eql i2 e2)
                (return (values 1 i1)) )
              (let ((delta (- (char-int (,op (schar v1 i1)))
                              (char-int (,op (schar v2 i2))) ) ))
                (unless (zerop delta) (return (values delta i1)))
                (incf i1)
                (incf i2) )) ) )
        )
        ;;
        (multiple-value-bind (v1 s1d e1d) (string-data v1 s1 e1)
        (multiple-value-bind (v2 s2d e2d) (string-data v2 s2 e2)
          (multiple-value-bind (diff posn) (compare v1 v2 s1d e1d s2d e2d)
            (values diff (+ posn (- s1d s1))) ) ) ) ) ) )
    )
    ;;
    (define string-ci-compare char-downcase)
    (define string-cs-compare identity) )


;;;; 16.2.12 make-string
(defun cl:make-string (size &key (initial-element #\Space initial-element-p)
                                 (element-type 'character) )
    (declare (type sequence-index size))
    (declare (type character initial-element))
    (declare (values simple-string))
  (unless (eq element-type 'character)
    (unless (subtypep element-type 'character)
      (error "Element-type ~S must be subtype of ~S."
          element-type
          'character )))
  (let ((string (.allocate-string size)))
    (when initial-element-p (fill string initial-element))
    string ) )
