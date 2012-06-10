;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 17 Sequence - Vector
;;; arch/generic/lisp/runtime/gen-r17-vector.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r17-vector.lisp#3 $
;
(in-package :si)

;;;; elt/vector
(defun elt/vector (vector index)
    (declare (values t))
    (declare (type vector vector))
    (declare (type sequence-index index))
  (unless (and (<= 0 index) (< index (length vector)))
    (sequence-index-error vector index) )
  (row-major-aref vector index) )


;;;; (setf elt/vector)
(defun (setf elt/vector) (datum vector index)
    (declare (values t))
    (declare (type vector vector))
    (declare (type sequence-index index))
  (unless (and (<= 0 index) (< index (length vector)))
    (sequence-index-error vector index) )
  (setf (row-major-aref vector index) datum) )


;;;; 17.3.11 length
(defun length/vector (vector)
  (etypecase vector
    (data-vector       (ref data-vector length vector))
    (vector-object     (ref vector-object fill-pointer vector)) ) )
