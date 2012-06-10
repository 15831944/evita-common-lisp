;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 49 Internal - Cells
;;; lisp/runtime/r01-cell.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r49-cell.lisp#6 $
;;;
;;; Description:
;;;  Implements functions for managing cells.
;;;
;
(in-package :si)

;;;; intern-plist-cell
;;;; intern-value-cell
;;;; intern-setf-cell
;
(macrolet (
  (define (name slot unbound)
    (let* ((intern (intern (format nil "INTERN-~A-CELL" name)))
           (find   (intern (format nil "FIND-~A-CELL" name)))
           (table  (intern (format nil "*~A-TABLE*" name)))
           (latch  (intern (format nil "*~A-TABLE-LATCH*" name)))
           (class  (intern (format nil "~A-CELL" name)))
           (ret    (if (eq name 'value) '(or value-cell tlv-record) class))
           (classd (class-description class)) )
     `(progn
        (defun ,find (name)
           (declare (values (or ,ret null)))
           (declare (type symbol name))
         (with-latch (,latch)
           (values (gethash/eq name ,table)) ) )

        (defun ,intern (name)
           (declare (type symbol name))
           (declare (values ,ret))
         (with-latch (,latch)
           (or (gethash/eq name ,table)
               (let ((cell (.allocate-record ,classd)))
                  (setf (ref ,class ,slot cell) ',unbound)
                  (setf (ref ,class name  cell) name)
                  ,@(when (eq name 'value)
                     `((setf (ref value-cell type  cell) :variable)) )
                  (setf (gethash/eq name ,table) cell) )) ) )) ) )
    )
    (define setf  function nil)
    (define value value #.(unbound-marker)) )
