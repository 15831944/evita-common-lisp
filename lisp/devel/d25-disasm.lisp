;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;  evcl - devel - 25 Environment - Disassembler
;;;; lisp/devel/d25-disasm.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d25-disasm.lisp#2 $
;;;
;;; Description:
;;;  This file contains cmdl package.
;;;     disasemble      25.2.14
;
(in-package :devel)

;;;; parse-fspec
(defun parse-fspec (fspec &optional (errorp t))
  (labels (
    ;; parse
    ;;  Parses function specification (fspec) then returns function object
    ;;  or nil if no coresponding function.
    (parse (fspec)
      (typecase fspec
        (function fspec)
        (symbol
          (when (fboundp fspec)
            (or (macro-function fspec) (symbol-function fspec)) ) )
        (ext:function-name
          ;; Retrieve setf-function or setf-expander defined by
          ;; defsetf or define-setf-expander.
          (if (fboundp fspec)
              (fdefinition fspec)
            (multiple-value-bind (kind local-p alist)
                (xc::function-information fspec)
                (declare (ignore local-p))
              (case kind
                ((:macro)
                  (cdr (assoc :macro alist)) )) )) )
        ((cons (or (eql labels) (eql flet)) cons)
          (let ((fname (second fspec)))
            (when (fboundp fname)
              (find-internal-function fspec (fdefinition fname)) ) ) )
        ((cons (eql :method) ext:function-name)
          (destructuring-bind (method fname &rest class*) fspec
              (declare (ignore method))
            (when (fboundp fname)
              (let* ((gf (fdefinition fname))
                     (mt (find-method gf (mapcar #'find-class class*))) )
                (when mt (clos:method-function mt)) )) ) )
        (otherwise
          (error "Invalid function specification: ~S" fspec) )) )
    )
    ;;
    (or (parse fspec)
        (when errorp (error "No such function ~S." fspec))) ) )


;;;; 25.2.14 disassemble
;
(defun cl:disassemble (fspec)
    (declare (values null))
  (let ((fn (parse-fspec fspec nil)))
    (when (null fn)
      (format t "Function ~S isn't defined." fspec)
      (return-from disassemble nil) )

    (when (typep fn 'clos:funcallable-standard-object)
      (setq fn (si::.funcallable-instance-function fn)) )

    (let ((*print-pretty* nil)
          (*print-circle* t)
          (*print-length* nil)
          (*print-level*  nil) )
      (disassemble-function fn)
      nil ) ) )
