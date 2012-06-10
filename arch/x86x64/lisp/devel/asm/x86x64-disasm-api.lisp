;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - arch - x86x64 - devel - Disassembler
;;; arch/x86x64/lisp/devel/x86x64-d25-disasm.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/devel/asm/x86x64-disasm-api.lisp#2 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :si)

(defun cl:disassemble (fspec)
    (declare (values null))
  (labels (
    ;; method-spec-p
    (method-spec-p (fspec)
      (and (eql (si::safe-list-length fspec) 4)
           (eq (car fspec) 'method)
           (let ((fname (second fspec)))
             (and (function-name-p fname)
                  (fboundp fname)
                  (typep (fdefinition fname) 'standard-generic-function) ) )
           (every (lambda (x) (and (symbolp x) (find-class x nil)))
                  (fourth fspec) )) )

    ;; parse-fsepc
    (parse-fspec (fspec)
      (or (parse-toplevel fspec)
          (when (and (consp fspec)
                     (or (eq (car fspec) 'labels) (eq (car fspec) 'flet)) )
            (parse-internal fspec) )) )

    ;; parse-internal
    (parse-internal (fspec)
      (let* ((fname (second fspec))
             (fn (parse-toplevel fname)) )
        (when fn
          (when (and (consp fname) (eq (first fname) 'setf))
            (setq fspec (list* (first fspec) (second fname) (cddr fspec))) )
          (search-inner fn fspec '()) ) ) )

    ;; parse-toplevel
    (parse-toplevel (fspec)
      (let ((fn (parse-toplevel-aux fspec)))
        (if (typep fn 'generic-function)
            (funcallable-instance-function fn)
          fn ) ) )

    ;; parse-toplevel-aux
    (parse-toplevel-aux (fspec)
      (cond
        ((functionp fspec)
          fspec )
        ((and fspec (symbolp fspec))
          (or
            (macro-function fspec)
            (when (fboundp fspec) (symbol-function fspec)) ) )
        ((function-name-p fspec)
          (or
            (when (fboundp fspec) (fdefinition fspec))
            ;; An expander of defsetf or define-setf-expander.
            (multiple-value-bind (kind localp alist)
                (c::function-information fspec)
                (declare (ignore kind localp))
              (cdr (assoc :macro alist)) )) )
        ((method-spec-p fspec)
          (method-function
            (find-method
              (fdefinition (second fspec))
              (third fspec)
              (mapcar #'find-class (fourth fspec)) )) )) )

    ;; search-inner
    (search-inner (fn fspec visits)
      (unless (member fn visits :test 'eq)
        (let ((visits (cons fn visits)))
          (with-code-annotation-iterator (next fn)
            (loop
              (multiple-value-bind (typ ofs datum) (next)
                  (declare (ignore ofs))
                (unless typ (return nil))
                (when (functionp datum)
                  (if (equal (function-name datum) fspec)
                      (return datum)
                    (let ((found (search-inner datum fspec visits)))
                      (when found (return found)) ))) )) ) )) )
    )
    (let ((fn (parse-fspec fspec)))
      (if fn
          (x86x64::disassemble-funobj fn)
        (format t "; No such function ~S.~%" fspec) )
      nil ) ) )
