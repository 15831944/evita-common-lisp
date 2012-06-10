;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XC; Base: 10 -*-
;;;;
;;;; evcl - X86/X64 Assembler - Declarations
;;; arch/x86x64/lisp/devel/x86x64-d24-loader.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/devel/asm/x86x64-asm-loader.lisp#2 $
;;;
;;; Description:
;;;  This fils contains declarations for X86 assembler:
;;;     %defsm
;
(in-package :x86x64)

;;;; %assemble
(defun %assemble (name lambda-list initargs)
    (declare (values function))
    (declare (ignore lambda-list))
  (labels (
    (alloc (class)
      (let ((classd (slot-value class 'si::instance-description)))
       (apply #'si::allocate-funobj classd initargs) ) )
    )
    ;;
    (let* ((class
            (prog1
                (find-class (getf initargs :class 'si::native-code-function))
              (loop while (remf initargs :class)) ) )
           (fn (alloc class)) )
      (apply #'si::initialize-funobj fn :name name initargs) ) ) )
