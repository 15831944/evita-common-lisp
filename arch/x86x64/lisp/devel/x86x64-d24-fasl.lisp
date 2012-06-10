;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - arch - x86x64 - devel - 24 System Construction - FASD
;;; arch/x86x64/lisp/devel/x86x64-d24-fasl.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/devel/x86x64-d24-fasl.lisp#2 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :si)

;;;; fasl-start-funobj
;;; FASL-OP-FUNOBJ takes two parameters, class and initargs.
(defun devel::fasl-start-funobj (size)
  (labels (
    ;; alloc-funobj
    (alloc-funobj (class size)
        (declare (values native-code-object))
        (declare (type class class))
        (declare (type sequence-index size))
      (let ((classd (ref class instance-description
                            (ref instance storage class) ) ))
        (.allocate-funobj classd size) ) )

    ;; fasl-op-funobj
    (fasl-op-funobj (state operand)
        (declare (type devel::fasl-state state))
        (declare (type list initargs))
        (declare (values unspecified))
      (let ((fn (devel::fasl-state-object state)))
        (if (functionp fn)
            (apply #'initialize-funobj fn operand)
          (let ((fn (alloc-funobj operand (devel::fasl-state-object state))))
            (setf (devel::fasl-state-object state) fn)
            (when (devel::fasl-state-marker state)
              (setf (cdr (devel::fasl-state-marker state)) fn) ) )) ) )
    )
    ;;
    (values #'fasl-op-funobj size 0 2) ) )
