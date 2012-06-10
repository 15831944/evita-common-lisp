;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - arch - x86x64 - devel - Assembler Definitions
;;; arch/x86x64/lisp/devel/asm/x86x64-asm-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/devel/asm/x86x64-isa-fns.lisp#1 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :x86x64)

;;;; %define-isa
(defun %define-isa (name &key size registers tlvoffset thfields
                         callee-saves caller-saves
                         aliases )
    (declare (ignore callee-save caller-save))
  (let ((isa (make-instance 'isa
                :name           name
                :size           size
                :callee-saves   callee-saves
                :caller-saves   caller-saves
                :registers      registers
                :tlvoffset      tlvoffset
                :thfields       thfields )) )

    ;; Populate register table
    (let ((regtab (slot-value isa 'regtab)))
      (loop
        for (regclass size names) in registers do
          (loop
            for name across names
            for code from 0
            for key = (intern (symbol-name name) :x86x64)
            for reg = (make-instance 'register
                        :name key
                        :regclass regclass
                        :code code
                        :size size )
             when name
               do (setf (gethash key regtab) reg) ))
      (loop
        for (alias real) in aliases
        for kalias = (intern (symbol-name alias) :x86x64)
        for kreal  = (find-symbol (symbol-name real)  :x86x64)
        for reg = (or (gethash kreal regtab)
                      (error "Undefined register name ~S" real) )
          unless kreal do (error "Can't use NIL as register alias.")
          do (setf (gethash kalias regtab) reg) ) )

    (setf (gethash name *isa-table*) isa)
    name ) )


;;;; find-isa
(defun find-isa (name &optional (errorp t))
  (or (gethash name *isa-table*)
      (when errorp (error "No such ISA ~S." name)) ) )


;;;; print-object isa
(defmethod cl:print-object ((o isa) s)
  (print-unreadable-object (o s :type t)
    (prin1 (slot-value o 'name)) )
  o )


;;;; print-object register
(defmethod cl:print-object ((o register) s)
  (print-unreadable-object (o s :type t)
    (format s "~A ~A ~Dbit"
        (slot-value o 'regclass)
        (slot-value o 'name)
        (slot-value o 'size) ) )
  o )
