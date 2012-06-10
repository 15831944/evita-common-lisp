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
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/devel/asm/x86x64-isa-defs.lisp#3 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :x86x64)

(defclass isa ()
  ((name
        :type       symbol
        :initarg    :name )
   (size
        :type       (member 32 64)
        :initarg    :size )
   (registers
        :type       list
        :initarg    :registers )
   ;; For compiler and stack walker
   (callee-saves
        :type       list
        :initarg    :callee-saves )
   (caller-saves
        :type       list
        :initarg    :caller-saves )
   ;; For disassemble
   (tlvoffset
        :type       (unsigned-byte 16)
        :initarg    :tlvoffset )
   (thfields
        :type       list
        :initarg    :thfields )
   ;; For assembler
   (regtab
        :type hash-table
        :initform (make-hash-table :test 'eq) ))
  (:metaclass structure-class) )


;;;; register
(defclass register ()
  ((name
        :type       symbol
        :initarg    :name )
   (regclass
        :type       symbol
        :initarg    :regclass )
   (code
        :type       (unsigned-byte 16)
        :initarg    :code )
   (size
        :type       (unsigned-byte 16)
        :initarg    :size ))
  (:metaclass structure-class) )


(defvar *isa-table* (make-hash-table :test 'eq))
(deftlv *isa*)
(declaim (type isa))

(defmacro define-isa (name (&key size) &rest args)
  (labels (
    (process-args (args)
      (with-collector (collect)
        (loop for (key val) on args by #'cddr do
          (collect key)
          (collect `(quote ,val)) )) )
    )
    ;;
  `(%define-isa ',name :size ,size ,@(process-args args)) ) )

(declaim (ftype (function (symbol &rest t) symbol) %define-isa))
(declaim (ftype (function (symbol &optional t) (or isa null)) find-isa))
