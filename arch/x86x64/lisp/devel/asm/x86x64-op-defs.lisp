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
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/devel/asm/x86x64-op-defs.lisp#1 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :x86x64)

;;;; *mnemonic-table*
;;; Description:
;;;  Mapping to mnemonic to instruction format.
(declaim (type hash-table *mnemonic-table*))
(defvar *mnemonic-table* (make-hash-table :test 'eq))


;;;; *opcode-table*
;;; Description:
;;;  Mapping to opcode to instruction format.
;;; Note:
;;;  Opcode can be (unsigned-byte 32).
(declaim (type hash-table *opcode-table*))
(defvar *opcode-table* (make-hash-table))


;;;; defformat
;;;     (defformat opcode mnemonic)
;;;     (defformat opcode mnemonic operand)
;;;     (defformat opcode opext mnemonic operand)
(defmacro defformat (opcode mnemonic &rest frob*)
  (labels (
    ;; ea-p
    (ea-p (opdfmt)
      (member opdfmt '(
            Eb Ed Ev Ew
            Md Mp Mpd Mps Mq
            Nq
            Pd Pq
            Qd Qdq Qq
            Udq Upd Ups
            Wdq Wp Wpd Wps Wsd Wss )) )
    ;; get-syntax
    (get-syntax (opdfmts)
      (cond
        ((ea-p (first opdfmts)) (values :modrm opdfmts))
        ((ea-p (second opdfmts)) (values :modrm opdfmts))
        (t (values :normal opdfmts)) ) )
    )
   (multiple-value-bind (syntax operands)
      (cond
        ((integerp mnemonic)
            (setq opcode (logior (ash opcode 8) mnemonic))
            (setq mnemonic (pop frob*))
            (values :opext (pop frob*)) )
        ((null mnemonic) (values (pop frob*) nil))
        ((null frob*) (values :normal nil))
        (t (get-syntax (pop frob*))) )
     (let ((initargs '()))
       (loop
         for (key val) on frob* by #'cddr do
           (setf (getf initargs key) `(quote ,val)) )
      `(%defformat ,opcode ',mnemonic ',syntax ',operands ,@initargs) ) ) ) )


(declaim (ftype (function (
                    (unsigned-byte 32)
                    symbol symbol list
                    &key (:aliases list) ) symbol)
    %defformat ) )
