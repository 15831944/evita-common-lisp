;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - arch - x86x64 - devel - Assembler Utility Functions
;;; arch/x86x64/lisp/devel/asm/x86x64-asm-fns.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/devel/asm/x86x64-op-fns.lisp#1 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :x86x64)

;;;; %defformat
(defun %defformat (opcode mnemonic syntax operands &key aliases)
    (declare (values symbol))
  (labels (
    ;; insert
    (insert (format presents)
        (declare (values cons))
      (let ((cb (length-insn format)))
        (cond
          ((null presents)
            (list format) )
          ((<= cb (length-insn (first presents)))
            (cons format presents) )
          (t
            (loop
              with tail = presents
              for runner on (cdr presents) do
                (when (< cb (length-insn (first runner)))
                  (loop-finish) )
                (setq tail runner)
              finally
                (setf (cdr tail) (cons format (cdr tail)))
                (return presents) ) )) ) )

    ;; install-mnemonic
    (install-mnemonic (format)
        (declare (values cons))
      (let* ((presents (gethash mnemonic *mnemonic-table*))
             (present (find opcode presents :key #'second)) )
        (when present
          (setq presents (delete present presents :test #'eq)) )
        (setf (gethash mnemonic *mnemonic-table*)
          (insert format presents) ) ) )

    ;; length-insn
    (length-insn (format)
      (let ((opcode   (second format))
            (syntax   (third  format))
            (operands (fourth format)) )
        (ecase syntax
          ((:modrm)
            (+ (length-opcode opcode) (length-operands operands) 1) )
          ((:normal)
            (+ (length-opcode opcode) (length-operands operands)) )
          ((:opext-leader) 0)
          ((:opext)
            (+ (length-opcode opcode) (length-operands operands)) )
          ((:twobytes) 0) ) ) )

    ;; length-opcode
    (length-opcode (opcode)
      (cond
        ((> opcode #xffffff) 4)
        ((> opcode #xffff) 3)
        ((> opcode #xff) 2)
        (t 1) ) )

    ;; length-operand
    (length-operand (operand)
      (ecase operand
        ((1) 0)
        ((AL CL DL BL AH CH DH BH) 0)
        ((CS DS ES SS) 0)
        ((DX) 0)
        ((eAX eCX eDX eBX eSP eBP eDI eSI) 0)
        ((Eb Ed Ev Ew Ev64) 1)
        ((Gb Gd Gv Gw) 0)
        ((Ib) 1)
        ((Iw) 2)
        ((Iv) 4)
        ((Iz) 4)
        ((Jb) 2)
        ((Jv) 4)
        ((Md Mp Mpd Mps Mq) 1)
        ((Nq) 1)
        ((Ob Ov) 1)
        ((Pd Pq) 1)
        ((Qd Qdq Qq) 1)
        ((rAX rCX rDX rBX rSP rBP rSI rDI) 0)
        ((Udq Upd Ups) 1)
        ((Vdq Vp Vpd Vps Vq Vsd Vss) 0)
        ((Wdq Wp Wpd Wps Wq Wsd Wss) 1) ) )

    ;; length-operands
    (length-operands (operands)
      (loop for operand in operands sum (length-operand operand)) )
    )
    ;;
   (let ((format `(,mnemonic ,opcode ,syntax ,operands)))
     (when mnemonic
       (let ((formats (install-mnemonic format)))
         (dolist (alias aliases)
           (setf (gethash alias *mnemonic-table*) formats) ) ))
     (setf (gethash opcode *opcode-table*) format)
     mnemonic ) ) )
