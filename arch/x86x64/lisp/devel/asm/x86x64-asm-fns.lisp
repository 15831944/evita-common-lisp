;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XC; Base: 10 -*-
;;;;
;;;; evcl - X86/X64 Assembler - Declarations
;;; arch/x86x64/lisp/devel/asm/x86x64-asm-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/devel/asm/x86x64-asm-fns.lisp#2 $
;;;
;;; Description:
;;;  This fils contains declarations for X86 assembler:
;;;
;;; Macros:
;;;   defasm
;;;
;;; Note:We don't support 16-bit addressing mode.
;
(in-package :x86x64)


;;;
;;; Printers
;;;
(defmethod cl:print-object ((o asm-annot) s)
  (print-unreadable-object (o s :type t)
    (format s "~S +~4,'0X datum=~S"
        (slot-value o 'kind)
        (slot-value o 'address)
        (slot-value o 'datum) ))
  o )

(defmethod cl:print-object ((o asm-bblock) s)
  (print-unreadable-object (o s :type t)
    (format s "~S +~4,'0X ~D +~4,'0X"
        (slot-value o 'name)
        (slot-value o 'address)
        (slot-value o 'offset)
        (slot-value o 'original) ))
  o )

(defmethod cl:print-object ((o asm-context) s)
  (print-unreadable-object (o s :identity t :type t)) o )


;;;; asm-emit-u8
(defun asm-emit-u8 (ctx u8)
    (declare (values (unsigned-byte 8)))
    (declare (type asm-context ctx))
    (declare (type (unsigned-byte 8) u8))
  (incf (slot-value ctx 'address))
  (let* ((ofs (slot-value ctx 'offset))
         (nxt (+ ofs 1))
         (cv  (slot-value ctx 'codebuf)) )
    (when (>= nxt (length cv))
      (let ((dv (make-array (truncate (* nxt 11) 10)
                    :element-type '(unsigned-byte 8) )))
        (setf (slot-value ctx 'codebuf) dv)
        (setq cv (replace dv cv)) ))
    (setf (slot-value ctx 'offset) nxt)
    (setf (elt cv ofs) u8) ) )


;;;; asm-encode-insn
(defun asm-encode-insn (ctx format operands)
    (declare (type asm-context ctx))
    (declare (type cons format))
    (declare (type list operands))
  (let ((cv         (slot-value ctx 'codebuf))
        (opcode-ofs (slot-value ctx 'offset))
        (isasiz     (slot-value (slot-value ctx 'isa) 'size))
        (modrm-ofs  0)
        ;;
        (mod.disp0  #x00)
        (mod.disp8  #x40)
        (mod.disp32 #x80)
        (mod.reg    #xC0)
        ;;
        (r/m.sib    4)
        (r/m.disp32 5)
        ;;
        (REX.W      #x48)
        (REX.R      #x44)
        (REX.X      #x42)
        (REX.B      #x41) )
  (labels (
    ;; emit-disp
    (emit-disp (disp)
      (etypecase disp
        (asm-annot
          (remember-annot disp)
          (emit-u32 0)
          mod.disp32 )
        (asm-labelref
          (remember-labelref disp)
          (emit-u32 0)
          mod.disp32 )
        (integer
          (cond
            ((eql disp 0)
              mod.disp0 )
            ((<= -128 disp 127)
              (emit-u8 (ldb (byte 8 0) disp))
              mod.disp8 )
            (t
              (emit-u32 (ldb (byte 32 0) disp))
              mod.disp32 )) )) )

    ;; emit-Iv
    (emit-Iv (opd)
      (let ((disp
              (etypecase opd
                (asm-annot (remember-annot opd) 0)
                (asm-labelref (remember-labelref opd) 0)
                (integer opd) ) ))
        (ecase isasiz
          ((32)
            (emit-u32 (ldb (byte 32 0) disp)) )
          ((64)
            ;; Note: REX.W is already emitted.
            (emit-u32 (ldb (byte 32  0) disp))
            (emit-u32 (ldb (byte 32 32) disp)) )) ) )

    ;; emit-Iz
    (emit-Iz (opd)
      (let ((disp
              (etypecase opd
                (asm-annot (remember-annot opd) 0)
                (asm-labelref (remember-labelref opd) 0)
                (integer opd) ) ))
        (emit-u32 (ldb (byte 32 0) disp)) ) )

    ;; emit-Iw
    (emit-Iw (opd)
      (emit-u8 (ldb (byte 8 0) opd))
      (emit-u8 (ldb (byte 8 8) opd)) )

    ;; emit-Jb
    (emit-Jb (opd)
        (declare (type asm-labelref opd))
      (let* ((bb (slot-value opd 'bblock))
             (diff
                (if (slot-value bb 'kind)
                    (- (slot-value bb 'address)
                       (+ (slot-value ctx 'address) 1) )
                    0 )) )
        (setf (slot-value opd 'kind) :relative)
        (remember-labelref opd)
        (if (<= -128 diff 127)
            (progn
              (incf (slot-value ctx 'address))
              (incf (slot-value ctx 'offset)
                  (if (eq (first format) 'jmp) 4 5) )
              (let ((bb (make-instance 'asm-bblock
                            :kind    :code
                            :address (slot-value ctx 'address)
                            :offset  (slot-value ctx 'offset )) ))
                (push bb (slot-value ctx 'bblocks))
                (setf (slot-value ctx 'bblock) bb) ))
            (progn
              (setf (slot-value opd 'longp) t)
              (decf (slot-value ctx 'address))
              (decf (slot-value ctx 'offset))
              (if (eq (first format) 'jmp)
                  (emit-u8 #xEB)
                (progn
                  (emit-u8 #x0F)
                  (emit-u8 (second format)) ))
              (emit-u32 0) )) ) )


    ;; emit-Jv
    (emit-Jv (opd)
        (declare (type asm-annot opd))
      (setf (slot-value opd 'kind)
        (if (functionp (slot-value opd 'datum)) :lcallee :ncallee) )
      (remember-annot opd)
      (emit-u32 0) )

    ;; emit-opcode
    (emit-opcode (opcode)
      (when (> opcode #xffffff)
        (emit-u8 (ldb (byte 8 24) opcode)) )
      (when (> opcode #xffff)
        (emit-u8 (ldb (byte 8 16) opcode)) )
      (when (> opcode #xff)
        (emit-u8 (ldb (byte 8 8) opcode)) )
      (emit-u8 (ldb (byte 8 0) opcode)) )

    ;; emit-REX
    (emit-REX (rex)
        (assert (<= #x40 rex #x4F))
      (let ((present (elt cv opcode-ofs)))
        (unless (<= #x40 present #x4F)
          (let ((ofs (slot-value ctx 'offset)))
            (ensure-codebuf 1)
            (replace cv cv
              :start1 (1+ opcode-ofs)
              :end1   (1+ ofs)
              :start2 opcode-ofs
              :end2   ofs )
            (when (plusp modrm-ofs) (incf modrm-ofs))
            (setq present #x40) ))
        (setf (elt cv opcode-ofs) (logior present rex)) ) )

    ;; emit-sib
    (emit-sib (ss idx r/m)
      (when (>= r/m 8) (emit-rex REX.B))
      (when (>= (ldb (byte 4 0) idx) 8) (emit-rex REX.X))
      (emit-u8 (logior ss (ash (ldb (byte 3 0) idx) 3) (ldb (byte 3 0) r/m))) )

    ;; emit-u32
    (emit-u32 (u32)
        (declare (type (unsigned-byte 32) u32))
      (let ((ofs (ensure-codebuf 4)))
        (setf (elt cv (+ ofs 0)) (ldb (byte 8  0) u32))
        (setf (elt cv (+ ofs 1)) (ldb (byte 8  8) u32))
        (setf (elt cv (+ ofs 2)) (ldb (byte 8 16) u32))
        (setf (elt cv (+ ofs 3)) (ldb (byte 8 24) u32)) ) )

    ;; emit-u8
    (emit-u8 (u8)
        (declare (type (unsigned-byte 8) u8))
      (let ((ofs (ensure-codebuf 1)))
        (setf (elt cv ofs) u8) ) )

    ;; ensure-codebuf
    (ensure-codebuf (n)
      (incf (slot-value ctx 'address) n)
      (let* ((ofs (slot-value ctx 'offset))
             (nxt (+ ofs n)) )
        (setf (slot-value ctx 'offset) nxt)
        (when (>= nxt (length cv))
          (let* ((newcv (make-array (truncate (* nxt 11) 10)
                          :element-type '(unsigned-byte 8) )) )
            (setq cv (replace newcv cv)) ))
        ofs ) )

    ;; process-operand
    (process-operand (opdfmt opd)
      (ecase opdfmt
        ((1) #+nil "no code")
        ((AL AH BL BH CL CH DL DH) #+nil "no code")
        ((DX) #+nil "no code")
        ;;
        ((eAX eBx eCX eDX eSP eBP eDI eSI)
          (when (eql (slot-value opd 'size) 64) (emit-rex REX.W)) )
        ((Eb Ob Ed Ev Ov Ew Ev64) (process-operand/rm opd))
        ;;
        ((Gb Gd Gv Gw) (process-operand/reg opd))
        ;;
        ((Ib) (emit-u8 (ldb (byte 8 0) opd)))
        ((Id Iz) (emit-Iz opd))
        ((Iv) (emit-Iv opd))
        ((Iw) (emit-Iw opd))
        ;;
        ((Jb) (emit-Jb opd))
        ((Jv) (emit-Jv opd))
        ;;
        ((Wp Wpd Wps Wsd Wss) (process-operand/rm opd))
        ;;
        ((Vp Vpd Vps Vsd Vss) (process-operand/reg opd)) ) )

    ;; process-operand/reg
    (process-operand/reg (opd)
        (declare (type register opd))
      (let ((code (slot-value opd 'code)))
        (when (eql (slot-value opd 'size) 64) (emit-rex REX.W))
        (update-modrm 0 code 0) ) )

    ;; process-operand/rm
    (process-operand/rm (opd)
      (multiple-value-bind (mod r/m)
          (etypecase opd
            (asm-ea
              (process-operand/ea opd) )
            (register
              (process-operand/ea/reg opd) ))
        (update-modrm mod 0 r/m) ) )

    ;; process-operand/ea
    (process-operand/ea (ea)
        (declare (type asm-ea ea))
      (when (eql (slot-value ea 'size) 64) (emit-rex REX.W))
      (let ((base  (slot-value ea 'base))
            (index (slot-value ea 'index)) )
        (cond
          ((null base)
            (process-operand/ea/disp ea) )
          ((null index)
            (process-operand/ea/base ea) )
          (t
            (process-operand/ea/index ea) )) ) )

    ;; process-operand/ea/base
    (process-operand/ea/base (ea)
        (declare (type asm-ea ea))
      (let* ((base (slot-value ea 'base))
             (r/m  (slot-value base 'code)) )
        ;; #x24 = ss=00 idx=100 base=100(no index)
        (when (eql (ldb (byte 3 0) r/m) r/m.sib) (emit-u8 #x24))
        (values (emit-disp (slot-value ea 'disp)) r/m) ) )

    ;; process-operand/ea/disp
    (process-operand/ea/disp (ea)
        (declare (type asm-ea ea))
      (let* ((disp  (slot-value ea 'disp))
             (index (slot-value ea 'index)) )
        (if (null index)
            (progn
              (emit-disp disp)
              (values mod.disp0 r/m.disp32) )
          (let* ((idx (slot-value index 'code))
                 (ss  (ss idx)) )
            (emit-sib ss idx r/m.disp32)
            (values (emit-disp disp) r/m.sib) )) ) )

    ;; process-operand/ea/index
    (process-operand/ea/index (ea)
        (declare (type asm-ea ea))
      (let* ((disp  (slot-value ea 'disp))
             (base  (slot-value ea 'base))
             (r/m   (slot-value base 'code))
             (index (slot-value ea 'index))
             (idx   (slot-value index 'code))
             (ss    (ss idx)) )
        (emit-sib ss idx r/m)
        (values (emit-disp disp) r/m.sib) ) )

    ;; process-operand/ea/reg
    (process-operand/ea/reg (reg)
        (declare (type register reg))
      (assert (plusp modrm-ofs))
      (let ((code (slot-value reg 'code)))
        (when (and (eq  (slot-value reg 'regclass) :gpr)
                   (eql (slot-value reg 'size) 64) )
          (emit-rex REX.W) )
        (values mod.reg code) ) )

    ;; process-modrm
    (process-modrm ()
      (let ((opcode (second format)))
        (emit-opcode opcode)
        (setq modrm-ofs (slot-value ctx 'offset))
        (emit-u8 0)
        (process-operands) ) )

    ;; process-normal
    (process-normal ()
      (let ((opcode (second format)))
        (emit-opcode opcode)
        (process-operands) ) )

    ;; process-operands
    (process-operands ()
      (loop
        for opdfmt in (fourth format)
        and opd in operands do
          (process-operand opdfmt opd) ) )

    ;; process-opext
    (process-opext ()
      (let ((opcode (second format)))
        (emit-opcode (ash opcode -8))
        (setq modrm-ofs (slot-value ctx 'offset))
        (emit-u8 (ash (ldb (byte 3 0) opcode) 3))
        (process-operands) ) )

    ;; remember-annot
    (remember-annot (annot)
        (declare (type asm-annot annot))
      (setf (slot-value annot 'address) (slot-value ctx 'address))
      (push annot (slot-value ctx 'anchors)) )

    ;; remember-labelref
    (remember-labelref (lr)
        (declare (type asm-labelref lr))
      (ecase (slot-value lr 'kind)
        ((:absolute)
          (remember-annot (make-instance 'asm-annot
                :kind    :label
                :address (slot-value ctx 'address)
                :datum   (slot-value lr 'bblock) )) )
        ((:relative)
          (setf (slot-value lr 'address) (slot-value ctx 'address))
          (setf (slot-value lr 'offset)  (slot-value ctx 'offset))
          (push lr (slot-value ctx 'anchors)) ) ) )

    ;; ss
    (ss (idx)
        (declare (values (member 0 #x40 #x80 #xC0)))
        (declare (type (unsigned-byte 16) idx))
      (ecase isasiz
        ((32) (ash (ash idx -3) 6))
        ((64) (ash (ash idx -4) 6)) ) )

    ;; update-modrm
    ;;  Note: Ob and Ov doesn't have ModR/M
    (update-modrm (mod reg r/m)
      (unless (eql modrm-ofs 0)
        (when (>= reg 8) (emit-rex REX.R))
        (when (>= r/m 8) (emit-rex REX.B))
        (setf (elt cv modrm-ofs)
          (logior (elt cv modrm-ofs)
              mod (ash (ldb (byte 3 0) reg) 3) (ldb (byte 3 0) r/m) ))) )
    )
    ;;
  (ecase (third format)
    ((:modrm)  (process-modrm))
    ((:normal) (process-normal))
    ((:opext)  (process-opext)) ) ) ) )


;;;; asm-error
(defun asm-error (ctx control &rest args)
    (declare (type asm-context ctx))
  (let ((cond (make-instance 'simple-warning
                    :format-control control
                    :format-arguments   args ) ))
    (format t "; ~A~%" cond) 
    (push cond (slot-value ctx 'failures))
    nil ) )


;;;; parse-assemble
(defun parse-assemble (isa name lambda-list form*)
    (declare (ignore name lambda-list))
  (labels (
    ;; compute-frame
    (compute-frame (ctx)
      (let ((frame-size (slot-value ctx 'frame-size)))
        (ecase (slot-value ctx 'frame-type)
          ((:fixed) (ash frame-size 4))
          ((:restify) (logior (ash frame-size 4) 1)) ) ) )

    ;; generate-annots
    (generate-annots (ctx)
      (with-collector (collect)
        (dolist (anchor (slot-value ctx 'anchors))
          (when (typep anchor 'asm-annot)
            (let* ((annot anchor)
                   (kind  (slot-value annot 'kind))
                   (addr  (slot-value annot 'address))
                   (datum
                     (let ((datum (slot-value annot 'datum)))
                       (case kind
                        ((:label) (slot-value datum 'address))
                        (otherwise datum) ) ) ))
              (collect (list kind addr datum)) )) ) ) )

    ;; generate-codevec
    (generate-codevec (ctx)
      (labels (
        ;; emit-nops
        ;;  byte  mnemonic              code
        ;;  1     XCHG EAX, EAX         90
        ;;  2     MOV  EBP, EBP         89 ED
        ;;  3     LEA  ECX, [ECX+0]     8D 49 00
        ;;  4     NOP  [EAX+0]          0F 1F 40 00
        ;;  5     NOP  [EAX+EAX*1+0]    0F 1F 44 00 00
        ;;  6     LEA  EAX, [EAX+0]     8D 80 00 00 00 00
        ;;  7     NOP  [EAX+0]          0F 1F 80 00 00 00 00
        ;;  8     NOP  [EAX+0]          0F 1F 84 00 00 00 00 00
        ;;  9     OPDSIZ NOP  [EAX+0]   66 0F 1F 84 00 00 00 00 00
        ;;
        ;; See: "Intel 64 and IA-32 Architecture Optimization Reference
        ;; Manual", November 2006, 3.5.1.8 "Using NOPs".
        ;;
        ;; Note: "Intel 64 and IA-32 Architecture Software Developer's
        ;; Manual", Volume 2B, November 2006, "NOP" describes multi-byte NOP
        ;; only encodings:
        ;;  2   OPDSIZ NOP                  66 90
        ;;  3   NOP [EAX]                   0F 1F 00
        ;;  6   OPDSIZ NOP [EAX+EAX*1+0]    66 0F 1F 44 00 00
        ;;
        ;; Note: AMD recommends OPDSIZ(66){0,3}+NOP(90)
        ;; See "AMD Software Optimization Guide for AMD64 Processors",
        ;; 4.12 "Code Padding with Operand-Size Override and NOP".
        ;;
        (emit-nops (cv bb)
          (let* ((adr (slot-value bb 'original))
                 (nxt (slot-value bb 'address))
                 (k  (- nxt adr)) )
            (loop
              while (>= k 9) do
                (emit-nop-9 cv adr)
                (incf adr 9)
                (decf k 9) )
            (case k
             ((1) (emit-nop-1 cv adr))
             ((2) (emit-nop-2 cv adr))
             ((3) (emit-nop-3 cv adr))
             ((4) (emit-nop-4 cv adr))
             ((5) (emit-nop-5 cv adr))
             ((6) (emit-nop-6 cv adr))
             ((7) (emit-nop-7 cv adr))
             ((8) (emit-nop-8 cv adr))) ) )

        (emit-nop-1 (cv adr)
          (setf (elt cv adr) #x90) )

        (emit-nop-2 (cv adr)
          (setf (elt cv (+ adr 0)) #x89)
          (setf (elt cv (+ adr 1)) #xED) )

        (emit-nop-3 (cv adr)
          (setf (elt cv (+ adr 0)) #x8D)
          (setf (elt cv (+ adr 1)) #x49)
          (setf (elt cv (+ adr 2)) #x00) )

        (emit-nop-4 (cv adr)
          (setf (elt cv (+ adr 0)) #x0F)
          (setf (elt cv (+ adr 1)) #x1F)
          (setf (elt cv (+ adr 2)) #x40)
          (setf (elt cv (+ adr 3)) #x00) )

        (emit-nop-5 (cv adr)
          (setf (elt cv (+ adr 0)) #x0F)
          (setf (elt cv (+ adr 1)) #x1F)
          (setf (elt cv (+ adr 2)) #x44)
          (setf (elt cv (+ adr 3)) #x00)
          (setf (elt cv (+ adr 3)) #x00) )

        (emit-nop-6 (cv adr)
          (setf (elt cv (+ adr 0)) #x8D)
          (setf (elt cv (+ adr 1)) #x80)
          (fill cv 0 :start (+ adr 2) :end (+ adr 6)) )

        (emit-nop-7 (cv adr)
          (setf (elt cv (+ adr 0)) #x0F)
          (setf (elt cv (+ adr 1)) #x1F)
          (setf (elt cv (+ adr 2)) #x80)
          (fill cv 0 :start (+ adr 3) :end (+ adr 7)) )

        (emit-nop-8 (cv adr)
          (setf (elt cv (+ adr 0)) #x0F)
          (setf (elt cv (+ adr 1)) #x1F)
          (setf (elt cv (+ adr 2)) #x84)
          (fill cv 0 :start (+ adr 3) :end (+ adr 8)) )

        (emit-nop-9 (cv adr)
          (setf (elt cv adr) #x66)
          (emit-nop-8 cv (1+ adr)) )
        )
      (let ((bblocks (slot-value ctx 'bblocks)))
        (push (make-instance 'asm-bblock
                :address (slot-value ctx 'address)
                :offset  (slot-value ctx 'offset) )
              bblocks )
        (setq bblocks (sort bblocks #'<
                        :key (lambda (bb) (slot-value bb 'address)) ))
        (setf (slot-value ctx 'bblocks) bblocks)
        (loop
           with cb = (slot-value ctx 'address)
           with cv = (make-array cb :element-type '(unsigned-byte 8))
           with codebuf = (slot-value ctx 'codebuf)
           for bb1 = (first bblocks) then bb2
           for bb2 in (rest bblocks) do
             (emit-nops cv bb1)
             (replace cv codebuf
                  :start1 (slot-value bb1 'address)
                  :start2 (slot-value bb1 'offset)
                  :end2   (slot-value bb2 'offset) )
           finally (return cv) ) ) ) )

    ;; generate-gcmap
    (generate-gcmap ()
      #+nil "FIXME 2007-04-15: NYI parse-assemble/generate-gcmap" )

    ;; init
    (init ()
      (let* ((bb  (make-instance 'asm-bblock :name nil :kind :code))
             (ctx (make-instance 'asm-context
                    :isa isa
                    :bblock bb
                    :bblocks (list bb)
                    :frame-size
                        (ecase (slot-value isa 'size)
                          ((32) 4)
                          ((64) 8) ) ) ) )
        ctx ) )

    ;; resolve-labelrefs
    ;; FIXME 2007-04-15 We should check cookie at align 16 and insert pad
    ;; to avoid this situation.
    (resolve-labelrefs (ctx)
      (let ((cv (slot-value ctx 'codebuf)))
      (labels (
        ;; align
        (align (adr k)
          (let ((rem (rem adr k)))
            (if (eql rem 0) 0 (- k rem)) ) )

        ;; patch-s32
        (patch-s32 (ofs s32)
          (setf (elt cv (+ ofs 0)) (ldb (byte 8  0) s32))
          (setf (elt cv (+ ofs 1)) (ldb (byte 8  8) s32))
          (setf (elt cv (+ ofs 2)) (ldb (byte 8 16) s32))
          (setf (elt cv (+ ofs 3)) (ldb (byte 8 24) s32)) )

        ;; relocate
        (relocate (delta runner)
          (loop
            for anchor in runner
            for adr = (slot-value anchor 'address) do
              (when (typep anchor 'asm-bblock)
                (let* ((bb anchor)
                       (org (+ (slot-value bb 'original) delta))
                       (alg (+ org (align org (slot-value bb 'align)))) )
                  (setq delta (- alg adr)) ))
              (setf (slot-value anchor 'address) (+ adr delta)) ) )

        ;; resolve
        (resolve ()
          (loop
            with changed = nil
            for runner on (slot-value ctx 'anchors)
            for anchor = (first runner) do
              (when (typep anchor 'asm-labelref)
                (when (resolve-1 anchor (rest runner))
                  (setq changed t) ))
            finally (return changed) ) )

        ;; resolve-1
        (resolve-1 (lr rest)
            (declare (type asm-labelref lr))
          (let* ((adr (slot-value lr 'address))
                 (ofs (slot-value lr 'offset))
                 (bb  (slot-value lr 'bblock))
                 (ra  (- (slot-value bb 'address) adr 1)) )

            (cond
              ((<= -128 ra 127)
                ;; short form
                (setf (elt cv ofs) (ldb (byte 8 0) ra))
                nil )
              ((slot-value lr 'longp)
                ;; long form
                (patch-s32 ofs ra)
                nil )
              (t
                (setf (slot-value lr 'longp) t)
                (if (eql (elt cv (- ofs 1)) #xEB)
                    (progn
                      (setf (elt cv (- ofs 1)) #xE9)
                      (relocate 3 rest) )
                  (progn
                    (decf ra 4)
                    (setf (elt cv ofs) (elt cv (- ofs 1)))
                    (setf (elt cv (- ofs 1)) #x0F)
                    (incf adr)
                    (setf (slot-value lr 'address) adr)
                    (incf ofs)
                    (setf (slot-value lr 'offset)  ofs)
                    (relocate 4 rest) ))
                (setq ra (- (slot-value bb 'address) adr 4))
                (patch-s32 ofs ra)
                t )) ) )
        )
        ;;
        (setf (slot-value ctx 'anchors)
            (sort (slot-value ctx 'anchors) #'<
                :key (lambda (a) (slot-value a 'address))) )
        (loop while (resolve)) ) ) )
    )
    ;;
    (let* ((ctx (init))
           (*asm-context* ctx) )

      (loop for runner on form* do
        (setf (slot-value ctx 'forms) runner)
        (asm-parse-form ctx (car runner)) )

      (dolist (bb (slot-value ctx 'bblocks))
        (unless (slot-value bb 'kind)
          (asm-error ctx "Label ~S is undefined." (slot-value bb 'name)) ) )
      (unless (slot-value ctx 'failures)
        (resolve-labelrefs ctx)
        (list :annotations  (generate-annots ctx)
              :class        (slot-value ctx 'class)
              :codevec      (generate-codevec ctx)
              :frame        (compute-frame ctx)
              :gcmap        (generate-gcmap) )) ) ) )
