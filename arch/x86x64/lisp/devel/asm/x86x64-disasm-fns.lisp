;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - arch - x86x64 - devel - Disassembler
;;; arch/x86x64/lisp/devel/asm/x86x64-disasm-fns.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/devel/asm/x86x64-disasm-fns.lisp#5 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :x86x64)

(declaim (ftype (function (si::native-code-object) list)
    compute-labels ) )

(declaim (ftype (function
    (si::native-code-object sequence-index &optional t)
    (values (integer 1 15) list list list) )
  decode-instruction ) )


;;;; compute-labels
(defun compute-labels (fn)
    (declare (values list))
    (declare (type si::native-code-object fn))
  (let ((fn.end
          (let* ((cb (- (ref si::native-code-object length fn)
                        (si::sizeof 'si::native-code-function) ) ))
            (si::!elt 'si::uint32 fn (- cb 8)) ) ))
  (let ((ctx.labels '()))
  (labels (
    ;; process
    (process (opdfmt operand)
      (case opdfmt
        ((Ev)   (process-Ev operand))
        ((Ev64) (process-Ev operand))
        ((Jb)   (process-Jv operand))
        ((Jv)   (process-Jv operand)) ) )

    ;; process-Ev
    (process-Ev (operand)
      (when (consp operand)
        (ecase (slot-value *isa* 'size)
          ((32) (second (member :eip operand)))
          ((64) (second (member :rip operand))) )) )

    ;; process-Jv
    (process-Jv (target)
      (when (and (integerp target) (<= 0 target fn.end))
        target ) )
    )
    (loop
      with offset = 0
      while (< offset fn.end) do
        (multiple-value-bind (length format operands)
            (decode-instruction fn offset)
          (let ((mnemonic (first format))
                (opdfmts  (fourth format)) )
              (declare (ignore mnemonic))
            (loop
              for opdfmt in opdfmts
              for operand in operands
              for label = (process opdfmt operand) do
                (when label (push label ctx.labels)) )
            (incf offset length) ) )
      finally
        (si::with-code-annotation-iterator (next fn)
          (loop
            (multiple-value-bind (typ ofs datum) (next)
                (declare (ignore ofs))
              (unless typ (return))
              (case typ
                ((:label) (push datum ctx.labels)) ) )) )
        (return (sort ctx.labels #'<)) ) ) ) ) )


;;;; decode-instruction
;;;
;;; Syntax:
;;;     decode-instruction fn ra &optional =>
;;;         length
;;;         format
;;;         operands
;;;         prefixes
(defun decode-instruction (fn ra &optional annot-p)
    (declare (values (integer 1 15) list list list))
    (declare (type si::native-code-object fn))
  (let ((ctx.ra ra)
        (ctx.prefixes '())
        (ctx.opdsiz 32)
        (ctx.adrsiz (slot-value *isa* 'size))
        (ctx.rex 0)
        (ctx.modrm #x100)
        ;;
        (REX.W 3)
        (REX.R 2)
        (REX.X 1)
        (REX.B 0) )
  (labels (
    ;; decode-opcode
    (decode-opcode ()
        (declare (values (unsigned-byte 32)))
      (loop
        (let ((opcode (read-u8)))
          (case opcode
            ((#xF0) (push :lock ctx.prefixes))
            ((#xF2) (push :repne ctx.prefixes))
            ((#xF3) (push :rep ctx.prefixes))
            ((#x26) (push :es ctx.prefixes))
            ((#x2E) (push :cs ctx.prefixes))
            ((#x36) (push :ss ctx.prefixes))
            ((#x3E) (push :ds ctx.prefixes))
            ((#x64) (push :fs ctx.prefixes))
            ((#x65) (push :gs ctx.prefixes))
            ((#x66) (setq ctx.opdsiz 16))
            ((#x67) (setq ctx.adrsiz 16))
            (otherwise
              ;; deocde REX prefix
              (when (and (eql (slot-value *isa* 'size) 64)
                         (<= #x40 opcode #x4F) )
                (setq ctx.rex opcode)
                (when (logbitp REX.W opcode) (setq ctx.opdsiz 64))
                (setq opcode (read-u8)) )

              ;; Check mandatory prefix
              (when (eql opcode #x0F)
                (cond
                  ((eql ctx.opdsiz 16)
                    (setq opcode (read-u8))
                    (if (gethash (logior #x660F00 opcode) *opcode-table*)
                        (progn
                          (setq opcode (logior #x660F00 opcode))
                          (setq ctx.opdsiz 32) )
                      (setq opcode (logior #x0F00 opcode)) ) )
                  ((member :repne ctx.prefixes)
                    (setq ctx.prefixes (delete :repne ctx.prefixes))
                    (setq opcode (logior (read-u8) #xF20F00)) )
                  ((member :rep ctx.prefixes)
                    (setq ctx.prefixes (delete :rep ctx.prefixes))
                    (setq opcode (logior (read-u8) #xF30F00)) )
                  (t
                    (setq opcode (logior #x0F00 (read-u8))) )))

              (when (logbitp REX.W ctx.rex) (setq ctx.opdsiz 64))

              (return opcode) )) )) )

    ;; decode-operand
    (decode-operand (format)
      (ecase format
        ((1)    1)
        ((AL)   (decode-reg :gpr 8 0))
        ((BL)   (decode-reg :gpr 8 3))
        ((CL)   (decode-reg :gpr 8 1))
        ((DL)   (decode-reg :gpr 8 2))
        ((AH)   (decode-reg :gpr 8 4))
        ((BH)   (decode-reg :gpr 8 7))
        ((CH)   (decode-reg :gpr 8 5))
        ((DH)   (decode-reg :gpr 8 6))

        ((eAX)  (decode-operand/eAX 0))
        ((eBP)  (decode-operand/eAX 5))
        ((eBX)  (decode-operand/eAX 3))
        ((eCX)  (decode-operand/eAX 1))
        ((eDI)  (decode-operand/eAX 7))
        ((eDX)  (decode-operand/eAX 2))
        ((eSI)  (decode-operand/eAX 6))
        ((eSP)  (decode-operand/eAX 4))

        ((DX)   (decode-reg :gpr 16 2))

        ((Eb)   (decode-ea :gpr 8))
        ((Ed)   (decode-ea :gpr 32))
        ((Ev)   (decode-operand/Ev))
        ((Ew)   (decode-ea :gpr 16))
        ((Ev64) (decode-operand/Ev64))

        ((Gb)   (decode-operand/Gb))
        ((Gd)   (decode-operand/Gd))
        ((Gq)   (decode-operand/Gq))
        ((Gv)   (decode-operand/Gv))

        ((Ib)   (read-s8))
        ((Iw)   (read-s16))
        ((Iv)   (decode-operand/Iv))
        ((Iz)   (decode-operand/Iz))

        ((Jb)   (decode-operand/Jb))
        ((Jv)   (decode-operand/Jv))

        ((Nq Pd Pq Qd Qq)
          (decode-ea :mmx 64) )

        ((Ob)   (cons :byte (decode-operand/Ov)))
        ((Ov)   (decode-operand/Ov))

        ((rAX)  (decode-operand/rAX 0))
        ((rBP)  (decode-operand/rAX 5))
        ((rBX)  (decode-operand/rAX 3))
        ((rCX)  (decode-operand/rAX 1))
        ((rDI)  (decode-operand/rAX 7))
        ((rDX)  (decode-operand/rAX 2))
        ((rSI)  (decode-operand/rAX 6))
        ((rSP)  (decode-operand/rAX 4))

        ((Udq)
          (decode-reg :xmm 128 (reg (ldb (byte 3 0) (get-modrm)) REX.R)) )

        ((Vdq Vpd Vps Vsd Vss)
          (decode-reg :xmm 128 (reg (ldb (byte 3 3) (get-modrm)) REX.R)) )

        ((Wdq Wpd Wps)
          (decode-ea :xmm 128) )

        ((Wsd)
          (decode-ea :xmm 64) )

        ((Wss)
          (decode-ea :xmm 32) )
        ) )

    ;; decode-operand/eAX
    (decode-operand/eAX (reg)
      (decode-reg :gpr ctx.opdsiz reg) )

    ;; decode-operand/Ev
    (decode-operand/Ev ()
      (decode-ea :gpr ctx.opdsiz) )

    ;; decode-operand/Ev64
    (decode-operand/Ev64 ()
      (if (eql (slot-value *isa* 'size) 64)
          (decode-ea :gpr 64)
        (decode-ea :gpr ctx.opdsiz) ) )

    ;; decode-operand/Gb
    (decode-operand/Gb ()
      (decode-reg :gpr 8 (reg (ldb (byte 3 3) (get-modrm)) REX.R)) )

    ;; decode-operand/Gd
    (decode-operand/Gd ()
      (decode-reg :gpr 32 (reg (ldb (byte 3 3) (get-modrm)) REX.R)) )

    ;; decode-operand/Gq
    (decode-operand/Gq ()
      (decode-reg :gpr 64 (reg (ldb (byte 3 3) (get-modrm)) REX.R)) )

    ;; decode-operand/Gv
    (decode-operand/Gv ()
      (decode-reg :gpr ctx.adrsiz (reg (ldb (byte 3 3) (get-modrm)) REX.R)) )

    ;; decode-operand/Id
    (decode-operand/Id ()
      (or (get-annot) (read-s32)) )

    ;; decode-operand/Iq
    (decode-operand/Iq ()
      (if (eql (slot-value *isa* 'size) 64)
          (let ((annot (get-annot)))
            (if annot
                (progn (incf ctx.ra 4) annot)
              (let ((l32 (read-u32))
                    (h32 (read-s32)) )
                (logior l32 (ash h32 32)) )) )
        (let ((l32 (read-u32))
              (h32 (read-s32)) )
          (logior l32 (ash h32 32)) )) )

    ;; decode-operand/Iv
    (decode-operand/Iv ()
      (ecase ctx.opdsiz
        ((8) (read-s8))
        ((16) (read-s16))
        ((32) (decode-operand/Id))
        ((64) (decode-operand/Iq)) ) )

    ;; decode-operand/Iz
    (decode-operand/Iz ()
      (ecase ctx.opdsiz
        ((8) (read-s8))
        ((16) (read-s16))
        ((32) (decode-operand/Id))
        ((64) (decode-operand/Id)) ) )

    ;; decode-operand/Jb
    (decode-operand/Jb ()
      (let ((disp8 (read-s8)))
        (+ ctx.ra disp8) ) )

    ;; decode-operand/Jv
    (decode-operand/Jv ()
      (or (get-annot) (let ((disp32 (read-s32))) (+ ctx.ra disp32))) )

    ;; decode-operand/Ov
    (decode-operand/Ov ()
      (ecase (slot-value *isa* 'size)
        ((32) (list (decode-operand/Id)))
        ((64) (list (decode-operand/Iq))) ) )

    ;; decode-operand/rAX
    (decode-operand/rAX (reg)
      (if (eql (slot-value *isa* 'size) 64)
          (decode-reg :gpr 64 reg)
        (decode-reg :gpr ctx.opdsiz reg) ) )

    ;; decode-ea
    (decode-ea (regclass opdsiz)
      (let* ((modrm (get-modrm))
             (mod (ldb (byte 2 6) modrm))
             (r/m (ldb (byte 3 0) modrm)) )
        (case mod
          ((0)  ; disp0
            (case r/m
              ((4)  ; [base+index]
                (opdsiz (decode-ea/sib mod) opdsiz) )
              ((5)  ; [disp32]
                (if (eql (slot-value *isa* 'size) 64)
                    (opdsiz (decode-ea/rip+disp32) opdsiz)
                  (opdsiz (decode-ea/disp32) opdsiz) ) )
              (otherwise
                (opdsiz
                  (list (decode-reg regclass ctx.adrsiz (reg r/m REX.B)))
                  opdsiz ) )) )
          ((1)  ; disp8
            (case r/m
              ((4)  ; [base+index+disp8]
                (let ((sib  (decode-ea/sib mod))
                      (disp (read-s8)) )
                  (opdsiz (nconc sib (list disp)) opdsiz) ) )
              (otherwise
                (let ((rb (decode-reg :gpr ctx.adrsiz (reg r/m REX.B)))
                      (disp (read-s8)) )
                  (opdsiz (list rb disp) opdsiz) ) )) )
          ((2)  ; disp32
            (case r/m
              ((4)  ; [base+index+disp32]
                (let ((sib  (decode-ea/sib mod))
                      (disp (decode-ea/disp32)) )
                  (unless (consp disp) (setq disp (list disp)))
                  (opdsiz (nconc sib disp) opdsiz) ) )
              (otherwise
                (let ((rb (decode-reg :gpr ctx.adrsiz (reg r/m REX.B)))
                      (disp (decode-ea/disp32)) )
                  (unless (consp disp) (setq disp (list disp)))
                  (opdsiz (cons rb disp) opdsiz) ) )) )
         ((3)   ; reg
            (decode-reg regclass opdsiz (reg r/m REX.B)) )) ) )

    ;; decode-ea/disp32
    (decode-ea/disp32 ()
      (if (eql (slot-value *isa* 'size) 64)
          (read-s32)
        (or (get-annot)
            (let ((addr (read-u32))
                  (faddr (ash (si::address-of fn) 2)) )
              (cond
                ((< faddr
                    addr
                    (+ faddr (ref si::native-code-object length fn)))
                  `(:eip ,(- addr faddr 12)) )
                ((<= addr #x7fffffff)
                  addr )
                (t
                  (- addr #.(ash 1 32)) )) )) ) )

    ;; decode-ea/rip+disp32
    (decode-ea/rip+disp32 ()
      (let ((disp32 (read-s32)))
        `(:rip ,(+ ctx.ra disp32)) ) )

    ;; decode-ea/sib
    ;;   7 6  5 4 3  2 1 0
    ;;  +----+------+------+
    ;;  | ss | index| base |
    ;;  +----+------+------+
    (decode-ea/sib (mod)
      (let* ((sib (read-u8))
             (scale (ldb (byte 2 6) sib))
             (idx   (ldb (byte 3 3) sib))
             (base  (reg (ldb (byte 3 0) sib) REX.B))
             (rb (decode-reg :gpr ctx.adrsiz base)) )
        (cond
          ((not (eql idx 4))    ; idx=esp
            (let ((rx (decode-reg/idx ctx.adrsiz idx scale)))
              (if (and (eql mod 0) (eql base 5))
                  (list (decode-ea/disp32) rx)
                (list rb rx) ) ) )
          ((eql base 5)         ; base=esp
            nil )
          (t
            (list rb) )) ) )

    ;; decode-reg
    (decode-reg (regclass opdsiz reg)
      (ecase regclass
        ((:gpr)
          (decode-reg/gpr opdsiz reg) )
        ((:fpr)
          (svref (third (assoc :fpr (slot-value *isa* 'registers))) reg) )
        ((:mmx)
          (svref (third (assoc :mmx (slot-value *isa* 'registers))) reg) )
        ((:xmm)
          (svref (third (assoc :xmm (slot-value *isa* 'registers))) reg) )) )

    ;; decode-reg/gpr
    (decode-reg/gpr (opdsiz reg)
      (let (#+nil (reg (if (eql ctx.rex 0) reg (+ reg 8))))
        (svref
            (third
                (find-if (lambda (x) (and (eq (first x) :gpr)
                                          (eq (second x) opdsiz) ))
                    (slot-value *isa* 'registers) ))
            reg ) ) )

    ;; decode-reg/idx
    (decode-reg/idx (adrsiz reg scale)
      (let ((reg (+ (reg reg REX.X)
                    (* (if (eql (slot-value *isa* 'size) 32) 8 16) scale) ) ))
        (svref
            (third
                (find-if (lambda (x) (and (eq (first x) :idx)
                                          (eq (second x) adrsiz) ))
                    (slot-value *isa* 'registers) ))
            reg ) ) )

    ;; get-annot
    (get-annot ()
      (when annot-p
         (multiple-value-bind (typ ofs datum)
             (si::find-code-annotation fn ctx.ra)
             (declare (ignore ofs))
           (when typ (incf ctx.ra 4) (list typ datum)) )) )

    ;; get-format
    (get-format (opcode)
      (let ((format (gethash opcode *opcode-table*)))
        (cond
          ((null format)
            (if (< opcode #x100)
                (progn
                  (decf ctx.ra)
                  '(db 0 :normal (ib)) )
              (progn
                (decf ctx.ra 2)
                '(db 0 :normal (ib ib)) )) )
          ((null (first format))
            (ecase (third format)
              ((:opext-leader)
                (let ((modrm (get-modrm)))
                  (get-format (logior (ash opcode 8)
                                      (ldb (byte 3 3) modrm) ) ) ) )
              ((:twobyte)
                (get-format (logior (ash opcode 8) (read-u8))) )) )
         (t format) ) ) )

    ;; get-modrm
    (get-modrm ()
      (when (eql ctx.modrm #x100)
        (setq ctx.modrm (read-u8)) )
      ctx.modrm )

    ;; fetch-s16
    (fetch-s16 (ofs)
      (si::!elt 'si::int16 fn ofs) )

    ;; fetch-s32
    (fetch-s32 (ofs)
      (si::!elt 'si::int32 fn ofs) )

    ;; fetch-s8
    (fetch-s8 (ofs)
      (si::!elt 'si::int8 fn ofs) )

    ;; fetch-u32
    (fetch-u32 (ofs)
      (si::!elt 'si::uint32 fn ofs) )

    ;; fetch-u8
    (fetch-u8 (ofs)
      (si::!elt 'si::uint8 fn ofs) )

    ;; opdsiz
    (opdsiz (opd siz)
      (cond
        ((eql (slot-value *isa* 'size) siz)
          opd )
        ((consp opd)
          (ecase siz
            ((8)   (cons :byte  opd))
            ((16)  (cons :word  opd))
            ((32)  (cons :dword opd))
            ((64)  (cons :qword opd))
            ((128) (cons :dqword opd)) ) )
        (t
          (ecase siz
            ((8)   `(:byte ,opd))
            ((16)  `(:word ,opd))
            ((32)  `(:dword ,opd))
            ((64)  `(:qword ,opd))
            ((128) `(:xmmword ,opd)) ) )) )

    ;; read-s16
    (read-s16 ()
      (prog1 (fetch-s16 ctx.ra) (incf ctx.ra 2)) )

    ;; read-s32
    (read-s32 ()
      (prog1 (fetch-s32 ctx.ra) (incf ctx.ra 4)) )

    ;; read-s8
    (read-s8 ()
      (prog1 (fetch-s8 ctx.ra) (incf ctx.ra)) )

    ;; read-u8
    (read-u8 ()
      (prog1 (fetch-u8 ctx.ra) (incf ctx.ra)) )

    ;; read-u32
    (read-u32 ()
      (prog1 (fetch-u32 ctx.ra) (incf ctx.ra 4)) )

    ;; reg
    (reg (reg rex)
      (if (logbitp rex ctx.rex) (logior 8 reg) reg) )
    )
    ;;
    ;; format ::= opcode mnemonic syntax operands
    (let* ((format (get-format (decode-opcode)))
            #+nil
           (operands (mapcar #'decode-operand (fourth format)))
            ;#+nil
           (operands
              (loop
                for opdfmt in (fourth format)
                  collect (decode-operand opdfmt) ) ))
      (values (- ctx.ra ra)
              format
              operands
              ctx.prefixes ) ) ) ) )


;;;; disassemble-funobj
(defun disassemble-funobj (fn)
    (declare (values si::native-code-object))
    (declare (type si::native-code-object fn))
  (let* ((*isa* (gethash #+x86 :x86 #+x64 :x64 *isa-table*))
         (isasiz (slot-value *isa* 'size)) )
  (multiple-value-bind (fn.frame fn.end)
    (let* ((cb (- (ref si::native-code-object length fn)
                (si::sizeof 'si::native-code-function) ) ))
      (values (si::!elt 'si::uint32 fn (- cb 4))
              (si::!elt 'si::uint32 fn (- cb 8)) ) )
  (let ((ctx.labels (compute-labels fn))
        (nbytes 6) )
  (labels (
    ;; fetch-u8
    (fetch-u8 (ofs)
      (si::!elt 'si::uint8 fn ofs) )

    ;; find-thfield
    (find-thfield (disp)
      (when (integerp disp)
        (loop
          for frob in (slot-value *isa* 'thfields)
          for (ofs len) = frob do
            (when (<= ofs disp (1- (+ ofs len)))
              (return frob) ))) )

    ;; format-opdsiz
    (format-opdsiz (operand)
      (if (not (consp operand))
          operand
        (case (first operand)
          ((:byte)   (format t "byte ptr ")  (cdr operand))
          ((:word)   (format t "word ptr ")  (cdr operand))
          ((:dword)  (format t "dword ptr ") (cdr operand))
          ((:qword)  (format t "qword ptr ") (cdr operand))
          ((:dqword) (format t "qword ptr ") (cdr operand))
          (otherwise operand) )) )

    ;; format-operand/Ev
    (format-operand/Ev (operand)
      (setq operand (format-opdsiz operand))
      (cond
        ((not (consp operand))
          (princ operand) )
        ((or (eq (first operand) 'ebp)
             (eq (first operand) 'rbp) )
          (ecase (length operand)
            ((1) (format t "[~A]" (first operand)))
            ((2)
              (let* ((disp (second operand))
                     (ofs-siz-name (find-thfield disp)) )
                (cond
                  ((null ofs-siz-name)
                    (format t "[$tcb~@D]" disp) )
                  ((eql (second ofs-siz-name)
                        (ash isasiz -3) )
                    (format t "$tcb.~A" (third ofs-siz-name)) )
                  (t
                    (format t "$tcb.~A[~D]"
                        (third ofs-siz-name)
                        (truncate (- disp (first ofs-siz-name))
                                  (ash isasiz -3) )) )) ) )
            ((3)
              (let* ((index (second operand))
                     (disp (third operand))
                     (ofs-siz-name (find-thfield disp)) )
                (cond
                  ((null ofs-siz-name)
                    (format t "[$tcb+~A~@D]" index disp) )
                  ((eql (second ofs-siz-name)
                        (ash isasiz -3) )
                    (format t "$tcb.~A[~A]"
                        (third ofs-siz-name)
                        index ) )
                  (t
                    (format t "$tcb.~A[~A~@D]"
                        (third ofs-siz-name)
                        index
                        (truncate (- disp (first ofs-siz-name))
                                  (ash isasiz -3) )) )) ) )) )

        ((eq (first operand) :symfun)
          (format t "#'~S" (second operand)) )
        ((eq (first operand) :symsetf)
          (format t "#'(setf ~S)" (second operand)) )
        ((eq (first operand) :symval)
          (format t "~S" (second operand)) )
        (t
          (loop
            initially (format t "[")
            finally (format t "]")
            for plus = "" then "+"
            for elt in operand do
              (if (symbolp elt)
                  (format t "~A~A" plus elt)
                (format t "~@D" elt) )) )) )

    ;; format-operand/Iv
    (format-operand/Iv (imm)
      (cond
        ((not (integerp imm))
          (case (first imm)
            ((:label)   (format t "L~4,'0X" (second imm)))
            ((:lispval) (format t "'~S" (second imm)))
            (otherwise (format t "~S" imm)) ) )
        ((>= imm #x1000)
          (format t "#x~X" imm) )
        ((<= imm #x-1000)
          (if (eql isasiz 32)
              (format t "#x~X" (+ imm (ash 1 32)))
            (format t "#x~X" (+ imm (ash 1 64))) ) )
        (t
          (format t "~D" imm) )) )

    ;; format-operand
    (format-operand/Jv (addr)
      (if (integerp addr)
          (format t "L~4,'0X" addr)
        (case (first addr)
          ((:ncallee) (format t "#'~S" (second addr)))
          (otherwise (format t "~S" (second addr))) )) )

    ;; format-operands
    (format-operands (format operands)
      (loop
        for comma = " " then ", "
        for opdfmt  in (fourth format)
        for operand in operands do
          (write-string comma)
          (case opdfmt
            ((Jb) (format t "L~4,'0X" operand))
            ((Jv) (format-operand/Jv operand))
            ((Iv) (format-operand/Iv operand))
            ((Iz) (format-operand/Iv operand))
            (otherwise (format-operand/Ev operand)) )) )

    ;; get-label
    (get-label (offset)
      (loop
        (when (endp ctx.labels) (return #\Space))
        (let ((label (first ctx.labels)))
          (when (< offset label) (return #\Space))
          (when (= offset label) (return #\L))
          (pop ctx.labels) )) )
    )
    ;;
    (format t "; Disassemble of ~S~%" fn)
    (format t "; frame = #x~X~%" fn.frame)
    (loop
      with offset = 0
      while (< offset fn.end) do
        (multiple-value-bind (length format operands prefixes)
            (decode-instruction fn offset t)

          ;; instruction address
          (format t "; ~C~4,'0X" (get-label offset) offset)

          ;; instruction bytes
          (loop
            for index from 0 below (min length nbytes) do
              (format t " ~2,'0X" (fetch-u8 offset))
              (incf offset) )
          (loop
            for index from length below nbytes do
              (write-string "   ") )

          ;; instruction prefixes
          (format t "  ~{~A ~}" prefixes)

          ;; mnemonic
          (if (null operands)
              (princ (first format))
            (progn
              (format t "~9A" (first format))
               (format-operands format operands) ))

          ;; additional instruction bytes
          (loop
            for index from nbytes below length do
              (when (zerop (rem index nbytes))
                (format t "~%;  ~4,'0X" offset) )
              (format t " ~2,'0X" (fetch-u8 offset))
              (incf offset) )
          (terpri) )
      finally
        (return fn) ) ) ) ) ) )
