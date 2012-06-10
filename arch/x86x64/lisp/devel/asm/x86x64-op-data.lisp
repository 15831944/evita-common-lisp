;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: X86X64; Base: 10 -*-
;;;;
;;;; evcl - arch - x86x64 - devel - Assembler Utility Functions
;;; arch/x86x64/x86x64-op-data.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/devel/asm/x86x64-op-data.lisp#2 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :x86x64)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x00
;;
(defformat #x00 ADD (Eb Gb))
(defformat #x01 ADD (Ev Gv))
(defformat #x02 ADD (Gb Eb))
(defformat #x03 ADD (Gv Ev))
(defformat #x04 ADD (AL Ib))
(defformat #x05 ADD (eAX Iz))
(defformat #x06 PUSH (ES))
(defformat #x07 POP (ES))
(defformat #x08 OR (Eb Gb))
(defformat #x09 OR (Ev Gv))
(defformat #x0A OR (Gb Eb))
(defformat #x0B OR (Gv Ev))
(defformat #x0C OR (AL Ib))
(defformat #x0D OR (eAX Iz))
(defformat #x0E PUSH (CS))
;(defformat #x0F nil :twobyte)
;(defformat #x660F38 nil :opext-leader)      ; three byte opcode
(defformat #x0F38 nil :opext-leader)        ; three byte opcode
;(defformat #x660F nil :twobyte)             ; SSE2 instruction

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x10
;;
(defformat #x10 ADC (Eb Gb))
(defformat #x11 ADC (Ev Gv))
(defformat #x12 ADC (Gb Eb))
(defformat #x13 ADC (Gv Ev))
(defformat #x14 ADC (AL Ib))
(defformat #x15 ADC (eAX Iz))
(defformat #x16 PUSH (SS))
(defformat #x17 POP (SS))
(defformat #x18 SBB (Eb Gb))
(defformat #x19 SBB (Ev Gv))
(defformat #x1A SBB (Gb Eb))
(defformat #x1B SBB (Gv Ev))
(defformat #x1C SBB (AL Ib))
(defformat #x1D SBB (eAX Iz))
(defformat #x1E PUSH (DS))
(defformat #x1F POP (DS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x20
;;
(defformat #x20 AND (Eb Gb))
(defformat #x21 AND (Ev Gv))
(defformat #x22 AND (Gb Eb))
(defformat #x23 AND (Gv Ev))
(defformat #x24 AND (AL Ib))
(defformat #x25 AND (eAX Iz))
;;(defformat #x26 ES)
(defformat #x27 DAA)
(defformat #x28 SUB (Eb Gb))
(defformat #x29 SUB (Ev Gv))
(defformat #x2A SUB (Gb Eb))
(defformat #x2B SUB (Gv Ev))
(defformat #x2C SUB (AL Ib))
(defformat #x2D SUB (eAX Iz))
;;(defformat #x2E CS)
(defformat #x2F DAS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x30
;;
(defformat #x30 XOR (Eb Gb))
(defformat #x31 XOR (Ev Gv))
(defformat #x32 XOR (Gb Eb))
(defformat #x33 XOR (Gv Ev))
(defformat #x34 XOR (AL Ib))
(defformat #x35 XOR (eAX Iz))
;;(defformat #x36 SS)
(defformat #x37 AAA)
(defformat #x38 CMP (Eb Gb))
(defformat #x39 CMP (Ev Gv))
(defformat #x3A CMP (Gb Eb))
(defformat #x3B CMP (Gv Ev))
(defformat #x3C CMP (AL Ib))
(defformat #x3D CMP (eAX Iz))
;;(defformat #x3E DS)
(defformat #x3F AAS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x40
;;
(defformat #x40 INC (eAX))
(defformat #x41 INC (eCX))
(defformat #x42 INC (eDX))
(defformat #x43 INC (eBX))
(defformat #x44 INC (eSP))
(defformat #x45 INC (eBP))
(defformat #x46 INC (eSI))
(defformat #x47 INC (eDI))
(defformat #x48 DEC (eAX))
(defformat #x49 DEC (eCX))
(defformat #x4A DEC (eDX))
(defformat #x4B DEC (eBX))
(defformat #x4C DEC (eSP))
(defformat #x4D DEC (eBP))
(defformat #x4E DEC (eSI))
(defformat #x4F DEC (eDI))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x50
;;
(defformat #x50 PUSH (rAX))
(defformat #x51 PUSH (rCX))
(defformat #x52 PUSH (rDX))
(defformat #x53 PUSH (rBX))
(defformat #x54 PUSH (rSP))
(defformat #x55 PUSH (rBP))
(defformat #x56 PUSH (rSI))
(defformat #x57 PUSH (rDI))
(defformat #x58 POP (rAX))
(defformat #x59 POP (rDX))
(defformat #x5A POP (rCX))
(defformat #x5B POP (rBX))
(defformat #x5C POP (rSP))
(defformat #x5D POP (rBP))
(defformat #x5E POP (rSI))
(defformat #x5F POP (rDI))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x60
;;
(defformat #x60   PUSHAD)
(defformat #x6660 PUSHA)
(defformat #x61   POPAD)
(defformat #x6661 POPA)
;;(defformat #x62 BOUND (Gv Ma))
;;(defformat #x63 ARPL (Ew Gw))   :processor 286 :privilege 0)
;;(defformat #x64   FS)
;;(defformat #x65   GS)
(defformat #x66   OPDSIZ)
;;(defformat #x67   ADDRSIZ)
(defformat #x68 PUSH (Iz))
(defformat #x69 IMUL (Gv Ev Iz))
(defformat #x6A PUSH (Ib))
(defformat #x6B IMUL (Gv Ev Ib))
;;(defformat #x6C INSB (Yb DX))
;;(defformat #x6D INSD (Yv DX))
;;(defformat #x666D INSW (Yv DX))
;;(defformat #x6E OUTSB (DX Xb))
;;(defformat #x6F OUTSD (DX Xd))
;;(defformat #x666F OUTSW ((DX Xw)))

;; x64 only
(defformat #x63 MOVSXD (Gv Ev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x70
;;
;(defformat #x70 Jcc (Jb))
(defformat #x70 JO  (Jb)    :aliases (Jcc))
(defformat #x71 JNO (Jb))
(defformat #x72 JB  (Jb)    :aliases (JB JC JNAE))
(defformat #x73 JAE (Jb)    :aliases (JNB JNC))
(defformat #x74 JE  (Jb)    :aliases (JZ JE))
(defformat #x75 JNE (Jb)    :aliases (JNZ JNE))
(defformat #x76 JBE (Jb)    :aliases (JNA))
(defformat #x77 JA  (Jb)    :aliases (JNBE))
(defformat #x78 JS  (Jb))
(defformat #x79 JNS (Jb))
(defformat #x7A JPE (Jb)    :aliases (JP))
(defformat #x7B JPO (Jb)    :aliases (JNP))
(defformat #x7C JL  (Jb)    :aliases (JNGE))
(defformat #x7D JGE (Jb)    :aliases (JNL))
(defformat #x7E JLE (Jb)    :aliases (JNG))
(defformat #x7F JG  (Jb)    :aliases (JNLE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x80
;;
(defformat #x80 nil :opext-leader)    ;; Grp1 Eb Ib
(defformat #x81 nil :opext-leader)    ;; Grp1 Ev Iv
(defformat #x82 nil :opext-leader)    ;; Grp1 Eb Ib
(defformat #x83 nil :opext-leader)    ;; Grp1 Ev Ib
(defformat #x84 TEST (Eb Gb))
(defformat #x85 TEST (Ev Gv))
(defformat #x86 XCHG (Eb Gb))
(defformat #x87 XCHG (Ev Gv))
(defformat #x88 MOV (Eb Gb))
(defformat #x89 MOV (Ev Gv))
(defformat #x8A MOV (Gb Eb))
(defformat #x8B MOV (Gv Ev))
;(defformat #x8C MOV (Ew Sw))   ; We don't support segment registers.
(defformat #x8D LEA (Gv Ev))
;(defformat #x8E MOV (Sw Ew))   ; We don't support segment registers.
(defformat #x8F nil :opext-leader)
(defformat #x8F 0 POP (Ev))

(defformat #x90   NOP)
(defformat #x90 XCHG (eAX eAX))
(defformat #x91 XCHG (eAX eCX))
(defformat #x92 XCHG (eAX eDX))
(defformat #x93 XCHG (eAX eBX))
(defformat #x94 XCHG (eAX eSP))
(defformat #x95 XCHG (eAX eBP))
(defformat #x96 XCHG (eAX eSI))
(defformat #x97 XCHG (eAX eDI))
(defformat #x6698 CBW)
(defformat #x98   CWDE)
(defformat #x99   CDQ)
(defformat #x6699 CWD)
;;(defformat #x9A CALLF (Ap))
(defformat #x9B   WAIT) ;; alias FWAIT
(defformat #x9C   PUSHFD)
(defformat #x669C PUSHF)
(defformat #x9D   POPFD)
(defformat #x669D POPF)
(defformat #x9E   SAHF)
(defformat #x9F   LAHF)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #xA0
;;
(defformat #xA0 MOV (AL  Ob))
(defformat #xA1 MOV (eAX Ov))
(defformat #xA2 MOV (Ob  AL))
(defformat #xA3 MOV (Ov  eAX))
(defformat #xA4 MOVSB)

(defformat #xA5   MOVSD)
(defformat #x66A5 MOVSW)
#+x64 (defformat #x48A5 MOVSQ)



(defformat #xA6   CMPSB)
(defformat #xA7   CMPSD)
(defformat #x66A7 CMPSW)
(defformat #xA8 TEST (AL Ib))
(defformat #xA9 TEST (eAX Iz))
(defformat #xAA   STOSB)
(defformat #xAB   STOSD)
(defformat #x66AB STOSW)
(defformat #xAC   LODSB)
(defformat #xAD   LODSD)
(defformat #x66AD LODSW)
(defformat #xAE   SCASB)
(defformat #xAF   SCASD)
(defformat #x66AF SCASW)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #xB0
;;
(defformat #xB0 MOV (AL Ib))
(defformat #xB1 MOV (CL Ib))
(defformat #xB2 MOV (DL Ib))
(defformat #xB3 MOV (BL Ib))
(defformat #xB4 MOV (AH Ib))
(defformat #xB5 MOV (CH Ib))
(defformat #xB6 MOV (DH Ib))
(defformat #xB7 MOV (BH Ib))

(defformat #xB8 MOV (eAX Iv))
(defformat #xB9 MOV (eCX Iv))
(defformat #xBA MOV (eDX Iv))
(defformat #xBB MOV (eBX Iv))
(defformat #xBC MOV (eSP Iv))
(defformat #xBD MOV (eBP Iv))
(defformat #xBE MOV (eSI Iv))
(defformat #xBF MOV (eDI Iv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #xC0
;;
(defformat #xC0 nil :opext-leader)    ;; Grp2 Eb Ib
(defformat #xC1 nil :opext-leader)    ;; Grp2 Ev Ib
(defformat #xC2 RET (Iw))
(defformat #xC3 RET)
;(defformat #xC4 LES (Gv Mp))
;(defformat #xC5 LDS (Gv Mp))
(defformat #xC6 nil :opext-leader)    ;; Grp11 Eb Ib    MOV_Eb_Ib
(defformat #xC7 nil :opext-leader)    ;; Grp11 Ev Iv    MOV_Ev_Iz
(defformat #xC8 ENTER (Iw Ib))
(defformat #xC9 LEAVE)
(defformat #xCA RETF (Iw))
(defformat #xCB RETF)
(defformat #xCC INT3)
(defformat #xCD INT (Ib))
(defformat #xCE INTO)
(defformat #xCF IRET)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #xD0
;;

(defformat #xD0 nil :opext-leader)    ;; Grp2  Eb 1
(defformat #xD1 nil :opext-leader)    ;; Grp2  Ev 1
(defformat #xD2 nil :opext-leader)    ;; Grp2  Eb CL
(defformat #xD3 nil :opext-leader)    ;; Grp2  Ev CL
(defformat #xD40A AAM)
(defformat #xD50A AAD)
(defformat #xD6 UNDEF)
(defformat #xD7 XLATB)
(defformat #xD7 XLAT)
(defformat #xD8 ESC_D8)
(defformat #xD9 ESC_D9)
(defformat #xDA ESC_DA)
(defformat #xDB ESC_DB)
(defformat #xDC ESC_DC)
(defformat #xDD ESC_DD)
(defformat #xDE ESC_DE)
(defformat #xDF ESC_DF)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #xE0
;;
(defformat #xE0 LOOPNE (Jb))   ;; alias LOOPNZ
(defformat #xE1 LOOPE (Jb))   ;; alias LOOPZ
(defformat #xE2 LOOP (Jb))
(defformat #xE3 JECXZ (Jb))
(defformat #xE4 IN (AL  Ib))
(defformat #xE5 IN (eAX Ib))
(defformat #xE6 OUT (Ib  AL))
(defformat #xE7 OUT (Ib eAX))
(defformat #xE8 CALL (Jv))
(defformat #xE9 JMP (Jv))
;;(defformat #xEA JMP (Ap))
(defformat #xEB JMP (Jb))
(defformat #xEC IN (AL DX))
(defformat #xED IN (eAX DX))
(defformat #xEE OUT (DX AL))
(defformat #xEF OUT (DX eAX))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #xF0
;;
(defformat #xF0 LOCK)           ;; prefix
(defformat #xF1 UD1)            ;; UD1 (undocumented)
(defformat #xF2 REPNE)          ;; prefix
(defformat #xF3 REP)            ;; prefix
(defformat #xF390 PAUSE)
(defformat #xF4 HLT)
(defformat #xF5 CMC)
(defformat #xF6 nil :opext-leader)
(defformat #xF7 nil :opext-leader)
(defformat #xF8 CLC)
(defformat #xF9 STC)
(defformat #xFA CLI)
(defformat #xFB STI)
(defformat #xFC CLD)
(defformat #xFD STD)
(defformat #xFE nil :opext-leader)    ;; Group 4
(defformat #xFF nil :opext-leader)    ;; Group 5


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Two Byte Opcode
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x0F00
;;

#+nil (defformat #x0F0B UD2)
;; Note: Reference section specified 0F 1F instead of 0F 0D.
#+nil (defformat #x0F0D NOP (Ev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x0F10
;;
(defformat #x0F10 MOVUPS (Vps Wps))
(defformat #x0F11 MOVUPS (Wps Vps))
(defformat #x0F12 MOVLPS (Vq  Mq))
(defformat #x0F13 MOVLPS (Mq  Vq))
(defformat #x0F14 UNPCKLPS (Vps Wq))
(defformat #x0F15 UNPCKHPS (Vps Wq))
(defformat #x0F16 MOVHPS (Vq  Mq))
(defformat #x0F17 MOVHPS (Mq  Vq))

;(defformat #x660F1F nil :opext-leader)
(defformat #x0F1F nil :opext-leader)
(defformat #x0F1F 0 NOP (Ev))

(defformat #x660F10 MOVUPD (Vps Wps))
(defformat #x660F11 MOVUPD (Wps Vps))
(defformat #x660F12 MOVLPD (Vq  Mq))
(defformat #x660F13 MOVLPD (Mq  Vq))
(defformat #x660F14 UNPCKLPD (Vpd Wq))
(defformat #x660F15 UNPCKHPD (Vpd Wq))
(defformat #x660F16 MOVHPD (Vq  Mq))
(defformat #x660F17 MOVHPD (Mq  Vq))

(defformat #xF30F10 MOVSS (Vss Wss))
(defformat #xF30F11 MOVSS (Wss Vss))
(defformat #xF30F12 MOVSLDUP (Vq  Wq))
(defformat #xF30F16 MOVSHDUP (Vq  Wq))

(defformat #xF20F10 MOVSD (Vsd Wsd))
(defformat #xF20F11 MOVSD (Wsd Vsd))
(defformat #xF20F12 MOVDDUP (Vq  Wq))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x0F20
;;
(defformat #x0F28 MOVAPS (Vps Wps))
(defformat #x0F29 MOVAPS (Wps Vps))
(defformat #x0F2A CVTPI2PS (Vps Qq))
(defformat #x0F2B MOVNTPS (Mps Vps))
(defformat #x0F2C CVTTPS2PI (Qq  Wps))
(defformat #x0F2D CVTPS2PI (Qq  Wps))
(defformat #x0F2E UCOMISS (Vss Wss))
(defformat #x0F2F COMISS (Vss Wss))

(defformat #x660F28 MOVAPD (Vpd Wpd))
(defformat #x660F29 MOVAPD (Wpd Vpd))
(defformat #x660F2A CVTPI2PD (Vpd Qq))
(defformat #x660F2B MOVNTPD (Mpd Vpd))
(defformat #x660F2C CVTTPD2PI (Qdq Wpd))
(defformat #x660F2D CVTPD2PI (Qdq Wpd))
(defformat #x660F2E UCOMISD (Vsd Wsd))
(defformat #x660F2F COMISD (Vsd Wsd))

(defformat #xF20F2A CVTSI2SD (Vsd Ed))
(defformat #xF20F2C CVTTSD2SI (Gd  Wsd))
(defformat #xF20F2C CVTSD2SI (Gd  Wsd))

(defformat #xF30F2A CVTSI2SS (Vss Ed))
(defformat #xF30F2C CVTTSS2SI (Gd  Wss))
(defformat #xF20F2D CVTSS2SI (Gd  Wss))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x0F30
;;
(defformat #x0F31 RDTSC)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x0F40
;;
(defformat #x0F40 CMOVcc (Gv Ev))

(defformat #x0F40 CMOVO (Gv Ev))    ;; alias CMOVcc
(defformat #x0F41 CMOVNO (Gv Ev))    ;; :processor 686
(defformat #x0F42 CMOVB (Gv Ev))    ;; alias CMOVC CMOVNAE
(defformat #x0F42 CMOVC (Gv Ev))    ;; alias CMOVC CMOVNAE
(defformat #x0F42 CMOVHAE (Gv Ev))    ;; alias CMOVC CMOVNAE
(defformat #x0F43 CMOVAE (Gv Ev))    ;; alias CMOVNB CMOVNC
(defformat #x0F43 CMOVNC (Gv Ev))    ;; alias CMOVNB CMOVNC
(defformat #x0F43 CMOVNB (Gv Ev))    ;; alias CMOVNB CMOVNC
(defformat #x0F44 CMOVE (Gv Ev))    ;; alias CMOVZ
(defformat #x0F45 CMOVNE (Gv Ev))    ;; alias CMOVNZ
(defformat #x0F46 CMOVBE (Gv Ev))    ;; alias CMOVNA
(defformat #x0F47 CMOVA (Gv Ev))    ;; alias CMOVNBE
(defformat #x0F48 CMOVS (Gv Ev))
(defformat #x0F49 CMOVNS (Gv Ev))
(defformat #x0F4A CMOVPE (Gv Ev))    ;; alias CMOVP
(defformat #x0F4B CMOVPO (Gv Ev))    ;; alias CMOVNP
(defformat #x0F4C CMOVL (Gv Ev))    ;; alias CMOVNGE
(defformat #x0F4D CMOVGE (Gv Ev))    ;; alias CMOVNL
(defformat #x0F4E CMOVLE (Gv Ev))    ;; alias CMOVNG
(defformat #x0F4F CMOVG (Gv Ev))    ;; alias CMOVNLE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x0F50
;;
(defformat #x0F50 MOVMSKPS (Gd  Ups))
(defformat #x0F51 SQRTPS (Vps Wps))
(defformat #x0F52 RSQRTPS (Vps Wps))
(defformat #x0F53 RCPPS (Vps Wps))
(defformat #x0F54 ANDPS (Vps Wps))
(defformat #x0F55 ANDNPS (Vps Wps))
(defformat #x0F56 ORPS (Vps Wps))
(defformat #x0F57 XORPS (Vps Wps))
(defformat #x0F58 ADDPS (Vps Wps))
(defformat #x0F59 MULPS (Vps Wps))
(defformat #x0F5A CVTPS2PD (Vpd Wpd))
(defformat #x0F5B CVTDQ2PS (Vps Wdq))
(defformat #x0F5C SUBPS (Vps Wps))
(defformat #x0F5D MINPS (Vps Wps))
(defformat #x0F5E DIVPS (Vps Wps))
(defformat #x0F5F MAXPS (Vps Wps))

(defformat #x660F50 MOVMSKPD (Gd  Upd))
(defformat #x660F51 SQRTPD (Vpd Wpd))
(defformat #x660F52 RSQRTPD (Vpd Wpd))
(defformat #x660F53 RCPPD (Vpd Wpd))
(defformat #x660F54 ANDPD (Vpd Wpd))
(defformat #x660F55 ANDNPD (Vpd Wpd))
(defformat #x660F56 ORPD (Vpd Wpd))
(defformat #x660F57 XORPD (Vpd Wpd))
(defformat #x660F58 ADDPD (Vpd Wpd))
(defformat #x660F59 MULPD (Vpd Wpd))
(defformat #x660F5A CVTPD2PD (Vpd Wpd))
(defformat #x660F5B CVTDQ2PD (Vpd Wdq))
(defformat #x660F5C SUBPD (Vpd Wpd))
(defformat #x660F5D MINPD (Vpd Wpd))
(defformat #x660F5E DIVPD (Vpd Wpd))
(defformat #x660F5F MAXPD (Vpd Wpd))

(defformat #xF30F51 SQRTSS (Vss Wss))
(defformat #xF30F58 ADDSS (Vss Wss))
(defformat #xF30F59 MULSS (Vss Wss))
(defformat #xF30F5A CVTSS2SD (Vsd Wss))
(defformat #xF30F5B CVTDQ2SS (Vdq Wps))
(defformat #xF30F5C SUBSS (Vss Wss))
(defformat #xF30F5D MINSS (Vss Wss))
(defformat #xF30F5E DIVSS (Vss Wss))
(defformat #xF30F5F MAXSS (Vss Wss))

(defformat #xF20F51 SQRTSD (Vsd Wsd))
(defformat #xF20F58 ADDSD (Vsd Wsd))
(defformat #xF20F59 MULSD (Vsd Wsd))
(defformat #xF20F5A CVTSD2SS (Vss Wsd))
;; #x5B
(defformat #xF20F5C SUBSD (Vsd Wsd))
(defformat #xF20F5D MINSD (Vsd Wsd))
(defformat #xF20F5E DIVSD (Vsd Wsd))
(defformat #xF20F5F MAXSD (Vsd Wsd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x0F60
;;
(defformat #x0F60 PUNPCKLBW (Pq Qd))     ;; MMX
(defformat #x0F61 PUNPCKLWD (Pq Qd))     ;; MMX
(defformat #x0F62 PUNPCKLDQ (Pq Qd))     ;; MMX
(defformat #x0F63 PCKSSWB (Pq Qd))     ;; MMX
(defformat #x0F64 PCMPGTB (Pq Qd))     ;; MMX
(defformat #x0F65 PCMPGTW (Pq Qd))     ;; MMX
(defformat #x0F66 PCMPGTD (Pq Qd))     ;; MMX
(defformat #x0F67 PACKUSWB (Pq Qd))     ;; MMX
(defformat #x0F68 PUNPCKHBW (Pq Qd))     ;; MMX
(defformat #x0F69 PUNPCKHWD (Pq Qd))     ;; MMX
(defformat #x0F6A PUNPCKHDQ (Pq Qd))     ;; MMX
(defformat #x0F6B PACKSSDW (Pq Qd))     ;; MMX
;;          #x0F6C
;;          #x0F6D
(defformat #x0F6E MOVD (Pd Ed))     ;; MMX
(defformat #x0F6F MOVQ (Pq Qq))     ;; MMX

(defformat #x660F60 PUNPCKLBW (Vdq Wdq))     ;; SSE
(defformat #x660F61 PUNPCKLWD (Vdq Wdq))     ;; SSE
(defformat #x660F62 PUNPCKLDQ (Vdq Wdq))     ;; SSE
(defformat #x660F63 PCKSSWB (Vdq Wdq))     ;; SSE
(defformat #x660F64 PCMPGTB (Vdq Wdq))     ;; SSE
(defformat #x660F65 PCMPGTW (Vdq Wdq))     ;; SSE
(defformat #x660F66 PCMPGTD (Vdq Wdq))     ;; SSE
(defformat #x660F67 PACKUSWB (Vdq Wdq))     ;; SSE
(defformat #x660F68 PUNPCKHBW (Vdq Wdq))     ;; SSE
(defformat #x660F69 PUNPCKHWD (Vdq Wdq))     ;; SSE
(defformat #x660F6A PUNPCKHDQ (Vdq Wdq))     ;; SSE
(defformat #x660F6B PACKSSDW (Vdq Wdq))     ;; SSE
(defformat #x660F6C PUNPCKLQDQ (Vdq Wdq))     ;; SSE
(defformat #x660F6D PUNPCKHQDQ (Vdq Wdq))     ;; SSE
(defformat #x660F6E MOVD (Vdq Ed))      ;; SSE
(defformat #x660F6F MOVDQA (Vdq Wdq))     ;; SSE

(defformat #xF30F6F MOVDQU (Vdq Wdq))     ;; SSE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x0F70
;;
(defformat #x0F70 PSHUFW (Pq Qq Ib)) ;; MMX
;; #x0F71x  Grp 12  PSRLW PSRAW PSLLW
;; #x0F72x  Grp 13  PSRLD PSRAD PSLLD
;; #x0F73x  Grp 14  PSRLQ PSRLDQ PSLLQ PSLLDQ
(defformat #x0F74 PCMPEQB (Pq Qq))     ;; MMX
(defformat #x0F75 PCMPEQW (Pq Qq))     ;; MMX
(defformat #x0F76 PCMPEQD (Pq Qq))     ;; MMX
(defformat #x0F77 EMMS) ;; MMX
(defformat #x0F78 VMREAD  (Ed Gd))     ;; VMX
(defformat #x0F79 VMWRITE (Gd Ed))     ;; VMX
;;          #x0F7A
;;          #x0F7B
;;          #x0F7C
;;          #x0F7D
(defformat #x0F7E MOVD (Pd Pd))     ;; MMX
(defformat #x0F7F MOVQ (Qq Pq))     ;; MMX

(defformat #x660F70 PSHUFD (Vdq Wdq Ib))
(defformat #x660F71 nil :opext-leader)
(defformat #x660F72 nil :opext-leader)
(defformat #x660F73 nil :opext-leader)
(defformat #x660F74 PCMPEQB (Vdq Wdq))
(defformat #x660F75 PCMPEQW (Vdq Wdq))
(defformat #x660F76 PCMPEQD (Vdq Wdq))
;;          #x660F77
;;          #x660F78
;;          #x660F79
;;          #x660F7A
;;          #x660F7B
(defformat #x660F7C HADDPD (Vpd Wpd))
(defformat #x660F7D HSUBPD (Vpd Wpd))
(defformat #x660F7E MOVD (Ed  Vdq))
(defformat #x660F7F MOVDQA (Wdq Vdq))

(defformat #xF20F70 PSHUFHW (Vdq Wdq Ib))
(defformat #xF20F7C HADDPS (Vps Wps))
(defformat #xF20F7D HSUBPS (Vps Wps))

(defformat #xF30F70 PSHUFLW (Vdq Wdq Ib))
(defformat #xF30F7E MOVQ (Vq  Wq))
(defformat #xF30F7F MOVDQU (Wdq Vdq))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x0F80
;;
(defformat #x0F80 JO  (Jv)    :aliases (Jcc))
(defformat #x0F81 JNO (Jv))
(defformat #x0F82 JB  (Jv)    :aliases (JB JC JNAE))
(defformat #x0F83 JAE (Jv)    :aliases (JNB JNC))
(defformat #x0F84 JE  (Jv)    :aliases (JZ JE))
(defformat #x0F85 JNE (Jv)    :aliases (JNZ JNE))
(defformat #x0F86 JBE (Jv)    :aliases (JNA))
(defformat #x0F87 JA  (Jv)    :aliases (JNBE))
(defformat #x0F88 JS  (Jv))
(defformat #x0F89 JNS (Jv))
(defformat #x0F8A JPE (Jv)    :aliases (JP))
(defformat #x0F8B JPO (Jv)    :aliases (JNP))
(defformat #x0F8C JL  (Jv)    :aliases (JNGE))
(defformat #x0F8D JGE (Jv)    :aliases (JNL))
(defformat #x0F8E JLE (Jv)    :aliases (JNG))
(defformat #x0F8F JG  (Jv)    :aliases (JNLE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x0FA2
;;
(defformat #x0FA2 CPUID)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x0FB0
;;
(defformat #x0FB6 MOVZX (Gv Eb))
(defformat #x0FB7 MOVZX (Gv Ew))
(defformat #x0FBE MOVSX (Gv Eb))
(defformat #x0FBF MOVSX (Gv Ew))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x0FC0
;;
(defformat #x0FC0 XADD (Eb Gb))
(defformat #x0FC1 XADD (Ev Gv))
(defformat #x0FC2 CMPSS (Vss Wss Ib))
(defformat #x0FC3 MOVNTI (Md Gd))
(defformat #x0FC4 PINSRW (Pq Ew Ib))
(defformat #x0FC5 PEXTRW (Gd Nq Ib))
(defformat #x0FC6 SHUFPS (Pq Ew Ib))
;; #x0FC7 Grp 9

(defformat #xF30FC2 CMPPS (Vps Wps Ib))
(defformat #x660FC2 CMPPD (Vpd Wpd Ib))
(defformat #xF20FC2 CMPSD (Vsd Wsd Ib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x0FD0
;;
(defformat #x660FD0 ADDSUBPD (Vpd Wpd))   ;; SSE3
(defformat #x660FD1 PSRLW (Vdq Wdq))   ;; SSE2
(defformat #x660FD2 PSRLD (Vdq Wdq))   ;; SSE2
(defformat #x660FD3 PSRLQ (Vdq Wdq))   ;; SSE2
(defformat #x660FD4 PADDQ (Vdq Wdq))   ;; SSE2
(defformat #x660FD5 PMULLW (Vdq Wdq))   ;; SSE2
(defformat #x660FD6 MOVQ (Wq  Vq))
;;(defformat #x660FD7 PMOVMKSB (Gd  Udq))
(defformat #x660FD8 PSUBUSB (Vdq Wdq))
(defformat #x660FD9 PSUBUSW (Vdq Wdq))
(defformat #x660FDA PMINUB (Vdq Wdq))
(defformat #x660FDB PAND (Vdq Wdq))
(defformat #x660FDC PADDSUB (Vdq Wdq))
(defformat #x660FDD PADDUBW (Vdq Wdq))
(defformat #x660FDE PMAXUB (Vdq Wdq))
(defformat #x660FDF PANDN (Vdq Wdq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x0FE0
;;
(defformat #x660FE0 PAVGB (Vdq Wdq))
(defformat #x660FE1 PSRAW (Vdq Wdq))
(defformat #x660FE2 PSRAD (Vdq Wdq))
(defformat #x660FE3 PAVGW (Vdq Wdq))
(defformat #x660FE4 PMULHUW (Vdq Wdq))
(defformat #x660FE5 PMULHW (Vdq Wdq))
(defformat #x660FE6 CVTTPD2DQ (Vdq Wdq))
;;(defformat #x660FE7 MOVNTDQ (Mdq Wdq))
(defformat #x660FE8 PSUBSB (Vdq Wdq))
(defformat #x660FE9 PSUBSW (Vdq Wdq))
(defformat #x660FEA PMINSW (Vdq Wdq))
(defformat #x660FEB POR (Vdq Wdq))
(defformat #x660FEC PADDSB (Vdq Wdq))
(defformat #x660FED PADDSW (Vdq Wdq))
(defformat #x660FEE PMAXSW (Vdq Wdq))
(defformat #x660FEF PXOR (Vdq Wdq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; #x0FF0
;;
;; #x660FF0
(defformat #x660FF1 PSLLW (Vdq Wdq))
(defformat #x660FF2 PSLLD (Vdq Wdq))
(defformat #x660FF3 PSLLQ (Vdq Wdq))
(defformat #x660FF4 PMULUDQ (Vdq Wdq))
(defformat #x660FF5 PMADDWD (Vdq Wdq))
(defformat #x660FF6 PSADBW (Vdq Wdq))
;;(defformat #x660FF7 MASKMOVDQU (Mdq Wdq))
(defformat #x660FF8 PSUBB (Vdq Wdq))
(defformat #x660FF9 PSUBW (Vdq Wdq))
(defformat #x660FFA PSUBD (Vdq Wdq))
(defformat #x660FFB PSUBQ (Vdq Wdq))
(defformat #x660FFC PADDB (Vdq Wdq))
(defformat #x660FFD PADDW (Vdq Wdq))
(defformat #x660FFE PADD (Vdq Wdq))
;; #x660FFF


;; Three-byte Opcode (First Two Bytes are 0F 38)

;; 0F3800
(defformat #x660F3800 PSHUFB (Vdq Wdq))
(defformat #x660F3801 PHADDW (Vdq Wdq))
(defformat #x660F3802 PHADDD (Vdq Wdq))
(defformat #x660F3803 PHADDSW (Vdq Wdq))
(defformat #x660F3804 PMADDSUBSW (Vdq Wdq))
(defformat #x660F3805 PHSUBW (Vdq Wdq))
(defformat #x660F3806 PHSUBD (Vdq Wdq))
(defformat #x660F3807 PHSUBSW (Vdq Wdq))
(defformat #x660F3808 PSIGNB (Vdq Wdq))
(defformat #x660F3809 PSIGNW (Vdq Wdq))
(defformat #x660F380A PSIGND (Vdq Wdq))
(defformat #x660F380B PMULHRSW (Vdq Wdq))

;; 0F3810
(defformat #x660F381C PABSB (Vdq Wdq))
(defformat #x660F381D PABSW (Vdq Wdq))
(defformat #x660F381E PABSD (Vdq Wdq))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Group 1
;;
(defformat #x80 0 ADD (Eb Ib))
(defformat #x80 1 OR  (Eb Ib))
(defformat #x80 2 ADC (Eb Ib))
(defformat #x80 3 SBB (Eb Ib))
(defformat #x80 4 AND (Eb Ib))
(defformat #x80 5 SUB (Eb Ib))
(defformat #x80 6 XOR (Eb Ib))
(defformat #x80 7 CMP (Eb Ib))

(defformat #x81 0 ADD (Ev Iz))
(defformat #x81 1 OR  (Ev Iz))
(defformat #x81 2 ADC (Ev Iz))
(defformat #x81 3 SBB (Ev Iz))
(defformat #x81 4 AND (Ev Iz))
(defformat #x81 5 SUB (Ev Iz))
(defformat #x81 6 XOR (Ev Iz))
(defformat #x81 7 CMP (Ev Iz))

(defformat #x83 0 ADD (Ev Ib))
(defformat #x83 1 OR  (Ev Ib))
(defformat #x83 2 ADC (Ev Ib))
(defformat #x83 3 SBB (Ev Ib))
(defformat #x83 4 AND (Ev Ib))
(defformat #x83 5 SUB (Ev Ib))
(defformat #x83 6 XOR (Ev Ib))
(defformat #x83 7 CMP (Ev Ib))

(defformat #xC0 0 ROL (Eb Ib))
(defformat #xC0 1 ROR (Eb Ib))
(defformat #xC0 2 RCL (Eb Ib))
(defformat #xC0 3 RCR (Eb Ib))
(defformat #xC0 4 SHL (Eb Ib)) (defformat #xC0 4 SAL (Eb Ib))
(defformat #xC0 5 SHR (Eb Ib))
;; 6
(defformat #xC0 7 SAR (Eb Ib))

(defformat #xC1 0 ROL (Ev Ib))
(defformat #xC1 1 ROR (Ev Ib))
(defformat #xC1 2 RCL (Ev Ib))
(defformat #xC1 3 RCR (Ev Ib))
(defformat #xC1 4 SHL (Ev Ib)) (defformat #xC1 4 SAL (Ev Ib))
(defformat #xC1 5 SHR (Ev Ib))
;; 6
(defformat #xC1 7 SAR (Ev Ib))

(defformat #xC6 0 MOV (Eb Ib))
(defformat #xC7 0 MOV (Ev Iz))

(defformat #xD0 0 ROL (Eb 1))
(defformat #xD0 1 ROR (Eb 1))
(defformat #xD0 2 RCL (Eb 1))
(defformat #xD0 3 RCR (Eb 1))
(defformat #xD0 4 SHL (Eb 1)) (defformat #xD0 4 SAL (Eb 1))
(defformat #xD0 5 SHR (Eb 1))
(defformat #xD0 6 SAR (Eb 1))

(defformat #xD1 0 ROL (Ev 1))
(defformat #xD1 1 ROR (Ev 1))
(defformat #xD1 2 RCL (Ev 1))
(defformat #xD1 3 RCR (Ev 1))
(defformat #xD1 4 SHL (Ev 1)) (defformat #xD1 4 SAL (Ev 1))
(defformat #xD1 5 SHR (Ev 1))
;; 6
(defformat #xD1 7 SAR (Ev 1))

(defformat #xD2 0 ROL (Eb CL))
(defformat #xD2 1 ROR (Eb CL))
(defformat #xD2 2 RCL (Eb CL))
(defformat #xD2 3 RCR (Eb CL))
(defformat #xD2 4 SHL (Eb CL)) (defformat #xD2 4 SAL (Eb CL))
(defformat #xD2 5 SHR (Eb CL))
;; 6
(defformat #xD2 7 SAR (Eb CL))

(defformat #xD3 0 ROL (Ev CL))
(defformat #xD3 1 ROR (Ev CL))
(defformat #xD3 2 RCL (Ev CL))
(defformat #xD3 3 RCR (Ev CL))
(defformat #xD3 4 SHL (Ev CL)) (defformat #xD3 4 SAL (Ev CL))
(defformat #xD3 5 SHR (Ev CL))
;; 6
(defformat #xD3 7 SAR (Ev CL))

;;  Group 3 - F6 F7
(defformat #xF6 0 TEST (Eb Ib))
(defformat #xF6 2 NOT (Eb))
(defformat #xF6 3 NEG (Eb))
(defformat #xF6 4 MUL (Eb))
(defformat #xF6 5 IMUL (Eb))
(defformat #xF6 6 DIV (Eb))
(defformat #xF6 7 IDIV (Eb))

(defformat #xF7 0 TEST (Ev Iz))
(defformat #xF7 2 NOT (Ev))
(defformat #xF7 3 NEG (Ev))
(defformat #xF7 4 MUL (Ev))      ;; vDX:eAX = eAX * Ev
(defformat #xF7 5 IMUL (Ev))      ;; vDX:eAX = eAX * Ev
(defformat #xF7 6 DIV (Ev))       ;; eAX vDX = vDX:eAX ; Ev
(defformat #xF7 7 IDIV (Ev))      ;; eAX vDX = vDX:eAX ; Ev

;; Group 4
(defformat #xFE 0 INC (Eb))
(defformat #xFE 1 DEC (Eb))

;; Group 5 #xFF
(defformat #xFF 0 INC (Ev))
(defformat #xFF 1 DEC (Ev))
(defformat #xFF 2 CALL (Ev64))
;;(defformat #xFF 3 CALLF (Ev))
(defformat #xFF 4 JMP (Ev64))
;;(defformat #xFF 5 JMPF (Ev))
(defformat #xFF 6 PUSH (Ev64))


;; Group 12
;;              #x660F71 0
;;              #x660F71 1
(defformat #x660F71 2 PSRLW (Udq Ib))
;;              #x660F71 3
(defformat #x660F71 4 PSRAW (Udq Ib))
;;              #x660F71 5
(defformat #x660F71 6 PSLLW (Udq Ib))
;;              #x660F71 7

;; Group 13
;;              #x660F72 0
;;              #x660F72 1
(defformat #x660F72 2 PSRLD (Udq Ib))
;;              #x660F72 3
(defformat #x660F72 4 PSRAD (Udq Ib))
;;              #x660F72 5
(defformat #x660F72 6 PSLLD (Udq Ib))
;;              #x660F72 7

;; Group 14
;;              #x660F73 0
;;              #x660F73 1
(defformat #x660F73 2 PSRLQ (Udq Ib))
(defformat #x660F73 3 PSRLDQ (Udq Ib))
;;              #x660F73 4
;;              #x660F73 5
(defformat #x660F73 6 PSLLQ (Udq Ib))
(defformat #x660F73 7 PSLLDQ (Udq Ib))
