;;;;
;;; evcl - kernel - CallLisp and DllLinkStab
;;; arch/x64/kernel/x64_ke.asm
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x64/kernel/x64_ke.asm#9 $
;;;
;
_text segment

Tag$Fixnum      equ 0
Tag$Fixnum1     equ 8
Tag$Function    equ 9

;; See arch/generic/gen_ke_arch64.h for definition of nil.
;       876543210
nil equ 3F0000082h



$rfn    equ r12
$rn     equ rcx
$rnil   equ r13
$rsp    equ rsp
$tcb    equ rbp

$r0     equ rax
$r1     equ rdx
$r2     equ rbx
$r3     equ rsi
$r4     equ rdi
$r5     equ r8
$r6     equ r9
$r7     equ r10
$r8     equ r11
$r9     equ r14

tcb equ (Thread PTR [rbp])

FrameType$FromForeign   equ 46724600h

;; Note: STRUCT Thread must be synchrnonized with C++ class Thread
;; defined in kernel/ke_thread.h.
Thread STRUCT
    m_classd        qword ?
    m_name          qword ?

    m_waiting       qword ?
    m_next_waiter   qword ?

    m_pConsArea     qword ?
    m_pRecordArea   qword ?
    m_pBinObjArea   qword ?
    m_pFunObjArea   qword ?

    m_pObStackArea  qword ?

    m_pNextThread   qword ?
    m_pPrevThread   qword ?

    m_fp            qword ?
    m_fn            qword ?
    m_n             qword ?
    mv_value        qword 128 dup (?)
Thread ENDS

FunObj STRUCT
    m_classd        qword ?
    m_cbFunction    qword ?
    m_cookie        qword ?
    m_name          qword ?
FunObj ENDS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Finally Frame
;;;
FinallyFrame STRUCT
    m_pOuter    qword ?
    m_eType     qword ?

    m_sp        qword ?
    m_fn        qword ?
    m_n         qword ?
    mv_arg      qword 1 ; variable length
FinallyFrame ENDS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tagbody Frame
;;;
TagbodyFrame STRUCT
    m_pOuter    qword ?
    m_eType     qword ?

    m_sp        qword ?
    m_fn        qword ?
    m_n         qword ?
    m_rgoGo     qword ?
TagbodyFrame ENDS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Xfer Frame
;;;
XferFrame STRUCT
    m_pOuter    qword ?
    m_eType     qword ?

    m_sp        qword ?
    m_fn        qword ?
    m_ip        qword ?
    m_name      qword ?
    m_n         qword ?
    mv_value    qword 3 dup (?)
XferFrame ENDS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;; CallLisp
;;;
;;; [1] Save nonvolatile registers, rbx, rsi, rdi, r12, r13, r14, r15, rbp
;;; [2] Make FromForeign frame
;;; [3] Compute callee entry point
;;; [4] Set arguments
;;; [5] Call lisp funciton
;;; [6] Set values
;;; [7] Pop FromForeign frame
;;; [8] Restore nonvolatile registers
;;;
;;;         +------------------+
;;;  rsp+0  | pFrame.m_fp      |
;;;         +------------------+
;;;  rsp+8  | pFrame.m_eType   |
;;;         +------------------+
;;;  rsp+16 |       pad        |
;;;         +------------------+
;;;  rsp+24 |       rbx        |
;;;         +------------------+
;;;  rsp+32 |       rsi        |
;;;         +------------------+
;;;  rsp+40 |       rdi        |
;;;         +------------------+
;;;  rsp+48 |       r12        |
;;;         +------------------+
;;;  rsp+56 |       r13        |
;;;         +------------------+
;;;  rsp+64 |       r14        |
;;;         +------------------+
;;;  rsp+72 |       r15        |
;;;         +------------------+
;;;  rsp+80 |       rbp        |
;;;         +------------------+
;;;  rsp+88 |    RA of caller  |    rsp+0 mod 16 = 8
;;;         +------------------+
;
PUBLIC CallLisp
CallLisp PROC FRAME
    ;; Allocate register save area(64) + Pad(8) + FromForeign(16)
    stackSize = (8 * 8) + 16 + 8   ; = 88
    sub rsp, stackSize
    .ALLOCSTACK stackSize

    ;; [1] Save nonvolatile registers
    mov [rsp+24], rbx
    mov [rsp+32], rsi
    mov [rsp+40], rdi
    mov [rsp+48], r12
    mov [rsp+56], r13
    mov [rsp+64], r14
    mov [rsp+72], r15
    mov [rsp+80], rbp

    .SAVEREG rbx, 24
    .SAVEREG rsi, 32
    .SAVEREG rdi, 40
    .SAVEREG r12, 48
    .SAVEREG r13, 56
    .SAVEREG r14, 64
    .SAVEREG r15, 72
    .SAVEREG rbp, 80
    .ENDPROLOG

    ;; rbp <- pThread
    mov $tcb, rcx

    ;; [2] Make FromForeign Frame
    mov rax, tcb.m_fp       ; rax <- tcb.m_fp
    mov tcb.m_fp, rsp
    mov [rsp+0], rax
    mov qword ptr [rsp+8], FrameType$FromForeign

    ;; [3] Compute callee entry point
    mov rax, tcb.m_fn
    add rax, SIZE(FunObj) - Tag$Function
    mov [rsp-8], rax

    ;; [4] Set arguments
    mov $rn,   tcb.m_n
    mov $r0,   tcb.mv_value[0*8]
    mov $r1,   tcb.mv_value[1*8]
    mov $r2,   tcb.mv_value[2*8]
    mov $r3,   tcb.mv_value[3*8]
    mov $r4,   tcb.mv_value[4*8]
    mov $r5,   tcb.mv_value[5*8]
    mov $r6,   tcb.mv_value[6*8]
    mov $r7,   tcb.mv_value[7*8]
    mov $r8,   tcb.mv_value[8*8]
    mov $r9,   tcb.mv_value[9*8]
    mov $rnil, nil

    ;; [5] Call lisp function
    call qword ptr [rsp-8]

    ;; [6] Save primary value
    mov tcb.mv_value[0*8], $r0

    ;; [6] Set number of values
    mov     $r0, Tag$Fixnum1
    cmovnc  $rn, rax
    mov     tcb.m_n, $rn

    ;; [6] Set values
    mov tcb.mv_value[1*8],  $r1
    mov tcb.mv_value[2*8],  $r2
    mov tcb.mv_value[3*8],  $r3
    mov tcb.mv_value[4*8],  $r4
    mov tcb.mv_value[5*8],  $r5
    mov tcb.mv_value[6*8],  $r6
    mov tcb.mv_value[7*8],  $r7
    mov tcb.mv_value[8*8],  $r8
    mov tcb.mv_value[9*8],  $r9

    ;; [7] Pop FromForeign frame
    mov rax, [rsp+0]
    mov tcb.m_fp, rax

    ;; Restore primary value
    mov rax, tcb.mv_value[0*8]

    ;; [8] Restore nonvolatile registers
    mov rbx, [rsp+24]
    mov rsi, [rsp+32]
    mov rdi, [rsp+40]
    mov r12, [rsp+48]
    mov r13, [rsp+56]
    mov r14, [rsp+64]
    mov r15, [rsp+72]
    mov rbp, [rsp+80]

    ;; Return to caller
    add rsp, stackSize
    ret
CallLisp ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DllLinkStab
;;;
;;; [1] Save argments into thread.mv_value
;;; [2] Compute address of Link Vector entry
;;; [3] Call C_resolver(thread, entry)
;;; [4] Restore arguments
;;; [5] Transfer control
;;;
;;;         +------------------+
;;;  rsp+0  |  home of arg[0]  | for DllResolve
;;;         +------------------+
;;;  rsp+8  |  home of arg[1]  | for DllResolve
;;;         +------------------+
;;;  rsp+16 |  home of arg[2]  | for DllResolve
;;;         +------------------+
;;;  rsp+24 |  home of arg[3]  | for DllResolve
;;;         +------------------+
;;;  rsp+32 |       rcx        |
;;;         +------------------+
;;;  rsp+40 |       rdx        |
;;;         +------------------+
;;;  rsp+48 |       r8         |
;;;         +------------------+
;;;  rsp+56 |       r9         |
;;;         +------------------+
;;;  rsp+64 |      callee      |    rsp-8
;;;         +------------------+
;;;  rsp+72 |    RA of caller  |    rsp+0 mod 16 = 8
;;;         +------------------+
;
EXTERN DllResolve : PROC
PUBLIC DllLinkStab
DllLinkStab PROC FRAME

    ;; [1] Save C/C++ arguments
    stackSize = (4*8) + (4*8) + 8  ; = 72
    sub rsp, stackSize
    .ALLOCSTACK stackSize

    mov [rsp+32], rcx   ; save argv[0]
    mov [rsp+40], rdx   ; save argv[1]
    mov [rsp+48], r8    ; save argv[2]
    mov [rsp+56], r9    ; save argv[3]

    .SAVEREG rcx, 32
    .SAVEREG rdx, 40
    .SAVEREG r8,  48
    .SAVEREG r9,  56
    .ENDPROLOG

    ;; [2] Compute address of pDllLinkEntry
    mov     rdx, [rsp+72]   ; rdx <- return address of caller
    movsxd  rax, dword ptr [rdx-4]  ; rax <- disp32 of CALL [RIP+disp32]
    add     rdx, rax

    ;; [3] Call DllResolve(pThread, pDllLinkEntry)
    mov rcx, $tcb
    call DllResolve
    mov [rsp+64], rax

    ;; [4] Restore C/C++ arguments
    mov rcx, [rsp+32]   ; restore argv[0]
    mov rdx, [rsp+40]   ; restore argv[1]
    mov r8,  [rsp+48]   ; restore argv[2]
    mov r9,  [rsp+56]   ; restore argv[3]

    add rsp, stackSize

    ;; [5] Transfer control
    jmp qword ptr [rsp-8]
DllLinkStab ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FinallyFrame_Transfer
;;;
;;;     rcx     pFrame
;;;     rdx     pThread
;;;
;;;         +------------------+
;;;  rsp+0  |  home of arg[0]  | for finally procedure
;;;         +------------------+
;;;  rsp+8  |  home of arg[1]  | for finally procedure
;;;         +------------------+
;;;  rsp+16 |  home of arg[2]  | for finally procedure
;;;         +------------------+
;;;  rsp+24 |  home of arg[3]  | for finally procedure
;;;         +------------------+
;;;  rsp+32 |       rbx        |
;;;         +------------------+
;;;  rsp+40 |       rsi        |
;;;         +------------------+
;;;  rsp+48 |       rdi        |
;;;         +------------------+
;;;  rsp+56 |       r12        |
;;;         +------------------+
;;;  rsp+64 |       r13        |
;;;         +------------------+
;;;  rsp+72 |       r14        |
;;;         +------------------+
;;;  rsp+80 |       r15        |
;;;         +------------------+
;;;  rsp+88 |       rbp        |
;;;         +------------------+
;;;  rsp+96 |       pad        |
;;;         +------------------+
;;;  rsp+104|       pad        |
;;;         +------------------+
;;;  rsp+112|    RA of caller  |    rsp+0 mod 16 = 8
;;;         +------------------+
;
PUBLIC FinallyFrame_Unwind
FinallyFrame_Unwind PROC FRAME
    stackSize = (8*4) + (8*8) + 8 ;= 104

    sub rsp, stackSize
    .ALLOCSTACK stackSize

    ;; [1] Save nonvolatile registers
    mov [rsp+32], rbx
        .SAVEREG rbx, 32
    mov [rsp+40], rsi
        .SAVEREG rsi, 40
    mov [rsp+48], rdi
        .SAVEREG rdi, 48
    mov [rsp+56], r12
        .SAVEREG rdi, 56
    mov [rsp+64], r13
        .SAVEREG rdi, 64
    mov [rsp+72], r14
        .SAVEREG rdi, 72
    mov [rsp+80], r15
        .SAVEREG rdi, 80
    mov [rsp+88], rbp
        .SAVEREG rdi, 88
    .ENDPROLOG

    pFinallyFrame equ (FinallyFrame PTR [r15])

    mov rbp, rdx    ; rbp <- pThread
    mov r15, rcx    ; r15 <- pFrame

    ;; [2] Compute target address
    mov $rfn, pFinallyFrame.m_fn
    add $rfn, SIZE(FunObj) - Tag$Function

    ;; [3] Copy arguments
    mov rcx, pFinallyFrame.m_n
    lea rdi, [tcb.mv_value+rcx]
    lea rsi, [pFinallyFrame.mv_arg+rcx]
    lea rbx, pFinallyFrame.mv_arg[11*8]

  copy_loop:
    cmp rsi, rbx
    jl copy_done

    sub rdi, 8
    sub esi, 8
    mov rax, [esi]
    mov [edi], eax
    jmp copy_loop

  copy_done:
    ;; [4] Load register arguments for finally procedure
    mov $r0,   pFinallyFrame.mv_arg[0*8]
    mov $r1,   pFinallyFrame.mv_arg[1*8]
    mov $r2,   pFinallyFrame.mv_arg[2*8]
    mov $r3,   pFinallyFrame.mv_arg[3*8]
    mov $r4,   pFinallyFrame.mv_arg[4*8]
    mov $r5,   pFinallyFrame.mv_arg[5*8]
    mov $r6,   pFinallyFrame.mv_arg[6*8]
    mov $r7,   pFinallyFrame.mv_arg[7*8]
    mov $r8,   pFinallyFrame.mv_arg[8*8]
    mov $r9,   pFinallyFrame.mv_arg[9*8]
    mov $rnil, nil

    ;; [5] Call finally procedure
    call $rfn

    ;; [8] Restore nonvolatile registers
    mov rbx, [rsp+32]
    mov rsi, [rsp+40]
    mov rdi, [rsp+48]
    mov r12, [rsp+56]
    mov r13, [rsp+64]
    mov r14, [rsp+72]
    mov r15, [rsp+80]
    mov rbp, [rsp+88]

    ;; Return to caller
    add rsp, stackSize
    ret
FinallyFrame_Unwind ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;; GoToLisp
;;;
;
PUBLIC GoToLisp
GoToLisp PROC
    ;; rbp <- pThread
    mov $tcb, rcx

    ;; [1] Compute callee entry point
    mov rax, tcb.m_fn
    add rax, SIZE(FunObj) - Tag$Function
    mov [rsp-8], rax

    ;; [2] Set arguments
    mov $rn,   tcb.m_n
    mov $r0,   tcb.mv_value[0*8]
    mov $r1,   tcb.mv_value[1*8]
    mov $r2,   tcb.mv_value[2*8]
    mov $r3,   tcb.mv_value[3*8]
    mov $r4,   tcb.mv_value[4*8]
    mov $r5,   tcb.mv_value[5*8]
    mov $r6,   tcb.mv_value[6*8]
    mov $r7,   tcb.mv_value[7*8]
    mov $r8,   tcb.mv_value[8*8]
    mov $r9,   tcb.mv_value[9*8]
    mov $rnil, nil

    ;; [3] Call lisp function
    jmp qword ptr [rsp-8]
GoToLisp ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TagbodyFrame_Transfer
;;;
;;;     rcx     pFrame
;;;     rdx     pThread
;;;     r8      pTag*
;;;
PUBLIC TagbodyFrame_Transfer
TagbodyFrame_Transfer PROC
    mov $tcb, rdx                       ; $tcb <- pThread
    mov rsp, (TagbodyFrame PTR [rcx]).m_sp

    ;; Compute target address
    mov rax, qword ptr [r8]
    shr rax, 3
    add rax, (TagbodyFrame PTR [rcx]).m_fn
    add rax, SIZE(FunObj) - Tag$Function
    mov $rnil, nil
    jmp rax
TagbodyFrame_Transfer ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XferFrame_Transfer
;;;
;;;     rcx     pFrame
;;;     rdx     pThread
;;;
PUBLIC XferFrame_Transfer
XferFrame_Transfer PROC
    mov $tcb, rdx   ; $tcb <- pThread

    pXferFrame equ (XferFrame PTR [rcx])

    mov rsp, pXferFrame.m_sp

    ;; Compute target address
    mov rax, pXferFrame.m_ip
    shr rax, 3
    add rax, pXferFrame.m_fn
    lea $rfn, [rax][SIZE(FunObj) - Tag$Function]

    ;; Load values
    mov $rn,   tcb.m_n
    mov $r0,   tcb.mv_value[0*8]
    mov $r1,   tcb.mv_value[1*8]
    mov $r2,   tcb.mv_value[2*8]
    mov $r3,   tcb.mv_value[3*8]
    mov $r4,   tcb.mv_value[4*8]
    mov $r5,   tcb.mv_value[5*8]
    mov $r6,   tcb.mv_value[6*8]
    mov $r7,   tcb.mv_value[7*8]
    mov $r8,   tcb.mv_value[8*8]
    mov $r9,   tcb.mv_value[9*8]
    mov $rnil, nil
    stc

    jmp $rfn
XferFrame_Transfer ENDP

_text ends
end
