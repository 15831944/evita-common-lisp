#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86 - Machine
// arch/x86/genesis/x86_ke_mach.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/kernel/x86_ke_mach.cpp#6 $
//
#include "./x86_ke_layout.h"

#include "../x86_arch.h"

#include "../../../kernel/ke_frame.h"
#include "../../../kernel/ke_thread.h"

namespace
{

enum Constant
{
    Tag_Fixnum              = Val_::Tag_Fixnum,
    Tag_Fixnum1             = Val_::Tag_Fixnum1,
    Tag_Function            = Val_::Tag_Function,
    FrameType_FromForeign   = Kernel::Frame::Type_FromForeign,
}; // Constant

typedef Kernel::Thread Context;

} // namespace

namespace X86
{

const uint
X86Mach::k_rgnGcMapReg2Idx[8] =
{
    1,  // 0 eax
    2,  // 1 ecx
    3,  // 2 edx
    4,  // 3 ebx
    0,  // 4 esp
    0,  // 5 ebp
    5,  // 6 esi
    6,  // 7 edi
}; // k_rgnGcMapReg2Idx


const uint
X86Mach::k_rgnGcMapIdx2Reg[6+1] =
{
    0,  // stab
    0,  // 0 eax
    1,  // 1 ecx
    2,  // 2 edx
    4,  // 3 ebx
    6,  // 6 esi
    7,  // 7 edi
}; // k_rgnGcMapReg2Idx

//////////////////////////////////////////////////////////////////////
//
// All Registers in x86
//
const uint k_regRegAll[8+8] =
    { Gpr_EAX, Gpr_ECX, Gpr_EDX, Gpr_EBX,
      Gpr_ESP, Gpr_EBP, Gpr_ESI, Gpr_EDI, 

      Fpr_XMM0, Fpr_XMM1, Fpr_XMM2, Fpr_XMM3, 
      Fpr_XMM4, Fpr_XMM5, Fpr_XMM6, Fpr_XMM7 };
            CASSERT(lengthof(k_regRegAll) == 8 + 8);

// GC Map Builder uses All GPR Set information.
const uint k_regGprAll[] =
    { Gpr_EAX, Gpr_ECX, Gpr_EDX, Gpr_EBX,
      Gpr_ESP, Gpr_EBP, Gpr_ESI, Gpr_EDI }; 
            CASSERT(lengthof(k_regGprAll) == 8);


// BUGBUG: Who does use FPR All RegSet?
const uint k_regFprAll[] =
    { Fpr_XMM0, Fpr_XMM1, Fpr_XMM2, Fpr_XMM3, 
      Fpr_XMM4, Fpr_XMM5, Fpr_XMM6, Fpr_XMM7 };
            CASSERT(lengthof(k_regFprAll) == 8);

//////////////////////////////////////////////////////////////////////
//
// Standard Calling Convention
//
//  $r0 EAX     first argument
//  $r1 EDX     second argument
//  $r2 EBX     third argument
//  $r3 ESI     fourth argument
//  $r4 EDI     fifth argument
//  $rn         number of arguments (arity).
//
//  Sixth argument and more are passed by thread register (mv_value).
//
const uint k_regGprArg[5] = { $r0, $r1, $r2, $r3, $r4 };
const uint k_regFprArg[4] = { $f0, $f1, $f2, $f3 };

// GprAlloc + FprAll - FprArgs
// FIXME 2007-03-11: Until we support callee save register in compiler,
// all registers are volatile at call site.
const uint k_regVolatile[] =
    { $r0, $r1, $r2, $r3, $r4, $rn,
      $f0, $f1, $f2, $f3, $f4, $f5, $f6, $f7 };


const uint k_regGprSave[] = { $rn, $r0, $r1, $r2, $r3, $r4 };
const uint k_regFprSave[] = { $f0, $f1, $f2, $f3, $f4, $f5, $f6, $f7 };


DefineRegSet(RegAll); DefineRegSet(GprAll); DefineRegSet(FprAll);
DefineRegSet(GprArg); DefineRegSet(FprArg);
DefineRegSet(Volatile);

DefineRegSet(GprSave); DefineRegSet(FprSave);


const Mach X86Mach::ISA = 
{
    Mach::Isa_CICS,
    $sp, &k_oRegAll,   &k_oGprAll, &k_oFprAll,
    $rn, &k_oVolatile, &k_oGprArg, &k_oFprArg, 
    &k_oGprSave, &k_oFprSave,
}; // X86Mach::k_oMachISA


} // X86


namespace Kernel
{

//////////////////////////////////////////////////////////////////////
//
//  CallLisp
//      ecx = Context
//      Context->m_n           = number of arguments
//      Context->m_fn          = callee (function)
//      Context->mv_value[0]    = arg_0
//      Context->mv_value[1]    = arg_1
//      ...
//      Context->mv_value[n-1]  = arg_n-1
//
Val __declspec(naked) __fastcall
CallLisp(Thread*)
{
    __asm
    {
        // Allocate save area
        sub     esp, 24     // 24 = caller save(16) + from_foreign(8)

        // Save caller save registers for C
        mov     [esp+8],  ebx
        mov     [esp+12], ebp
        mov     [esp+16], esi
        mov     [esp+20], edi

        // ebp <- pContext
        mov     ebp, ecx

        // Make FromForeign Frame
        mov     eax, [ebp] Context.m_fp
        mov     [ebp] Context.m_fp, esp
        mov     [esp+0], eax
        mov     dword ptr [esp+4], FrameType_FromForeign

        // Set callee entry point
        mov     eax, [ebp] Context.m_fn
        add     eax, SIZE FunObj - Tag_Function
        mov     [esp-4], eax

        // Set arguments
        mov     ecx, [ebp] Context.m_n
        mov     eax, [ebp] Context.mv_value[0*4]
        mov     edx, [ebp] Context.mv_value[1*4]
        mov     ebx, [ebp] Context.mv_value[2*4]
        mov     esi, [ebp] Context.mv_value[3*4]
        mov     edi, [ebp] Context.mv_value[4*4]

        // Call lisp function
        call    dword ptr [esp-4]

        // Save primary value
        mov     [ebp] Context.mv_value[0*4], eax

        // Set number of values
        mov     eax, Tag_Fixnum1
        cmovnc  ecx, eax
        mov     [ebp] Context.m_n, ecx

        // Set values to thread
        mov     [ebp] Context.mv_value[1*4], edx
        mov     [ebp] Context.mv_value[2*4], ebx
        mov     [ebp] Context.mv_value[3*4], esi
        mov     [ebp] Context.mv_value[4*4], edi

        // Restore m_fp
        mov     eax, [esp+0]
        mov     [ebp] Context.m_fp, eax

        // Restore primary value
        mov     eax, [ebp] Context.mv_value[0*4]

        // Restore caller save registers
        mov     ebx, [esp+8]
        mov     ebp, [esp+12]
        mov     esi, [esp+16]
        mov     edi, [esp+20]

        add     esp, 24
        ret
    } // __asm
} // CallLisp

__declspec(noreturn) void __declspec(naked) __fastcall
GoToLisp(Thread*)
{
    __asm
    {
        // ebp <- pContext
        mov     ebp, ecx

        // Set callee entry point
        mov     eax, [ebp] Context.m_fn
        add     eax, SIZE FunObj - Tag_Function
        mov     [esp-4], eax

        // Set arguments
        mov     ecx, [ebp] Context.m_n
        mov     eax, [ebp] Context.mv_value[0*4]
        mov     edx, [ebp] Context.mv_value[1*4]
        mov     ebx, [ebp] Context.mv_value[2*4]
        mov     esi, [ebp] Context.mv_value[3*4]
        mov     edi, [ebp] Context.mv_value[4*4]

        // Call lisp function
        jmp    dword ptr [esp-4]
    } // __asm
} // GoToLisp

} // Kernel
