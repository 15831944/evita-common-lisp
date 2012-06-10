//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86x64 Machine Dependent Frame
// arch/kernel/x86x64_ke_interrupt.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/kernel/x86x64_ke_interrupt.h#2 $
//
#if !defined(INCLUDE_arch_x86x64_kernel_x86x64_ke_interrupt_h)
#define INCLUDE_arch_x86x64_kernel_x86x64_ke_interrupt_h

#include "./x86x64_ke_thread.h"

#include "../x86x64_arch.h"
#include "../x86x64_opcode.h"
#include "./x86x64_ke_gcmap.h"
#include "./x86x64_ke_layout.h"

#include "../mini/x86x64_mini_lisp.h"

namespace Kernel
{

class X86X64Interrupt
{
    private: X86X64Interrupt() {}

    //////////////////////////////////////////////////////////////////////
    //
    // set_callee
    //  callee is symbol or function, which can be called without arguments.
    //
    public: static FunObj::Annon set_callee(
        FunObj*         pFunObj,
        uint8*          pbCode,
        Val             callee )
    {
        uint ofs = static_cast<uint>(pbCode - pFunObj->GetCodeVec());

        if (functionp(callee))
        {
            pFunObj->PatchCallee(ofs, callee);
            return FunObj::Annon(FunObj::Annon::Type_LocalCallee, ofs);
        }
        else
        {
            Val fn = register_caller(callee, pFunObj->Encode());
            pFunObj->PatchCallee(ofs, fn);
            return FunObj::Annon(FunObj::Annon::Type_NamedCallee, ofs);
        }
    } // set_callee

    public: static Val make_detour_call(
        Val     fn,
        uint    curr_ofs,
        uint    safe_ofs,
        Val     callee );

    public: static Val make_detour_jump(
        Val     fn,
        uint    curr_ofs,
        uint    safe_ofs,
        uint32  gcdesc,
        Val     callee );

    public: static Val make_detour_ra(
        Val fn,
        UInt ip,
        Val callee );

    public: static Val make_detour_ra_aux(
        Val callee );

    public: static UInt  GetIP(CONTEXT*);
    public: static UInt* GetSP(CONTEXT*);
    public: static void  Push(CONTEXT*, UInt);
    public: static void  SetIP(CONTEXT*, uint8*);
}; // X86X64Interrupt

} // Kernel

#endif //!defined(INCLUDE_arch_x86x64_kernel_x86x64_ke_interrupt_h)
