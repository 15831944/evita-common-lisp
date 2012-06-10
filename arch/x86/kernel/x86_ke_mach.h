//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86 Machine Dependent Structures
// arch/kernel/x86_ke_mach.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/kernel/x86_ke_mach.h#4 $
//
#if !defined(INCLUDE_arch_x86_kernel_x86_ke_mach_h)
#define INCLUDE_arch_x86_kernel_x86_ke_mach_h

#include "./x86_ke_arch.h"
#include "../../../kernel/ke_mach.h"

namespace X86
{

//////////////////////////////////////////////////////////////////////
//
// Mach
//  Machine description of x86.
//
class X86Mach : public Arch32
{
    public: static size_t const Thread_Lead_Extra   = 4 * 32;

    enum Limits
    {
        Call_Arguments_Limit    = 128,
        Lambda_Parameters_Limit = Call_Arguments_Limit,
        Multiple_Values_Limit   = Call_Arguments_Limit,

        TLV_Limit    = 1024,
    }; // Limits

    public: static const char16* GetName() { return L"X86"; }

    public: static const uint k_rgnGcMapReg2Idx[8];
    public: static const uint k_rgnGcMapIdx2Reg[6+1];

    public: static const Mach ISA;
}; // X86Mach

} // X86

#endif //!defined(INCLUDE_arch_x86_kernel_x86_ke_mach_h)
