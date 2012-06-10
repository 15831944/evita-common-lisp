//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - X64 Machine Dependent Structures
// arch/kernel/X64_ke_mach.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/kernel/x64_ke_mach.h#4 $
//
#if !defined(INCLUDE_arch_x64_kernel_X64_ke_mach_h)
#define INCLUDE_arch_x64_kernel_X64_ke_mach_h

#include "./x64_ke_arch.h"
#include "../../../kernel/ke_mach.h"

namespace X64
{

//////////////////////////////////////////////////////////////////////
//
// X64Mach
//  Machine description of X64.
//
class X64Mach : public Arch64
{
    // We can have 32 thread service call.
    public: static size_t const Thread_Lead_Extra = 256;

    enum Limits
    {
        Call_Arguments_Limit    = 128,
        Lambda_Parameters_Limit = Call_Arguments_Limit,
        Multiple_Values_Limit   = Call_Arguments_Limit,

        TLV_Limit    = 1024,
    }; // Limits

    public: static const char16* GetName() { return L"X64"; }

    public: static const uint k_rgnGcMapReg2Idx[16];
    public: static const uint k_rgnGcMapIdx2Reg[14+1];

    public: static const Mach ISA;
}; // X64Mach

} // X64

#endif //!defined(INCLUDE_arch_x64_kernel_X64_ke_mach_h)
