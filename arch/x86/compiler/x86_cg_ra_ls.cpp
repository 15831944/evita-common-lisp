#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x86 - Linear Scan Register Allocator
// arch/x86/compiler/x86_cg_ra_ls.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/compiler/x86_cg_ra_ls.cpp#6 $
//
#include "./x86_cg_defs.h"
#include "./x86_cg_instruction.h"
#include "./x86_cg_target.h"

#include "../../x86x64/compiler/x86x64_cg_ra_ls.h"

namespace Compiler
{

using namespace X86;
using namespace LinearScanRA;

//////////////////////////////////////////////////////////////////////
//
// Register Allocation
//
const uint k_regGprAlloc[] = { $r0, $r1, $r2, $r3, $r4, $rn };

// All FPRs are allocable.
const uint k_regFprAlloc[] = { $f0, $f1, $f2, $f3, $f4, $f5, $f6, $f7 };

// Set of non-byte acessbile allocable registers
const uint k_regNoByteAlloc[] = { Gpr_ESI, Gpr_EDI };

DefineRegSet(GprAlloc);
DefineRegSet(FprAlloc);
DefineRegSet(NoByteAlloc);


//////////////////////////////////////////////////////////////////////
//
// Linear Scan Register Allocator
//
class X86RegisterAllocator :
    public X86X64RegisterAllocator<X86Target, 8 + 8>
{
    public: X86RegisterAllocator() :
        X86X64RegisterAllocator<X86Target, 8 + 8>(
            L"X86-RA-LS",
            &k_oGprAlloc, &k_oFprAlloc ) {}

    public: const RegSet*
        GetNonAllocable(const LiveInterval* p) const
        {
            if (p->m_nFlags & Flag_ByteReg)
            {
                return &k_oNoByteAlloc;
            }
            else
            {
                return &k_oEmptyRegSet;
            }
        } // GetNonAllocable
}; // X86RegisterAllocator


// x86_allocate_register
void x86_allocate_register()
{
    X86RegisterAllocator oPass;
    oPass.Run();
} // x86_allocate_register

} // Compiler
