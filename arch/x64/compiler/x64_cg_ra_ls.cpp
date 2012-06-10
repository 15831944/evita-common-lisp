#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x64 - Linear Scan Register Allocator
// arch/x64/compiler/x64_cg_ra_ls.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/compiler/x64_cg_ra_ls.cpp#5 $
//
#include "./x64_cg_defs.h"
#include "./x64_cg_instruction.h"
#include "./x64_cg_target.h"

#include "../../x86x64/compiler/x86x64_cg_ra_ls.h"

namespace Compiler
{

using namespace X64;
using namespace LinearScanRA;

//////////////////////////////////////////////////////////////////////
//
// Register Allocation
//
// Note: We need to have at least one free register for restoring
// special variable binding.
//
const uint k_regGprAlloc[] = {
    $r0, $r1, $r2, $r3, $r4, $r5, $r6, $r7,
    $r8, $r9,
    $rn, $rfn
}; // k_regGprAlloc

// All FPRs are allocable.
const uint k_regFprAlloc[] = {
    $f0, $f1, $f2,  $f3,  $f4,  $f5,  $f6,  $f7,
    $f8, $f9, $f10, $f11, $f12, $f13, $f14, $f15
}; // k_regFprAlloc


DefineRegSet(GprAlloc);
DefineRegSet(FprAlloc);


//////////////////////////////////////////////////////////////////////
//
// Linear Scan Register Allocator
//
class X64RegisterAllocator : public X86X64RegisterAllocator<X64Target, 16 + 16>
{
    public: X64RegisterAllocator() :
        X86X64RegisterAllocator<X64Target, 16 + 16>(
            L"X64-RA-LS",
            &k_oGprAlloc, &k_oFprAlloc ) {}
}; // X64RegisterAllocator


// x64_allocate_register
void x64_allocate_register()
{
    X64RegisterAllocator oPass;
    oPass.Run();
} // x64_allocate_register

} // Compiler
