#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86 - Machine
// arch/x86/genesis/x86_ke_mach.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/kernel/x64_ke_mach.cpp#7 $
//
#include "./x64_ke_mach.h"

#include "../x64_arch.h"

namespace X64
{
const uint
X64Mach::k_rgnGcMapReg2Idx[16] =
{
    1,  // 0 eax
    2,  // 1 ecx
    3,  // 2 edx
    4,  // 3 ebx
    0,  // 4 esp
    0,  // 5 ebp
    5,  // 6 esi
    6,  // 7 edi
    7,  // 8 r8
    8,  // 9 r9
    9,  // 10 r10
   10,  // 11 r11
   11,  // 12 r12
   12,  // 13 r13
   13,  // 14 r14
   14,  // 15 r15
}; // k_rgnGcMapReg2Idx


const uint
X64Mach::k_rgnGcMapIdx2Reg[14+1] =
{
    0,  // stab
    0,  // 0 eax
    1,  // 1 ecx
    2,  // 2 edx
    3,  // 3 ebx
    6,  // 6 esi
    7,  // 7 edi
    8,  // 8 r8
    9,  // 9 r9
   10,  // 10 r10
   11,  // 11 r11
   12,  // 12 r12
   13,  // 13 r13
   14,  // 14 r14
   15,  // 15 r15
}; // k_rgnGcMapReg2Idx


//////////////////////////////////////////////////////////////////////
//
// All Registers in x64
//
const uint k_regRegAll[] =
    { Gpr_RAX, Gpr_RCX, Gpr_RDX, Gpr_RBX,
      Gpr_RSP, Gpr_RBP, Gpr_RSI, Gpr_RDI,
      Gpr_R8,  Gpr_R9,  Gpr_R10, Gpr_R11,
      Gpr_R12, Gpr_R13, Gpr_R14, Gpr_R15,

      Fpr_XMM0,  Fpr_XMM1,  Fpr_XMM2,  Fpr_XMM3,
      Fpr_XMM4,  Fpr_XMM5,  Fpr_XMM6,  Fpr_XMM7,
      Fpr_XMM8,  Fpr_XMM9,  Fpr_XMM10, Fpr_XMM11,
      Fpr_XMM12, Fpr_XMM13, Fpr_XMM14, Fpr_XMM15 };
            CASSERT(lengthof(k_regRegAll) == 16 + 16);

const uint k_regGprAll[] =
    { Gpr_RAX, Gpr_RCX, Gpr_RDX, Gpr_RBX,
      Gpr_RSP, Gpr_RBP, Gpr_RSI, Gpr_RDI,
      Gpr_R8,  Gpr_R9,  Gpr_R10, Gpr_R11,
      Gpr_R12, Gpr_R13, Gpr_R14, Gpr_R15 };
            CASSERT(lengthof(k_regGprAll) == 16);

// BUGBUG: Who does use FPR All RegSet?
const uint k_regFprAll[] =
    { Fpr_XMM0,  Fpr_XMM1,  Fpr_XMM2,  Fpr_XMM3,
      Fpr_XMM4,  Fpr_XMM5,  Fpr_XMM6,  Fpr_XMM7,
      Fpr_XMM8,  Fpr_XMM9,  Fpr_XMM10, Fpr_XMM11,
      Fpr_XMM12, Fpr_XMM13, Fpr_XMM14, Fpr_XMM15 };
            CASSERT(lengthof(k_regFprAll) == 16);


//////////////////////////////////////////////////////////////////////
//
// Calling Convention
//
const uint k_regGprArg[] = // 10 registers
    { $r0, $r1, $r2, $r3, $r4, $r5, $r6, $r7, $r8, $r9, };

const uint k_regFprArg[] = // 4 registers
    { $f0, $f1, $f2, $f3 };

// GprAlloc + FprAll - FprArgs
// FIXME 2007-03-11: Until we support callee save register in compiler,
// all registers are volatile at call site.
const uint k_regVolatile[] = 
  { $r0, $r1, $r2, $r3, $r4, $r5, $r6, $r7, $r8, $r9,
    $rn, $rfn,
    $f0, $f1, $f2,  $f3,  $f4,  $f5,  $f6,  $f7,
    $f8, $f9, $f10, $f11, $f12, $f13, $f14, $f15 };


const uint k_regGprSave[] =
    { Gpr_EAX, Gpr_ECX, Gpr_EDX, Gpr_EBX,
                        Gpr_ESI, Gpr_EDI,
      Gpr_R8,  Gpr_R9,  Gpr_R10, Gpr_R11,
      Gpr_R12, Gpr_R13, Gpr_R14, Gpr_R15 };

const uint k_regFprSave[] =
  { $f0, $f1, $f2,  $f3,  $f4,  $f5,  $f6,  $f7,
    $f8, $f9, $f10, $f11, $f12, $f13, $f14, $f15 };


DefineRegSet(RegAll); DefineRegSet(GprAll); DefineRegSet(FprAll);

DefineRegSet(GprArg); DefineRegSet(FprArg);
DefineRegSet(Volatile);

DefineRegSet(GprSave); DefineRegSet(FprSave);


const Mach X64Mach::ISA =
{
    Mach::Isa_CICS,
    $sp, &k_oRegAll,   &k_oGprAll, &k_oFprAll,
    $rn, &k_oVolatile, &k_oGprArg, &k_oFprArg,
    &k_oGprSave, &k_oFprSave,
}; // X86Mach::k_oMachISA

} // Kernel
