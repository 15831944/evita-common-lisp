//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - Code Generator
// x86_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/compiler/x86_cg_defs.h#2 $
//
#if !defined(INCLUDE_compiler_cg_x86_defs_h)
#define INCLUDE_compiler_cg_x86_defs_h

#include "../../../compiler/cg/cg_defs.h"

#include "../x86_arch.h"
#include "../kernel/x86_ke_layout.h"
#include "../kernel/x86_ke_mach.h"

namespace Compiler
{

typedef X86::Function::Annon Annon;

// Note:
//  We define AnnonType here, since we have to have Type_Function.
//  Also, it is better to have Type_Anchor for debugging purpose.
//
enum AnnonType
{
    Annon_LispVal       = Annon::Type_LispVal,        // 0
    Annon_NamedCallee   = Annon::Type_NamedCallee,    // 1
    Annon_LocalCallee   = Annon::Type_LocalCallee,    // 2
    Annon_SymFun        = Annon::Type_SymFun,         // 3
    Annon_SymVal        = Annon::Type_SymVal,         // 4
    Annon_SymSetf       = Annon::Type_SymSetf,        // 5
    Annon_Callee        = Annon::Type_Callee,         // 6
    Annon_Delta2        = Annon::Type_Delta2,         // 7

    Annon_TlvOffset     = Annon::Type_TlvOffset,      // 8

    Annon_ExitPoint     = Annon::Type_ExitPoint,      // 9

    Annon_ClosedLit     = Annon::Type_ClosedLit,      // 10
    Annon_ClosedVar     = Annon::Type_ClosedVar,      // 11

    Annon_DllLink       = Annon::Type_DllLink,        // 12
    Annon_Data          = Annon::Type_Data,           // 13

    Annon_Anchor        = Annon::Type_MAX_1,
    Annon_Function,
}; // AnnonType


class x86EFlags;
class x86Reloc;
class x86ThreadData;

} // Compiler

#endif //!defined(INCLUDE_compiler_cg_x86_defs_h)
