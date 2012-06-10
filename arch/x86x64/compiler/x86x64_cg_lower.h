//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - x86x64 - Lower Pass
// arch/x86x64/compiler/x86x64_cg_lower.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_lower.h#3 $
//
#if !defined(INCLUDE_arch_x86x64_compiler_cg_lower_h)
#define INCLUDE_arch_x86x64_compiler_cg_lower_h

#include "../../../compiler/ir/ir_pass.h"

#include "./x86x64_cg_instruction.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// X86X64LowerPass
//
// Description:
//  Lower instruction operations satisfying x86 ISA restrictions:
//
// Note: You SHOULD NOT introduce new instruction. If you insert
// instruction, you MUST follow what LOWER pass does.
//
class X86X64LowerPass : public FunctionPass
{
    protected: X86X64LowerPass(const char16* pwsz) :
        FunctionPass(pwsz) {}

    protected: virtual void process_function(Function*);
    protected: virtual void process_instruction(Instruction*) = 0;
}; // X86X64LowerPass

} // Compiler

#endif //!defined(INCLUDE_arch_x86x64_compiler_cg_lower_h)
