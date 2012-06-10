//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x86 - ra-ls
// cg/x86/x86_ra_ls.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_expand_trap.h#6 $
//
#if !defined(INCLUDE_arch_x86x64_compiler_cg_expand_trap_h)
#define INCLUDE_arch_x86x64_compiler_cg_expand_trap_h

#include "../../../compiler/ir/ir_pass.h"
#include "../../../compiler/ir/ir_function.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// ExpandTrapPass
//
class X86X64ExpandTrapPass : public FunctionPass
{
    WorkList_<Instruction> m_oErrorBlocks;

    protected: X86X64ExpandTrapPass(const char16* pwsz) :
        FunctionPass(pwsz) {}

    protected: virtual void
        insert_service(Literal*, Values*, Instruction*) = 0;

    virtual void process_function(Function*);

    BBlock* find_trap(Instruction*);

    void process_TRAPIF(Instruction*);
}; // ExpandTrapPass

} // Compiler

#endif //!defined(INCLUDE_arch_x86x64_compiler_cg_expand_trap_h)
