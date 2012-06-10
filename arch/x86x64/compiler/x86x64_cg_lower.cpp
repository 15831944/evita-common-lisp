#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x86x64 - Lower Pass
// arch/x86x64/compiler/x86x64_cg_lower.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_lower.cpp#5 $
//
#include "./x86x64_cg_lower.h"

namespace Compiler
{
//////////////////////////////////////////////////////////////////////
//
// X86X64LowerPass::process_function
//
void
X86X64LowerPass::process_function(Function* pFun)
{
    html_log_format(1, L"<h1>process ~S</h1>~%", pFun);

    foreach (Function::EnumBBlock, oEnumBB, pFun)
    {
        BBlock* pCurr = oEnumBB.Get();

        BBlock::EnumInsn oEnum(pCurr);
        while (! oEnum.AtEnd())
        {
            Instruction* pInsn = oEnum.Get();
                oEnum.Next();

            process_instruction(pInsn);
        } // for each insn
    } // for each bblock
} // X86X64LowerPass::process_function

} // Compiler
