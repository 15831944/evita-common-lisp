#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - Utility Functions
// compiler/cg/cg_fns.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_fns.cpp#4 $
//
#include "./cg_defs.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// CgAssembler::get_next_bblock
//
// Called by:
//  X86CgAsmPass::process_BRANCH
//  X86CgAsmPass::process_JUMP
//
// Description:
//  Returns next bblock of pBBlock. If next bblock contains only
//  NONLOCAL and JUMP instruction and JUMP to next of next bblock,
//  return next of next bblock.
//
BBlock* get_next_bblock(BBlock* pBBlock)
{
    ASSERT(NULL != pBBlock);

    for (;;)
    {
        BBlock* pNext = pBBlock->GetNext();
        if (! pNext->GetFirstInsn()->Is<NonlocalInsn>())
        {
            return pNext;
        }

        JumpInsn* pJump = pNext->GetFirstInsn()->GetNext()->
            DynamicCast<JumpInsn>();

        if (NULL == pJump)
        {
            return pNext;
        }

        pBBlock = pNext;
        if (pJump->GetTarget() != pBBlock->GetNext())
        {
            return pNext;
        }
    } // for
} // CgAssembler::get_next_bblock


// is_useless_JUMP
bool is_useless_JUMP(Instruction* pJump)
{
    return get_next_bblock(pJump->GetBBlock()) ==
           pJump->GetSx()->StaticCast<Label>()->GetBBlock();

} // is_useless_JUMP

} // Compiler
