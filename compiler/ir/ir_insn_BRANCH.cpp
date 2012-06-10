#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction BRANCH
// ir/instruction/ir_insn_BRANCH.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_BRANCH.cpp#3 $
//
#include "./ir_instruction.h"

#include "./ir_bblock.h"
#include "./ir_fns.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// BranchInsn::BranchInsn
//
BranchInsn::BranchInsn(
    Bool*   pBool,
    BBlock* pTrue,
    BBlock* pFalse ) :
        Three_Operands_Instruction(
            pBool,
            pTrue->GetLabel(),
            pFalse->GetLabel() )
{
    // nothing to do
} // BranchInsn::BranchInsn

} // Compiler
