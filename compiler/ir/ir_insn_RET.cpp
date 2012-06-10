#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction RET
// ir/instruction/ir_insn_RET.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_RET.cpp#3 $
//
#include "./ir_instruction.h"

#include "./ir_defs.h"
#include "./ir_bblock.h"
#include "./ir_fns.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
//  RetInsn::Realize
//
//  Description:
//   Realizes RET instruction.
//      o Add edget from bblock where RET instruction resides on to exit
//        bblock.
void
RetInsn::Realize()
{
    AddCfgEdge(
        GetBBlock(),
        GetFunction()->GetExitBB(), 
        CfgEdge::Kind_Exit );

    Instruction::Realize();
} // RetInsn::Realize


//////////////////////////////////////////////////////////////////////
//
//  RetInsn::Unrealize
//
//  Description:
//   Unrealizes RET instruction.
//      o Remove edget from bblock where RET instruction resides on to exit
//        bblock.
void
RetInsn::Unrealize()
{
    RemoveCfgEdge(
        GetBBlock(),
        GetFunction()->GetExitBB() );

    Instruction::Unrealize();
} // RetInsn::Unrealize

} // Compiler
