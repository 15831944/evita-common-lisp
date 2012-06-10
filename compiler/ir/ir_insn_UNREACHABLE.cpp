#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction CLOSE
// ir/instruction/ir_insn_CLOSE.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_UNREACHABLE.cpp#3 $
//
#include "./ir_instruction.h"

#include "./ir_function.h"
#include "./ir_fns.h"

namespace Compiler
{


//////////////////////////////////////////////////////////////////////
//
//  UnreachableInsn::Realize
//
//  Description:
//   Realizes RET instruction.
//      o Add edget from bblock where UNREACHABLE instruction resides on
//        to exit bblock.
void
UnreachableInsn::Realize()
{
    AddCfgEdge(
        GetBBlock(),
        GetFunction()->GetExitBB(), 
        CfgEdge::Kind_Exit );

    Instruction::Realize();
} // UnreachableInsn::Realize


//////////////////////////////////////////////////////////////////////
//
//  UnreachableInsn::Unrealize
//
//  Description:
//   Unrealizes RET instruction.
//      o Remove edget from bblock where UNREACHABLE instruction resides
//        on to exit bblock.
void
UnreachableInsn::Unrealize()
{
    RemoveCfgEdge(
        GetBBlock(),
        GetFunction()->GetExitBB() );

    Instruction::Unrealize();
} // UnreachableInsn::Unrealize

} // Compiler
