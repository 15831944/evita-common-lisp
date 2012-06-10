#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction JUMP
// ir/instruction/ir_insn_JUMP.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_JUMP.cpp#3 $
//
#include "./ir_defs.h"
#include "./ir_bblock.h"
#include "./ir_instruction.h"

#include "../cm/cm_session.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// JumpInsn::JumpInsn
//
JumpInsn::JumpInsn(BBlock* pTarget) :
    One_Operand_Instruction(pTarget->GetLabel()) {}

} // Compiler
