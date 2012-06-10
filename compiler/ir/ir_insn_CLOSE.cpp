#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction CLOSE
// ir/instruction/ir_insn_CLOSE.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_CLOSE.cpp#3 $
//
#include "./ir_instruction.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// CloseInsn::IsUseless
//
bool
CloseInsn::IsUseless() const
{
    return NULL == GetSx()->StaticCast<Frame>()->GetDfn();
} // CloseInsn::IsUseless

} // Compiler
