#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction CLOSURE
// ir/instruction/ir_insn_CLOSURE.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_CLOSURE.cpp#1 $
//
#include "./ir_instruction.h"

#include "./ir_function.h"

namespace Compiler
{

Operand* ClosureInsn::SimplifyOutputAux() const
{
    Function* pTempl = GetSx()->StaticCast<Function>();
    if (pTempl->MayBeClosure()) return m_pOutput;
    if (pTempl->HasUpVar())     return m_pOutput;
    return pTempl;
} // ClosureInsn::SimplifyOutputAux

} // Compiler
