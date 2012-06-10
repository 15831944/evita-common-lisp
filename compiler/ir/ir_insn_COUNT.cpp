#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction cmp
// ir/instruction/ir_insn_cmp.cpp
//
// Copyright (C) 1996-2006 by Count Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_COUNT.cpp#2 $
//
#include "./ir_instruction.h"

#include "./ir_fns.h"

namespace Compiler
{

Operand* CountInsn::SimplifyOutputAux() const
{
    ValuesInsn* pValues = GetVx()->GetDfn()->DynamicCast<ValuesInsn>();
    if (NULL == pValues) return GetOutput();
    return NewLiteral(Fixnum::Encode(pValues->GetOperandCount()));
} // CountInsn::SimplifyOutputAux

} // Compiler
