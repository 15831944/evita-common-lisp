#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction TYPEP
// ir/instruction/ir_insn_TYPEP.cpp
//
// Copyright (C) 1996-2006 by Count Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_TYPEP.cpp#1 $
//
#include "./ir_instruction.h"

#include "./ir_fns.h"

namespace Compiler
{


Operand* TypepInsn::SimplifyOutputAux() const
{
    if (GetSx()->Is<Literal>())
    {
        return ty_typep(GetLx(), GetLy()) ? Bool_True : Bool_False;
    }

    return GetOutput();
} // CountInsn::SimplifyOutputAux

} // Compiler
