#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction cmp
// ir/instruction/ir_insn_cmp.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_PROJECT.cpp#6 $
//
#include "./ir_instruction.h"

#include "./ir_fns.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// ProjectInsn::SimplifyOutputAux
//  If %vx is defined by VALUES instruction, pick it up.
Operand* ProjectInsn::SimplifyOutputAux() const
{
    Values* pVx = GetVx();

    Operand* pSd = GetOutput();

    uint nNth = static_cast<uint>(Fixnum::Decode_(GetLy()));

    if (NULL == pVx)
    {
        if (0 == nNth)
        {
            pSd = GetSx();
        }
        else
        {
            pSd = NewLiteral(nil);
        }
    }
    else
    {
        ValuesInsn* pValues = pVx->GetDfn()->DynamicCast<ValuesInsn>();

        if (NULL != pValues)
        {
            if (nNth < pValues->GetOperandCount())
            {
                pSd = pValues->GetOperand(nNth);
            }
            else
            {
                pSd = NewLiteral(nil);
            }
        } // pSd
    } // if

    return pSd;
} // ProjectInsn::SimplifyOutputAux

} // Compiler
