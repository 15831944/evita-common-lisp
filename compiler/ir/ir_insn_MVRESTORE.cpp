#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction cmp
// ir/instruction/ir_insn_cmp.cpp
//
// Copyright (C) 1996-2006 by MvRestore Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_MVRESTORE.cpp#2 $
//
#include "./ir_instruction.h"

#include "./ir_fns.h"

namespace Compiler
{

Operand* MvRestoreInsn::SimplifyOutputAux() const
{
    Register* pRx = GetRx();

    Instruction* pDfn = pRx->GetDfn();

    switch (pDfn->GetOpcode())
    {
    case IrOp_COPY:
        // x86-ENSURE changes MVSAVE to MVSAVE+COPY.
        if (pDfn == GetPrev()) return pDfn->GetSx();
        break;

    case IrOp_MVSAVE:
        if (NULL == pDfn->GetVx()) return pDfn->GetSx();

        {
            ValuesInsn* pValues = pDfn->GetVx()->GetDfn()->
                DynamicCast<ValuesInsn>();

            if (NULL != pValues && 1 == pValues->GetOperandCount())
            {
                return pValues->GetSx();
            }
        }

        if (pDfn == GetPrev()) return pDfn->GetSx();
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch opcode

    return GetOutput();
} // MvRestoreInsn::SimplifyOutputAux

} // Compiler
