#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction VARDEF
// ir/instruction/ir_insn_VARDEF.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_VARDEF.cpp#5 $
//

#include "./ir_defs.h"

#include "./ir_instruction.h"
#include "./ir_bblock.h"
#include "./ir_function.h"

#include "../cm/cm_module.h"
#include "../cm/cm_session.h"


namespace Compiler
{

VarDefInsn::VarDefInsn(
    Register*   pRd,
    Variable*   pVar,
    Operand*    pSx ) :
        Two_Operands_Instruction(pVar, pSx, ty_closed_cell, pRd)
{
    pRd->SetVar(pVar);
    SetVar(pVar);

    Register* pRx = pSx->DynamicCast<Register>();
    if (NULL != pRx && NULL == pRx->GetVar())
    {
        pRx->SetVar(pVar);
        pRx->GetDfn()->SetVar(pVar);
    }
} // VarDefInsn::VarDefInsn

} // Compiler
