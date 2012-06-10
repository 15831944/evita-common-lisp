#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction TBLJMP
// ir/instruction/ir_insn_TBLJMP.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_TBLJMP.cpp#3 $
//
#include "./ir_instruction.h"

#include "./ir_bblock.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// TblJmpInsn consturctor
//
TblJmpInsn::TblJmpInsn(
    Register*   pRx,
    BBlock*     prgpBBlock[],
    UINT        cBBlocks )
{
    m_cAllocs   = 0;

    UINT cOperands = cBBlocks + 1;

    m_cAllocs   = static_cast<UINT>(ROUNDUP(cOperands, 4));
    m_prgpOperandBox = new OperandBox*[m_cAllocs];
    for (UINT i = 0; i < cBBlocks; i++)
    {
        OperandBox* pBox = new OperandBox();
        pBox->SetOperand(prgpBBlock[i]->GetLabel());
        m_prgpOperandBox[i+1] = pBox;
    } // for

    {
        OperandBox* pBox = new OperandBox();
        m_prgpOperandBox[0] = pBox;
        pBox->SetOperand(pRx);
    }

    m_cOperands = cOperands;
} // TblJmpInsn::TblJmpInsn


} // Compiler
