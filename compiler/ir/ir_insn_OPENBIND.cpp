#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - OPENBIND instruction
// ir/instruction/ir_insn_OPENBIND.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_OPENBIND.cpp#3 $
//
#include "./ir_defs.h"

#include "./ir_bblock.h"
#include "./ir_instruction.h"

namespace Compiler
{


//////////////////////////////////////////////////////////////////////
//
// OpenBindInsn::AddBind
//
// Note: OpenBind instruction must be realized before calling this method.
//
Operand*
OpenBindInsn::AddBind(Val cell, Operand* pSx)
{
    ASSERT(value_cell_p(cell) || tlv_record_p(cell));
    ASSERT(NULL != pSx);

    ASSERT(m_cOperands <= m_cAllocs);

    if (m_cOperands ==  m_cAllocs)
    {
        m_cAllocs += 2;

        OpenBindOperandBox** prgpOperandBox =
            new OpenBindOperandBox*[m_cAllocs];

        ::CopyMemory(
            prgpOperandBox,
            m_prgpOperandBox,
            m_cOperands * sizeof(OpenBindOperandBox*) );

        m_prgpOperandBox = prgpOperandBox;
    } // if

    OpenBindOperandBox* pBox = new OpenBindOperandBox();

    m_prgpOperandBox[m_cOperands] = pBox;
        m_cOperands += 1;

    pBox->SetCell(cell);
    pBox->SetOperand(pSx);

    if (IsRealized())
    {
        pBox->SetInstruction(this);
        pSx->Realize(pBox);
    }

    return pSx;
} // OpenBindInsn::AddBind


//////////////////////////////////////////////////////////////////////
//
// OpenBindInsn::HtmlPrint
//
void
OpenBindInsn::HtmlPrint(Val stream, bool fDef) const
{
    HtmlA(stream, fDef);

    html_format(stream, L"(~A", GetMnemonic());

    html_format(stream, L" <i>~W</i> ~:S &lt;=", m_ty, m_pOutput);

    foreach (EnumInput, oEnum, this)
    {
        OpenBindOperandBox* pBox = oEnum.GetBox();
        html_format(stream, L" (~W ~S)",
            pBox->GetSymbol(),
            pBox->GetOperand() );
    } // for each operand

    write_string(L")</a>", stream);
} // OpenBindInsn::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
// OpenBindInsn::RemoveBindt
//
// Description:
//  Removes input operand for specified bblock.
//
void
OpenBindInsn::RemoveBind(Val sym)
{
    ASSERT(NULL != sym);

    OpenBindOperandBox** ppRunner = m_prgpOperandBox;
    OpenBindOperandBox** ppEnd = m_prgpOperandBox + m_cOperands;
    while (ppRunner < ppEnd)
    {
        OpenBindOperandBox* pBox = *ppRunner;
        if (pBox->GetSymbol() == sym)
        {
            ::CopyMemory(
                ppRunner,
                ppRunner + 1,
                ppEnd - (ppRunner + 1) );

            m_cOperands -= 1;
            return;
        }
    } // while

    CAN_NOT_HAPPEN();
} // OpenBindInsn::RemoveBind

} // Compiler
