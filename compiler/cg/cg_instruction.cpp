#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg- instruction
// cg/cg_instruction.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_instruction.cpp#4 $
//
#include "./cg_instruction.h"

#include "../ir/ir_fns.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// FrameRefInsn::HtmlPrint
//
void
FrameRefInsn::HtmlPrint(Val stream, bool fDef) const
{
    HtmlA(stream, fDef);

    html_format(stream, L"(~A", GetMnemonic());

    if (fDef)
    {
        html_format(stream, L" <i>~W</i> ~:S &lt;=", m_ty, m_pOutput);
    }
    else
    {
        html_format(stream, L" <i>~W</i> ~S &lt;=", m_ty, m_pOutput);
    }

    html_format(stream, L" ~S", m_pFrameOwner);

    write_string(L")</a>", stream);
} // FrameRefInsn::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
// StackVecInsn constructor
//
StackVecInsn::StackVecInsn(
    Register*       pRd,
    Operand* const* prgpOperand,
    uint            cOperands )
{
    m_ty = ty_ptr_int;
    m_pOutput = pRd;
    m_cOperands = cOperands;
    m_prgpOperandBox = new OperandBox*[m_cOperands];
    for (uint i = 0; i < cOperands; i++)
    {
        OperandBox* pBox = new OperandBox();
        pBox->SetOperand(prgpOperand[i]);
        m_prgpOperandBox[i] = pBox;
    } // for i
} // StackVecInsn::StackVecInsn


//////////////////////////////////////////////////////////////////////
//
// VecRefInsn::HtmlPrint
//
void
VecRefInsn::HtmlPrint(Val stream, bool fDef) const
{
    HtmlA(stream, fDef);

    html_format(stream, L"(~A", GetMnemonic());

    if (fDef)
    {
        html_format(stream, L" <i>~W</i> ~:S &lt;=", m_ty, m_pOutput);
    }
    else
    {
        html_format(stream, L" <i>~W</i> ~S &lt;=", m_ty, m_pOutput);
    }

    html_format(stream, L" ~S ~D", GetSx(), m_nNth);

    write_string(L")</a>", stream);
} // VecRefInsn::HtmlPrint

} // Compiler
