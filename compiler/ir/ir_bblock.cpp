#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR - bblock
// ir_bblock.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_bblock.cpp#7 $
//

#include "./ir_bblock.h"

#include "../cm/cm_session.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// BBlock constructor
//
BBlock::BBlock(Kind eKind) :
    m_pDFData(NULL),
    m_pDomInfo(NULL),
    m_pLoopInfo(NULL),
    m_pPostDomInfo(NULL)
{
    ASSERT(Kind_Anchor != eKind);

    m_nName  = Session::NewBBlockName();
    m_oLabels.Append_(new Label(this));
    // Mark anchor instruction realized for insertion.
    InsnList::m_oAnchor.SetParent(this);
} // BBlock::BBlock


//////////////////////////////////////////////////////////////////////
//
// BBlock::AppendInsn
//
Instruction*
BBlock::AppendInsn(Instruction* pInsn)
{
    pInsn->SetParent(this);
    static_cast<InsnList*>(this)->Append_(pInsn);
    pInsn->Realize();
    return pInsn;
} // BBlock::AppendInsn


//////////////////////////////////////////////////////////////////////
//
//  BBlock::DoesDominate
//
//  Description:
//   Returns true if this bblock dominates pBBlock.
//
bool
BBlock::DoesDominate(const BBlock* pBBlock) const
{
    for (;;)
    {
        if (this == pBBlock)
        {
            return true;
        }

        pBBlock = pBBlock->GetDomInfo()->GetParent();
        if (NULL == pBBlock)
        {
            return false;
        }
    } // for
} // BBlock::DoesDominate


//////////////////////////////////////////////////////////////////////
//
//  BBlock::DoesPostDominate
//
//  Description:
//   Returns true if this bblock PostDominates pBBlock.
//
bool
BBlock::DoesPostDominate(const BBlock* pBBlock) const
{
    for (;;)
    {
        if (this == pBBlock)
        {
            return true;
        }

        pBBlock = pBBlock->GetPostDomInfo()->GetParent();
        if (NULL == pBBlock)
        {
            return false;
        }
    } // for
} // BBlock::DoesPostDominate


//////////////////////////////////////////////////////////////////////
//
// BBlock::HasMoreThanOnePred
//
bool
BBlock::HasMoreThanOnePred() const
{
    UINT cPreds = 0;
    foreach (EnumInEdge, oEnum, this)
    {
        cPreds += 1;

        if (2 == cPreds)
        {
            return true;
        }
    } // for each pred

    return false;
} // BBlock::HasMoreThanOnePred


//////////////////////////////////////////////////////////////////////
//
// BBlock::HasMoreThanOneSucc
//
bool
BBlock::HasMoreThanOneSucc() const
{
    UINT cSuccs = 0;
    foreach (EnumOutEdge, oEnum, this)
    {
        cSuccs += 1;

        if (2 == cSuccs)
        {
            return true;
        }
    } // for each succ

    return false;
} // BBlock::HasMoreThanOneSucc


//////////////////////////////////////////////////////////////////////
//
// BBlock::HasNonlocalInEdge()
//
bool BBlock::HasNonlocalInEdge() const
{
    foreach (EnumInEdge, oEnum, this)
    {
        if (oEnum.Get()->GetKind() == CfgEdge::Kind_Nonlocal)
        {
            return true;
        }
    } // for each in-edge
    return false;
} // BBlock::HasNonlocalInEdge()


//////////////////////////////////////////////////////////////////////
//
// BBlock::HasOnlyOnePred
//
bool
BBlock::HasOnlyOnePred() const
{
    UINT cPreds = 0;
    foreach (EnumInEdge, oEnum, this)
    {
        cPreds += 1;

        if (2 == cPreds)
        {
            return false;
        }
    } // for each Pred

    return 1 == cPreds;
} // BBlock::HasMoreThanOnePred


//////////////////////////////////////////////////////////////////////
//
// BBlock::HasOnlyOneSucc
//
bool
BBlock::HasOnlyOneSucc() const
{
    UINT cSuccs = 0;
    foreach (EnumOutEdge, oEnum, this)
    {
        cSuccs += 1;

        if (2 == cSuccs)
        {
            return false;
        }
    } // for each succ

    return 1 == cSuccs;
} // BBlock::HasMoreThanOneSucc


//////////////////////////////////////////////////////////////////////
//
// BBlock::HtmlPrint
//
void
BBlock::HtmlPrint(Val stream, bool fDef) const
{
    HtmlA(stream, fDef);
    cm_format(stream, L"BB~D", GetName());
    write_string(L"</a>", stream);
} // BBlock::HtmlPrint

} // Compiler
