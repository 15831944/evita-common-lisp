#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR - Loop Tree
// ir/ir_loop.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_loop.cpp#1 $
//
#include "./ir_defs.h"
#include "./ir_loop.h"

#include "../cm/cm_pass.h"
#include "../cm/cm_session.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// ir_common_dominator
//
BBlock* ir_common_dominator(BBlock* a, BBlock* b)
{
    while (a->GetDomInfo()->GetDepth() > b->GetDomInfo()->GetDepth())
        { a = a->GetDomInfo()->GetParent(); }

    while (b->GetDomInfo()->GetDepth() > a->GetDomInfo()->GetDepth())
        { b = b->GetDomInfo()->GetParent(); }

    while (a != b)
    {
        a = a->GetDomInfo()->GetParent();
        b = b->GetDomInfo()->GetParent();
    }

    return a;
} // ir_common_dominator

//////////////////////////////////////////////////////////////////////
//
// ancestor
//  Finding the outermost processed loop.
//
static LoopInfo* ancestor(LoopInfo* pNode)
{
    while (NULL != pNode->m_pParent) { pNode = pNode->m_pParent; }
    return pNode;
} // ancestor

//////////////////////////////////////////////////////////////////////
//
// find_body
//
static void find_body(LoopInfo* pHead, LoopInfo::Kind eKind)
{
    pHead->m_eKind = eKind;

    WorkList_<LoopInfo> oQueue;
    foreach (LoopInfo::EnumGenerator, oEnum, pHead)
    {
        LoopInfo* pNode = oEnum.Get();
        LoopInfo* pAncestor = ancestor(pNode);
        if (! oQueue.Has(pAncestor))
        {
            pHead->AddBody(pAncestor);
            oQueue.Push(pAncestor);
        }
    } // for each generator

    while (! oQueue.IsEmpty())
    {
        LoopInfo* pNode = oQueue.Pop();
        foreach (BBlock::EnumInEdge, oEnum, pNode->m_pBBlock)
        {
            LoopInfo* pPred = oEnum.Get()->GetFrom()->GetLoopInfo();
            if (pPred != pHead)
            {
                LoopInfo* pAncestor = ancestor(pPred);
                if (pAncestor != pHead)
                {
                    pHead->AddBody(pAncestor);
                    oQueue.Push(pAncestor);
                }
            }
        } // for each pred
    } // for each elt
} // find_body


//////////////////////////////////////////////////////////////////////
//
// find_loop
//
static void find_loop(LoopInfo* pNode)
{
    LoopInfo* pEnd = pNode;
    bool fBackEdge = false;
    WorkList_<LoopInfo> oLoop;
    foreach (BBlock::EnumInEdge, oEnum, pNode->m_pBBlock)
    {
        CfgEdge* pEdge = oEnum.Get();
        if (pEdge->IsBackward())
        {
            fBackEdge = true;

            LoopInfo* pPred = pEdge->GetFrom()->GetLoopInfo();

            pEnd = ir_common_dominator(pEnd->m_pBBlock, pPred->m_pBBlock)->
                GetLoopInfo();

            if (! oLoop.Has(pPred) && pPred != pNode)
            {
                oLoop.Push(pPred);
            }
        } // if backedge
    } // for each pred

    if (oLoop.IsEmpty())
    {
        if (fBackEdge)
        {
            pNode->m_eKind = LoopInfo::Kind_Single;
            pNode->m_nDepth += 1;
        }
        return;
    }

    while (! oLoop.IsEmpty())
    {
        LoopInfo* pNode = oLoop.Pop();
        pNode->m_pGenerator = pEnd->m_pGenerator;
        pEnd->m_pGenerator = pNode;
    } // while

    if (pEnd == pNode)
    {
        find_body(pNode, LoopInfo::Kind_Single);
    }
} // find_loop


//////////////////////////////////////////////////////////////////////
//
// dump_loop_node
//
static void dump_loop_node(const LoopInfo* pHead)
{
    const char16* pwsz;

    switch (pHead->m_eKind)
    {
    case LoopInfo::Kind_Leaf:
        html_log_format(2, L"<li>~S/~D</li>~%",
            pHead->m_pBBlock, pHead->m_nDepth );
        return;
    case LoopInfo::Kind_Root:     pwsz = L"Root"; break;
    case LoopInfo::Kind_Single:   pwsz = L"Single"; break;
    case LoopInfo::Kind_Multiple: pwsz = L"Multiple"; break;
    default:
        CAN_NOT_HAPPEN();
    } // switch kind

    html_log_format(2, L"<li>~A: ~S/~D",
        pwsz, pHead->m_pBBlock, pHead->m_nDepth );

    html_log_format(2, L"<ul>~%");
    foreach (LoopInfo::EnumChild, oEnum, pHead)
    {
        dump_loop_node(oEnum.Get());
    } // for each member
    html_log_format(2, L"</ul></li>~%");
} // dump_loop_node


//////////////////////////////////////////////////////////////////////
//
// ir_dump_loop_tree
//
void ir_dump_loop_tree(Function* pFun)
{
    html_log_format(2, L"<h2>Loop Tree of ~S</h2>~%", pFun);

    dump_loop_node(pFun->GetEntryBB()->GetLoopInfo());
} // ir_dump_loop_tree


//////////////////////////////////////////////////////////////////////
//
// ir_compute_loop_tree
//
LoopInfo* ir_compute_loop_tree(Function* pFun)
{
    ir_number_instructions(pFun);

    foreach (Function::EnumBBlock, oEnum, pFun)
    {
        BBlock* pBBlock = oEnum.Get();
        if (NULL == pBBlock->GetLoopInfo())
        {
            pBBlock->SetLoopInfo(new LoopInfo(pBBlock));
        }
    } // for each bblock

    foreach (Function::EnumBBlock_Postorder, oEnum, pFun)
    {
        BBlock* pBBlock = oEnum.Get();
        LoopInfo* pNode = pBBlock->GetLoopInfo();
        if (NULL != pNode->m_pGenerator)
        {
            find_body(pNode, LoopInfo::Kind_Multiple);
        }
        find_loop(pNode);
    } // for each bblock

    LoopInfo* pEntry = pFun->GetEntryBB()->GetLoopInfo();
    pEntry->m_pGenerator = pFun->GetExitBB()->GetLoopInfo();

    find_body(pEntry, LoopInfo::Kind_Root);

    pFun->GetEntryBB()->GetLoopInfo()->m_nDepth = 1;

    if (nil != Session::Get()->GetPass()->GetLogStream(1))
    {
        ir_dump_loop_tree(pFun);
    }

    return pEntry;
} // ir_compute_loop_tree

} // Compiler
