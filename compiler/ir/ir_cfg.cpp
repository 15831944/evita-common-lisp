#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR - CFG
// ir/ir_cfg.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_cfg.cpp#5 $
//
//  This file contains CFG related functions:
//      AddCfgEdge
//      FindCfgEdge
//      InsertBBlock
//      NewBBlock
//      ir_replace_cfg_edge_to
//      RemoveCfgEdge
//      ir_remove_bblock
//      ir_split_bblock_after
//      SplitCfgEdge
//
#include "./ir_defs.h"
#include "./ir_cfg.h"
#include "./ir_bblock.h"
#include "./ir_function.h"
#include "./ir_fns.h"

namespace Compiler
{

// CfgEdge::HtmlPrint
void
CfgEdge::HtmlPrint(Val stream, bool) const
{
    static const LPCWSTR k_rgwszEdge[Kind_MAX_1] =
    {
        L"Anchor",
        L"Pseudo",
        L"Normal",
        L"NonLocal",
        L"Exit",
    }; // k_rgwszEdge

    html_format(stream, L"(~A-Edge~A ~S->~S)",
        k_rgwszEdge[m_eKind],
        m_fBackward ? L"*" : L"",
        m_pFrom,
        m_pTo );
} // CfgEdge::HtmlPrint

//////////////////////////////////////////////////////////////////////
//
// Add an CFG edge between two bblocks
//
CfgEdge*
AddCfgEdge(
    BBlock*         pFrom,
    BBlock*         pTo,
    CfgEdge::Kind   eKind )
{
    ASSERT(NULL != pFrom);
    ASSERT(NULL != pTo);

    // For inline, we don't check parent.
    //ASSERT(pFrom->GetParent() == pTo->GetParent());

    pFrom->GetParent()->SetCfgChanged();
    pTo->GetParent()->SetCfgChanged();

    CfgEdge* pEdge = new CfgEdge(pFrom, pTo, eKind);

    pFrom->LinkOutEdge(pEdge);
    pTo->LinkInEdge(pEdge);

    return pEdge;
} // AddCfgEdge


//////////////////////////////////////////////////////////////////////
//
// ir_eliminate_infinite_loop
//  Eliminates infinite loop by adding edge from the block in infinite
//  loop to exit block.
//
bool ir_eliminate_infinite_loop(Function* pFun)
{
    class BackwardDFS
    {
        public: static uint Run(BBlock* pBBlock)
        {
            ASSERT(! pBBlock->GetFlag());
            uint cBlocks = 0;
            dfs(pBBlock, &cBlocks);
            return cBlocks;
        } // Run

        static void dfs(BBlock* pBBlock, uint* inout_cBlocks)
        {
            if (pBBlock->GetFlag()) return;
            *inout_cBlocks += 1;
            pBBlock->SetFlag(1);
            foreach (BBlock::EnumInEdge, oEnum, pBBlock)
            {
                dfs(oEnum.GetNode(), inout_cBlocks);
            } // for each out edge
        } // dfs
    }; // BackwardDFS

    class ForwardDFS
    {
        public: static uint Run(Function* pFun)
        {
            foreach (Function::EnumBBlock, oEnum, pFun)
            {
                oEnum.Get()->SetFlag(0);
            } // for each bblock

            uint cBlocks = 0;
            dfs(pFun->GetEntryBB(), &cBlocks);
            return cBlocks;
        } // Run

        static void dfs(BBlock* pBBlock, uint* inout_cBlocks)
        {
            if (pBBlock->GetFlag()) return;
            *inout_cBlocks += 1;
            pBBlock->SetFlag(1);
            foreach (BBlock::EnumOutEdge, oEnum, pBBlock)
            {
                dfs(oEnum.GetNode(), inout_cBlocks);
            } // for each out edge
        } // dfs
    }; // ForwardDFS

    pFun->PrepareTraversal();

    uint cForward = ForwardDFS::Run(pFun);

    foreach (Function::EnumBBlock, oEnum, pFun)
    {
        oEnum.Get()->SetFlag(0);
    } // for each bblock

    BBlock* pExitBB = pFun->GetExitBB();

    uint cNotVisit = cForward - BackwardDFS::Run(pExitBB);

    if (0 == cNotVisit)
    {
        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            oEnum.Get()->SetFlag(0);
        } // for each bblock
        return false;
    }

    html_log_format(4, L"<h4>Eliminate Infinite Loop for ~S</h4>~%", pFun);

    while (cNotVisit >= 1)
    {
        BBlock* pBBlock = NULL;
        foreach (Function::EnumBBlock_Postorder, oEnum, pFun)
        {
            if (! oEnum.Get()->GetFlag())
            {
                pBBlock = oEnum.Get();
                break;
            }
        } // for each bblock

        if (NULL == pBBlock) CAN_NOT_HAPPEN();

        html_log_format(5, L"add edge ~S to ~S.~:%", pBBlock, pExitBB);

        AddCfgEdge(pBBlock, pExitBB, CfgEdge::Kind_Pseudo);

        cNotVisit -= BackwardDFS::Run(pBBlock);
    } // while

    foreach (Function::EnumBBlock, oEnum, pFun)
    {
        oEnum.Get()->SetFlag(0);
    } // for each bblock

    return true;
} // ir_eliminate_infinite_loop


//////////////////////////////////////////////////////////////////////
//
// ir_eliminate_unreachable_bblocks
//
void
ir_eliminate_unreachable_bblocks(Function* pFun)
{
    pFun->PrepareTraversal();

    Function::EnumBBlock oEnum(pFun);
    while (! oEnum.AtEnd())
    {
        BBlock* pBBlock = oEnum.Get();
            oEnum.Next();
        if (0 == pBBlock->GetPreorder())
        {
            ir_remove_bblock(pBBlock);
        }
    } // while
} // ir_eliminate_unreachable_bblocks


//////////////////////////////////////////////////////////////////////
//
// FindCfgEdge
//
CfgEdge*
FindCfgEdge(BBlock* pPred, BBlock* pSucc)
{
    ASSERT(NULL != pPred);
    ASSERT(NULL != pSucc);

    foreach (BBlock::EnumOutEdge, oEnum, pPred)
    {
        CfgEdge* pEdge = oEnum.Get();
        if (pEdge->GetTo() == pSucc)
        {
            return pEdge;
        }
    } // for each edge

    return NULL;
} // FindCfgEdge


//////////////////////////////////////////////////////////////////////
//
// InsertBBlock
//
BBlock*
InsertBBlock(BBlock* pBBlock, BBlock* pRef)
{
    ASSERT(NULL != pRef);
    Function* pFun = pRef->GetFunction();
    pBBlock->SetParent(pFun);
    return pFun->Insert(pBBlock, pRef);
} // InsertBBlock


//////////////////////////////////////////////////////////////////////
//
// NewBBlock
//
BBlock*
NewBBlock()
{
    return new BBlock(BBlock::Kind_Normal);
} // NewBBlock


//////////////////////////////////////////////////////////////////////
//
// RemoveCfgEdge
//
// Description:
//  Removes specified edge from out-edge list of current and
//  in-edge list of successor, and remove input operands of
//  PHI instructions.
//
// Note:
//  We don't fold single input operand PHI instructions. Caller
//  should handle this.
//
CfgEdge*
RemoveCfgEdge(CfgEdge* pEdge)
{
    ASSERT(NULL != pEdge);

    BBlock* pCurr = pEdge->GetFrom();
    BBlock* pSucc = pEdge->GetTo();

    pCurr->GetParent()->SetCfgChanged();

    pCurr->UnlinkOutEdge(pEdge);
    pSucc->UnlinkInEdge(pEdge);

    if (! pSucc->HasInEdge())
        { html_log_format(5, L"succ ~S has no in-edge.~:%", pSucc); }

    foreach (BBlock::EnumInsn, oEnum, pSucc)
    {
        PhiInsn* pPhi = oEnum.Get()->DynamicCast<PhiInsn>();
            if (NULL == pPhi) break;

        html_log_format(6, L"remove ~S from ~S:~S~:%",
            pCurr, pPhi->GetBBlock(), pPhi );

        pPhi->RemoveInput(pCurr);
    } // for each insn

    return pEdge;
} // RemoveCfgEdge


//////////////////////////////////////////////////////////////////////
//
// ir_remove_bblock
//
BBlock*
ir_remove_bblock(BBlock* pBBlock)
{
    html_log_format(3, L"remove ~S~:%", pBBlock);

    BBlock::EnumInsn oEnum(pBBlock);
    while (! oEnum.AtEnd())
    {
        Instruction* pInsn = oEnum.Get();
        oEnum.Next();
        ir_remove_insn(pInsn);
    } // while


    pBBlock->GetParent()->SetCfgChanged();
    pBBlock->GetParent()->Remove(pBBlock);

    return pBBlock;
} // ir_remove_bblock


//////////////////////////////////////////////////////////////////////
//
// ir_replace_cfg_edge_from
//
//  Called by:
//      SplitCfgEdge
//
CfgEdge*
ir_replace_cfg_edge_from(CfgEdge* pEdge, BBlock* pNewPred)
{
    ASSERT(NULL != pEdge);
    ASSERT(NULL != pNewPred);

    BBlock* pSucc    = pEdge->GetTo();
    BBlock* pOldPred = pEdge->GetFrom();

    // For inline, we don't check parent.
    //ASSERT(pOldPred->GetParent() == pNewPred->GetParent());

    pOldPred->GetParent()->SetCfgChanged();
    pNewPred->GetParent()->SetCfgChanged();

    pOldPred->UnlinkOutEdge(pEdge);
    pEdge->SetFrom(pNewPred);
    pNewPred->LinkOutEdge(pEdge);

    ReplacePhiOperands(pNewPred, pOldPred, pSucc);

    return pEdge;
} // ir_replace_cfg_edge_from


//////////////////////////////////////////////////////////////////////
//
// ir_replace_cfg_edge_to
//
//  Called by:
//      SplitCfgEdge
//
CfgEdge*
ir_replace_cfg_edge_to(CfgEdge* pEdge, BBlock* pNewSucc)
{
    ASSERT(NULL != pEdge);
    ASSERT(NULL != pNewSucc);

    BBlock* pPred = pEdge->GetFrom();
    BBlock* pOldSucc = pEdge->GetTo();

    // For inline, we don't check parent.
    //ASSERT(pOldSucc->GetParent() == pNewSucc->GetParent());

    pNewSucc->GetParent()->SetCfgChanged();
    pOldSucc->GetParent()->SetCfgChanged();

    pOldSucc->UnlinkInEdge(pEdge);
    pEdge->SetTo(pNewSucc);
    pNewSucc->LinkInEdge(pEdge);

    ReplacePhiOperands(pNewSucc, pPred, pOldSucc);

    return pEdge;
} // ir_replace_cfg_edge_to


//////////////////////////////////////////////////////////////////////
//
//  ReplacePhiOperands
//
//  Called by:
//      ir_replace_cfg_edge_to
//      ir_split_bblock_after
//
//  Description:
//    Replaces input operand's bblock in pCurr from pOldIn to pNew.
//
void
ReplacePhiOperands(BBlock* pNewIn, BBlock* pOldIn, BBlock* pCurr)
{
    foreach (BBlock::EnumInsn, oEnum, pCurr)
    {
        PhiInsn* pPhiInsn = oEnum.Get()->DynamicCast<PhiInsn>();
        if (NULL == pPhiInsn)
        {
            break;
        }

        foreach (PhiInsn::EnumInput, oEnum, pPhiInsn)
        {
            PhiInsn::PhiOperandBox* pBox = oEnum.GetBox();
            if (pBox->GetBBlock() == pOldIn)
            {
                pBox->SetBBlock(pNewIn);
            }
        } // for each input
    } // for each phi
} // ReplacePhiOperands


//////////////////////////////////////////////////////////////////////
//
//  ir_split_bblock_after
//
//  Description:
//   Splits basic block after specified instruction into two basic blocks and
//   returns newly created successor basic block.
// 
//  Note: This function doesn't make edge between curr and succ. Caller
//  must inserts last instruction into curr.
// 
//       before                after
//    +----------+          +----------+
//    |          |          |          |
//    |          |          |          |    curr
//    |   insn   |  =>      |   insn   |
//    |          |          +----------+
//    |          |
//    +----------+          +----------+
//                          |          |    succ
//                          |          |
//                          +----------+
//
//  Note: Self-tail call optimization calls this funciton for making loop
//  head.
//
BBlock*
ir_split_bblock_after(Instruction* pInsn)
{
    BBlock* pCurr = pInsn->GetBBlock();
    BBlock* pSucc = NewBBlock();

    InsertBBlock(pSucc, pCurr->GetNext());

    // Move instruction after pInsn to pSucc
    {
        Instruction* pRunner = pInsn->GetNext();
        Instruction* pLast =   pCurr->GetLastInsn();

        for (;;)
        {
            Instruction* pNext = pRunner->GetNext();

            pRunner->Unlink_();
            pRunner->SetParent(pSucc);
            static_cast<BBlock::InsnList*>(pSucc)->Append_(pRunner);

            if (pRunner == pLast)
            {
                break;
            }

            pRunner = pNext;
        } // while
    }

    // Move out edegs
    {
        BBlock::EnumOutEdge oEnum(pCurr);
        while (! oEnum.AtEnd())
        {
            CfgEdge* pEdge = oEnum.Get();
            oEnum.Next();

            // BUGBUG: NYI: We have to handle nonlocal edge.

            // Redirect From
            pCurr->UnlinkOutEdge(pEdge);
            pEdge->SetFrom(pSucc);
            pSucc->LinkOutEdge(pEdge);

            ReplacePhiOperands(pSucc, pCurr, pEdge->GetTo());
        } // for each out edge
    }

    pCurr->GetParent()->SetCfgChanged();

    return pSucc;
} // ir_split_bblock_after


//////////////////////////////////////////////////////////////////////
//
//  SplitCfgEdge
//
//  Description:
//   Inserts new bblock between predecessor and successor of specified
//  edge and returns new bbock.
//
//      +-------------------+           +-------------------+
//      |                   |           |                   |
//      | pEdge->GetFrom()  |           | pEdge->GetFrom()  |
//      |                   |           |                   |
//      +-------------------+           +-------------------+
//              |                                |
//              V                                V (moved edge)
//      +-------------------+           +-------------------+
//      |                   |           |                   |
//      | pEdge->GetTo()    |           | pCurr             |
//      |                   |           |                   |
//      +-------------------+           +-------------------+
//                                               |
//                                               V (new edge)
//                                      +-------------------+
//                                      |                   |
//                                      | pEdge->GetTo()    |
//                                      |                   |
//                                      +-------------------+
//
BBlock*
SplitCfgEdge(CfgEdge* pEdge)
{
    ASSERT(NULL != pEdge);

    BBlock* pPred = pEdge->GetFrom();
    BBlock* pSucc = pEdge->GetTo();
    BBlock* pCurr = new BBlock(BBlock::Kind_Normal);

    // Redirect To
    pSucc->UnlinkInEdge(pEdge);
    pEdge->SetTo(pCurr);
    pCurr->LinkInEdge(pEdge);

    ReplacePhiOperands(pCurr, pPred, pSucc);


    // Replace label operand
    pPred->GetLastInsn()->ReplaceLabelOperand(pCurr, pSucc);

    InsertBBlock(pCurr, pSucc);
    pCurr->AppendInsn(new JumpInsn(pSucc));

    return pCurr;
} // SplitCfgEdge

} // Compiler
