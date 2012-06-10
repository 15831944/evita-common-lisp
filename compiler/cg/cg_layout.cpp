#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - Layout
// compiler/cg/cg_layout.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_layout.cpp#4 $
//
// Description:
// Simple layout method:
//    Classifies basic blocks by last instruction and successors into
//    following four classes:
//      o normal(0)        - block ends with JUMP or BRANCH.
//      o return(1)        - block ends with RET.
//      o nonlocal exit point (2) - block has only nonlocal in-edge.
//      o terminate(3)     - block ends with UNREACHABLE.
// 
//    When a block ends with JUMP, its class is a class of successor.
//    When a block ends with BRANCH, its class is determined by successors.
//
//
#include "./cg_defs.h"

#include "../ir/ir_defs.h"
#include "../ir/ir_function.h"
#include "../ir/ir_pass.h"

namespace Compiler
{

namespace
{

//////////////////////////////////////////////////////////////////////
//
// Layouter
//
class Layouter : public FunctionPass
{
    public: Layouter() : FunctionPass(L"CG-LAYOUT") {}

    protected: virtual void process_function(Function* pFun)
    {
        ASSERT(NULL != pFun);

        html_log_format(1, L"<h2>process ~S</h2>~%", pFun);

        BBlock* pEntryBB = pFun->GetEntryBB();
        BBlock* pExitBB  = pFun->GetExitBB();

        // Remove all basic blocks except for entry and exit.
        foreach (Function::EnumBBlock_Postorder, oEnum, pFun)
        {
            BBlock* pCurr = oEnum.Get();
            if (pCurr == pEntryBB || pCurr == pExitBB)
            {
                continue;
            }

            static_cast<BBlock::LayoutSite*>(pCurr)->Unlink_();
            pCurr->SetFlag(computeSimpleLayoutOrder(pCurr));

            html_log_format(2, L"~S [~D] ~S~:%",
                pCurr,
                pCurr->GetFlag(),
                pCurr->GetLastInsn() );
        } // for each bblock

        for (int iOrder = 0; iOrder < 4; iOrder++)
        {
            foreach (Function::EnumBBlock_Reverse_Postorder, oEnum, pFun)
            {
                BBlock* pCurr = oEnum.Get();

                if (pCurr == pEntryBB || pCurr == pExitBB)
                {
                    continue;
                }

                if (pCurr->GetFlag() == iOrder)
                {
                    pCurr->SetFlag(0);
                    static_cast<BBlock::LayoutSite*>(pExitBB)->Link_(pCurr);
                }
            } // for each bblock
        } // for iOrder
    } // process_simple

    protected: int computeSimpleLayoutOrder(BBlock* pCurr)
    {
        ASSERT(NULL != pCurr);

        if (! pCurr->HasMoreThanOnePred())
        {
            BBlock::EnumInEdge oEnum(pCurr);
            if (oEnum.Get()->GetKind() == CfgEdge::Kind_Nonlocal)
            {
                return 2;
            }
        }


        Instruction* pLast = pCurr->GetLastInsn();

        if (pLast->Is<JumpInsn>())
        {
            BBlock* pTargetBB =
                pLast->GetSx()->StaticCast<Label>()->GetBBlock();

            return 0 != pTargetBB->GetFlag() ? pTargetBB->GetFlag() : 0;
        }

        if (pLast->Is<BranchInsn>())
        {
            int iTrue =
                pLast->GetSy()->StaticCast<Label>()->GetBBlock()->GetFlag();

            int iFalse =
                pLast->GetSz()->StaticCast<Label>()->GetBBlock()->GetFlag();

            return iTrue < iFalse ? iTrue : iFalse;
        }

        if (pLast->Is<RetInsn>())
        {
            return 1;
        }

        if (pLast->Is<UnreachableInsn>())
        {
            return 3;
        }

        CAN_NOT_HAPPEN();
    } // computeSimpleLayoutOrder

}; // Layouter

} // namespace



//////////////////////////////////////////////////////////////////////
//
// Code Generator Entry Point
//
void
cg_layout_bblocks()
{
    Layouter oPass;
    oPass.Run();
} // CodeGenerator::layoutBBlocks

} // Compiler
