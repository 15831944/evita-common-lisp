#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - SSA to CFG
// cg/cg_ssa2cfg.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_ssa2cfg.cpp#6 $
//
// Description:
//  This file contains SSA to CFG transformation phase. This phase transforms
//  PHI instruction into COPY and PHI-COPY instructions for Register.
//
//  We keep PHI instrucitons for Values.
//
//
#include "./cg_defs.h"

#include "../ir/ir_defs.h"
#include "../ir/ir_function.h"
#include "../ir/ir_fns.h"
#include "../ir/ir_pass.h"

#include "./cg_instruction.h"

namespace Compiler
{

namespace
{

//////////////////////////////////////////////////////////////////////
//
// Converter
//
class Converter : public FunctionPass
{
    public: Converter() : FunctionPass(L"CG-SSA2CFG") {}

    protected: WorkList_<Instruction> m_oPhiInsns;

    ////////////////////////////////////////////////////////////
    //
    // RegExt
    //
    protected: class RegExt : public Atom
    {
        public: Register* m_pMap;
        public: bool      m_fUsedByAnother;

        public: RegExt(Register* pRx) :
            m_pMap(NULL),
            m_fUsedByAnother(false)
        {
            pRx->SetExtension<RegExt>(this);
        } // RegExt
    }; // RegExt

    protected: virtual void process_function(Function*);

    void prepare_bblock(BBlock*);
    void process_bblock(BBlock*);
    void schedule_copies(BBlock*);

    bool live_out_p(Register*, BBlock*);
}; // Converter


bool
Converter::live_out_p(Register* pRd, BBlock* pBBlock)
{
    return pRd->GetDfn()->GetBBlock()->DoesDominate(pBBlock);
} // Converter::live_out_p


//////////////////////////////////////////////////////////////////////
//
// prepare_bblock
//
void
Converter::prepare_bblock(BBlock* pCurr)
{
    ASSERT(NULL != pCurr);

    {
        // Checks pCurr has PHI instructions for Register.
        BBlock::EnumInsn oEnum(pCurr);

        while (! oEnum.AtEnd())
        {
            PhiInsn* pPhi = oEnum.Get()->DynamicCast<PhiInsn>();
                if (NULL == pPhi) break;
                oEnum.Next();

            if (NULL != pPhi->GetRd())
            {
                m_oPhiInsns.Push(pPhi);
            }
        } // for each PHI

        while (! oEnum.AtEnd())
        {
            SigmaInsn* pSigma = oEnum.Get()->DynamicCast<SigmaInsn>();
                oEnum.Next();
                if (NULL == pSigma) continue;
            ir_replace_all_users(pSigma->GetSx(), pSigma->GetRd());
            ir_remove_insn(pSigma);
        } // for each insn
    }

    // Splits critical edge
    {
        BBlock::EnumInEdge oEnum(pCurr);
        while (! oEnum.AtEnd())
        {
            CfgEdge* pEdge = oEnum.Get();
            oEnum.Next();

            if (pEdge->IsPseudo()) break;
            if (pEdge->IsNonlocal()) break;

            // Note: we should not split a critical edge if that edge is
            // backward.
            if (pEdge->GetFrom()->HasMoreThanOneSucc() &&
                pEdge->GetTo()->HasMoreThanOnePred() )
            {
                html_log_format(2, L"split critical edge ~S~:%", pEdge);

                SplitCfgEdge(pEdge);

                html_log_format(3, L"~S~:%", pEdge);
            }
        } // for each in-edge
    }
} // Converter::prepare_bblock


//////////////////////////////////////////////////////////////////////
//
// process_bblock
//
//  1. Collects Phi output from all succsessors into copy task list.
//  2. Set up list work list of initial copies
//  3. Iterate over the worklist, inserting copies
//
void
Converter::process_bblock(BBlock* pCurr)
{
    ASSERT(NULL != pCurr);

    html_log_format(3, L"process ~S~%", pCurr);

    schedule_copies(pCurr);

    foreach (BBlock::EnumChild, oEnum, pCurr)
    {
        process_bblock(oEnum.Get());
    } // for each child
} // Converter::process_bblock(BBlock* pCurr)


//////////////////////////////////////////////////////////////////////
//
// Converter::schedule_copies
//
//   We also transform Values Phi instruction with single value input.
//      BB5: PHI ty %vd <= (BB3 %rx) (BB4 %vy)
//    ==>
//      BB3: VALUES ty %vx <= %rx
//           JUMP BB5
//      BB5: PHI ty %vd <= (BB3 %vx) (BB4 %vy)
//
void
Converter::schedule_copies(BBlock* pCurr)
{
    ASSERT(NULL != pCurr);

    CopyTask::WorkList oCopyTasks;

    Instruction* pLast = pCurr->GetLastInsn();

    html_log_format(4, L"<ul>");

    foreach (BBlock::EnumOutEdge, oEnum, pCurr)
    {
        BBlock* pSucc = oEnum.GetNode();
        foreach (BBlock::EnumInsn, oEnum, pSucc)
        {
            PhiInsn* pPhi = oEnum.Get()->DynamicCast<PhiInsn>();

            if (NULL == pPhi) break; // No more PHI instructions.

            OperandBox* pBox = pPhi->GetInputBox(pCurr);
            Operand* pSx = pBox->GetOperand();

            if (NULL != pPhi->GetVd())
            {
                if (! pSx->Is<Values>())
                {
                    // Single value input for values Phi.
                    // Inserts VALUES instruction, and
                    // Updates PHI input
                    Values* pVx = new Values();
                    pLast = ir_insert_insn(new ValuesInsn(pVx, pSx), pLast);
                    pBox->Replace(pVx);
                } // if
                continue;
            }

            Register* pRd = pPhi->GetRd();

            if (NULL == pRd) continue;

            if (NULL == pRd->GetExtension<RegExt>()) new RegExt(pRd);

            pRd->MarkNotSSA();

            if (! pSx->Is<Register>())
            {
                html_log_format(4, L"<li>literal ~S</li>~%", pRd);

                pLast = ir_insert_insn(
                    new PhiCopyInsn(pPhi->GetTy(), pRd, pSx),
                    pLast );
            }
            else if (pRd != pSx)
            {
                Register* pRx = pSx->StaticCast<Register>();
                oCopyTasks.Push(new CopyTask(pPhi->GetTy(), pRd, pRx));

                if (NULL == pRx->GetExtension<RegExt>()) new RegExt(pRx);

                html_log_format(4, L"<li>copy ~S &lt;= ~S</li>~%", pRd, pRx);

                pRd->GetExtension<RegExt>()->m_pMap = pRd;
                pRx->GetExtension<RegExt>()->m_pMap = pRx;
                pRx->GetExtension<RegExt>()->m_fUsedByAnother = true;
            }
        } // for each PhiInsn
    } // for each succ

    html_log_format(4, L"</ul>~%");

    CopyTask::WorkList oPendings;
    CopyTask::WorkList oReadies;

    // Pass 2: Set up work list of initial copies
    {
        while (! oCopyTasks.IsEmpty())
        {
            CopyTask* pTask = oCopyTasks.Pop();
            Register* pRd = pTask->m_pRd;
            if (! pRd->GetExtension<RegExt>()->m_fUsedByAnother)
            {
                oReadies.Push(pTask);
            }
            else
            {
                oPendings.Push(pTask);
            }
        } // while
    } // Pass 2

    // Pass 3: Iterate over the worklist, inserting copies
    {
        CopyTask::WorkList  oTemps;
        CopyTask::WorkList* pReadies  = &oReadies;
        CopyTask::WorkList* pPendings = &oPendings;
        CopyTask::WorkList* pTemps    = &oTemps;

        for (;;)
        {
            while (! pReadies->IsEmpty())
            {
                CopyTask* pTask = pReadies->Pop();
                Register* pRd = pTask->m_pRd;
                Register* pRx = pTask->m_pRx;

                // Resolution of [Lost-Copy]:
                //  When pRd is live-out from pCurr. To avoid lost-copy,
                //  we make copy of pRd.
                //
                //  When pRd is used in children, we make copy of pRd and
                //  rewrite use sites in children.
                if (live_out_p(pRd, pCurr))
                {
                    Register::Enum oEnum(pRd);
                    Register* pRt = NULL;
                    while (! oEnum.AtEnd())
                    {
                        OperandBox* pBox = oEnum.Get();
                        oEnum.Next();

                        BBlock* pBBlock = pBox->GetInstruction()->GetBBlock();
                        if (pCurr != pBBlock && pCurr->DoesDominate(pBBlock))
                        {
                            if (NULL == pRt)
                            {
                                pRt = new Register();

                                ir_insert_insn(
                                    new CopyInsn(pRt, pRd),
                                    pLast );

                                html_log_format(5,
                                    L"~S [lost-copy]~:%",
                                    pLast );
                            } // if

                            pBox->Replace(pRt);
                        } // if
                    } // while
                } // if live_out_p

                html_log_format(5, L"(phi-copy ~S &lt;= ~S)~:%",
                    pRd,
                    pRx->GetExtension<RegExt>()->m_pMap );

                ir_insert_insn(
                    new PhiCopyInsn(
                        pTask->m_ty,
                        pRd,
                        pRx->GetExtension<RegExt>()->m_pMap ),
                    pLast );

                pRx->GetExtension<RegExt>()->m_pMap = pRd;

                pRd->GetExtension<RegExt>()->m_fUsedByAnother = false;
                pRx->GetExtension<RegExt>()->m_fUsedByAnother = false;

                ASSERT(pTemps->IsEmpty());
                while (! pPendings->IsEmpty())
                {
                    CopyTask* pTask = pPendings->Pop();
                    if (pTask->m_pRd == pRx)
                    {
                        pReadies->Push(pTask);
                    }
                    else
                    {
                        pTemps->Push(pTask);
                    }
                } // while

                swap(pPendings, pTemps);
            } // while ready

            if (pPendings->IsEmpty()) break;

            // Free %rd
            {
                CopyTask* pTask = pPendings->Pop();

                Register* pRd = pTask->m_pRd;
                Register* pRt = new Register();

                html_log_format(5, L"(phi-temp ~S &lt;= ~S)~:%", pRt, pRd);

                ir_insert_insn(
                    new PhiCopyInsn(pTask->m_ty, pRt, pRd),
                    pLast );

                pRd->GetExtension<RegExt>()->m_pMap = pRt;

                pReadies->Push(pTask);
            }
        } // for
    } // Pass 3
} // Converter::process_bblock


//////////////////////////////////////////////////////////////////////
//
// process_function
//
void
Converter::process_function(Function* pFun)
{
    ASSERT(NULL != pFun);

    pFun->PrepareTraversal();
    Function::EnumBBlock oEnum(pFun);
    while (! oEnum.AtEnd())
    {
        BBlock* pCurr = oEnum.Get();
        oEnum.Next();
        prepare_bblock(pCurr);
    } // while

    ComputeDominance(pFun);

    process_bblock(pFun->GetEntryBB());

    // Remove Phi instructions
    {
        WorkList_<Instruction>* pPhiInsns = &m_oPhiInsns;
        while (! pPhiInsns->IsEmpty())
        {
            Instruction* pPhiInsn = pPhiInsns->Pop();
            ir_remove_insn(pPhiInsn);
        } // while
    }
} // Converter::process_function

} // namespace


//////////////////////////////////////////////////////////////////////
//
// Code Generator Entry Point
//
void
cg_convert_SSA_to_CFG()
{
    Converter oPass;
    oPass.Run();
} // cg_convert_SSA_to_CFG

} // Compiler
