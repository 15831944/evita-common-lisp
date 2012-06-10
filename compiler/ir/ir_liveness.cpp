#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR - Liveness Analyzer
// ir_livness.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_liveness.cpp#5 $
//
#include "./ir_defs.h"
#include "./ir_dfa.h"
#include "./ir_function.h"
#include "./ir_fns.h"

#include "../cm/cm_pass.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Compute Liveness
//
// Assumption:
//  There is no unreachable bblock.
//
class LivenessAnalyzer
{
    private: WorkList_<Register> m_oRegs;

    // Run
    public: void Run(Function* pFun)
    {
        uint cRegs = 0;

        foreach (Function::EnumReg, oEnum, pFun)
        {
            Register* pRd = oEnum.Get();
            pRd->ResetIndex();

            if (pRd->IsPseudo()) continue;

            if (! m_oRegs.Has(pRd))
            {
                pRd->SetIndex(cRegs);
                cRegs += 1;

                m_oRegs.Push(pRd);
            }
        } // for each reg

        if (0 == cRegs)
        {
            foreach (Function::EnumBBlock, oEnum, pFun)
            {
                DataFlowBB* pBBlock = oEnum.Get()->Extend<DataFlowBB>();
                pBBlock->InitDataFlow(Session::Get(), 0);
            } // for each bblock
            return;
        }

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            DataFlowBB* pBBlock = oEnum.Get()->Extend<DataFlowBB>();
            pBBlock->InitDataFlow(Session::Get(), cRegs);
            computeLocal(pBBlock);
        } // for each bblock

        dumpLiveness(pFun);

        SolveBackward(pFun);

        dumpLiveness(pFun);

        unless (0 == pFun->GetEntryBB()->Extend<DataFlowBB>()->
                    GetIn()->CountOne() )
        {
            warn(L"Livness: LiveIn(Entry) of ~S isn't empty.", 
                pFun->GetName() );
            return;
        }

        unless (0 == pFun->GetExitBB()->Extend<DataFlowBB>()->
                    GetIn()->CountOne() )
        {
            warn(L"Livness: LiveIn(Exit) of ~S isn't empty.", 
                pFun->GetName() );
            return;
        }

        unless (0 == pFun->GetExitBB()->Extend<DataFlowBB>()->
                    GetOut()->CountOne() )
        {
            warn(L"Livness: LiveOut(Exit) of ~S isn't empty.", 
                pFun->GetName() );
            return;
        }
    } // Run

    // computeLocal
    //  Computes VarKill[b] and UEVar[b] for specified bblock.
    //  Note: We ignore PHI instruction.
    void computeLocal(DataFlowBB* pBBlock)
    {
        ASSERT(NULL != pBBlock);

        foreach (BBlock::EnumInsn, oEnum, pBBlock)
        {
            Instruction* pInsn = oEnum.Get();

            if (! pInsn->Is<PhiInsn>())
            {
                foreach (Instruction::EnumInput, oEnum, pInsn)
                {
                    Register* pRx = oEnum.GetReg();
                    if (NULL != pRx && pRx->HasIndex())
                    {
                        if (! pBBlock->IsKill(pRx->GetIndex()))
                        {
                            pBBlock->SetIn(pRx->GetIndex());
                        }
                    }
                } // for each operand
            } // if

            Register* pRd = pInsn->GetRd();
            if (NULL != pRd && pRd->HasIndex())
            {
                pBBlock->SetKill(pRd->GetIndex());
            }
        } // for each instruction
    } // computeLocal

    // dumpLiveness
    void dumpLiveness(Function* pFun)
    {
        Val stream = Session::GetPass()->GetLogStream(3);
        if (! streamp(stream)) return;

        log_format(3, L"<h3>Liveness of ~S</h3>~%", pFun->GetName());

        log_format(3, L"<table border='1'>");
        log_format(3, L"<tr>~%");
            log_format(3, L"<th>BB</th>~%");
            log_format(3, L"<th>Pred</th>~%");
            log_format(3, L"<th>Succ</th>~%");
            log_format(3, L"<th>In</th>~%");
            log_format(3, L"<th>Out</th>~%");
            log_format(3, L"<th>Kill</th>~%");
        log_format(3, L"</tr>~%");

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            DataFlowBB* pBBlock = oEnum.Get()->Extend<DataFlowBB>();

            html_log_format(3, L"<tr><td>~S</td>~%", pBBlock);

            log_format(3, L"<td>");
                foreach (BBlock::EnumInEdge, oEnum, pBBlock)
                {
                    html_log_format(3, L" ~S", oEnum.GetNode());
                } // for each pred
            log_format(3, L"</td>~%");

            log_format(3, L"<td>");
                foreach (BBlock::EnumOutEdge, oEnum, pBBlock)
                {
                    html_log_format(3, L" ~S", oEnum.GetNode());
                } // for each pred
            log_format(3, L"</td>~%");

            log_format(3, L"<td>");
                dumpBitVec(pBBlock->GetIn());
            log_format(3, L"</td>~%");

            log_format(3, L"<td>");
                dumpBitVec(pBBlock->GetOut());
            log_format(3, L"</td>~%");

            #if 0
                log_format(3, L"<td>");
                    dumpBitVec(pBBlock->GetKill());
                log_format(3, L"</td>~%");
            #endif

            log_format(3, L"</tr>~2%");
        } // for each bblock
        log_format(3, L"</table>~%");
    } // dumpLiveness

    // dumpBitVec
    void dumpBitVec(const BitVec* pBitVec)
    {
        foreach (WorkList_<Register>::Enum, oEnum, &m_oRegs)
        {
            Register* pRd = oEnum.Get();

            if (pRd->GetRep() != pRd)
            {
                continue;
            }

            if (pBitVec->IsOne(pRd->GetIndex()))
            {
                html_log_format(3, L" ~S", pRd);
            }
        } // for each reg
    } // dumpBitVec
}; // LivenessAnalyzer


//////////////////////////////////////////////////////////////////////
//
// Compute Liveness
//
void
ComputeLiveness(Function* pFunction)
{
    ASSERT(NULL != pFunction);

    LivenessAnalyzer oAnalizer;
    oAnalizer.Run(pFunction);
} // ComputeLiveness

} // Compiler
