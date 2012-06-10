#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - Compiler Base
// cm_base.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_dfa.cpp#4 $
//

#include "./ir_defs.h"
#include "./ir_dfa.h"

#include "./ir_function.h"

#include "../cm/cm_pass.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Solve Data Flow Equation Backward
//
//    LiveIn[b]  = UEVar[b] union (LiveLiveOut[b] - VarKill[b])
//    LiveLiveOut[b] = union(s = succs) LiveIn[S]
//
// Note: We use LiveKill[exit] for work bitvec.
// Note: All bblock in pFun must be reachable.
//
void SolveBackward(Function* pFun)
{
    ASSERT(NULL != pFun);

    Timee oTimee(L"DFA.SolveBackward");

    BitVec* pWork = pFun->GetExitBB()->Extend<DataFlowBB>()->GetKill();

    bool fChanged = true;
    while (fChanged)
    {
        fChanged = false;

        // As of EAC, p466, we should use reverse preorder (RPO of
        // reverse CFG), instead of Postorder.
        foreach (Function::EnumBBlock_Reverse_Preorder, oEnum, pFun)
        {
            DataFlowBB* pCurr = oEnum.Get()->Extend<DataFlowBB>();

            ASSERT(pCurr->GetPreorder() >= 1);

            // LiveOut[b] = LiveOut[b] + LiveIn[s : succs]
            foreach (BBlock::EnumOutEdge, oEnum, pCurr)
            {
                DataFlowBB* pSucc = oEnum.GetNode()->Extend<DataFlowBB>();

                pCurr->GetOut()->Ior(pSucc->GetIn());
            } // for each succ

            // LiveIn[b] = (LiveOut[b] - VarKill[b]) + UEVar[b]
            pWork->Copy(pCurr->GetOut());
            pWork->AndC2(pCurr->GetKill());
            pWork->Ior(pCurr->GetIn());

            if (! pCurr->GetIn()->Equal(pWork))
            {
                pWork = pCurr->SetIn(pWork);
                fChanged = true;
            }
        } // for each bblock
    } // while

    pWork->FillZero();
    pFun->GetExitBB()->Extend<DataFlowBB>()->SetKill(pWork);
} // SolveBackward

} // Compiler
