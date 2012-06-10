#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - opt - Clean Phase
// compiler/opt/opt_clean.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/opt/opt_clean.cpp#13 $
//
//
// Description:
//  TBD
#include "./opt_defs.h"

#include "../ir/ir_bblock.h"
#include "../ir/ir_instruction.h"
#include "../ir/ir_pass.h"

namespace Compiler
{

namespace
{
//////////////////////////////////////////////////////////////////////
//
// Clean Optimizer
//
class CleanPass : public FunctionPass
{
    public: CleanPass() : FunctionPass(L"OPT-CLEAN") {}

    // process_function
    virtual void process_function(Function* pFun)
    {
        ASSERT(NULL != pFun);

        uint nCount = 0;
        for  (;;)
        {
            bool fChanged = false;

            nCount += 1;

            html_log_format(1, L"<h2>[~D] clean function ~S</h2>~%",
                nCount, pFun );

            ir_eliminate_unreachable_bblocks(pFun);

            foreach (Function::EnumBBlock_Postorder, oEnum, pFun)
            {
                BBlock* pCurr = oEnum.Get();
                if (pFun->GetEntryBB() == pCurr)
                {
                    continue;
                }

                if (process_bblock(pCurr)) fChanged = true;
            } // for each bblock

            if (! fChanged) break;
        } // for

        if (nCount >= 2)
        {
            ir_remove_useless_instructions(pFun);
        }
    } // process_function

    // process_bblock
    static bool process_bblock(BBlock* pCurr)
    {
        ASSERT(NULL != pCurr);

        bool fChanged = merge_tails(pCurr);

        Instruction* pLast = pCurr->GetLastInsn();

        if (pLast->Is<BranchInsn>())
        {
            if (process_BRANCH(pLast->StaticCast<BranchInsn>()))
            {
                fChanged = true;
            }
        }
        else if (pLast->Is<JumpInsn>())
        {
            if (process_JUMP(pLast->StaticCast<JumpInsn>()))
            {
                fChanged = true;
            }
        }

        return fChanged;
    } // process_bblock

    // process_BRANCH
    static bool process_BRANCH(BranchInsn* pBranch)
    {
        BBlock* pTrueBB  = pBranch->GetTrue();
        BBlock* pFalseBB = pBranch->GetFalse();

        if (pBranch->GetSx() == Bool_True)
        {
            add_PHI_inputs(
                pBranch->GetBBlock(),
                pBranch->GetBBlock(),
                pTrueBB );

            pFalseBB = pTrueBB;
        }
        else if (pBranch->GetSx() == Bool_False)
        {
            add_PHI_inputs(
                pBranch->GetBBlock(),
                pBranch->GetBBlock(),
                pFalseBB );

            pTrueBB = pFalseBB;
        }

        if (pTrueBB == pFalseBB)
        {
            // fold Branch
            html_log_format(2, L"<h3>fold ~S ~S</h3>~%",
                pBranch->GetBBlock(),
                pBranch );

            JumpInsn* pJump = new JumpInsn(pTrueBB);
            ir_replace_insn(pJump, pBranch);
            process_JUMP(pJump);
            return true;
        } // if

        if (process_BRANCH_PHI(pBranch)) return true;

        return false;
    } // process_BRANCH

    // process_BRANCH_PHI
    //  BB1: BRANCH %b1 BB2 BB3
    //  BB2: JUMP BB4
    //  BB3: JUMP BB4
    //  BB4: PHI %r2 <- (BB2 %r3) (BB3 %r4)
    //       ...
    //  ==>
    //  BB1: SELECT %r2 <- %b1 %r3 %r4
    //       ...
    //  BB1: JUMP BB4
    //  BB4: ...
    //
    //  Branch targets must have only one in-edge.
    //
    static bool process_BRANCH_PHI(BranchInsn* pBranch)
    {
        BBlock* pTrue = pBranch->GetTrue();
        JumpInsn* pJumpT = pTrue->GetFirstInsn()->DynamicCast<JumpInsn>();
            if (NULL == pJumpT) return false;
            if (pTrue->HasMoreThanOnePred()) return false;

        BBlock* pFalse = pBranch->GetFalse();
        JumpInsn* pJumpF = pFalse->GetFirstInsn()->DynamicCast<JumpInsn>();
            if (NULL == pJumpF) return false;
            if (pFalse->HasMoreThanOnePred()) return false;

        BBlock* pBBlock = pJumpT->GetTarget();
        if (pJumpF->GetTarget() != pBBlock) return false;

        if (pBBlock->CountInEdge() != 2) return false;

        if (! pBBlock->GetFirstInsn()->Is<PhiInsn>()) return false;

        html_log_format(2, L"<h3>fold BRANCH+PHI ~S:~S</h3>~%",
            pBranch->GetBBlock(), pBranch );

        Bool* pBx = pBranch->GetBx();

        for (;;)
        {
            PhiInsn* pPhi = pBBlock->GetFirstInsn()->DynamicCast<PhiInsn>();
                if (NULL == pPhi) break;

            ir_insert_insn(
                new SelectInsn(pPhi->GetOutput(), pBx,
                    pPhi->GetInput(pTrue),
                    pPhi->GetInput(pFalse) ),
                pBranch );

            ir_remove_insn(pPhi);
        } // for each PHI

        JumpInsn* pJump = new JumpInsn(pBBlock);
        ir_replace_insn(pJump, pBranch);
        process_JUMP(pJump);

        return true;
    } // process_BRANCH_PHI

    // process_JUMP
    static bool process_JUMP(JumpInsn* pJump)
    {
        ASSERT(NULL != pJump);

        BBlock* pCurr = pJump->GetBBlock();

        if (pCurr->HasMoreThanOneSucc()) return false;  // OPENxxx

        if (process_JUMP_empty(pJump))      return true;
        if (process_JUMP_coalesce(pJump))   return true;
        if (process_JUMP_BRANCH(pJump))     return true;
        if (process_JUMP_PHI_RET(pJump))    return true;
        if (process_JUMP_PHI_BRANCH(pJump)) return true;

        return false;
    } // process_JUMP

    // add_PHI_inputs
    //  Adds phi operand for pNewBB with %sx of pOldBB.
    static void add_PHI_inputs(
        BBlock* pNewBB,
        BBlock* pOldBB,
        BBlock* pCurr )
    {
        html_log_format(4, L"add phi operands: new=~S old ~S at ~S.~:%",
            pNewBB, pOldBB, pCurr );

        foreach (BBlock::EnumInsn, oEnum, pCurr)
        {
            PhiInsn* pPhiInsn = oEnum.Get()->DynamicCast<PhiInsn>();
                if (NULL == pPhiInsn) break;

            pPhiInsn->AddInput(pNewBB, pPhiInsn->GetInput(pOldBB));
        } // for each insn
    } // add_PHI_inputs

    // merge_tails
    static bool merge_tails(BBlock* pCurr)
    {
        uint cPreds = 0;
        foreach (BBlock::EnumInEdge, oEnum, pCurr)
        {
            BBlock* pPred = oEnum.GetNode();
            if (! pPred->GetLastInsn()->Is<JumpInsn>())
            {
                return false;
            }

            if (pPred->GetFirstInsn()->Is<JumpInsn>())
            {
                // Empty block
                return false;
            }

            if (pPred->HasMoreThanOneSucc())
            {
                return false;
            }

            cPreds += 1;
        } // for each in-edge

        if (cPreds <= 1) return false;

        Instruction* pRef = pCurr->GetFirstInsn();
        while (pRef->Is<PhiInsn>())
        {
            pRef = pRef->GetNext();
        } // while

        bool fChanged = false;

        for (;;)
        {
            Instruction* pTail = NULL;
            foreach (BBlock::EnumInEdge, oEnum, pCurr)
            {
                BBlock* pPred = oEnum.GetNode();

                if (pPred->GetLastInsn() == pPred->GetFirstInsn())
                {
                    return fChanged;
                }

                Instruction* pLast = pPred->GetLastInsn()->GetPrev();
                if (NULL == pTail)
                {
                    pTail = pLast;
                }
                else if (! pTail->Identical(pLast))
                {
                    return fChanged;
                }
            } // for each pred

            ASSERT(NULL != pTail);

            if (! fChanged)
            {
                fChanged = true;
                html_log_format(3, L"<h3>Tail merge: ~S</h3>~%", pCurr);
            }

            pRef = ir_move_insn(pTail, pRef);

            {
                BBlock::EnumInEdge oEnum(pCurr);
                    oEnum.Next();
                while (! oEnum.AtEnd())
                {
                    BBlock* pPred = oEnum.GetNode();
                    Instruction* pLast = pPred->GetLastInsn()->GetPrev();
                    ir_remove_insn(pLast);
                    oEnum.Next();
                } // for each pred
            }
        } // for
    } // merge_tails

    // move_labels
    static void move_labels(BBlock* pTo, BBlock* pFrom)
    {
        for (;;)
        {
            BBlock::EnumLabel oEnum(pFrom);
            if (oEnum.AtEnd()) break;
            Label* pLabel = oEnum.Get();
            pLabel->SetBBlock(pTo);
            pLabel->Unlink_();
            pTo->AppendLabel(pLabel);
        } // for
    } // move_labels

    // process_JUMP_coalesce
    //  Coalesces curr and succ where succ has only one predecessor.
    static bool process_JUMP_coalesce(JumpInsn* pJump)
    {
        BBlock* pCurr = pJump->GetBBlock();
        BBlock* pSucc = pJump->GetTarget();

        if (! pSucc->HasOnlyOnePred()) return false;

        html_log_format(2, L"<h3>coalesce ~S <= ~S</h3>~%", pCurr, pSucc);

        // Fold PHI in succ
        for (;;)
        {
            PhiInsn* pPhiInsn = pSucc->GetFirstInsn()->DynamicCast<PhiInsn>();
            if (NULL == pPhiInsn) break;
            ir_replace_all_users(pPhiInsn->GetInput(pCurr), pPhiInsn->GetOutput());
            ir_remove_insn(pPhiInsn);
        } // for

        // Move all out-edges from succ to curr
        {
            BBlock::EnumOutEdge oEnum(pSucc);
            while (! oEnum.AtEnd())
            {
                CfgEdge* pEdge = oEnum.Get();
                    oEnum.Next();

                ir_replace_cfg_edge_from(pEdge, pCurr);
            } // while
        }

        // Move instructions from succ to curr

        // BUGBUG: We should connect curr tail and succ head instead move
        // all instructions.
        ir_remove_insn(pCurr->GetLastInsn());
        while (pSucc->HasInsn())
        {
            Instruction* pInsn = pSucc->GetFirstInsn();
            pInsn->Unlink_();
            pInsn->SetParent(pCurr);
            static_cast<BBlock::InsnList*>(pCurr)->Append_(pInsn);
        } // while

        move_labels(pCurr, pSucc);
        ir_remove_bblock(pSucc);

        return true;
    } // process_JUMP_coalesce

    // process_JUMP_BRANCH
    //  Hoists branch in succ to curr.
    static bool process_JUMP_BRANCH(JumpInsn* pJump)
    {
        BBlock* pCurr = pJump->GetBBlock();
        BBlock* pSucc = pJump->GetTarget();

        if (pSucc->HasInsn()) return false;
        if (! pSucc->GetLastInsn()->Is<BranchInsn>()) return false;

        html_log_format(2, L"<h3>hoist/BRANCH ~S &lt;= ~S</h3>~%",
            pCurr, pSucc );

        BranchInsn* pBranch = pSucc->GetLastInsn()->StaticCast<BranchInsn>();
        BBlock* pTrueBB  = pBranch->GetTrue();
        BBlock* pFalseBB = pBranch->GetFalse();

        ir_replace_insn(
            new BranchInsn(
                pBranch->GetSx()->StaticCast<Bool>(),
                pTrueBB,
                pFalseBB ),
            pCurr->GetLastInsn());

        add_PHI_inputs(pCurr, pSucc, pTrueBB);
        add_PHI_inputs(pCurr, pSucc, pFalseBB);

        return true;
    } // process_JUMP_BRANCH

    // process_JUMP_empty
    //  Remove empty basic block curr.
    //    o Remove edge curr->succ
    //    o Redicte edge pred->curr to pred-succ
    static bool process_JUMP_empty(JumpInsn* pJump)
    {
        BBlock* pCurr = pJump->GetBBlock();
        BBlock* pSucc = pJump->GetTarget();

        if (pCurr->GetFirstInsn() != pJump) return false;
        if (pSucc->GetFirstInsn()->Is<PhiInsn>()) return false;
        if (pCurr == pSucc) return false; // for (defun foo () (foo))

        html_log_format(2, L"<h3>remove empty ~S => ~S</h2>~%", pCurr, pSucc);

        // Rediect pred->curr to pred->succ
        {
            BBlock::EnumInEdge oEnum(pCurr);
            while (! oEnum.AtEnd())
            {
                CfgEdge* pEdge = oEnum.Get();
                    oEnum.Next();

                ir_replace_cfg_edge_to(pEdge, pSucc);
            } // while
        }

        move_labels(pSucc, pCurr);
        ir_remove_bblock(pCurr);

        return true;
    } // process_JUMP_empty

    // process_JUMP_PHI_BRANCH
    //  from format-roman-aux
    //      BB11:0: (JUMP BB12)
    //
    //      BB12:0: (PHI T %r20 <= (BB13 %r21) (BB11 'NIL))
    //      BB12:0: (EQ BOOL %b35 <- %r20 'NIL)
    //      BB12:0: (BRANCH %b35 BB19 BB20)
    //    ==>
    //      BB11:0: (EQ BOOL %b36 <- 'NIL 'NIL)
    //      BB11:0: (BRANCH  %b35 BB19 BB20)
    static bool process_JUMP_PHI_BRANCH(JumpInsn* pJump)
    {
        BBlock* pJoin = pJump->GetTarget();
        BBlock::EnumInsn oEnum(pJoin);

        PhiInsn* pPhi = oEnum.Get()->DynamicCast<PhiInsn>();
            if (NULL == pPhi) return false;
            oEnum.Next();

        if (NULL == pPhi->GetRd()) return false;

        // Is single user?
        {
            Output::EnumUseSite oEnum(pPhi->GetRd());
            if (oEnum.AtEnd()) return false;    // no users
            oEnum.Next();
            if (! oEnum.AtEnd()) return false;  // more than one user
        }

        Instruction* pCmp = oEnum.Get();
            if (pCmp->GetBd() == NULL) return false;
            unless (pCmp->GetSx() == pPhi->GetRd() ||
                    pCmp->GetSy() == pPhi->GetRd() )
                { return false; }
            oEnum.Next();

        BranchInsn* pBranch = oEnum.Get()->DynamicCast<BranchInsn>();
            if (NULL == pBranch) return false;

        html_log_format(2, L"<h3>fold JUMP+PHI+BRANCH ~S:~S + ~S</h3>~%",
            pJump->GetBBlock(), pJump, pBranch );

        Operand* pSx = pPhi->GetInput(pJump->GetBBlock());

        Instruction* pNewCmp = ir_insert_insn(pCmp->Clone(), pJump);
            if (pCmp->GetSx() == pPhi->GetRd())
                { pNewCmp->GetOperandBox(0)->Replace(pSx); }

            if (pCmp->GetSy() == pPhi->GetRd())
                { pNewCmp->GetOperandBox(1)->Replace(pSx); }

        Bool* pBd = pNewCmp->GetBd()->Simplify()->StaticCast<Bool>();

        BBlock* pCurr = pJump->GetBBlock();

        // Note: We must fold BRANCH here instead of later opt-CLEAN.
        // If we don't fold, single operand PHI folding creates
        // wrong register reference. See analyze-lambda-list in
        // m00-fns.lisp.
        if (pBd == Bool_True)
        {
            JumpInsn* pNewJump = new JumpInsn(pBranch->GetTrue());
            ir_replace_insn(pNewJump, pJump);
            add_PHI_inputs(pCurr, pJoin, pBranch->GetTrue());
            process_JUMP(pNewJump);
        }
        else if (pBd == Bool_False)
        {
            JumpInsn* pNewJump = new JumpInsn(pBranch->GetFalse());
            ir_replace_insn(pNewJump, pJump);
            add_PHI_inputs(pCurr, pJoin, pBranch->GetFalse());
            process_JUMP(pNewJump);
        }
        else
        {
            BranchInsn* pNewBranch = new BranchInsn(
                pBd,
                pBranch->GetTrue(),
                pBranch->GetFalse() );

            ir_replace_insn(pNewBranch, pJump);

            // BUGBUG: Due to new BranchInsn calls Operand::Simplify,
            // simplified %bx operand may be defined in another
            // basic block. See NeInsn::simplify_not.
            // We should revise NeInsn::simplify_not or have
            // bool operand ensuring.
            pNewBranch->GetOperandBox(0)->Replace(pNewCmp->GetBd());

            add_PHI_inputs(pCurr, pJoin, pBranch->GetTrue());
            add_PHI_inputs(pCurr, pJoin, pBranch->GetFalse());

            process_BRANCH(pNewBranch);
        } // if

        return true;
    } // process_JUMP_PHI_BRANCH

    // process_JUMP_PHI_RET
    //  from format-roman-aux
    //      BB11:0: (JUMP BB12)
    //
    //      BB12:0: (PHI T %r20 <= (BB13 %r21) (BB11 'NIL))
    //      BB12:0: (RET %r20)
    //    ==>
    //      BB11:0: RET 'nil
    static bool process_JUMP_PHI_RET(JumpInsn* pJump)
    {
        BBlock* pJoin = pJump->GetTarget();
        BBlock::EnumInsn oEnum(pJoin);

        PhiInsn* pPhi = oEnum.Get()->DynamicCast<PhiInsn>();
            if (NULL == pPhi) return false;
            oEnum.Next();

        RetInsn* pRet = oEnum.Get()->DynamicCast<RetInsn>();
            if (NULL == pRet) return false;

        html_log_format(2, L"<h3>fold JUMP+PHI+RET ~S:~S + ~S</h3>~%",
            pJump->GetBBlock(), pJump, pRet );

        Operand* pSx = pPhi->GetInput(pJump->GetBBlock());
        ir_replace_insn(new RetInsn(pSx), pJump);

        return true;
    } // process_JUMP_PHI_RET
}; // CleanPass

} // namespace


//////////////////////////////////////////////////////////////////////
//
// optimize_clean
//
void optimize_clean()
{
    CleanPass oPass;
    oPass.Run();
} // optimize_clean

} // Compiler
