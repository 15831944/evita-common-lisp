#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - opt - SSA to SSI
// compiler/opt/opt_ssa2ssi.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/opt/opt_ssa2ssi.cpp#26 $
//
//
// Description:
//  This file contains SSA to SSI pass.
//
#include "./opt_defs.h"
#include "./opt_fns.h"

#include "../cm/cm_session.h"   // Session::RememberSource

#include "../ir/ir_bblock.h"
#include "../ir/ir_instruction.h"
#include "../ir/ir_pass.h"

namespace Compiler
{

namespace
{

//////////////////////////////////////////////////////////////////////
//
// Entry for NE+BRANCH
//
struct Entry
{
    Val m_fname;
    bool (*m_pfn)(ValuesInsn*, Ty*, Ty*);
}; // Entry


// process_enp
static bool process_endp(
    ValuesInsn* ,//pArgs,
    Ty*         out_truety,
    Ty*         out_falsety )
{
    *out_truety  = ty_cons;
    *out_falsety = ty_null;
    return true;
} // process_endp


// process_Ge2
static bool process_Ge2(
    ValuesInsn* pArgs,
    Ty*         out_truety,
    Ty*         out_falsety )
{
    Ty ty1 = pArgs->GetSx()->GetTy();
    Ty ty2 = pArgs->GetSy()->GetTy();

    TyInteger oTy1 = TyInteger::Parse(ty1);
        if (! oTy1.IsValid()) return false;

    TyInteger oTy2 = TyInteger::Parse(ty2);
        if (! oTy2.IsValid()) return false;

    *out_truety  = GreaterThanOrEqual(oTy1, oTy2).Unparse();
    *out_falsety = LessThan(oTy1, oTy2).Unparse();

    return true;
} // process_Ge2


// process_Gt2
static bool process_Gt2(
    ValuesInsn* pArgs,
    Ty*         out_truety,
    Ty*         out_falsety )
{
    Ty ty1 = pArgs->GetSx()->GetTy();
    Ty ty2 = pArgs->GetSy()->GetTy();

    TyInteger oTy1 = TyInteger::Parse(ty1);
        if (! oTy1.IsValid()) return false;

    TyInteger oTy2 = TyInteger::Parse(ty2);
        if (! oTy2.IsValid()) return false;

    *out_truety  = GreaterThan(oTy1, oTy2).Unparse();
    *out_falsety = LessThanOrEqual(oTy1, oTy2).Unparse();

    return true;
} // process_Gt2


// process_Le2
static bool process_Le2(
    ValuesInsn* pArgs,
    Ty*         out_truety,
    Ty*         out_falsety )
{
    Ty ty1 = pArgs->GetSx()->GetTy();
    Ty ty2 = pArgs->GetSy()->GetTy();

    TyInteger oTy1 = TyInteger::Parse(ty1);
        if (! oTy1.IsValid()) return false;

    TyInteger oTy2 = TyInteger::Parse(ty2);
        if (! oTy2.IsValid()) return false;

    *out_truety  = LessThanOrEqual(oTy1, oTy2).Unparse();
    *out_falsety = GreaterThan(oTy1, oTy2).Unparse();

    return true;
} // process_Le2


// process_Lt2
static bool process_Lt2(
    ValuesInsn* pArgs,
    Ty*         out_truety,
    Ty*         out_falsety )
{
    Ty ty1 = pArgs->GetSx()->GetTy();
    Ty ty2 = pArgs->GetSy()->GetTy();

    TyInteger oTy1 = TyInteger::Parse(ty1);
        if (! oTy1.IsValid()) return false;

    TyInteger oTy2 = TyInteger::Parse(ty2);
        if (! oTy2.IsValid()) return false;

    *out_truety  = LessThan(oTy1, oTy2).Unparse();
    *out_falsety = GreaterThanOrEqual(oTy1, oTy2).Unparse();

    return true;
} // process_Lt2


//////////////////////////////////////////////////////////////////////
//
// Functions for NE+BRANCH
//
const Entry k_rgoEntry[] =
{
    { Qendp, process_endp },
    { QGe2,  process_Ge2 },
    { QGt2,  process_Gt2 },
    { QLe2,  process_Le2 },
    { QLt2,  process_Lt2 }
}; // k_rgoEntry


//////////////////////////////////////////////////////////////////////
//
// Call Optimizer
//
class PassSSAtoSSI : public FunctionPass
{
    public: PassSSAtoSSI() : FunctionPass(L"OPT-SSA2SSI") {}

    // process_function
    virtual void process_function(Function* pFun)
    {
        ir_number_instructions(pFun);
        ComputePostDominance(pFun);

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* pCurr = oEnum.Get();
            foreach (BBlock::EnumInsn, oEnum, pCurr)
            {
                Instruction* pInsn = oEnum.Get();
                switch (pInsn->GetOpcode())
                {
                case IrOp_BOUND:       process_BOUND(pInsn); break;
                case IrOp_CALL:        process_CALL(pInsn); break;
                case IrOp_RUNTIMECAST: process_RUNTIMECAST(pInsn); break;
                } // switch opcode
            } // for each insn

            BranchInsn* pBranch = pCurr->GetLastInsn()->
                DynamicCast<BranchInsn>();

            if (NULL != pBranch) process_BRANCH(pBranch);
        } // for each bblock

        ir_remove_useless_instructions(pFun);
    } // process_function

    // process_BOUND
    // Note: We should fold BOUND by DVNT instead of here.
    static void process_BOUND(Instruction* pBound)
    {
        Register* pRy = pBound->GetRy();
            if (NULL == pRy) return;
        Operand* pArray = pBound->GetSx();
        Register::EnumUseSite oEnum(pRy);
        while (! oEnum.AtEnd())
        {
            Instruction* pUser = oEnum.Get()->GetInstruction();
                oEnum.Next();

            if (pUser == pBound) continue;
            if (! pUser->Is<BoundInsn>()) continue;
            if (pUser->GetSx() != pArray) continue;
            if (pUser->GetSy() != pRy) continue;
            if (! ir_dominate_p(pBound, pUser)) continue;
            ir_replace_all_users(pBound->GetRd(), pUser->GetRd());
            ir_remove_insn(pUser);
        } // for each user
    } // process_BOUND

    // process_BRANCH
    static void process_BRANCH(BranchInsn* pBranch)
    {
        Instruction* pDfn = pBranch->GetSx()->StaticCast<Bool>()->GetDfn();

        if (NULL == pDfn) return;   // constant branch

        switch (pDfn->GetOpcode())
        {
        case IrOp_EQ:    process_BRANCH_EQ(pBranch, pDfn); break;
        case IrOp_NE:    process_BRANCH_NE(pBranch, pDfn); break;
        case IrOp_TYPEP: process_BRANCH_TYPEP(pBranch, pDfn); break;
        } // switch opcode
    } // process_BRANCH

    // process_EQ
    //  propagate (not null)
    // 1800: (VALUES (VALUES (INTEGER 0 *) (INTEGER 2 36)) %v27 <- %r10 %r13)
    // 1900: (CALL T %r28 <- '</2 %v27)
    // 2000: (EQ BOOL %b29 <- %r28 'NIL)
    // 2100: (BRANCH %b29 BB16 BB17)
    static void process_BRANCH_EQ(BranchInsn* pBranch, Instruction* pEq)
    {
        if (! pEq->GetSy()->Is<Literal>()) return;
        if (pEq->GetLy() != nil) return;

        Register* pRx = pEq->GetRx();
            if (NULL == pRx) return;

        html_log_format(3, L"<h3>process_BRANCH_EQ ~S:~S ~S=~W</h3>~%",
            pEq->GetBBlock(), pEq, pRx, pRx->GetTy() );

        if (process_BRANCH_CALL(pRx, pBranch->GetFalse(), pBranch->GetTrue()))
        {
            return;
        }

        propagate_type(pBranch->GetTrue(), pRx, Qnull);
        propagate_not(pBranch, pBranch->GetFalse(), pRx);
    } // process_EQ

    // process_BRANCH_NE
    //  200: (VALUES (VALUES T) %v15 <= %r44)
    //  300: (CALL T %r16 <= #'ENDP %v15)
    //  400: (NE BOOL %b17 <= %r16 'NIL)
    //  500: (BRANCH %b17 BB15 BB16)
    static void process_BRANCH_NE(BranchInsn* pBranch, Instruction* pNe)
    {
        if (! pNe->GetSy()->Is<Literal>()) return;
        if (pNe->GetLy() != nil) return;

        Register* pRx = pNe->GetRx();
            if (NULL == pRx) return;

        html_log_format(3, L"<h3>process_BRANCH_NE ~S:~S ~S=~W</h3>~%",
            pNe->GetBBlock(), pNe, pRx, pRx->GetTy() );

        if (process_BRANCH_CALL(pRx, pBranch->GetTrue(), pBranch->GetFalse()))
        {
            return;
        }

        propagate_not(pBranch, pBranch->GetTrue(), pRx);
    } // process_BRANCH_NE

    // process_BRANCH_CALL
    static bool process_BRANCH_CALL(
        Register*   pR1,
        BBlock*     pTrue,
        BBlock*     pFalse )
    {
        CallInsn* pCall = pR1->GetDfn()->DynamicCast<CallInsn>();
            if (NULL == pCall) return false;
            if (pCall->IsNotInline()) return false;

        Literal* pCallee = pCall->GetSx()->DynamicCast<Literal>();
            if (NULL == pCallee) return false;

        ValuesInsn* pArgs = pCall->GetVy()->GetDfn()->
                DynamicCast<ValuesInsn>();
            if (NULL == pArgs) return false;


        html_log_format(4, L"<h4>process_BRANCH_CALL: ~S:~S</h4>~%",
            pCall->GetBBlock(), pCall );

        bool fChanged = false;

        if (pCallee->GetDatum() == QGe2)
        {
            if (opt_eliminate_BOUND(
                    pFalse->GetFirstInsn(),
                    pArgs->GetRx(),
                    pArgs->GetRy() ) )
            {
                fChanged = true;
            }
        }
        else if (pCallee->GetDatum() == QLt2)
        {
            if (opt_eliminate_BOUND(
                    pTrue->GetFirstInsn(),
                    pArgs->GetRx(),
                    pArgs->GetRy() ) )
            {
                fChanged = true;
            }
        } // if

        for (
            const Entry* pRunner = &k_rgoEntry[0];
            pRunner < &k_rgoEntry[lengthof(k_rgoEntry)];
            pRunner++ )
        {
            if (pCallee->GetDatum() == pRunner->m_fname)
            {
                html_log_format(3, L"<h3>process ~S:~S ~S</h3>~%",
                    pCall->GetBBlock(), pCall, pArgs );

                Ty truety, falsety;
                if (! pRunner->m_pfn(pArgs, &truety, &falsety))
                {
                    return fChanged;
                }

                Register* pRx = pArgs->GetRx();
                    if (NULL == pRx) return fChanged;

                propagate_type(pTrue,  pRx, truety);
                propagate_type(pFalse, pRx, falsety);
                return true;
            } // if
        } // for each entry

        return fChanged;
    } // process_BRANCH_NE_CALL

    // process_CALL
    //  Inserts SIGMA for arguments.
    static void process_CALL(Instruction* pCall)
    {
        ValuesInsn* pArgs = pCall->GetVy()->GetDfn()->
                DynamicCast<ValuesInsn>();
            if (NULL == pArgs) return;

        tyArgsIterator oArgs(pCall->GetSx()->GetTy());

        foreach (Instruction::EnumInput, oEnum, pArgs)
        {
            Ty ty = oArgs.GetTy();
                oArgs.Next();
            if (ty == ty_t) continue;

            Register* pRx = oEnum.Get()->DynamicCast<Register>();
                if (NULL == pRx) continue;

            if (Subtypep_Yes == ty_subtypep(pRx->GetTy(), ty)) continue;

            update_users(pCall->GetNext(), ty, pRx);
        } // for each arg
    } // process_CALL

    // process_RUNTIMECAST
    static void process_RUNTIMECAST(Instruction* pCast)
    {
        html_log_format(3, L"<h3>process ~S:~S</h3>~%",
            pCast->GetBBlock(), pCast );

        Register* pRx = pCast->GetRx();
            if (NULL == pRx) return;

        Ty ty = pCast->GetTy();

        // Update ty from post-dominated RUNTIMECAST instructions.
        {
            html_log_format(3, L"Post-Dominance Tree walk");
            html_log_format(3, L"<ol>~%");

            bool fNop = pCast->HasAttr(Instruction::Attr_Nop);

            Register::EnumUseSite oEnum(pRx);
            while (! oEnum.AtEnd())
            {
                Instruction* pUser = oEnum.Get()->GetInstruction();
                    oEnum.Next();

                if (pUser == pCast) continue;
                unless (pUser->Is<RuntimeCastInsn>()) continue;
                unless (ir_post_dominate_p(pUser, pCast)) continue;

                Ty newty = ty_and(ty, pUser->GetTy());

                if (nil == newty)
                {
                    html_log_format(1,
                        L"<h1>Type conflicts: ~S:~S ~S:~S</h1>~%",
                        pCast->GetBBlock(), pCast,
                        pUser->GetBBlock(), pUser );

                    Val form = pUser->GetRd()->GetForm();
                    Session::Get()->RememberSource(form);
                    warn(L"Type conflict in ~S and ~S: ~S",
                        ty, pUser->GetTy(), form );
                }
                else
                {
                    if (ir_dominate_p(pCast, pUser))
                    {
                        unless (pUser->HasAttr(Instruction::Attr_Nop))
                        {
                            fNop = false;
                        }

                        ir_replace_all_users(pCast->GetRd(), pUser->GetRd());
                        ir_remove_insn(pUser);
                    }

                    ty = newty;
                }
            } // for each user

            html_log_format(3, L"</ol>~%");

            unless (ty_equal(pCast->GetTy(), ty))
            {
                unless (fNop)
                {
                    pCast->ClearAttr(Instruction::Attr_Nop);
                }

                html_log_format(3, L"update ~S:~S -> ~W~:%",
                    pCast->GetBBlock(), pCast, ty );

                pCast->SetTy(ty);
            }
        } // post-dominance

        // Update dominated RUNTIMECAST instructions
        {
            html_log_format(3, L"Dominance Tree walk");
            html_log_format(3, L"<ol>~%");

            Register::EnumUseSite oEnum(pRx);
            while (! oEnum.AtEnd())
            {
                Instruction* pUser = oEnum.Get()->GetInstruction();
                    oEnum.Next();

                if (pUser == pCast) continue;

                unless (pUser->Is<RuntimeCastInsn>()) continue;
                unless (ir_dominate_p(pCast, pUser)) continue;

                Ty newty = ty_and(ty, pUser->GetTy());

                if (nil == newty)
                {
                    html_log_format(1,
                        L"<h1>Type conflicts: ~S:~S ~S:~S</h1>~%",
                        pCast->GetBBlock(), pCast,
                        pUser->GetBBlock(), pUser );

                    Val form = pUser->GetRd()->GetForm();
                    Session::Get()->RememberSource(form);
                    warn(L"Type conflict in ~S and ~S: ~S",
                        ty, pUser->GetTy(), form );
                }
                else
                {
                    if (Subtypep_Yes == ty_subtypep(newty, ty))
                    {
                        unless (pUser->HasAttr(Instruction::Attr_Nop))
                        {
                            pCast->ClearAttr(Instruction::Attr_Nop);
                        }

                        ir_replace_all_users(pCast->GetRd(), pUser->GetRd());
                        ir_remove_insn(pUser);
                    }
                    else
                    {
                        html_log_format(3, L"<li>update ~S:~S -> ~W</li>~%",
                            pUser->GetBBlock(), pUser, newty );

                        pUser->SetTy(newty);
                    }
                } // if
            } // for each user

            html_log_format(3, L"</ol>~%");
        } // dominace tree
    } // process_RUNTIMECAST

    // process_BRANCH_TYPEP
    static void process_BRANCH_TYPEP(BranchInsn* pBranch, Instruction* pTypep)
    {
        Register* pRx = pTypep->GetRx();
            if (NULL == pRx) return;

        html_log_format(3, L"<h3>process ~S:~S ~S=~W</h3>~%",
            pTypep->GetBBlock(), pTypep,  pRx, pRx->GetTy() );

        Ty ty = pTypep->GetLy();

        propagate_type(pBranch->GetTrue(), pRx, ty);
        propagate_not(pBranch, pBranch->GetFalse(), pRx);
    } // process_BRANCH_TYPEP

    // propagate_not
    //  Propagate NOT-type.
    static void propagate_not(
        BranchInsn* pBranch,
        BBlock*     pBBlock,
        Register*   pRx )
    {
        Ty noty = ty_diff(pRx->GetTy(), ty_null);

        html_log_format(4, L"<h4>propagate_not: ~S:~S ~S ~S ~W</h4>~%",
            pBranch->GetBBlock(), pBranch,
            pBBlock,
            pRx,
            noty );

        if (Qnot == noty)
        {
            // Not-type is too hairy.
            return;
        }

        if (nil == noty)
        {
            // There is no chance to go pBBlock.
            pBranch->GetOperandBox(0)->Replace(
                pBranch->GetTrue() == pBBlock ? Bool_False : Bool_True );
            return;
        }

        // propagate (not null) to true block.
        propagate_type(pBBlock, pRx, noty);
    } // process_not

    // propagate_type
    // BUGBUG: We should handle TYPEP+PHI. See r23-reader.lisp: patch.
    //
    //  Note:
    //      (defun foo (x)
    //          (declare (values (integer 0 15)))
    //        (when (> x 0) (print (+ x 1)))
    //        x )
    //
    //   o RET x is dominated by BRANCH.
    //   o RET x post-dominates BRANCH.
    //
    static void propagate_type(
        BBlock*         pTrueBB,
        Register*       pRx,
        Ty              ty )
    {
        html_log_format(4, L"<h4>propagate ~W on ~S</h4>~%", ty, pTrueBB);

        if (NULL == pRx) return;
        if (pTrueBB->HasMoreThanOnePred()) return;

        Instruction* pFirst = pTrueBB->GetFirstInsn();
            while (pFirst->Is<PhiInsn>()) pFirst = pFirst->GetNext();

        update_users(pFirst, ty, pRx);
    } // propagate_type

    // update_users
    static void update_users(
        Instruction*    pFirst,
        Ty              ty,
        Register*       pRx )
    {
        Register* pR2 = NULL;

        Register::EnumUseSite oEnum(pRx);
        while (! oEnum.AtEnd())
        {
            OperandBox* pBox = oEnum.Get();
                oEnum.Next();

            Instruction* pUser = pBox->GetInstruction();

            if (! pUser->Is<PhiInsn>())
            {
                if (! ir_dominate_p(pFirst, pUser)) continue;
            }
            else
            {
                PhiInsn::PhiOperandBox* pPhiBox = pUser->
                    StaticCast<PhiInsn>()->GetInputBox(pBox);

                if (! pFirst->GetBBlock()->DoesDominate(pPhiBox->GetBBlock()))
                {
                    continue;
                }
            }

            if (NULL == pR2)
            {
                pR2 = new Register();

                Instruction* pSigma = ir_insert_insn(
                    new SigmaInsn(ty, pR2, pRx),
                    pFirst );

                pSigma->SetIndex(pFirst->GetIndex() - 1);
                pFirst = pSigma;
            } // if

            if (pUser->GetRd() == pR2) continue;

            pBox->Replace(pR2);
        } // for each use
    } // update_users

    // get_veclen - Returns register contains vector if it is argument
    // of function length.
    static Register* get_veclen(Register* pRx)
    {
        if (NULL == pRx) return NULL;

        CallInsn* pCall = pRx->GetDfn()->DynamicCast<CallInsn>();
            if (NULL == pCall) return NULL;
            if (pCall->IsNotInline()) return NULL;

        Literal* pCallee = pCall->GetSx()->DynamicCast<Literal>();
            if (NULL == pCallee) return NULL;

        unless (pCallee->GetDatum() == Qlength) return NULL;

        ValuesInsn* pArgs = pCall->GetVy()->GetDfn()->
                DynamicCast<ValuesInsn>();
            if (NULL == pArgs) return NULL;

        return follow(pArgs->GetRx());
    } // get_veclen

    // propagate_veclen
    static void propagate_veclen(
        Instruction*    pStart,
        Register*       pRvector,
        Register*       pRindex,
        Register*       pRindex0 )
    {
        if (NULL == pRindex) return;

        html_log_format(3, L"<h4>propagate_veclen: ~S:~S ~S ~S ~S</h4>~%",
            pStart->GetBBlock(), pStart, pRvector, pRindex, pRindex0 );

        Register::EnumUseSite oEnum(pRindex);
        while (! oEnum.AtEnd())
        {
            Instruction* pUser = oEnum.Get()->GetInstruction();
                oEnum.Next();

            unless (ir_dominate_p(pStart, pUser)) continue;

            html_log_format(3, L"~S:~D:~S~:%",
                pUser->GetBBlock(), pUser->GetIndex(), pUser );

            switch (pUser->GetOpcode())
            {
            case IrOp_BOUND:
                if (follow(pUser->GetRx()) == pRvector &&
                    follow(pUser->GetRy()) == pRindex0 )
                {
                    ir_replace_all_users(pUser->GetRy(), pUser->GetRd());
                }
                break;

            case IrOp_RUNTIMECAST:
            case IrOp_SIGMA:
                propagate_veclen(pStart, pRvector, pUser->GetRd(), pRindex0);
                break;
            } // switch opcode
        } // for each user
    } // propagate_veclen

    static Register* follow(Register* pRx)
    {
        for (;;)
        {
            if (NULL == pRx) return NULL;

            Instruction* pDfn = pRx->GetDfn();
            switch (pDfn->GetOpcode())
            {
            case IrOp_RUNTIMECAST:
            case IrOp_SIGMA:
            {
                Register* pR2 = pDfn->GetRx();
                if (NULL == pR2) return pRx;
                pRx = pR2;
                break;
            } // runtimecast
            default:
                return pRx;
            } // switch opcode
        } // for
    } // follow
}; // PassSSAtoSSI

} // namespace


//////////////////////////////////////////////////////////////////////
//
// optimize_strength_reduction
//
void optimize_ssa2ssi()
{
    PassSSAtoSSI oPass;
        oPass.Run();
} // optimize_ssa2ssi

} // Compiler
