#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - Expand RUNTIMECAST instruction
// compiler/cg/cg_expand_cast.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_expand_typep.cpp#1 $
//
// Description:
//  This pass expands RUNTIMECAST instructions into TYPEP, BRANCH and
//  TRAPIF.
//
#include "./cg_defs.h"

#include "../ir/ir_defs.h"
#include "../ir/ir_function.h"
#include "../ir/ir_pass.h"

namespace Compiler
{

namespace
{

// is_equable
static bool is_equable(Val x)
{
    if (fixnump(x)) return t;
    return ! numberp(x);
} // is_equable


// ir_update_PHI
void ir_update_PHI(BBlock* pNew, BBlock* pCur, BBlock* pJoin)
{
    foreach (BBlock::EnumInsn, oEnum, pJoin)
    {
        PhiInsn* pPhi = oEnum.Get()->DynamicCast<PhiInsn>();
            if (NULL == pPhi) break;

        Operand* pSx = pPhi->GetInput(pCur);
        pPhi->AddInput(pNew, pSx);
    } // for each insn
} // ir_updatePHI


//////////////////////////////////////////////////////////////////////
//
// Expander
//
class Expander
{
    Instruction*    m_pTypep;
    Bool*           m_pBd;
    Register*       m_pRx;
    Ty              m_ty;

    Expander(
        Instruction*    pTypep,
        Bool*           pBd,
        Register*       pRx,
        Ty              ty ) :
            m_pTypep(pTypep),
            m_pBd(pBd),
            m_pRx(pRx),
            m_ty(ty) {}

    public: static bool Run(Instruction* pCast)
    {
        Expander oExpander(
            pCast,
            pCast->GetBd(), pCast->GetRx(), pCast->GetLy() );

        return oExpander.run();
    } // Run

    // run
    private: bool run()
    {
        if (! consp(m_ty))
        {
            if (symbolp(m_ty))
            {
                Val klass = find_class(m_ty, nil, TLV(AenvironmentA));
                if (nil != klass && CLASS_built_in_class != class_of(klass))
                {
                    m_pTypep->GetOperandBox(1)->Replace(NewLiteral(klass));
                }
            }

            return false;
        }

        Val op = first(m_ty);
        if (op == Qeql) { expand_EQL(); return false; }
        if (op == Qor)  { return expand_OR(); }

        return false;
    } // Expand

    // expand_EQL
    private: void expand_EQL()
    {
        Val obj = second(m_ty);
        if (is_equable(obj))
        {
            ir_replace_insn(
                new EqInsn(m_pBd, m_pRx, NewLiteral(obj)),
                m_pTypep );
            return;
        }

        Values* pVy = new Values;
        ir_insert_insn(
            new ValuesInsn(pVy, m_pRx, NewLiteral(obj)),
            m_pTypep );

        Register* pRd = new Register;
        ir_insert_insn(
            new CallInsn(ty_t, pRd, NewLiteral(Qeql), pVy),
            m_pTypep );

        ir_replace_insn(
            new NeInsn(m_pBd, pRd, NewLiteral(nil)),
            m_pTypep );
    } // expand_typep_EQL

    // expand_OR
    private: bool expand_OR()
    {
        html_log_format(3, L"<h3>process ~S:~S</h3>~%",
            m_pTypep->GetBBlock(), m_pTypep );

        Val elts = rest(m_ty);
        if (nil == elts)
        {
            ir_replace_all_users(Bool_True, m_pBd);
            return false;
        }

        if (nil == rest(elts))
        {
            m_pTypep->GetOperandBox(1)->Replace(NewLiteral(first(elts)));
            return true;
        }

        Instruction* pUser = m_pBd->GetUseInsn();
        switch (pUser->GetOpcode())
        {
        case IrOp_BRANCH:
            expand_OR_BRANCH(pUser);
            break;

        case IrOp_SELECT:
            expand_OR_SELECT(pUser);
            break;

        default:
            warn(L"Unexpected TYPEP user.");
            break;
        } // switch opcode

        return true;
    } // expand_OR

    // expand_OR_BRANCH
    //  BB1:    TYPEP
    //          BRANCH True BB2
    //
    //  BB2:    TYPEP
    //          BRANCH True BB3
    //          ...
    //  BBn:    JUMP Last
    //
    //  Last:   original TYPEP
    //          ...
    //          BRANCH True False
    //
    private: void expand_OR_BRANCH(Instruction* pBranch)
    {
        BBlock* pTrue  = pBranch->GetSy()->StaticCast<Label>()->GetBBlock();

        BBlock* pCurr = m_pTypep->GetBBlock();
        BBlock* pLast = ir_split_bblock_after(m_pTypep);
        ir_move_insn(m_pTypep, pLast->GetFirstInsn());

        foreach (EnumList, oEnum, rest(m_ty))
        {
            Ty ty = ty_expand(oEnum.Get());

            if (rest(oEnum.GetList()) == nil)
            {
                m_pTypep->GetOperandBox(1)->Replace(NewLiteral(ty));
                return;
            }

            BBlock* pNext;

            if (cddr(oEnum.GetList()) == nil)
            {
                pNext = pLast;
            }
            else
            {
                pNext = NewBBlock();
                InsertBBlock(pNext, pLast);
            } // if

            Bool* pBx = new Bool();
            pCurr->AppendInsn(new TypepInsn(pBx, m_pRx, ty));
            pCurr->AppendInsn(new BranchInsn(pBx, pTrue, pNext));

            ir_update_PHI(pCurr, pBranch->GetBBlock(), pTrue);

            pCurr = pNext;
        } // for each elty
        CAN_NOT_HAPPEN();
    } // expand_OR_BRANCH

    // expand_OR_SELECT
    //  BB1:    TYPEP
    //          BRANCH Join BB2
    //
    //  BB2:    TYPEP
    //          BRANCH Join BB3
    //
    //          ...
    //
    //
    //  BBn:    original TYPEP
    //          original SELECT
    //          JMP Join
    //
    //  Join:   PHI
    private: void expand_OR_SELECT(Instruction* pSelect)
    {
        BBlock* pCurr = m_pTypep->GetBBlock();
        BBlock* pLast = ir_split_bblock_after(m_pTypep);
        ir_move_insn(m_pTypep, pLast->GetFirstInsn());

        PhiInsn* pPhi = new PhiInsn(pSelect->GetTy(), pSelect->GetRd());
        BBlock* pJoin = ir_split_bblock_after(pSelect);
        ir_insert_insn(pPhi, pJoin->GetFirstInsn());

        foreach (EnumList, oEnum, rest(m_ty))
        {
            Ty ty = ty_expand(oEnum.Get());

            if (rest(oEnum.GetList()) == nil)
            {
                m_pTypep->GetOperandBox(1)->Replace(NewLiteral(ty));
                Instruction* pJump = pLast->AppendInsn(new JumpInsn(pJoin));
                ir_move_insn(pSelect, pJump);
                Register* pRd = new Register();
                pSelect->SetOutput(pRd);
                pPhi->AddInput(pCurr, pRd);
                return;
            }

            BBlock* pNext;

            if (cddr(oEnum.GetList()) == nil)
            {
                pNext = pLast;
            }
            else
            {
                pNext = NewBBlock();
                InsertBBlock(pNext, pLast);
            } // if

            Bool* pBx = new Bool();
            pCurr->AppendInsn(new TypepInsn(pBx, m_pRx, ty));
            pCurr->AppendInsn(new BranchInsn(pBx, pJoin, pNext));

            pPhi->AddInput(pCurr, pSelect->GetSy());

            pCurr = pNext;
        } // for each elty

        CAN_NOT_HAPPEN();
    } // expand_OR_SELECT
}; // Expander


//////////////////////////////////////////////////////////////////////
//
// ExpandTypepPass
//
class ExpandTypepPass : public FunctionPass
{
    public: ExpandTypepPass() : FunctionPass(L"CG-TYPEP") {}

    protected: virtual void process_function(Function* pFun)
    {
        html_log_format(2, L"<h2>Process ~S</h2>~%", pFun);

        for (;;)
        {
            WorkList_<Instruction> oTasks;
            foreach (Function::EnumBBlock, oEnum, pFun)
            {
                BBlock* pBBlock = oEnum.Get();
                foreach (BBlock::EnumInsn, oEnum, pBBlock)
                {
                    Instruction* pInsn = oEnum.Get();
                    switch (pInsn->GetOpcode())
                    {
                    case IrOp_TYPEP:
                        oTasks.Push(pInsn);
                        break;
                    } // switch opcode
                } // for each insn
            } // for each bblock

            bool fChanged = false;
            while (! oTasks.IsEmpty())
            {
                if (Expander::Run(oTasks.Pop())) fChanged = true;
            } // for each task

            if (! fChanged) break;
        } // for
    } // process_function
}; // ExpandTypepPass

} // namespace

//////////////////////////////////////////////////////////////////////
//
// Code Generator Entry Point
//
void
cg_pass_expand_typep()
{
    ExpandTypepPass oPass;
    oPass.Run();
} // cg_pass_expand_typep

} // Compiler
