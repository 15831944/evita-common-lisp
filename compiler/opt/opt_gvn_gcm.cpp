#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - opt - Inline Pass
// compiler/opt/opt_closure.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/opt/opt_gvn_gcm.cpp#1 $
//
//
// Description:
//  This file contains "Inline" pass->
//
#include "./opt_defs.h"

#include "../cm/cm_module.h"
#include "../cm/cm_session.h"

#include "../ir/ir_loop.h"
#include "../ir/ir_pass.h"

namespace Compiler
{

namespace
{

//////////////////////////////////////////////////////////////////////
//
// PassGcm
//
class PassGcm : public SubPass
{
    PassGcm(Pass* p, Function* f) : SubPass(p, L"*GCM", f) {}

    // Run
    public: static void Run(Pass* pPass, Function* pFun)
    {
        PassGcm oPass(pPass, pFun);
            oPass.run();
    } // Run

    BBlock* m_pRoot;
    WorkList_<Instruction> m_oPinned;

    enum
    {
        Early_Visit = 1,
        Late_Visit  = 0,
    }; // enum

    // run
    void run()
    {
        ir_number_instructions(m_pFun);

        m_pRoot = m_pFun->GetStartBB();

        html_log_format(3, L"<h4>Schedule Ealry ~S</h4>", m_pFun);

        foreach (Function::EnumBBlock_Postorder, oEnum, m_pFun)
        {
            BBlock* pBBlock = oEnum.Get();
            foreach (BBlock::EnumInsn, oEnum, pBBlock)
            {
                early_process_insn(oEnum.Get());
            } // for each insn
        } // for each bblock


        html_log_format(3, L"<h4>Schedule Late ~S</h4>", m_pFun);

        while (! m_oPinned.IsEmpty())
        {
            late_process_insn(m_oPinned.Pop());
        } // for each pinned
    } // run

    // early_process_insn
    void early_process_insn(Instruction* pInsn)
    {
        unless (is_pinned(pInsn)) return;
        pInsn->SetFlag(Early_Visit);
        m_oPinned.Push(pInsn);
        foreach (Instruction::EnumInput, oEnum, pInsn)
        {
            Output* pSd = oEnum.Get()->ToOutput();
                if (NULL == pSd) continue;
            html_log_format(3, L"<ul>~%");
            early_schedule(pSd->GetDfn());
            html_log_format(3, L"</ul>~%");
        } // for each insn
    } // early_process_insn

    // early_schedule
    void early_schedule(Instruction* pInsn)
    {
        if (NULL == pInsn) return;  // from %b0 or %b1

        html_log_format(3, L"<li>~S:~D:~S~%",
            pInsn->GetBBlock(), pInsn->GetIndex(), pInsn );

        html_log_format(3, L"<ul>~%");

        early_schedule_aux(pInsn);

        html_log_format(3, L"</ul> ealiest: ~S</li>~%", get_bblock(pInsn));
    } // early_schedule

    void early_schedule_aux(Instruction* pInsn)
    {
        if (Early_Visit == pInsn->GetFlag()) return;

        pInsn->SetFlag(Early_Visit);

        BBlock* pEarly = m_pRoot;

        foreach (Instruction::EnumInput, oEnum, pInsn)
        {
            Output* pSd = oEnum.Get()->ToOutput();
                if (NULL == pSd) continue;

            Instruction* pIn = pSd->GetDfn();

            early_schedule(pIn);

            if (pEarly->GetDomInfo()->GetDepth() < get_dom_depth(pIn))
            {
                pEarly = get_bblock(pIn);
            }
        } // for each input

        unless (is_pinned(pInsn)) set_bblock(pInsn, pEarly);
    } // early_schedule

    // late_process_insn
    void late_process_insn(Instruction* pInsn)
    {
        pInsn->SetFlag(Late_Visit);
        Output* pSd = pInsn->GetOutput();
            if (Obj_Void == pSd) return;

        foreach (Output::EnumUseSite, oEnum, pSd)
        {
            html_log_format(3, L"<ul>~%");

            Instruction* pInsn = oEnum.Get()->GetInstruction();
            late_schedule(pInsn);

            html_log_format(3, L"</ul>~%");
        } // for each insn
    } // late_process_insn

    // late_schedule
    static void late_schedule(Instruction* pInsn)
    {
        html_log_format(3, L"<li>~S:~D:~S~%",
            pInsn->GetBBlock(), pInsn->GetIndex(), pInsn );

        html_log_format(3, L"<ul>~%");

        late_schedule_aux(pInsn);

        html_log_format(3, L"</ul> late: ~S</li>~%", pInsn->GetBBlock());
    } // late_schedule

    // late_schedule_aux
    static void late_schedule_aux(Instruction* pInsn)
    {
        if (NULL == pInsn) return;  // from %b0 or %b1
        if (Late_Visit == pInsn->GetFlag()) return;
        if (is_pinned(pInsn)) return;

        pInsn->SetFlag(Late_Visit);

        BBlock* pLCA = NULL;
        foreach (Output::EnumUseSite, oEnum, pInsn->GetOutput())
        {
            Instruction* pUser = oEnum.Get()->GetInstruction();
            late_schedule(pUser);
            BBlock* pUse = pUser->GetBBlock();
            if (pUser->Is<PhiInsn>())
            {
                pUse = reinterpret_cast<PhiInsn::PhiOperandBox*>(
                    oEnum.Get() )->GetBBlock();
            } // if

            pLCA = find_LCA(pLCA, pUse);
        } // for each input

        BBlock* pEarly = get_bblock(pInsn);
        BBlock* pBest  = pLCA;

        while (pLCA != pEarly)
        {
            if (get_loop_depth(pBest) > get_loop_depth(pLCA))
                { pBest = pLCA; }

            pLCA = pLCA->GetDomInfo()->GetParent();
        } // while

        if (pBest == pInsn->GetBBlock()) return;

        html_log_format(2, L"<b>");
            ir_move_insn(pInsn, locate(pBest, pInsn));
        html_log_format(2, L"</b>");
    } // late_schedule_aux

    // find_LCA -- Find Least Common Ancestor
    static BBlock* find_LCA(BBlock* a, BBlock* b)
    {
        if (NULL == a) return b;

        while (a->GetDomInfo()->GetDepth() > b->GetDomInfo()->GetDepth())
            { a = a->GetDomInfo()->GetParent(); }

        while (b->GetDomInfo()->GetDepth() > a->GetDomInfo()->GetDepth())
            { b = b->GetDomInfo()->GetParent(); }

        while (a != b)
        {
            a = a->GetDomInfo()->GetParent();
            b = b->GetDomInfo()->GetParent();
        } // while

        return a;
    } // find_LCA

    // locate
    static Instruction* locate(BBlock* pBBlock, Instruction* pInsn)
    {
        Output* pSd = pInsn->GetOutput();
        foreach (BBlock::EnumInsn, oEnum, pBBlock)
        {
            Instruction* pRefInsn = oEnum.Get();
                if (pRefInsn->Is<PhiInsn>()) continue;
                foreach (Instruction::EnumInput, oEnum, pRefInsn)
                {
                    if (oEnum.Get() == pSd) return pRefInsn;
                } // for each input
        } // for each insn
        return pBBlock->GetLastInsn();
    } // locate

    static uint get_loop_depth(BBlock* pBBlock)
        { return pBBlock->GetLoopInfo()->m_nDepth; }

    static BBlock* get_bblock(Instruction* pInsn)
    {
        BBlock* pBBlock = pInsn->GetExtension<BBlock>();
           if (NULL != pBBlock) return pBBlock;
        return pInsn->SetExtension<BBlock>(pInsn->GetBBlock());
    } // get_bblock

    static uint get_dom_depth(Instruction* pInsn)
        { return get_bblock(pInsn)->GetDomInfo()->GetDepth(); }

    static BBlock* set_bblock(Instruction* pInsn, BBlock* pBBlock)
        { return pInsn->SetExtension<BBlock>(pBBlock); }

    // is_pinned
    static bool is_pinned(Instruction* pInsn)
    {
        if (Obj_Void == pInsn->GetOutput()) return true;
        if (pInsn == pInsn->GetBBlock()->GetLastInsn()) return true;
        switch (pInsn->GetOpcode())
        {
        case IrOp_BOUND:  return true;
        case IrOp_CALL:  return true;

         case IrOp_COUNT: case IrOp_PROJECT: case IrOp_VALUES:
            return true;    // values can't be beyond block boundary.

        case IrOp_ELT: case IrOp_SLOT:
            return true;    // We don't support derived pointer in GC.

        case IrOp_ENTRY: return true;

        case IrOp_LOAD: case IrOp_STORE:
            return true;

        case IrOp_OPENBIND: case IrOp_OPENBLOCK: case IrOp_OPENCATCH:
        case IrOp_OPENFINALLY: case IrOp_OPENTAGBODY:
            return true;

        case IrOp_PHI:          return true;
        case IrOp_PROLOGUE:     return true;
        case IrOp_RUNTIMECAST: return true;

        case IrOp_VARDEF: case IrOp_UPVARDEF:
            // We haven't inserted USE for local function call yet.
            return true;
        } // switch opcode
        return false;
    } // is_pinned
}; // PassGcm


// BB13: 700: (PHI LIST %r35 <= (BB5 %r13) (BB16 %r33))
// BB13: 800: (EQ BOOL %b17 <- %r35 'NIL)
// BB13: 900: (BRANCH %b17 BB8 BB16)
//
// BB16: 1700: (RUNTIMECAST LIST %r33 <- %r32 'LIST)
// BB16: 1800: (JUMP BB13)
//
// ==>
//
// BB13: 800: (EQ BOOL %b17 <- %r13 'NIL)
// BB13: 900: (BRANCH %b17 BB8 BB16)
//
// BB16: 1700: (RUNTIMECAST LIST %r33 <- %r32 'LIST)
// BB16: 1701: (EQ BOOL %b100 <- %r33 'NIL)
// BB16: 1800: (BRANCH %b100 BB8 BB16)
//
class PassLoopInversion : public SubPass
{
    PassLoopInversion(Pass* p, Function* f) :
        SubPass(p, L"*LOOPINVERSION", f) {}

    // Run
    public: static void Run(Pass* pPass, Function* pFun)
    {
        PassLoopInversion oPass(pPass, pFun);
            oPass.run();
    } // Run

    // run
    void run()
    {
        WorkList_<Instruction> oLoopEnd;

        foreach (Function::EnumBBlock, oEnum, m_pFun)
        {
            BBlock* pBBlock = oEnum.Get();
            JumpInsn* pJump = pBBlock->GetLastInsn()->DynamicCast<JumpInsn>();
                if (NULL == pJump) continue;

            BBlock* pHead = pJump->GetTarget();

            if(! pHead->DoesDominate(pBBlock)) continue;

            {
                BBlock::EnumInsn oEnum(pHead);
                PhiInsn* pPhi = oEnum.Get()->DynamicCast<PhiInsn>();
                    if (NULL == pPhi) continue;
                    oEnum.Next();

                Instruction* pTest = oEnum.Get();
                    Bool* pBx = pTest->GetBd();
                    if (NULL == pBx) continue;

                Output* pSd = pPhi->GetOutput();

                unless (pTest->GetSx() == pSd || pTest->GetSy() == pSd)
                    { continue; }

                oEnum.Next();
                if (! oEnum.Get()->Is<BranchInsn>()) continue;
                if (oEnum.Get()->GetSx() != pBx) continue;
            }

            CfgEdge* pEdge = ir_get_cfg_edge(pBBlock, pHead);
                if (! pEdge->IsBackward()) continue;

            if (pHead->CountInEdge() != 2) continue;

            oLoopEnd.Push(pJump);
        } // for each bblock

        while (! oLoopEnd.IsEmpty())
        {
            JumpInsn*    pJump = oLoopEnd.Pop()->StaticCast<JumpInsn>();
            BBlock*      pHead = pJump->GetTarget();
            PhiInsn*     pPhi = pHead->GetFirstInsn()->StaticCast<PhiInsn>();
            Instruction* pTest = pPhi->GetNext();
            BranchInsn*  pBranch = pTest->GetNext()->StaticCast<BranchInsn>();

            html_log_format(3, L"<h3>process ~S:~S</h3>~%",
                pJump->GetBBlock(), pJump);

            Instruction* pNewTest = pTest->Clone();

            ir_insert_insn(pNewTest, pJump);

            Operand* pSx = pPhi->GetInput(pJump->GetBBlock());

            if (pNewTest->GetSx() == pPhi->GetOutput())
            {
                pNewTest->GetOperandBox(0)->Replace(pSx);
            }

            if (pNewTest->GetSy() == pPhi->GetOutput())
            {
                pNewTest->GetOperandBox(1)->Replace(pSx);
            }

            Bool* pBx = pNewTest->GetBd();

            ir_replace_insn(
                new BranchInsn(pBx, pBranch->GetTrue(), pBranch->GetFalse()),
                pJump );
        } // for each loop end
    } // run
}; // PassLoopInversion


//////////////////////////////////////////////////////////////////////
//
// PassGvnGcm
//  Does Click's GVN and GCM.
//
class PassGvnGcm : public FunctionPass
{
    public: PassGvnGcm() : FunctionPass(L"OPT-GVNGCM") {}

    virtual void process_function(Function* pFun)
    {
        ir_compute_loop_tree(pFun);
        //PassLoopInversion::Run(this, pFun);
        PassGcm::Run(this, pFun);
    } // process_function
}; // PassGvnGcm

} // namespace

//////////////////////////////////////////////////////////////////////
//
// optimize_gvn_gcm
//
void
optimize_gvn_gcm()
{
    PassGvnGcm oPass;
        oPass.Run();
} // optimize_gvn_gcm

} // Compiler
