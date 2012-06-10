#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - opt - CFG to SSA Pass
// compiler/opt/opt_cf2ssa.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/opt/opt_cfg2ssa.cpp#10 $
//
//
// Description:
//  This file contains "CFG to SSA" pass. This pass promotes
//  variables allocated on stack-cell to register by rewriting:
//      o VARDEF, STORE => PHI
//      o LOAD => update operands
//
//
#include "./opt_defs.h"

#include "../ir/ir_dfa.h"
#include "../ir/ir_pass.h"

namespace Compiler
{

namespace
{

enum VarFlag
{
    VarFlag_Local,
    VarFlag_Global,
    VarFlag_Liveness,
}; // VarFlag


//////////////////////////////////////////////////////////////////////
//
// Variabe Liveness Pass
//
//  Compute liveness of variables.
//
//  Since we don't know exact set of upvar here, so we don't ignore
//  upvars.
//
class VarLivenessPass : public SubPass
{
    public: VarLivenessPass(Pass* pPass) :
        SubPass(pPass, L"*Liveness") {}

    // Run
    public: bool Run(Function* pFun, uint cVars)
    {
        compute_local_liveness(pFun, cVars);

        SolveBackward(pFun);

        check_nonlocal(pFun);

        bool fContinue = false;
        foreach (Function::EnumVar, oEnum, pFun)
        {
            Variable* pVar = oEnum.Get();

            if (pVar->GetStorage() == Variable::Storage_Register)
            {
                pVar->SetFlag(VarFlag_Liveness);
                fContinue = true;
            }
            else
            {
                pVar->ResetIndex();
            }
        } // for each var

        return fContinue;
    } // Run

    // check_nonlocal
    void check_nonlocal(Function* pFun)
    {
        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            DataFlowBB* pBBlock = oEnum.Get()->Extend<DataFlowBB>();
                if (! pBBlock->HasNonlocalInEdge()) continue;

            foreach (Function::EnumVar, oEnum, pFun)
            {
                Variable* pVar = oEnum.Get();

                if (! pVar->HasIndex()) continue;
                if (! pBBlock->IsIn(pVar->GetIndex())) continue;

                pVar->ResetIndex();
                html_log_format(2, L"LiveIn at nonlocal ~S: ~S~:%",
                    pBBlock,
                    pVar );

                if (pVar->GetStorage() == Variable::Storage_Register)
                {
                    pVar->SetStorage(Variable::Storage_Stack);

                    pVar->GetDfn()->SetTy(ty_c6_stack_cell);

                    html_log_format(3, L"Demote to stack: ~S~:%",
                        pVar );
                } // if
            } // for each var
        } // for each bblock
    } // check_nonlocal

    // compute_local_livness
    void compute_local_liveness(Function* pFun, uint cVars)
    {
        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            DataFlowBB* pBBlock = oEnum.Get()->Extend<DataFlowBB>();
            pBBlock->InitDataFlow(Session::Get(), cVars);

            foreach (BBlock::EnumInsn, oEnum, pBBlock)
            {
                Instruction* pInsn = oEnum.Get();
                switch (pInsn->GetOpcode())
                {
                case IrOp_LOAD:
                {
                    Variable* pVar = map_reg_to_var(pInsn->GetRx());
                    if (NULL != pVar && ! pBBlock->IsKill(pVar->GetIndex()))
                    {
                        pBBlock->SetIn(pVar->GetIndex());
                    }
                    break;
                } // LOAD

                case IrOp_STORE:
                {
                    Variable* pVar = map_reg_to_var(pInsn->GetRx());
                    if (NULL != pVar)
                    {
                        pBBlock->SetKill(pVar->GetIndex());
                    }
                    break;
                } // STORE

                case IrOp_VARDEF:
                {
                    Variable* pVar = pInsn->GetSx()->StaticCast<Variable>();
                    if (pVar->HasIndex())
                    {
                        pBBlock->SetKill(pVar->GetIndex());
                    }
                    break;
                } // VARDEF
                } // switch opcode
            } // for each instruction
        } // for each bblock
    } // compute_local_liveness

    // map_reg_to_var
    Variable* map_reg_to_var(Register* pRx)
    {
        SlotInsn* pSlot = pRx->GetDfn()->DynamicCast<SlotInsn>();
            if (NULL == pSlot) return NULL;

        if (! pSlot->GetSz()->Is<Register>()) return NULL;

        VarDefInsn* pVarDef = pSlot->GetRz()->GetDfn()->
            DynamicCast<VarDefInsn>();
                if (NULL == pVarDef) return NULL;

        Variable* pVar = pVarDef->GetSx()->StaticCast<Variable>();
            if (! pVar->HasIndex())
            {
                ASSERT(Variable::Storage_Register != pVar->GetStorage());
                return NULL;
            }

        ASSERT(
            Variable::Storage_Register == pVar->GetStorage() ||
            Variable::Storage_Heap     == pVar->GetStorage() );
        return pVar;
    } // map_reg_to_var
}; // VarLivenessPass


//////////////////////////////////////////////////////////////////////
//
// CFG to SSA Pass
//
class Cfg2SsaPass : public FunctionPass
{
    public: Cfg2SsaPass() :
        FunctionPass(L"OPT-CFG2SSA") {}

    protected: virtual void process_function(Function* pFun)
    {
        html_log_format(1, L"<h2>Process ~S</h2>~%", pFun);

        if (prepare(pFun))
        {
            ComputeDominance(pFun);
            pass_insert(pFun);
            pass_rename(pFun->GetEntryBB());
            ir_remove_useless_instructions(pFun);
        }
    } // process_function

    // RegData
    class RegData
    {
        protected: typedef SLinkAnchor_<Operand> OperandList;
        protected: OperandList  m_oStack;

        // For debugging
        protected: Register* m_pRd;
            public: Register* GetRd() const { return m_pRd; }

        public: RegData(Register* pRd) :
            m_pRd(pRd) {}

        public: Operand* GetTop() const
        {
            ASSERT(! m_oStack.IsEmpty());
            return m_oStack.GetFirst()->GetItem();
        } // GetTop

        public: bool IsStackEmpty() const
            { return m_oStack.IsEmpty(); }

        public: Operand* Pop()
        {
            ASSERT(! m_oStack.IsEmpty());
            Operand* pSx = m_oStack.Pop();

            html_log_format(3, L"pop ~S ~S => ~S~:%",
                m_pRd->GetVar(),
                m_pRd,
                pSx );

            return pSx;
        } // Pop

        public: void Push(Operand* pSx)
        {
            html_log_format(3, L"push ~S ~S &lt;= ~S~:%",
                m_pRd->GetVar(),
                m_pRd,
                pSx );

            m_oStack.Push(new OperandList::SLinkSite(pSx));
        } // Push
    }; // RegData

    typedef SLinkAnchor_<RegData> RegDataList;

    // pass_insert
    void pass_insert(Function* pFun)
    {
        foreach (Function::EnumVar, oEnum, pFun)
        {
            Variable* pVar = oEnum.Get();
            if (Variable::Storage_Register != pVar->GetStorage())
            {
                continue;
            }

            if (NULL == pVar->GetDfn())
            {
                // Removed/Promoted variable
                continue;
            }

            html_log_format(1, L"process ~S ~S<ul>~%",
                pVar->GetDfn()->GetBBlock(),
                pVar->GetDfn() );

            pass_insert_1(pFun, pVar);

            html_log_format(1, L"</ul>~%");
        } // for each variable
    } // pass_insert

    // pass_insert_1
    void pass_insert_1(Function* pFun, Variable* pVar)
    {
        Register* pCell = pVar->GetDfn()->GetRd();

        RegData* pRegData = new RegData(pCell);
        pCell->SetExtension<RegData>(pRegData);

        WorkList_<BBlock> oDoneList;
            oDoneList.Push(pFun->GetExitBB());

        WorkList_<BBlock> oWorkList;

        // Initialize work list
        foreach (BBlock::EnumFrontier, oEnum, pFun->GetEntryBB())
        {
            BBlock* pFrontier = oEnum.Get();
            if (! pFrontier->IsPushed())
            {
                oWorkList.Push(pFrontier);
            }
        } // for each frontier

        // Add all STORE site to work list
        foreach (Register::EnumUseSite, oEnum, pCell)
        {
            Instruction* pSlot = oEnum.Get()->GetInstruction();
                if (! pSlot->Is<SlotInsn>())
                {
                    continue;
                }

            foreach (Register::EnumUseSite, oEnum, pSlot->GetRd())
            {
                Instruction* pStore = oEnum.Get()->GetInstruction();
                if (pStore->Is<StoreInsn>())
                {
                    // Add DF(use-site)
                    BBlock* pCurr = pStore->GetBBlock();
                    if (! pCurr->IsPushed())
                    {
                        html_log_format(2, L"<li>~S ~S</li>~%",
                            pCurr,
                            pStore );

                        foreach (BBlock::EnumFrontier, oEnum, pCurr)
                        {
                            BBlock* pFrontier = oEnum.Get();
                            if (! pFrontier->IsPushed())
                            {
                                oWorkList.Push(pFrontier);
                            }
                        } // for each frontier
                    } // if
                } // if
            } // for each use site of SLOT
        } // for each use site of VARDEF

        uint nIndex = pVar->GetIndex();
        while (! oWorkList.IsEmpty())
        {
            BBlock* pCurr = oWorkList.Pop();
            switch (pVar->GetFlag())
            {
            case VarFlag_Global:
                pass_insert_phi(pCurr, pVar, pRegData);
                break;

            case VarFlag_Liveness:
                if (pCurr->Extend<DataFlowBB>()->IsIn(nIndex))
                {
                    pass_insert_phi(pCurr, pVar, pRegData);
                }
                break;
            } // switch VarFlag

            oDoneList.Push(pCurr);

            foreach (BBlock::EnumFrontier, oEnum, pCurr)
            {
                BBlock* pFrontier = oEnum.Get();
                if (! pFrontier->IsPushed())
                {
                    oWorkList.Push(pFrontier);
                }
            } // for each frontier
        } // while
    } // pass_insert_1

    // pass_insert_phi
    void pass_insert_phi(BBlock* pCurr, Variable* pVar, RegData* pRegData)
    {
        Register* pRd = new Register(pVar);
        pRd->SetExtension<RegData>(pRegData);

        PhiInsn* pPhi = new PhiInsn(pVar->GetTy(), pRd);
        ir_insert_insn(pPhi, pCurr->GetFirstInsn());

        foreach (BBlock::EnumInEdge, oEnum, pCurr)
        {
            BBlock* pPred = oEnum.GetNode();
            pPhi->AddInput(pPred, Obj_Void);
        } // for each pred

        html_log_format(2, L"<li>~S PHI ~S ~S</li>~%",
            pCurr,
            pVar,
            pRd );
    } // pass_insert_phi

    // pass_rename
    void pass_rename(BBlock* pCurr)
    {
        html_log_format(1, L"rename ~S~:%", pCurr);
        RegDataList oKillList;

        {
            BBlock::EnumInsn oEnum(pCurr);
            while (! oEnum.AtEnd())
            {
                Instruction* pInsn = oEnum.Get();
                    oEnum.Next();

                if (pInsn->Is<LoadInsn>())
                {
                    pass_rename_LOAD(pInsn);
                }
                else if (pInsn->Is<PhiInsn>())
                {
                    pass_rename_PHI(pInsn, &oKillList);
                }
                else if (pInsn->Is<StoreInsn>())
                {
                    pass_rename_STORE(pInsn, &oKillList);
                }
                else if (pInsn->Is<VarDefInsn>())
                {
                    pass_rename_VARDEF(pInsn, &oKillList);
                }
            } // for each instruction
        }

        foreach (BBlock::EnumOutEdge, oEnum, pCurr)
        {
            BBlock* pSucc = oEnum.GetNode();
            pass_rename_set_succ_phi(pCurr, pSucc);
        } // for each succ

        foreach (BBlock::EnumChild, oEnum, pCurr)
        {
            pass_rename(oEnum.Get());
        } // for each chil

        // Pop stacks
        foreach (RegDataList::Enum, oEnum, &oKillList)
        {
            RegData* pRegData = oEnum.Get();
            pRegData->Pop();
        } // for each var
    } // pass_rename

    void pass_rename_LOAD(Instruction* pLoad)
    {
        RegData* pRegData = pass_rename_track(pLoad->GetRx());
        if (NULL == pRegData)
        {
            return;
        }
        // BUGBUG: NYI: static-cast
        ir_replace_all_users(pRegData->GetTop(), pLoad->GetRd());
        ir_remove_insn(pLoad);
    } // pass_rename_LOAD

    // pass_rename_PHI - Push %rd to stack.
    void pass_rename_PHI(Instruction* pPhi, RegDataList* pKillList)
    {
        Register* pRd = pPhi->GetRd();
        if (NULL == pRd)
        {
            // Non-Register PHI instruction. It may be Values or Bool.
            return;
        }

        RegData* pRegData = pRd->GetExtension<RegData>();
        if (NULL == pRegData)
        {
            // PHI instruction not related variable.
            return;
        }

        pRegData->Push(pRd);
        pKillList->Push(new RegDataList::SLinkSite(pRegData));
    } // pass_rename_PHI

    void pass_rename_STORE(Instruction* pStore, RegDataList* pKillList)
    {
        RegData* pRegData = pass_rename_track(pStore->GetRx());
        if (NULL == pRegData)
        {
            return;
        }

        pRegData->Push(pStore->GetSy());
        ir_remove_insn(pStore);
        pKillList->Push(new RegDataList::SLinkSite(pRegData));
    } // pass_rename_STORE

    void pass_rename_VARDEF(Instruction* pVarDef, RegDataList* pKillList)
    {
        RegData* pRegData = pVarDef->GetRd()->GetExtension<RegData>();
        if (NULL == pRegData)
        {
            return;
        }

        // Note: We dont' remove VARDEF instruction here.
        pRegData->Push(pVarDef->GetSy());
        pKillList->Push(new RegDataList::SLinkSite(pRegData));
    } // pass_rename_VARDEF

    // pass_rename_set_succ_phi
    void pass_rename_set_succ_phi(BBlock* pCurr, BBlock* pSucc)
    {
        BBlock::EnumInsn  oEnum(pSucc);
        while (! oEnum.AtEnd())
        {
            PhiInsn* pPhi = oEnum.Get()->DynamicCast<PhiInsn>();
                oEnum.Next();

            if (NULL == pPhi)
            {
                break;
            }

            Register* pRd = pPhi->GetRd();
            if (NULL == pRd)
            {
                continue;
            }

            RegData* pRegData = pRd->GetExtension<RegData>();
            if (NULL == pRegData)
            {
                // PHI instruction not related to variable.
                continue;
            }

            if (pRegData->IsStackEmpty())
            {
                // Useless PHI instruction. It is created by loop.
                ir_remove_insn(pPhi);
                continue;
            }

            Operand* pSx = pRegData->GetTop();

            html_log_format(2, L"~S set_phi ~S ~S &lt;= (~S ~S)~:%",
                pSucc,
                pRegData->GetRd()->GetVar(),
                pRegData->GetRd(),
                pCurr,
                pSx );

            pPhi->GetInputBox(pCurr)->Replace(pSx);
        } // for each insn
    } // pass_rename_set_succ_phi

    // pass_rename_track
    RegData* pass_rename_track(Register* pRx)
    {
        if (NULL == pRx)
        {
            return NULL;
        }

        SlotInsn* pSlot = pRx->GetDfn()->DynamicCast<SlotInsn>();
        if (NULL == pSlot)
        {
            return NULL;
        }

        Register* pCell = pSlot->GetRz();
        if (NULL == pCell || ! pCell->GetDfn()->Is<VarDefInsn>())
        {
            return NULL;
        }

        return pCell->GetExtension<RegData>();
    } // pass_rename_track

    // prepare
    bool prepare(Function* pFun)
    {
        uint nIndex = 0;
        bool fHasNonlocalBlock = pFun->HasNonlocalBlock();
        bool fLiveness = fHasNonlocalBlock;

        if (! fLiveness)
        {
            OptimizeQualities* pQualities =
                &Session::Get()->m_oOptimizeQualities;

            fLiveness =
                pQualities->GetSpeed() > pQualities->GetCompilationSpeed();
        }

        // Assign index to variable allocated to register.
        foreach (Function::EnumVar, oEnum, pFun)
        {
            Variable* pVar = oEnum.Get();
            if (NULL == pVar->GetDfn()) continue;

            switch (pVar->GetStorage())
            {
            case Variable::Storage_Register:
                pVar->SetIndex(nIndex);
                nIndex += 1;
                break;

            case Variable::Storage_Heap:
                if (fHasNonlocalBlock)
                {
                    pVar->SetIndex(nIndex);
                    nIndex += 1;
                }
                break;
            } // switch storage
        } // for each variable

        if (0 == nIndex)
        {
            // No variable for promotion
            return false;
        }

        if (fLiveness)
        {
            return prepare_liveness(pFun, nIndex);
        }
        else
        {
            prepare_computeApproximateLiveness(pFun);
            return true;
        }
    } // prepare

    // prepare_computeApproximateLiveness
    //  Sets LiveIn[UseBB, Var] if UseBB isn't DfnBB.
    void prepare_computeApproximateLiveness(Function* pFun)
    {
        foreach (Function::EnumVar, oEnum, pFun)
        {
            Variable* pVar = oEnum.Get();
            if (Variable::Storage_Register != pVar->GetStorage())
            {
                continue;
            }

            Instruction* pVarDef = pVar->GetDfn();
            if (NULL == pVarDef)
            {
                // Removed or promoted variable
                continue;
            }

            BBlock* pDfnBB = pVarDef->GetBBlock();
            foreach (Register::EnumUseSite, oEnum, pVar->GetDfn()->GetRd())
            {
                OperandBox* pBox = oEnum.Get();
                BBlock* pUseBB = pBox->GetInstruction()->GetBBlock();
                if (pUseBB != pDfnBB)
                {
                    pVar->SetFlag(VarFlag_Global);
                    break;
                }
            } // for each use site
        } // for each variable
    } // prepare_computeApproximateLiveness

    // prepare_liveness
    bool prepare_liveness(Function* pFun, uint cVars)
    {
        VarLivenessPass oPass(this);
            return oPass.Run(pFun, cVars);
    } // prepare_liveness

}; // Cfg2SsaPass

} // namspace

//////////////////////////////////////////////////////////////////////
//
// Optimizer::optimize_cfg2ssa
//
void optimize_cfg2ssa()
{
    Cfg2SsaPass oOptimizer;
    oOptimizer.Run();
} // Optimizer::optimize_cfg2ssa

} // Compiler
