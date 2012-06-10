#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - opt - Type Analysis Pass
// compiler/opt/opt_type.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/opt/opt_type.cpp#23 $
//
//
// Description:
//  This file contains "Type Analysis" pass. 
//
#include "./opt_defs.h"

#include "../ir/ir_bblock.h"
#include "../ir/ir_instruction.h"
#include "../ir/ir_pass.h"

#include "../cm/cm_module.h"
#include "../cm/cm_session.h"

namespace Compiler
{

namespace
{

//////////////////////////////////////////////////////////////////////
//
// SubPassPropagateType
//
class SubPassPropagateType
{
    // Run
    public: static void Run(Function* pFun)
    {
        SubPassPropagateType oSubPass;
            oSubPass.run(pFun);
    } // Run

    uint            m_nSccNum;
    Instruction*    m_pStackTop;

    // SubPassPropagateType ctor
    SubPassPropagateType() : m_nSccNum(0), m_pStackTop(NULL) {}

    typedef Instruction::SccInfo SccInfo;

    // run
    void run(Function* pFun)
    {
        html_log_format(3, L"<h3>Update ~S</h3>~%", pFun);

        prepare(pFun);

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* pBBlock = oEnum.Get();
            foreach (BBlock::EnumInsn, oEnum, pBBlock)
            {
                Instruction* pInsn = oEnum.Get();
                if (pInsn->m_nSccNum == 0) dfs(pInsn);
            } // for each insn
        } // for each bblock
    } // run

    // dfs
    uint dfs(Instruction* pInsn)
    {
        ASSERT(0 == pInsn->m_nSccNum);

        Output* pSd = pInsn->GetOutput();
            if (pSd == Obj_Void) return m_nSccNum;

        m_nSccNum += 1;

        pInsn->m_nSccNum  = m_nSccNum;
        pInsn->m_pSccNext = m_pStackTop;

        m_pStackTop = pInsn;

        uint nMin = m_nSccNum;
        foreach (Instruction::EnumInput, oEnum, pInsn)
        {
            Output* pSd = oEnum.Get()->ToOutput();
                if (NULL == pSd) continue;

            Instruction* pDfn = pSd->GetDfn();

            uint nSccNum = pDfn->m_nSccNum;
                if (0 == nSccNum) nSccNum = dfs(pDfn);

            nMin = min(nMin, nSccNum);
        } // for each user

        if (pInsn->m_nSccNum == nMin)
        {
            Instruction* pSccId   = pInsn;
            Instruction* pSccNext = NULL;

            for (;;)
            {
                Instruction* pTop = m_pStackTop;
                    m_pStackTop = pTop->m_pSccNext;

                    pTop->m_pSccId   = pSccId;
                    pTop->m_pSccNext = pSccNext;
                    pTop->m_nSccNum  = static_cast<uint>(-1);

                if (pTop == pSccId) break;

                pSccNext = pTop;
            } // for

            process_SCC(pInsn);
        } // if

        return nMin;
    } // dfs

    // prepare
    static void prepare(Function* pFun)
    {
        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* pBBlock = oEnum.Get();
            foreach (BBlock::EnumInsn, oEnum, pBBlock)
            {
                Instruction* pInsn = oEnum.Get();
                pInsn->ResetScc();
            } // for each insn
        } // for each bblock
    } // prepare

    enum SccKind
    {
        Single,     // a single member in SCC
        Prural,     // prural members in SCC
        Increase,   // Montonically increased
        Decrease,   // Monotonically decreased
    }; // SccKind

    // classify_SCC
    static SccKind classify_SCC(
        Instruction*            pLeader,
        WorkList_<Instruction>* pMembers )
    {
        if (NULL == pLeader->m_pSccNext) return Single;

        sort_SCC(pLeader, pMembers);

        PhiInsn* pPhi = pMembers->Get()->DynamicCast<PhiInsn>();
            if (NULL == pPhi) return Prural;
            if (2 != pPhi->GetOperandCount()) return Prural;

        pPhi->SortInput();

        return get_induction_call(pPhi->GetRd(), pPhi->GetRy());
    } // classify_SCC

    // get_induction_call
    static SccKind get_induction_call(Register* pRd, Register* pRx)
    {
        if (NULL == pRx) return Prural;

        // Skip RUNTIMECAST
        while (pRx->GetDfn()->Is<RuntimeCastInsn>())
        {
            pRx = pRx->GetDfn()->GetRx();
            if (NULL == pRx) return Prural;
        } // while

        CallInsn* pCall = pRx->GetDfn()->DynamicCast<CallInsn>();
            if (NULL == pCall) return Prural;
            if (pCall->IsNotInline()) return Prural;

        Literal* pCallee = pCall->GetSx()->DynamicCast<Literal>();
            if (NULL == pCallee) return Prural;

        ValuesInsn* pArgs = pCall->GetVy()->GetDfn()->
                DynamicCast<ValuesInsn>();
            if (NULL == pArgs) return Prural;
            if (2 != pArgs->GetOperandCount()) return Prural;

        Register* pR2 = pArgs->GetRx();

        for (;;)
        {
            if (NULL == pR2) return Prural;
            if (! pR2->GetDfn()->Is<SigmaInsn>()) break;
            pR2 = pR2->GetDfn()->GetRx();
        } // for

        if (pR2 != pRd) return Prural;
        unless (pArgs->GetSy()->Is<Literal>()) return Prural;
        unless (fixnump(pArgs->GetLy())) return Prural;

        Int iInc = Fixnum::Decode_(pArgs->GetLy());

        if (pCallee->GetDatum() == QAdd2)
        {
            return iInc >= 0 ? Increase : Decrease;
        }

        if (pCallee->GetDatum() == QSub2)
        {
            return iInc >= 0 ? Decrease : Increase;
        }

        return Prural;
    } // classify_SCC

    // process_SCC -- process Strongly Connected Component.
    static void process_SCC(Instruction* pLeader)
    {
        WorkList_<Instruction> oMembers;

        switch (classify_SCC(pLeader, &oMembers))
        {
        case Single:   process_SCC_single(pLeader); break;
        case Prural:   process_SCC_prural(&oMembers); break;
        case Increase: process_SCC_increase(&oMembers); break;
        case Decrease: process_SCC_decrease(&oMembers); break;
        default: CAN_NOT_HAPPEN();
        } // switch SCC class
    } // process_SCC

    // process_SCC_decrease
    static void process_SCC_decrease(WorkList_<Instruction>* pMembers)
    {
        Instruction* pPhi = pMembers->Get();

        TyInteger oTy1 = TyInteger::Parse(pPhi->GetSx()->GetTy());

        if (! oTy1.IsValid())
        {
            process_SCC_prural(pMembers);
        }
        else
        {
            html_log_format(4, L"<b>SCC ~D</b> decrease~:%",
                pPhi->GetIndex(), pPhi->GetBBlock() );

            TyInteger oTy2(Interval__OpenLower, oTy1.m_upper);
            process_SCC_monotonic(pMembers, oTy2.Unparse());
        }
    } // process_SCC_decrease

    // process_SCC_increase
    static void process_SCC_increase(WorkList_<Instruction>* pMembers)
    {
        Instruction* pPhi = pMembers->Get();

        TyInteger oTy1 = TyInteger::Parse(pPhi->GetSx()->GetTy());

        if (! oTy1.IsValid())
        {
            process_SCC_prural(pMembers);
        }
        else
        {
            html_log_format(4, L"<b>SCC ~D</b> increase~:%",
                pPhi->GetIndex(), pPhi->GetBBlock() );

            TyInteger oTy2(oTy1.m_lower, Interval__OpenUpper);
            process_SCC_monotonic(pMembers, oTy2.Unparse());
        }
    } // process_SCC_increase

    // process_SCC_monotonic
    static void process_SCC_monotonic(WorkList_<Instruction>* pMembers, Ty ty)
    {
        Instruction* pPhi = pMembers->Get();

        html_log_format(4, L"update ~S:~D:~S -> ~W~:%",
            pPhi->GetBBlock(), pPhi->GetIndex(), pPhi, ty );

        pPhi->SetTy(ty);

        pMembers->Pop();

        while (! pMembers->IsEmpty())
        {
            Instruction* pInsn = pMembers->Pop();

            html_log_format(4, L"~S:~D:~S~:%",
                pInsn->GetBBlock(), pInsn->GetIndex(), pInsn );

            update(pInsn);
        } // while

        pPhi->UpdateTy();
        pPhi->SetTy(ty_and(pPhi->GetTy(), ty));

        html_log_format(4, L"~S:~D:~S -> ~W~:%",
            pPhi->GetBBlock(), pPhi->GetIndex(), pPhi, pPhi->GetTy() );
    } // process_SCC_monotonic

    // process_SCC_prural
    static void process_SCC_prural(WorkList_<Instruction>* pMembers)
    {
        Instruction* pLeader = pMembers->Get();

        uint nNth = 0;
        for (;;)
        {
            nNth += 1;

            html_log_format(4, L"<b>[~D] SCC ~D prural</b>~:%",
                    nNth, pLeader->GetIndex() );

            bool fChanged = false;

            foreach (WorkList_<Instruction>::Enum, oEnum, pMembers)
            {
                Instruction* pInsn = oEnum.Get();

                html_log_format(4, L"~S:~D:~S~:%",
                    pInsn->GetBBlock(), pInsn->GetIndex(), pInsn );

                switch (pInsn->GetOpcode())
                {
                case IrOp_CALL:
                    // BUGBUG: NYI: We should call CallInsn::UpdateTy once
                    // we implement SCC check in +/2.
                    break;

                case IrOp_PHI:
                    if (1 == nNth)
                    {
                        update_PHI(pInsn);
                        fChanged = true;
                    }
                    else
                    {
                        if (pInsn->UpdateTy()) fChanged = true;
                    }
                    break;

                case IrOp_RUNTIMECAST:
                case IrOp_SIGMA:
                    if (1 == nNth) break;
                    // FALLTHROUGH

                default:
                    if (update(pInsn)) fChanged = true;
                    break;
                } // switch opcode
            } // for each member

            if (! fChanged) break;
        } // for

        foreach (WorkList_<Instruction>::Enum, oEnum, pMembers)
        {
            Instruction* pInsn = oEnum.Get();
            pInsn->UpdateTy();
        } // for

        pMembers->MakeEmpty();
    } // process_SCC_prural

    // process_SCC_single
    static void process_SCC_single(Instruction* pLeader)
    {
        #if 0
            html_log_format(4, L"Single: ~S:~S~:%",
                pLeader->GetBBlock(), pLeader );
        #endif

        update(pLeader);
    } // process_SCC_single

    // sort_SCC - Sort members in RPO of CFG.
    static void sort_SCC(
        Instruction*            pLeader,
        WorkList_<Instruction>* pMembers )
    {
        for (;;)
        {
            Instruction* pMax = NULL;
            foreach (Instruction::EnumScc, oEnum, pLeader)
            {
                Instruction* pInsn = oEnum.Get();
                if (pMembers->Has(pInsn)) continue;
                if (NULL == pMax || pMax->GetIndex() < pInsn->GetIndex())
                    { pMax = pInsn; }
            } // for each member

            if (NULL == pMax) break;
            pMembers->Push(pMax);
        } // for
    } // sort_SCC

    // update
    static bool update(Instruction* pInsn)
    {
        Ty curty = pInsn->GetTy();

        if (! pInsn->UpdateTy()) return false;

        html_log_format(4, L"updated ~S:~S from ~W~:%",
            pInsn->GetBBlock(), pInsn, curty );

        return true;
    } // update

    // update_PHI
    static void update_PHI(Instruction* pPhi)
    {
        Ty newty = nil;
        foreach (Instruction::EnumInput, oEnum, pPhi)
        {
            Operand* pSx = oEnum.Get();

            Ty ty = pSx->GetTy();

            Output* pSd = pSx->ToOutput();

            if (NULL != pSd)
            {
                Instruction* pDfn = pSd->GetDfn();

                if (pDfn->m_pSccId == pPhi->m_pSccId &&
                    pDfn->GetIndex() >= pPhi->GetIndex() )
                {
                    continue;
                }
            } // if

            newty = ty_or(newty, ty);
        } // for each input

        if (ty_equal(pPhi->GetTy(), newty)) return;

        html_log_format(4, L"update ~S:~S -> ~W~:%",
            pPhi->GetBBlock(), pPhi, newty );

        pPhi->SetTy(newty);
    } // update_PHI
}; // SubPassPropagateType


//////////////////////////////////////////////////////////////////////
//
// ir_void_function
//  If all call sites don't have output, we change all RET to void.
//
bool void_function(Function* pFun)
{
    if (pFun->HasUseSite()) return false;
    if (! pFun->HasCallSite()) return false;    // toplevel

    foreach (Function::EnumCallSite, oEnum, pFun)
    {
        Instruction* pCall = oEnum.Get()->GetInstruction();
        if (pCall->GetOutput() != Obj_Void) return false;
    } // for each call

    html_log_format(4, L"~S => void~:%", pFun);

    ty_set_function_value(pFun->GetTy(), ty_void);

    bool fChanged = false;
    foreach (BBlock::EnumInEdge, oEnum,  pFun->GetExitBB())
    {
        BBlock* pBBlock = oEnum.Get()->GetFrom();

        RetInsn* pRet = pBBlock->GetLastInsn()->
            DynamicCast<RetInsn>();

        if (NULL == pRet) continue;

        pRet->GetOperandBox(0)->Replace(Obj_Void);
        fChanged = true;
    } // for each pred

    return fChanged;
} // void_function


//////////////////////////////////////////////////////////////////////
//
// PassAnalyzeTYpe
//
//  o for each function
//   * Update instruction output type.
//   * Update function value type if function is sole member of SCC.
//  o for each plural SCC
//   * Update function value type
//
class PassAnalyzeType : public ModulePass
{
    bool m_fChanged;

    // List of instructions needed to update type.
    WorkList_<Instruction>  m_oInsns;

        void add_users(Instruction* pInsn)
        {
            Output* pSd = pInsn->GetOutput();
                if (pSd == Obj_Void) return;

            foreach (Output::EnumUseSite, oEnum, pSd)
            {
                Instruction* pUser = oEnum.Get()->GetInstruction();
                if (! m_oInsns.Has(pUser)) m_oInsns.Push(pUser);
            } // for each user
        } // add_users

    public: PassAnalyzeType() : ModulePass(L"OPT-TYPE") {}

    // process_module - Entry point
    virtual void process_module(Module* pModule)
    {
        foreach (Module::EnumFunction, oEnum, pModule)
        {
            Function* pFun = oEnum.Get();
            pFun->GetSccInfo()->ResetScc();
            pFun->GetSccInfo()->m_pSccId = pFun;
            void_function(pFun);
        } // for each fun

        compute_fun_scc(pModule);

        {
            uint nNth = 0;
            do
            {
                if (! Session::Get()->CanContinue()) break;
                nNth += 1;
                html_log_format(2, L"<hr><h2>[~D] Analyze</h2>~%", nNth);
            } while (update_module(pModule));
        }

        foreach (Module::EnumFunction, oEnum, pModule)
        {
            Function* pFun = oEnum.Get();
            pFun->GetSccInfo()->ResetScc();
            ir_remove_useless_instructions(pFun);
            SubPassPropagateType::Run(pFun);
        } // for each fun
    } // process_module

    // update_module
    bool update_module(Module* pModule)
    {
        m_fChanged = false;

        foreach (Module::EnumFunction_Postorder, oEnum, pModule)
        {
            Function* pLeader = oEnum.Get();
            if (pLeader->GetSccInfo()->m_pSccId != pLeader) continue;

            foreach (Function::EnumScc, oEnum, pLeader)
            {
                process_function(oEnum.Get());
            } // for each scc member component
        } // for each scc member region

        foreach (Module::EnumFunction_Postorder, oEnum, pModule)
        {
            Function* pLeader = oEnum.Get();
            if (pLeader->GetSccInfo()->m_pSccId   != pLeader) continue;
            if (pLeader->GetSccInfo()->m_pSccNext == NULL) continue;

            update_fun_plural(pLeader);
        } // for each leader

        foreach (Module::EnumFunction, oEnum, pModule)
        {
            Function* pFun = oEnum.Get();
            if (update_paramty(pFun)) m_fChanged = true;
        } // for each fun

        while (! m_oInsns.IsEmpty())
        {
            Instruction* pInsn = m_oInsns.Pop();
            if (pInsn->UpdateTy()) add_users(pInsn);
        } // while

        return m_fChanged;
    } // update_module

    // update_paramty
    //  Propagates argument type to callee.
    //  Result:
    //    [1] Update PROLOGUE.
    //    [2] Update PROJECT.
    bool update_paramty(Function* pFun)
    {
        html_log_format(3, L"<h3>Update parameter type ~S</h3>~%", pFun);

        if (pFun->HasUseSite())
        {
            html_log_format(4, L"~S has use-site.~:%", pFun);
            return false;
        }

        if (! pFun->HasCallSite())
        {
            // toplevel function
            return false;
        }

        int iMin = pFun->GetArityMin();
            if (0 == iMin)
            {
                html_log_format(4, L"~S takes no parameter.~:%", pFun);
                return false;
            }

        bool fChanged = false;

        Val runner = second(pFun->GetTy());

        for (int iNth = 0; iNth < iMin; iNth++)
        {
            Val paramty = nil;

            foreach (Function::EnumCallSite, oEnum, pFun)
            {
                CallInsn* pCall = oEnum.Get()->GetInstruction()->
                    StaticCast<CallInsn>();

                Ty arg_ty = ty_nth(pCall->GetVy()->GetTy(), iNth);

                paramty = ty_or(paramty, arg_ty);
                if (ty_t == paramty) break;
            } // for each call site

            Val origty = first(runner);
            if (ty_t != paramty && ! ty_equal(origty, paramty))
            {
                html_log_format(4, L"update param ty ~W => ~W~:%",
                    origty,
                    paramty );

                setf_car(paramty, runner);
                fChanged = true;
            }

            runner = cdr(runner);
        } // for i

        if (! fChanged) return false;

        html_log_format(4, L"update param ty ~S => ~W~:%",
            pFun,
            pFun->GetTy() );

        Ty ll_ty = ty_get_function_param(pFun->GetTy());

        Instruction* pPrologue = pFun->GetPrologueInsn();
            pPrologue->SetTy(ll_ty);

        if (NULL == pPrologue->GetVd()) return true;

        // BUGBUG: We should have ProjectInsn::UpdateTy.
        foreach (Values::EnumUseSite, oEnum, pPrologue->GetVd())
        {
            ProjectInsn* pProject = oEnum.Get()->GetInstruction()->
                    DynamicCast<ProjectInsn>();
                if (NULL == pProject) continue;

            Ty ty = ty_nth(
                ll_ty,
                static_cast<uint>(Fixnum::Decode_(pProject->GetLy())) );

            if (ty_equal(pProject->GetTy(), ty)) continue;

            pProject->SetTy(ty);
            add_users(pProject);
        } // for each project

        return true;
    } // update_paramty

    // compute_fun_scc
    static void compute_fun_scc(Module* pModule)
    {
        html_log_format(3, L"<h3>Compute SCC in Call Graph</h3>~%");

        foreach (Module::EnumFunction, oEnum, pModule)
        {
            Function* pFun = oEnum.Get();
            if (0 == pFun->m_nSccNum) FunSccBuilder::Run(pFun);
        } // for each fun
    } // compute_fun_scc

    // compute_fun_ty
    //  Computes value type of function from union of RET.
    static Ty compute_fun_ty(Function* pFun)
    {
        html_log_format(4, L"compute_fun_ty: ~S<ol>~%", pFun);

        Val value_ty = nil;
        foreach (BBlock::EnumInEdge, oEnum, pFun->GetExitBB())
        {
            Instruction* pRet = oEnum.GetNode()->GetLastInsn();
            if (! pRet->Is<RetInsn>()) continue;

            Val ret_ty = get_ty(pRet->GetSx());

            html_log_format(4, L"<li>~S:~S ty=~W</li>~%",
                pRet->GetBBlock(), pRet, ret_ty );

            if (ty_void == ret_ty)
            {
                value_ty = ty_void;
                break;
            }

            value_ty = ty_or_values(value_ty, ret_ty);
        } // for each edge

        html_log_format(4, L"</ol>~%"); 
        html_log_format(4, L"compute_fun_ty: ~W~:%", value_ty);

        if (ty_void == value_ty)
        {
            foreach (BBlock::EnumInEdge, oEnum, pFun->GetExitBB())
            {
                Instruction* pRet = oEnum.GetNode()->GetLastInsn();
                if (pRet->Is<RetInsn>())
                {
                    pRet->GetOperandBox(0)->Replace(Obj_Void);
                }
            } // for each pred
        }

        return value_ty;
    } // compute_fun_ty

    // get_ty
    static Ty get_ty(Operand* pSx)
    {
        Output* pSd = pSx->ToOutput();
            if (NULL == pSd) return pSx->GetTy();

        CallInsn* pCall = pSd->GetDfn()->DynamicCast<CallInsn>();
            if (NULL == pCall) return pSx->GetTy();

        Function* pCallee = pCall->GetSx()->DynamicCast<Function>();
            if (NULL == pCallee) return pCall->GetTy();

        Function* pCaller = pCall->GetParent()->GetFunction();

        if (pCallee->m_pSccId == pCaller->m_pSccId)
        {
            return nil;
        }

        return pCall->GetTy();
    } // get_ty

    // process_function
    // BUGBUG: We should process SCC on call graph as same as SCC on SSA graph.
    void process_function(Function* pFun)
    {
        html_log_format(2, L"<h2>Process SCC[~D] ~S</h2>~%",
            pFun->GetPostorder(), pFun );

        SubPassPropagateType::Run(pFun);

        if (pFun->m_pSccId == pFun && pFun->m_pSccNext == NULL)
        {
            update_fun_ty(pFun);
        }
    } // process_function

    // update_fun_plural
    void update_fun_plural(Function* pLeader)
    {
        html_log_format(3, L"<h3>Update SCC[~D] ~S</h3>~%",
            pLeader->m_nSccNum, pLeader );

        Ty ty = nil;
        foreach (Function::EnumScc, oEnum, pLeader)
        {
            ty = ty_or(ty, compute_fun_ty(oEnum.Get()));
            if (ty == ty_values_rest_t) break;
            // Note: Don't stop iteration when ty is t. Other function
            // can be values type.
        } // for each scc member

        foreach (Function::EnumScc, oEnum, pLeader)
        {
            Function* pFun = oEnum.Get();
            update_fun_ty_aux(pFun, ty);
        } // for each scc member
    } // update_fun_plural

    // update_fun_ty
    void update_fun_ty(Function* pFun)
    {
        // Update function value type
        Ty fun_ty = pFun->GetTy();
        Ty cur_ty = ty_get_function_value(fun_ty);
        if (ty_void == cur_ty) return;

        update_fun_ty_aux(pFun, compute_fun_ty(pFun));
    } // update_fun_ty

    // update_fun_ty_aux
    void update_fun_ty_aux(Function* pFun, Ty new_ty)
    {
        Ty fun_ty = pFun->GetTy();
        Ty cur_ty = ty_get_function_value(fun_ty);

        if (equal(new_ty, cur_ty)) return;

        m_fChanged = true;

        setf_car(new_ty, cddr(fun_ty));

        html_log_format(3, L"update ~S: ~W => ~W~:%",
            pFun, cur_ty, new_ty );

        foreach (Function::EnumCallSite, oEnum, pFun)
        {
            rewrite_CALL_vd_to_rd(oEnum.Get()->GetInstruction(), new_ty);
        } // for each call site
    } // update_fun_ty_aux

    // rewrite_CALL_vd_to_rd
    //  Rewrite multiple value receiver to single value receiver.
    static void rewrite_CALL_vd_to_rd(Instruction* pCall, Ty ty)
    {
        html_log_format(4, L"update ~S:~S => ~W~:%", 
            pCall->GetBBlock(), pCall, ty );

        if (pCall->GetTy() == ty_void) return;

        pCall->SetTy(ty);

        if (ty_is_values(ty)) return;

        Values* pVd = pCall->GetVd();
            if (NULL == pVd) return;

        Register* pRd = new Register();
        pCall->SetOutput(pRd);

        ir_insert_insn(new ValuesInsn(pVd, pRd), pCall->GetNext());

        Values::EnumUseSite oEnum(pVd);
        while (! oEnum.AtEnd())
        {
            OperandBox* pBox = oEnum.Get();
                oEnum.Next();

            Instruction* pUse = pBox->GetInstruction();
            switch (pUse->GetOpcode())
            {
            case IrOp_PHI:
            case IrOp_RET:
                pBox->Replace(pRd);
                break;
            } // switch opcode
        } // for each use
    } // rewrite_CALL_vd_to_rd

    // FunSccBuilder
    class FunSccBuilder
    {
        typedef Function::SccInfo SccInfo;

        uint        m_nSccNum;
        Function*   m_pStackTop;

        FunSccBuilder() : m_nSccNum(0), m_pStackTop(NULL) {}

        public: static void Run(Function* pFun)
        {
            FunSccBuilder oSccBuilder;
            oSccBuilder.dfs(pFun);
        } // Run

        uint dfs(Function* pFun)
        {
            ASSERT(0 == pFun->m_nSccNum);

            m_nSccNum += 1;

            pFun->m_nSccNum  = m_nSccNum;
            pFun->m_pSccNext = m_pStackTop;
            m_pStackTop = pFun;

            html_log_format(5, L"dfs(~S)~D~:%", pFun, pFun->m_nSccNum);

            uint nMin = m_nSccNum;
            foreach (BBlock::EnumInEdge, oEnum, pFun->GetExitBB())
            {
                BBlock* pBBlock = oEnum.GetNode();
                RetInsn* pRet = pBBlock->GetLastInsn()->DynamicCast<RetInsn>();
                if (NULL == pRet) continue;

                Output* pOx = pRet->GetSx()->ToOutput();
                    if (NULL == pOx) continue;

                CallInsn* pCall = pOx->GetDfn()->DynamicCast<CallInsn>();
                    if (NULL == pCall) continue;

                Function* pCallee = pCall->GetSx()->DynamicCast<Function>();
                    if (NULL == pCallee) continue;

                uint nSccNum = pCallee->GetSccInfo()->m_nSccNum;
                    if (0 == nSccNum)
                    {
                        nSccNum = dfs(pCallee);
                    }

                nMin = min(nMin, nSccNum);
            } // for each input

            if (pFun->m_nSccNum == nMin)
            {
                Function* pSccId = pFun;
                Function* pSccNext = NULL;
                for (;;)
                {
                    Function* pTop = m_pStackTop;
                        m_pStackTop = pTop->GetSccInfo()->m_pSccNext;

                    SccInfo* pSccInfo = pTop->GetSccInfo();
                        pSccInfo->m_pSccId   = pSccId;
                        pSccInfo->m_pSccNext = pSccNext;

                    html_log_format(4, L"SCC[~D] ~S~:%",
                        pSccId->m_nSccNum, pTop );

                    pSccInfo->m_nSccNum  = static_cast<uint>(-1);

                    pSccNext = pTop;

                    if (pTop == pFun) break;
                } // for
            } // if

            return nMin;
        } // dfs
    }; // FunSccBuilder
}; // PassAnalyzeType

} // namespace

//////////////////////////////////////////////////////////////////////
//
// optimize_analyze_type
//
void optimize_analyze_type()
{
    PassAnalyzeType oPass;
        oPass.Run();
} // optimize_analyze_type

} // Compiler
