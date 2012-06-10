#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - opt - Closure Promotion Phase
// compiler/opt/opt_closure.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/opt/opt_closure.cpp#7 $
//
//
// Description:
//  This file contains "Closure Promotion" phase. This phase promotes
//  variables allocated on closed-cell to stack-cell and rewrites
//  corresponding instructions.
//
//     Before                       After
//    --------------------------+------------------------------
//     VARDEF <closed-cell> %rd  |   VARDEF <stack-cell> %rd
//     SLOT <closed-cell>        |   SLOT <stack-cell>
//     SLOT <closed-cell>        |   SLOT <literal-cell>
//
#include "./opt_defs.h"

#include "../ir/ir_pass.h"
#include "../cm/cm_session.h"

namespace Compiler
{

namespace
{

enum Usage
{
    Usage_Call,
    Usage_Downward,
    Usage_Upward,
}; // Usage

//////////////////////////////////////////////////////////////////////
//
// Closure Optimizer
//
class ClosureOptimizer : public ModulePass
{
    public: ClosureOptimizer() :
        ModulePass(L"OPT-CLOSURE") {}

    protected: virtual void process_module(Module* pModule)
    {
        ir_eliminate_unreachable_functions();
        prepare(pModule);
        allocate_vars(pModule);

        // Rewrite callee before caller
        foreach (Module::EnumFunction_Postorder, oEnum, pModule)
        {
            rewrite(oEnum.Get());
        } /// for each fun

        foreach (Module::EnumFunction, oEnum, pModule)
        {
            oEnum.Get()->Reset();
        } /// for each fun
    } // Run

    // allocate_vars
    void allocate_vars(Module* pModule)
    {
        // Set variable storage
        //  o literal   when used in closure.
        //  o stack     when used in inner function
        foreach (Module::EnumFunction, oEnum, pModule)
        {
            Function* pFun = oEnum.Get();

            if (Usage_Upward == pFun->GetFlag())
            {
                foreach (Function::EnumUpVar, oEnum, pFun)
                {
                    Variable* pVar = oEnum.Get();
                    pVar->SetStorage(Variable::Storage_Literal);
                } // for each
            }
            else
            {
                foreach (Function::EnumUpVar, oEnum, pFun)
                {
                    Variable* pVar = oEnum.Get();
                    if (pVar->GetStorage() == Variable::Storage_Register)
                    {
                        pVar->SetStorage(Variable::Storage_Stack);
                    }
                } // for each
            }
        } // for each fun

        foreach (Module::EnumFunction, oEnum, pModule)
        {
            Function* pFun = oEnum.Get();

            foreach (Function::EnumVar, oEnum, pFun)
            {
                Instruction* pVarDef = oEnum.Get()->GetDfn();
                if (NULL != pVarDef)
                {
                    allocate_var(pVarDef);
                }
            } // for each var

            foreach (Function::EnumUpVarSite, oEnum, pFun)
            {
                allocate_var(oEnum.Get()->GetInstruction());
            } // for each var
        } // for each fun
    } // allocate_vars

    // allocate_var
    void allocate_var(Instruction* pInsn)
    {
        Variable* pVar = pInsn->GetSx()->StaticCast<Variable>();

        if (Variable::Storage_Literal != pVar->GetStorage())
        {
            return;
        }

        foreach (Register::EnumUseSite, oEnum, pInsn->GetRd())
        {
            OperandBox* pBox = oEnum.Get();
            Instruction* pSlot = pBox->GetInstruction()->
                DynamicCast<SlotInsn>();
            if (NULL != pSlot)
            {
                foreach (Register::EnumUseSite, oEnum, pSlot->GetRd())
                {
                    Instruction* pInsn = oEnum.Get()->GetInstruction();
                    if (pInsn->Is<StoreInsn>())
                    {
                        pVar->SetStorage(Variable::Storage_Heap);
                        return;
                    }
                } // for use site
            } // if
        } // for use site
    } // allocate_var

    // prepare
    void prepare(Module* pModule)
    {
        foreach (Module::EnumFunction, oEnum, pModule)
        {
            Function* pFun = oEnum.Get();

            foreach (Function::EnumVar, oEnum, pFun)
            {
                Variable* pVar = oEnum.Get();
                pVar->SetStorage(Variable::Storage_Register);
            } // for each var

            Usage eUsage = Usage_Call;
            foreach (Function::EnumUseSite, oEnum, pFun)
            {
                OperandBox* pBox = oEnum.Get();

                if (! pBox->GetInstruction()->Is<OpenFinallyInsn>())
                {
                    eUsage = Usage_Upward;
                    break;
                }

                eUsage = Usage_Downward;
            } // for each use site
            pFun->SetFlag(eUsage);
        } // for each fun
    } // prepare

    // rewrite
    void rewrite(Function* pFun)
    {
        html_log_format(2, L"<h2>rewrite ~S</h2>~%", pFun);

        if (! rewrite_aux(pFun)) return;

        //bool fClosure = pFun->IsClosure();

        // Remove literal-cells initialized with literal.
        ir_remove_useless_instructions(pFun);

        #if 0

        unless (pFun->IsClosure() && ! pFun->HasUpVar()) return;

        // Remove CLOSURE instructions if closure has no upvar.
        foreach (Function::EnumUseSite, oEnum, pFun)
        {
            ClosureInsn* pClosure = oEnum.Get()->GetInstruction()->
                DynamicCast<ClosureInsn>();

            if (NULL != pClosure)
            {
                ir_replace_all_users(pFun, pClosure->GetRd());
            }
        } // for each use site of pFun
        #endif
    } // rewrite

    // rewrite_aux
    //  SLOT <closed-cell> => SLOT <stack-cell> if SC(var) is register/stack.
    bool rewrite_aux(Function* pFun)
    {
        bool fHasUseless = false;

        foreach (Function::EnumVar, oEnum, pFun)
        {
            Variable*    pVar = oEnum.Get();
            Instruction* pVarDef = pVar->GetDfn();

            if (NULL == pVarDef)
            {
                // Removed variable
                continue;
            }

            html_log_format(4, L"<h4>process local ~S</h4>~%", pVarDef);

            switch (pVar->GetStorage())
            {
            case Variable::Storage_Register:
            case Variable::Storage_Stack:
                pVarDef->SetTy(ty_c6_stack_cell);
                rewrite_SLOTs(pVarDef->GetRd(), NewLiteral(Qc6_stack_cell));
                break;

            case Variable::Storage_Literal:
                pVarDef->SetTy(ty_c6_literal_cell);
                rewrite_SLOTs(pVarDef->GetRd(), NewLiteral(Qc6_literal_cell));
                rewrite_for_literal(pVarDef->GetRd(), pVarDef->GetSy());
                fHasUseless = true;
                break;

            case Variable::Storage_Heap:
                // nothing to do
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch eStorage
        } // for each variable

        Function::EnumUpVarSite oEnum(pFun);
        while (! oEnum.AtEnd())
        {
            Instruction* pUpVarDef = oEnum.Get()->GetInstruction();
                oEnum.Next();

            Variable* pVar = pUpVarDef->GetSx()->StaticCast<Variable>();

            html_log_format(4, L"<h4>process upvar ~S</h4>~%", pVar);

            switch (pVar->GetStorage())
            {
            case Variable::Storage_Register:
            case Variable::Storage_Stack:
                pUpVarDef->SetTy(ty_c6_stack_cell);
                rewrite_SLOTs(pUpVarDef->GetRd(), NewLiteral(Qc6_stack_cell));
                break;

            case Variable::Storage_Literal:
                pUpVarDef->SetTy(ty_c6_literal_cell);

                rewrite_SLOTs(
                    pUpVarDef->GetRd(),
                    NewLiteral(Qc6_literal_cell) );

                if (pVar->GetDfn()->GetSy()->Is<Literal>())
                {
                    rewrite_for_literal(
                        pUpVarDef->GetRd(),
                        pVar->GetDfn()->GetSy());

                    ir_remove_insn(pUpVarDef);

                    fHasUseless = true;
                }
                break;

            case Variable::Storage_Heap:
                // nothing to do
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch eStorage
        } // for each upvar site

        return fHasUseless;
    } // rewrite_aux

    // rewrite_for_literal
    void rewrite_for_literal(Register* pCell, Operand* pSx)
    {
        Register::EnumUseSite oEnum(pCell);

        while (! oEnum.AtEnd())
        {
            OperandBox* pBox = oEnum.Get();
                oEnum.Next();

            Instruction* pInsn = pBox->GetInstruction();
            if (! pInsn->Is<SlotInsn>())
            {
                pBox->Replace(pSx);
            }
            else
            {
                foreach (Register::EnumUseSite, oEnumLoad, pInsn->GetRd())
                {
                    LoadInsn* pLoad = oEnumLoad.Get()->GetInstruction()->
                        StaticCast<LoadInsn>();

                    Register::EnumUseSite oEnum(pLoad->GetRd());
                    while (! oEnum.AtEnd())
                    {
                        OperandBox* pBox = oEnum.Get();
                            oEnum.Next();
                        pBox->Replace(pSx);
                    } // for each use site of LOAD
                } // for each use site of SLOT
            } // if
        } // for each use site
    } // rewrite_for_literal

    // rewrite_SLOTs
    //  NOTE: We don't update type of VALUES instruction here.
    void rewrite_SLOTs(Register* pCell, Literal* pType)
    {
        foreach (Register::EnumUseSite, oEnum, pCell)
        {
            OperandBox* pBox = oEnum.Get();
            Instruction* pSlot = pBox->GetInstruction();

            html_log_format(5, L"~S~:%", pSlot);

            if (pSlot->Is<SlotInsn>())
            {
                pSlot->GetOperandBox(0)->SetOperand(pType);
            }
        } // for each use site
    } // rewrite_SLOTs
}; // ClosureOptimizer

} // namespace

//////////////////////////////////////////////////////////////////////
//
// optimize_closure
//
void
optimize_closure()
{
    ClosureOptimizer oOptimizer;
    oOptimizer.Run();
} // optimize_closure

} // Compiler
