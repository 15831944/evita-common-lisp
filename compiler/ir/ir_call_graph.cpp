#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - Call Graph
// ir/ir_call_graph.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_call_graph.cpp#8 $
//

#include "./ir_defs.h"
#include "./ir_fns.h"
#include "./ir_call_graph.h"

#include "../cm/cm_session.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// ir_eliminate_unreachable_functions
//
void
ir_eliminate_unreachable_functions()
{
    Module* pModule = Session::Get()->GetModule();

    pModule->PrepareTraversal();

    Module::EnumFunction oEnum(pModule);
    while (! oEnum.AtEnd())
    {
        Function* pFun = oEnum.Get();
            oEnum.Next();
        if (0 == pFun->GetPreorder())
        {
            ir_remove_function(pFun);
        }
    } // while
} // ir_eliminate_unreachable_functions


//////////////////////////////////////////////////////////////////////
//
// ir_find_cg_edge
//
CgEdge* ir_find_cg_edge(Function* pFrom, Function* pTo)
{
    foreach (Function::EnumInEdge, oEnum, pTo)
    {
        CgEdge* pEdge = oEnum.Get();
        if (pEdge->GetFrom() == pFrom) return pEdge;
    } // for each in-edge
    return NULL;
} // ir_find_cg_edge


//////////////////////////////////////////////////////////////////////
//
// NewFunction
//
Function*
NewFunction(Val name, Function::Kind eKind)
{
    Function* pFun = new Function(name, eKind);

    // Make entry and exit bblock.
    BBlock* pEntryBB = pFun->Append(new BBlock(BBlock::Kind_Normal));
    BBlock* pStartBB = pFun->Append(new BBlock(BBlock::Kind_Normal));
    BBlock* pExitBB  = pFun->Append(new BBlock(BBlock::Kind_Normal));

    pEntryBB->AppendInsn(new 
        EntryInsn(ty_values_rest_t, new Values(), NewLiteral(Knone)) );

    pEntryBB->AppendInsn(new JumpInsn(pStartBB));

    pExitBB->AppendInsn(new ExitInsn());

    AddCfgEdge(pEntryBB, pExitBB, CfgEdge::Kind_Pseudo);

    Session::Get()->GetModule()->Append(pFun);

    return pFun;
} // NewFunction


//////////////////////////////////////////////////////////////////////
//
// ir_remove_function
//
// Called by:
//  ir_eliminate_unreachable_functions
//
void
ir_remove_function(Function* pFun)
{
    ASSERT(NULL != pFun);

    Function::EnumBBlock oEnum(pFun);
    while (! oEnum.AtEnd())
    {
        BBlock* pBBlock = oEnum.Get();
        oEnum.Next();
        ir_remove_bblock(pBBlock);
    } // while

    pFun->GetParent()->Remove(pFun);
} // ir_remove_function


//////////////////////////////////////////////////////////////////////
//
// ir_split_closure
//
// Splits closure into a template and a body function then returns
// a template function.
//
Function* ir_split_closure(Function* pClosure)
{
    Function* pTempl = NewFunction(
        pClosure->GetName(),
        Function::Kind_Template );

    pTempl->SetArity(pClosure->GetArityMin(), pClosure->GetArityMin());

    pTempl->SetRestParam(
        pClosure->HasRestParam() ||
        pClosure->GetArityMin() != pClosure->GetArityMax() );

    html_log_format(2,
        L"<h3>split closure ~S to template ~S and body.</h3>~%",
        pClosure,
        pTempl );

    // Update use site
    {
        Function::EnumUseSite  oEnum(pClosure);
        while (! oEnum.AtEnd())
        {
            Instruction* pInsn = oEnum.Get()->GetInstruction();
                oEnum.Next();

            if (pInsn->Is<ClosureInsn>())
            {
                html_log_format(3, L"update ~S to ", pInsn);

                pInsn->GetOperandBox(0)->Replace(pTempl);

                html_log_format(3, L" ~S~:%", pInsn);
            }
            else
            {
                html_log_format(1,
                    L"<h1 class=e>"
                    L" Unexpected closure user ~S:~S.",
                    pInsn->GetBBlock(),
                    pInsn );

                warn(L"Broken closure reference.");
            }
        } // for each use site
    } // call site

    // Copy UpVars
    {
        Instruction* pInsn = pTempl->GetEntryBB()->GetLastInsn();
        foreach (Function::EnumUpVar, oEnum, pClosure)
        {
            Variable* pVar = oEnum.Get();
            Ty ty;
            switch (pVar->GetStorage())
            {
            case Variable::Storage_Heap:
                ty = ty_closed_cell;
                break;
            case Variable::Storage_Literal:
                ty = ty_c6_literal_cell;
                break;
            default:
                warn(L"Broken UPVAR: ~S", pVar->GetName());
                ty = ty_t;
                break;
            } // switch storage

            ir_insert_insn(
                new UpVarDefInsn(ty, new Register(), pVar),
                pInsn );
        } // for each upvar
    } // upvar

    BBlock* pStartBB = pTempl->GetStartBB();

    // Prologue
    {
        Values* pVd = new Values();
        Instruction* pPrologue = new PrologueInsn(
            ty_values_rest_t, pVd,
            pTempl->GetEntryInsn()->GetVd(),
            NewLiteral(pTempl->HasRestParam() ? Kstack : Knone) );
        pStartBB->AppendInsn(pPrologue);
    } // prolgoue

    // Insert CALL
    {
        Instruction* pArgsI;
        ValuesInsn* pValues = new ValuesInsn(new Values());
        {
            Values* pVx = pTempl->GetPrologueInsn()->GetVd();

            int iMin = pTempl->GetArityMin();
            for (int i = 0; i < iMin; i++)
            {
                Register* pRx = new Register();
                Instruction* pProject =
                    new ProjectInsn(Qt, pRx, pVx, i);

                pStartBB->AppendInsn(pProject);
                html_log_format(3, L"append ~S~:%", pProject);

                pValues->InsertBefore(pRx, i);
            } // for i

            if (! pTempl->HasRestParam())
            {
                pArgsI = pValues;
            }
            else
            {
                Register* pRx = new Register();
                Instruction* pProject =
                    new ProjectInsn(Qt, pRx, pVx, iMin);

                pStartBB->AppendInsn(pProject);
                html_log_format(3, L"append ~S~:%", pProject);

                pValues->InsertBefore(pRx, iMin);

                pArgsI = new ValuesAInsn(pValues->GetVd(), pValues);
            } // if

            pStartBB->AppendInsn(pArgsI);
            html_log_format(3, L"append ~S~:%", pArgsI);
        }

        Val value_ty = ty_get_function_value(pClosure->GetTy());
        Output* pSd = NewOutput(value_ty);
        Instruction* pCall = new CallInsn(
            value_ty, pSd, pClosure, pArgsI->GetVd() );

        pStartBB->AppendInsn(pCall);
        html_log_format(3, L"append ~S~:%", pCall);

        Instruction* pRet = new RetInsn(pSd);
        pStartBB->AppendInsn(pRet);
        html_log_format(3, L"append ~S~:%", pRet);
    } // call

    return pTempl;
} // ir_split_closure

} // Compiler
