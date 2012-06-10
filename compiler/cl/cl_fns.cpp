#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - Common Lisp Parser - Utility
// cl_fns.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_fns.cpp#7 $
//
// Description:
//  This file contatins following functions:
//      ClParser::internVarCell
//      ClParser::isToplevelEvalWhen
//      ClParser::mergeUpVars
//      ClParser::newBBlock
//
#include "./cl_defs.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// ClParser::internVarCell
//
//  o Returns %rd of VARDEF instruction.
//  o Returns %rd of UPVAR instruction in pFun for pVar->
//
Register*
ClParser::internVarCell(Variable* pVar)
{
    Register* pCell = ir_find_variable(GetFunction(), pVar);
    if (NULL == pCell)
    {
        pCell = new Register();

        Function* pUser = GetFunction();

        ir_insert_insn(
            new UpVarDefInsn(pCell, pVar),
            pUser->GetEntryInsn()->GetNext() );
    } // if

    return pCell;
} // ClParser::internVarCell


//////////////////////////////////////////////////////////////////////
//
// ClParser::mergeUpVars
//
// Description:
//  Inserts UPVARDEF into caller for upvars used in pCallee-> This
//  function is called during building operands of CLOSURE instruction.
//
void
ClParser::mergeUpVars(Function* pCallee)
{
    Function* pCaller = GetFunction();

    if (pCaller == pCallee)
    {
        return;
    }

    foreach (Function::EnumUpVar, oEnum, pCallee)
    {
        Variable* pVar = oEnum.Get();
        if (pVar->GetOwner() != pCaller)
        {
            internVarCell(pVar);
        }
    } // for each upvar
} // ClParser:mergeUpVars


//////////////////////////////////////////////////////////////////////
//
// New BBlock
//
BBlock*
ClParser::newBBlock()
{
    BBlock* rBBlock = NewBBlock();
    return GetFunction()->AppendBBlock(rBBlock);
} // ClParser::unreachable_form


namespace
{

static void
replace_load(Operand* pSy, Register* pRd)
{
    Ty varty = pSy->GetTy();
    Register* pRy = pSy->DynamicCast<Register>();
    Val initform = NULL == pRy ? nil : pRy->GetForm();

    Register::Enum oEnum(pRd);

    while(! oEnum.AtEnd())
    {
        OperandBox* pBox = oEnum.Get();
            oEnum.Next();

        RuntimeCastInsn* pCast = pBox->GetInstruction()->
            DynamicCast<RuntimeCastInsn>();

        if (NULL != pCast)
        {
            Ty expected_ty = pCast->GetLy();
            if (nil == ty_and(varty, expected_ty))
            {
                if (NULL == pRy || nil == initform)
                {
                    warn(L"Type of variable ~S must be ~S instead of ~S.",
                        pRd->GetVar()->GetName(),
                        expected_ty, varty );
                }
                else
                {
                    Session::Get()->RememberSource(initform);
                    warn(L"Type of variable ~S must be ~S instead of ~S from ~S.",
                        pRd->GetVar()->GetName(),
                        expected_ty, varty, initform );
                }
            } // if
        } // if cast

        pBox->Replace(pSy);
    } // for each box
} // replace_load


//////////////////////////////////////////////////////////////////////
//
// replace_loads
//
// Description:
//  Replace Load Instructions.
//
//      VARDEF closed-cell %cell <= name %sy
//      ...
//      SLOT (ptr ty) %r1 <= closed-cell value %cell
//      LOAD %r2 <= %r1
//
static void
replace_loads(Operand* pSy, Register* pRCell)
{
    html_log_format(4, L"<ol>");

    Register::EnumUseSite oEnum(pRCell);
    while (! oEnum.AtEnd())
    {
        OperandBox* pBox = oEnum.Get();
            oEnum.Next();

        Instruction* pInsn = pBox->GetInstruction();

        html_log_format(4, L"<li>~S:~S~%", pInsn->GetBBlock(), pInsn);

        if (pInsn->Is<SlotInsn>())
        {
            html_log_format(4, L"<ol>");

            Register::EnumUseSite oEnum(pInsn->GetRd());
            while (! oEnum.AtEnd())
            {
                Instruction* pLoad= oEnum.Get()->GetInstruction();
                    oEnum.Next();

                html_log_format(4, L"<li>~S:~S~%",
                    pLoad->GetBBlock(), pLoad );

                if (pLoad->Is<LoadInsn>())
                {
                    replace_load(pSy, pLoad->GetRd());
                }
            } // for each use site

            html_log_format(4, L"</ol>");
        }
        else
        {
            // Maybe ValuesInsn
            pBox->Replace(pSy);
        }

        html_log_format(4, L" </li>~%");
    } // for each use site of VARDEF

    html_log_format(4, L"</ol>");
} // replace_loads

} // namespace

//////////////////////////////////////////////////////////////////////
//
// Optimize_VARDEF
//
bool
ClParser::optimize_VARDEF(Variable* pVar)
{
    if (! pVar->HasDfn()) return false;

    VarDefInsn* pVarDef = pVar->GetDfn()->StaticCast<VarDefInsn>();

    html_log_format(4, L"<h4>try optimize ~S:~S</h4>~%", 
        pVarDef->GetBBlock(), pVarDef );

    if (! pVar->IsSSA())
    {
        html_log_format(4, L"<p>~S isn't SSA.</p>~%", pVar);
        return false;
    }

    bool fOptimized = false;

    Operand* pSy = pVarDef->GetSy();

    if (0 == pVar->GetUpVarCount())
    {
        replace_loads(pSy, pVarDef->GetRd());
        fOptimized = true;
    } // if

    // Replaces upvars if pSy is literal.
    if (pSy->Is<Literal>())
    {
        foreach (Module::EnumFunction, oEnum, Session::Get()->GetModule())
        {
            Function* pFun = oEnum.Get();

            bool fUpdated = false;

            foreach (Function::EnumUpVarSite, oEnum, pFun)
            {
                OperandBox* pUseSite = oEnum.Get();
                if (pUseSite->GetOperand() == pVar)
                {
                    UpVarDefInsn* pUpVarDef = pUseSite->GetInstruction()->
                        StaticCast<UpVarDefInsn>();

                    replace_loads(pSy, pUpVarDef->GetRd());
                    fUpdated = true;
                } // if
            } // for upvar site

            if (fUpdated)
            {
                fOptimized = true;

                // Remove upvar related instructions in a user.
                //ir_remove_useless_instructions(pFun);
            }
        } // for each fun
    } // if

    return fOptimized;
} // ClParser::optimize_VARDEF

} // Compiler
