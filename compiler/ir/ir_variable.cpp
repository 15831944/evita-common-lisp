#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - variable
// ir_variable.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_variable.cpp#6 $
//
#include "./ir_operand.h"
#include "./ir_fns.h"
#include "./ir_function.h"
#include "./ir_instruction.h"

namespace MiniLisp
{
    Val print_symbol(Val, Val);
}

namespace Compiler
{

static const char16*
k_rgwszStorage[] =
{
    L"anchor",
    L"heap",
    L"literal",
    L"register",
    L"stack",
}; // k_rgwszStorage

//////////////////////////////////////////////////////////////////////
//
//  Variable::HtmlPrint
//
void Variable::HtmlPrint(Val stream, bool fDef) const
{
    HtmlA(stream, fDef);

    html_format(stream, L"[var ~A ~W]",
        k_rgwszStorage[GetStorage()], GetName() );

    cm_format(stream, L"</a>");
} // Variable::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
// Variable::IsSSA
//
//  Description:
//   Returns true if all usage is LOAD.
//
bool
Variable::IsSSA() const
{
    if (NULL == GetDfn()) return false;

    VarDefInsn* pVarDef = GetDfn()->StaticCast<VarDefInsn>();

    foreach (Register::EnumUseSite, oEnum, pVarDef->GetRd())
    {
        OperandBox*  pBox = oEnum.Get();

        Instruction* pSlot = pBox->GetInstruction();

        if (! pSlot->Is<SlotInsn>())
        {
            html_log_format(4, L"~S is used by ~S.~:%",
                this, pSlot->GetBBlock(), pSlot );
            return false;
        }

        foreach (Register::EnumUseSite, oEnum, pSlot->GetRd())
        {
            Instruction* pLoad = oEnum.Get()->GetInstruction();

            if (! pLoad->Is<LoadInsn>())
            {
                html_log_format(4, L"~S is used by ~S:~S.~:%",
                    this, pLoad->GetBBlock(), pLoad);
                return false;
            }
        } // for each user of SLOT
    } // for each use

    // BUGBUG: We should check inner functions instead of checking
    // m_pUpVarCount.
    if (0 != m_nUpVarCount)
    {
        html_log_format(4, L"~S is used by inner function.~:%", this);
        return false;
    }

    return true;
} // Variable::IsSSA


//////////////////////////////////////////////////////////////////////
//
// Variable::Realize
//
void Variable::Realize(OperandBox* pBox)
{
    Instruction* pInsn = pBox->GetInstruction();
    Function* pUser = pInsn->GetBBlock()->GetFunction();

    switch (pInsn->GetOpcode())
    {
    case IrOp_UPVARDEF:
        ASSERT(GetOwner() != pUser);
        pUser->AddUpVarSite(pBox);
        m_nUpVarCount += 1;
        break;

    case IrOp_VARDEF:
        ASSERT(NULL == m_pOwner);
        m_pOwner = pUser;
        pUser->AddVar(this);
        SetDfn(pInsn);
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch opcode
} // Variable::Realize


//////////////////////////////////////////////////////////////////////
//
// Variable::Unrealize
//
void Variable::Unrealize(OperandBox* pBox)
{
    Instruction* pInsn = pBox->GetInstruction();
    Function* pUser = pInsn->GetBBlock()->GetFunction();


    switch (pInsn->GetOpcode())
    {
    case IrOp_UPVARDEF:
        pUser->RemoveUpVarSite(pBox);

        ASSERT(m_nUpVarCount >= 1);
        m_nUpVarCount -= 1;
        break;

    case IrOp_VARDEF:
        // OPT_Inline moves UpVarDef instruction to owner.
        ASSERT(pInsn->Is<VarDefInsn>());

        // Note: We don't remove variable from list for generating
        // debug information.
        SetDfn(NULL);
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch opcode
} // Variable::Unrealize


//////////////////////////////////////////////////////////////////////
//
// ir_find_variable
//
//  o Returns %rd of VARDEF instruction.
//  o Returns %rd of UPVARDEF instruction in pFun for pVar.
//
Register*
ir_find_variable(Function* pUser, Variable* pVar)
{
    ASSERT(NULL != pUser);
    ASSERT(NULL != pVar);

    if (pVar->GetOwner() == pUser)
    {
        return pVar->GetDfn()->GetRd();
    }
    else
    {
        foreach (Function::EnumUpVarSite, oEnum, pUser)
        {
            OperandBox* pBox = oEnum.Get();
            if (pBox->GetOperand() == pVar)
            {
                ASSERT(pBox->GetInstruction()->Is<UpVarDefInsn>());
                return pBox->GetInstruction()->GetRd();
            }
        } // for each upvar

        return NULL;
    } // if
} // ir_find_variable

} // Compiler
