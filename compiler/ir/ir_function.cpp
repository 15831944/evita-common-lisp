#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - ir - Function
// compiler/ir/ir_function.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_function.cpp#6 $
//
#include "./ir_function.h"

#include "../cm/cm_session.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Function constructor
//
Function::Function(
    Val         name,
    Kind        eKind ) :
        Operand(Operand::Kind_Function),
        m_document(nil),
        m_cExtraParams(0),
        m_funobj(nil),
        m_eKind(eKind),
        m_fMayBeClosure(false),
        m_name(name),
        m_fRest(false),
        m_iMin(0),
        m_iMax(0)
{
    ASSERT(Kind_Anchor != eKind);

    m_nName = Session::NewName();
    m_ty = ty_function;
} // Function::Function


//////////////////////////////////////////////////////////////////////
//
//  Function::HtmlPrint
//
void Function::HtmlPrint(Val stream, bool fDef) const
{
    HtmlA(stream, fDef);

    if (IsClosure())
    {
        cm_format(stream, L"[closure ~S]", GetName());
    }
    else if (Kind_Finally == m_eKind)
    {
        cm_format(stream, L"[finally ~S]", GetName());
    }
    else if (HasUpVar())
    {
        cm_format(stream, L"[inner ~S]", GetName());
    }
    else
    {
        cm_format(stream, L"[function ~S]", GetName());
    }

    write_string(L"</a>", stream);
} // Function::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
// Function::AddVar
//
Variable*
Function::AddVar(Variable* pVar)
{
    ASSERT(NULL != pVar);
    pVar->SetOwner(this);
    return m_oVariables.Append_(pVar);
} // Function::AddVar


//////////////////////////////////////////////////////////////////////
//
// Function::RemoveVar
//
Variable*
Function::RemoveVar(Variable* pVar)
{
    ASSERT(NULL != pVar);
    //pVar->SetOwner(NULL);
    return m_oVariables.Remove_(pVar);
} // Function::RemoveVar


// GetEntryBB
BBlock* Function::GetEntryBB() const
{
    return Layout::GetHead();
} // Function::GetEntryBB

// GetExitBB
BBlock* Function::GetExitBB() const
{
    return Layout::GetTail();
} // Function::GetExitBB

// GetStartBB
BBlock* Function::GetStartBB() const
{
    JumpInsn* pJump = GetEntryBB()->GetLastInsn()->StaticCast<JumpInsn>();
    return pJump->GetTarget();
} // Function::GetStartBB


// GetEntryInsn
EntryInsn* Function::GetEntryInsn() const
{
    Instruction* pInsn = GetEntryBB()->GetFirstInsn();
    ASSERT(pInsn->Is<EntryInsn>());
    return pInsn->StaticCast<EntryInsn>();
} // GetEntryInsn


// GetPrologueInsn
PrologueInsn* Function::GetPrologueInsn() const
{
    Instruction* pInsn = GetStartBB()->GetFirstInsn();
    ASSERT(pInsn->Is<PrologueInsn>());
    return pInsn->StaticCast<PrologueInsn>();
} // GetPrologueInsn


//////////////////////////////////////////////////////////////////////
//
// Function::EnumReg constructor
//
Function::EnumReg::EnumReg(Function* pFun)
{
    m_oEnumBBlock.Reset(pFun);
    m_oEnumInsn.Reset(m_oEnumBBlock.Get());
    Next();
} // Function::EnumReg::EnumReg


//////////////////////////////////////////////////////////////////////
//
// Function::AtEnd
//
bool
Function::EnumReg::AtEnd() const
{
    return m_oEnumInsn.AtEnd();
} // Function::EnumReg::AtEnd


//////////////////////////////////////////////////////////////////////
//
// Function::EnumReg::Get
//
Register*
Function::EnumReg::Get() const
{
    return m_oEnumInsn.Get()->GetRd();
} // Function::EnumReg::Get


//////////////////////////////////////////////////////////////////////
//
// Function::EnumReg::Next
//
void
Function::EnumReg::Next()
{
    ASSERT(! m_oEnumInsn.AtEnd());

    m_oEnumInsn.Next();

    for (;;)
    {
        while (! m_oEnumInsn.AtEnd())
        {
            Instruction* pInsn = m_oEnumInsn.Get();
            Register* pRd = pInsn->GetRd();

            if (NULL != pRd)
            {
                if (pRd->GetDfn() == NULL)
                {
                    pRd->SetDfn(pInsn);
                    return;
                }

                if (pRd->GetDfn() == pInsn)
                {
                    return;
                }
            }
            m_oEnumInsn.Next();
        } // for

        m_oEnumBBlock.Next();
        if (m_oEnumBBlock.AtEnd())
        {
            break;
        }

        m_oEnumInsn.Reset(m_oEnumBBlock.Get());
    } // for
} // Function::EnumReg::Next


//////////////////////////////////////////////////////////////////////
//
// Function::HasNonlocalBlock
//
bool Function::HasNonlocalBlock() const
{
    foreach (EnumBBlock, oEnum, const_cast<Function*>(this))
    {
        if (oEnum.Get()->HasNonlocalInEdge())
        {
            return true;
        }
    } // for each bblock
    return false;
} // Function::HasNonlocalBlock

//////////////////////////////////////////////////////////////////////
//
// Is Closure?
//
bool
Function::IsClosure() const
{
    return HasUpVar() && HasUseSite() &&
        ! m_oUseSites.GetHead()->GetInstruction()->Is<OpenFinallyInsn>();
} // Function::IsClosure


//////////////////////////////////////////////////////////////////////
//
// Is Stack Resity?
//
bool
Function::IsStackRestify() const
{
    return GetPrologueInsn()->GetLy()== Kstack;
} // Function::IsStackResity


//////////////////////////////////////////////////////////////////////
//
// Need Arity?
//
bool
Function::NeedArity() const
{
    if (m_fRest)
    {
        return true;
    }

    if (m_iMin != m_iMax)
    {
        return true;
    }

    return HasUseSite();
} // Function::NeedArity


//////////////////////////////////////////////////////////////////////
//
// Function::Realize
//
void
Function::Realize(OperandBox* pBox)
{
    Function* pCaller = pBox->GetInstruction()->GetBBlock()->GetFunction();
    Function* pCallee = this;

    // Add call graph edge
    {
        bool fPresent = false;
        foreach (EnumCaller, oEnum, pCallee)
        {
            if (oEnum.Get()->GetFrom() == pCaller)
            {
                fPresent = true;
            }
        } // for each caller

        if (! fPresent)
        {
            CgEdge* pEdge = new CgEdge(pCaller, pCallee);
            static_cast<EdgeInAnchor*>(pCallee)->Append_(pEdge);
            static_cast<EdgeOutAnchor*>(pCaller)->Append_(pEdge);
        }
    }

    if (pBox->GetKind() == OperandBox::Kind_Callee)
    {
        m_oCallSites.Append_(pBox);
    }
    else
    {
        m_oUseSites.Append_(pBox);
    }
} // Function::Realize


//////////////////////////////////////////////////////////////////////
//
// Function::Unrealize
//
void
Function::Unrealize(OperandBox* pBox)
{
    Function* pCaller = pBox->GetInstruction()->GetBBlock()->GetFunction();
    Function* pCallee = this;

    if (pBox->GetKind() == OperandBox::Kind_Callee)
    {
        m_oCallSites.Remove_(pBox);
    }
    else
    {
        m_oUseSites.Remove_(pBox);
    }

    // Remove call graph edge
    {
        CgEdge* pEdge = NULL;
        foreach (EnumCaller, oEnum, pCallee)
        {
            if (oEnum.Get()->GetFrom() == pCaller)
            {
                pEdge = oEnum.Get();
                break;
            }
        } // for each caller

        ASSERT(NULL != pEdge);

        bool fNeed = false;

        foreach (UseSiteList::Enum, oEnum, &m_oCallSites)
        {
            OperandBox* pBox = oEnum.Get();

            Function* pUser =
                pBox->GetInstruction()->GetBBlock()->GetFunction();

            if (pUser == pCaller)
            {
                fNeed = true;
                break;
            }
        } // for each box

        if (! fNeed)
        {
            foreach (UseSiteList::Enum, oEnum, &m_oUseSites)
            {
                OperandBox* pBox = oEnum.Get();

                Function* pUser =
                    pBox->GetInstruction()->GetBBlock()->GetFunction();

                if (pUser == pCaller)
                {
                    fNeed = true;
                    break;
                }
            } // for each box

            if (! fNeed)
            {
                static_cast<EdgeInAnchor*>(pCallee)->Remove_(pEdge);
                static_cast<EdgeOutAnchor*>(pCaller)->Remove_(pEdge);
            }
        } // if
    }
} // Function::Unrealize

} // Compiler
