#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - Context
// compiler/cl/cl_Context.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_context.cpp#3 $
//
#include "./cl_defs.h"
#include "../cm/cm_base.h"
#include "../ir/ir_instruction.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Context constructor
//
ClParser::Context::Context(Function* pFunction, LexEnv* pLexEnv) :
    m_pLexEnv(pLexEnv)
{
    m_pCurr  = pFunction->GetStartBB();
    m_pSucc  = pFunction->GetExitBB();
} // ClParser::Context::Context


//////////////////////////////////////////////////////////////////////
//
// Get context function
//
Function*
ClParser::Context::GetFunction() const
{
    // m_pCurr can be null when unreachable.
    return m_pSucc->GetParent();
} // ClParser::Context::GetFunction


ClParser::Context::BindingScope::BindingScope(
    LexEnv::Kind    eKind,
    Context*        pContext,
    Ty              value_ty ) :
        m_pContext(pContext),
        m_oLexEnv(eKind, pContext->GetFunction(), value_ty)
{
    m_oLexEnv.SetOuter(pContext->GetLexEnv());
    pContext->m_pLexEnv = &m_oLexEnv;
} // ClParser::Context::BindingScope

ClParser::Context::BindingScope::~BindingScope()
{
    m_pContext->m_pLexEnv = m_pContext->m_pLexEnv->GetOuter();
} // ClParser::Context::BindingScope::~BindingScope


//////////////////////////////////////////////////////////////////////
//
// Get Linkage
//
ClParser::Context::Linkage
ClParser::Context::GetLinkage() const
{
    if (NULL == m_pCurr)
    {
        return Linkage_Unreachable;
    }

    if (m_pSucc->IsExitBBlock())
    {
        return Linkage_Return;
    }

    if (m_pSucc == m_pCurr)
    {
        return Linkage_Next;
    }

    if (m_pSucc->HasInsn() && m_pSucc->GetFirstInsn()->Is<PhiInsn>())
    {
        return Linkage_Phi;
    }

    return Linkage_Jump;
} // ClParser::Context::GetLinkage


//////////////////////////////////////////////////////////////////////
//
// ClParser::endPhiContext
//
Operand*
ClParser::endPhiContext(Ty expected_ty)
{
    if (GetContext()->GetLinkage() == Context::Linkage_Return)
    {
        return Obj_Unreachable;
    }

    if (ty_void == expected_ty)
    {
        return emitLinkage(Obj_Void);
    }

    BBlock* pCurr = GetContext()->GetCurr();
    Instruction* pPhi = pCurr->GetFirstInsn();
    return emitLinkage(pPhi->GetOutput());
} // ClParser::endPhiContext


//////////////////////////////////////////////////////////////////////
//
// ClParser::startPhiContext
//
BBlock*
ClParser::startPhiContext(Ty expected_ty)
{
    BBlock* pSucc = GetContext()->GetSucc();

    switch (GetContext()->GetLinkage())
    {
    case Context::Linkage_Next:
        pSucc = NULL;
        goto make_phi_point;

    case Context::Linkage_Jump:
    make_phi_point:
    {
        BBlock* pNewSucc = newBBlock();
        Output* pRd = NewOutput(expected_ty);
        GetContext()->SetSucc(pNewSucc);
        if (pRd->Is<Register>() || pRd->Is<Values>())
        {
            pNewSucc->AppendInsn(new PhiInsn(expected_ty, pRd));
        }
        break;
    } // make_phi_point

    case Context::Linkage_Phi:
        break;

    case Context::Linkage_Return:
        break;
    } // switch linkage

    return pSucc;
} // ClParser::startPhiContext

} // Compiler
