#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction VALUES
// ir/instruction/ir_insn_VALUES.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_VALUES.cpp#7 $
//
#include "./ir_instruction.h"

#include "./ir_fns.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// ValuesInsn consturctor
//
void
ValuesInsn::init(
    Values*     pVd,
    Operand*    prgpOperand[],
    uint        cOperands )
{
    m_pOutput = pVd;

    m_cAllocs   = static_cast<uint>(ROUNDUP(cOperands, 4));
    m_prgpOperandBox = new OperandBox*[m_cAllocs];

    for (uint i = 0; i < cOperands; i++)
    {
        OperandBox* pBox = new OperandBox();

        Operand* pOperand = prgpOperand[i];
        pBox->SetOperand(pOperand);
        m_prgpOperandBox[i] = pBox;
    } // for

    m_cOperands = cOperands;
    UpdateTy();
} // ValuesInsn::ValuesInsn


//////////////////////////////////////////////////////////////////////
//
// ValuesInsn consturctor
//
void ValuesInsn::init(
    Values*     pVd,
    Operand*    pOperand,
    uint        cOperands )
{
    m_pOutput = pVd;

    m_cAllocs   = static_cast<uint>(ROUNDUP(cOperands, 4));
    m_prgpOperandBox = new OperandBox*[m_cAllocs];

    Operand* pOperandNil = NewLiteral(nil);

    for (uint i = 0; i < cOperands; i++)
    {
        OperandBox* pBox = new OperandBox();
        pBox->SetInstruction(this);
        m_prgpOperandBox[i] = pBox;
        pBox->SetOperand(pOperand);
        pOperand = pOperandNil;
    } // for

    m_cOperands = cOperands;

    UpdateTy();
} // ValuesInsn::ValuesInsn


//////////////////////////////////////////////////////////////////////
//
// ValuesInsn consturctor
//
ValuesInsn::ValuesInsn(
    Values*     pVd,
    Operand*    pSx,
    Operand*    pSy )
{
    m_pOutput = pVd;

    m_cOperands = 2;

    m_cAllocs   = static_cast<uint>(ROUNDUP(m_cOperands, 4));
    m_prgpOperandBox = new OperandBox*[m_cAllocs];

    {
        OperandBox* pBox = new OperandBox();
        pBox->SetOperand(pSx);
        m_prgpOperandBox[0] = pBox;
    }

    {
        OperandBox* pBox = new OperandBox();
        pBox->SetOperand(pSy);
        m_prgpOperandBox[1] = pBox;
    }

    UpdateTy();
} // ValuesInsn::ValuesInsn


//////////////////////////////////////////////////////////////////////
//
// ValuesInsn consturctor
//
//  For x86-SELECT, lowering VALUES*
//
void ValuesInsn::init(
    Values*         pVd,
    Instruction*    pInsn,
    uint            nStart )
{
    m_pOutput = pVd;

    m_cOperands = pInsn->GetOperandCount() - nStart;
    m_cAllocs = m_cOperands;
    m_prgpOperandBox = new OperandBox*[m_cAllocs];

    uint nNth = 0;
    uint nIndex = 0;
    foreach (Instruction::EnumInput, oEnum, pInsn)
    {
        OperandBox* pBox = new OperandBox();

        if (nNth >= nStart)
        {
            pBox->SetOperand(oEnum.Get());
            m_prgpOperandBox[nIndex] = pBox;
                nIndex += 1;
        }

        nNth += 1;
    } // for each operand

    UpdateTy();
} // ValuesInsn::ValuesInsn


//////////////////////////////////////////////////////////////////////
//
// ValuesInsn::InsertBefore
//
Operand*
ValuesInsn::InsertBefore(Operand* pOperand, uint nNth)
{
    ASSERT(nNth <= m_cOperands);
    ASSERT(m_cOperands <= m_cAllocs);

    if (m_cOperands == m_cAllocs)
    {
        m_cAllocs = (max(m_cOperands, 5) * 120) / 100;

        OperandBox** prgpOperandBox = m_prgpOperandBox;

        m_prgpOperandBox = new OperandBox*[m_cAllocs];

        ::CopyMemory(
            m_prgpOperandBox,
            prgpOperandBox,
            sizeof(OperandBox*) * m_cOperands );
    } // if

    // Shift operands
    evcl_memmove(
        m_prgpOperandBox + nNth + 1,
        m_prgpOperandBox + nNth,
        sizeof(OperandBox*) * (m_cOperands - nNth) );

    OperandBox* pBox = new OperandBox();
    m_prgpOperandBox[nNth] = pBox;
    pBox->SetOperand(pOperand);
    pBox->SetInstruction(this);

    m_cOperands += 1;
    if (IsRealized())
    {
        pOperand->Realize(pBox);
    }

    if (nil != m_ty)
    {
        setf_cdr(cons(t, cdr(m_ty)), m_ty);
    }

    UpdateTy();

    return pOperand;
} // ValuesInsn::InsertBefore


// get_call_list
static CallInsn* get_call_list(Instruction* pInsn)
{
    CallInsn* pCall = pInsn->DynamicCast<CallInsn>();
        if (NULL == pCall) return NULL;
        if (pCall->IsNotInline()) return NULL;

    Literal* pCallee = pCall->GetSx()->DynamicCast<Literal>();
        if (NULL == pCallee) return NULL;
        if (pCallee->GetDatum() != Qlist) return NULL;

    return pCall;
} // get_call_list


//////////////////////////////////////////////////////////////////////
//
// ValuesAInsn::SimplifyOutputAux
//      VALUES %v1 <= %s2
//      CALL list %r3 <= #'list %v1
//      ...
//      VALUES* %v4 <= %r3
//  => %s2
//
// For mv-save/mv-restor after opt-inline.
//
Operand* ValuesAInsn::SimplifyOutputAux() const
{
    if (GetOperandCount() != 1) return m_pOutput;

    Register* pRx = GetRx();
        if (NULL == pRx) return m_pOutput;

    CallInsn* pList = get_call_list(pRx->GetDfn());
        if (NULL == pList) return m_pOutput;

    ValuesInsn* pArgs = pList->GetVy()->GetDfn()->DynamicCast<ValuesInsn>();
        if (NULL == pArgs) return m_pOutput;

    switch (pArgs->GetOperandCount())
    {
    case 1: return pArgs->GetSx();
    case 0: return NewLiteral(nil);
    } // switch count

    return m_pOutput;
} // ValuesAInsn::SimplifyOutputAux


//////////////////////////////////////////////////////////////////////
//
// ValuesInsn::UpdateTy
//
bool ValuesInsn::UpdateTy()
{
    if (consp(m_ty))
    {
        bool fChanged = false;
        Val last = m_ty;
        foreach (EnumInput, oEnum, this)
        {
            Operand* pSx = oEnum.Get();
            last = cdr(last);
            if (! equal(car(last), pSx->GetTy()))
            {
                setf_car(pSx->GetTy(), last);
                fChanged = true;
            }
        } // for

        return fChanged;
    }
    else
    {
        m_ty = list(Qvalues);
        Val last = m_ty;
        foreach (EnumInput, oEnum, this)
        {
            Operand* pSx = oEnum.Get();
            last = setf_cdr(list(pSx->GetTy()), last);
        } // for

        return true;
    } // if
} // ValuesInsn::UpdateTy()


//
//////////////////////////////////////////////////////////////////////
//
// ValuesAInsn::UpdateTy
// Note: The last operand of VALUES* must be list.
//
bool ValuesAInsn::UpdateTy()
{
    if (consp(m_ty))
    {
        bool fChanged = false;
        Val last = m_ty;
        foreach (EnumInput, oEnum, this)
        {
            Operand* pSx = oEnum.Get();
            last = cdr(last);

            if (car(last) == QArest)
            {
                // ignore type for &rest.
                break;
            }

            if (! equal(car(last), pSx->GetTy()))
            {
                setf_car(pSx->GetTy(), last);
                fChanged = true;
            }
        } // for

        return fChanged;
    }
    else
    {
        m_ty = list(Qvalues);
        Val last = m_ty;
        foreach (EnumInput, oEnum, this)
        {
            Operand* pSx = oEnum.Get();
            last = setf_cdr(list(pSx->GetTy()), last);
        } // for

        setf_car(QArest, last);
        setf_cdr(list(t), last);

        return true;
    } // if
} // ValuesAInsn::UpdateTy

} // Compiler
