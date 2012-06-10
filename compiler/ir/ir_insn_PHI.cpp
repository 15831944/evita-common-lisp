#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - PHI instruction
// ir/instruction/ir_insn_PHI.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_PHI.cpp#13 $
//
#include "./ir_instruction.h"

#include "./ir_bblock.h"
#include "./ir_fns.h"

namespace Compiler
{

BBlock* PhiInsn::PhiOperandBox::GetBBlock() const
    { return m_pLabel->GetBBlock(); }

BBlock* PhiInsn::PhiOperandBox::SetBBlock(BBlock* p)
    { m_pLabel = p->GetLabel(); return p; }


// PhiInsn constructor
PhiInsn::PhiInsn(Ty ty, Output* pRd, const PhiInsn* pPhi) :
    Instruction(ty, pRd)
{
    m_cAllocs  =  pPhi->m_cAllocs;
    m_cOperands = pPhi->m_cOperands;
    m_prgpOperandBox = new PhiOperandBox*[m_cAllocs];

    PhiOperandBox** ppBox = m_prgpOperandBox;
    foreach (PhiInsn::EnumInput, oEnum, pPhi)
    {
        PhiOperandBox* pBox = new PhiOperandBox();
        pBox->SetBBlock(oEnum.GetBox()->GetBBlock());
        pBox->SetOperand(oEnum.Get());
        pBox->SetVar(oEnum.GetBox()->GetVar());
        *ppBox++ = pBox;
    } // for each input
} // PhiInsn::PhiInsn


//////////////////////////////////////////////////////////////////////
//
// PhiInsn::AddInput
//
// Note: Phi instruction must be realized before calling this method.
//
Operand*
PhiInsn::AddInput(BBlock* pBBlock, Operand* pSx)
{
    ASSERT(NULL != pBBlock);
    ASSERT(NULL != pSx);

    ASSERT(m_cOperands <= m_cAllocs);

    //if (pSx == Obj_Void) warn(L"PhiInsn::AddInput: void");

    if (m_cOperands ==  m_cAllocs)
    {
        m_cAllocs += 2;

        PhiOperandBox** prgpOperandBox = new PhiOperandBox*[m_cAllocs];

        ::CopyMemory(
            prgpOperandBox,
            m_prgpOperandBox,
            sizeof(PhiOperandBox*) * m_cOperands );

        m_prgpOperandBox = prgpOperandBox;
    } // if

    PhiOperandBox* pBox = new PhiOperandBox();

    m_prgpOperandBox[m_cOperands] = pBox;
        m_cOperands += 1;

    pBox->SetBBlock(pBBlock);
    pSx = pBox->SetOperand(pSx);

    if (IsRealized())
    {
        pBox->SetInstruction(this);
        pSx->Realize(pBox);
    }

    //UpdateTy();
    return pSx;
} // PhiInsn::AddInput


//////////////////////////////////////////////////////////////////////
//
// PhiInsn::GetInput
//
PhiInsn::PhiOperandBox*
PhiInsn::GetInputBox(BBlock* pBBlock) const
{
    ASSERT(NULL != pBBlock);

    foreach (EnumInput, oEnum, this)
    {
        PhiOperandBox* pPresent = oEnum.GetBox();
        if (pPresent->GetBBlock() == pBBlock) return pPresent;
    } // for each operand

    CAN_NOT_HAPPEN();
} // PhiInsn::GetInput


//////////////////////////////////////////////////////////////////////
//
// PhiInsn::GetInput
//
PhiInsn::PhiOperandBox*
PhiInsn::GetInputBox(OperandBox* pBox) const
{
    foreach (EnumInput, oEnum, this)
    {
        PhiOperandBox* pPresent = oEnum.GetBox();
        if (pBox == pPresent) return pPresent;
    } // for each operand

    CAN_NOT_HAPPEN();
} // PhiInsn::GetInput


//////////////////////////////////////////////////////////////////////
//
// PhiInsn::HtmlPrint
//
void
PhiInsn::HtmlPrint(Val stream, bool fDef) const
{
    HtmlA(stream, fDef);

    html_format(stream, L"(~A", GetMnemonic());

    html_format(stream, L" <i>~W</i> ~:S &lt;=", m_ty, m_pOutput);

    foreach (EnumInput, oEnum, this)
    {
        PhiOperandBox* pBox = oEnum.GetBox();
        html_format(stream, L" (~S ~S)",
            pBox->GetBBlock(),
            pBox->GetOperand() );
    } // for each operand

    write_string(L")</a>", stream);
} // PhiInsn::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
// PhiInsn::IsUseless
//  If the output is used only by PHI instruction itself.
//  Note: Unused keyword parameter creates this case.
bool
PhiInsn::IsUseless() const
{
    foreach (Output::EnumUseSite, oEnum, m_pOutput)
    {
        OperandBox* pBox = oEnum.Get();
        if (pBox->GetInstruction() != this) return false;
    } // for each output

    return true;
} // PhiInsnUseless


//////////////////////////////////////////////////////////////////////
//
// PhiInsn::RemoveInput
//  Removes input operand for specified bblock.
//
void
PhiInsn::RemoveInput(BBlock* pBBlock)
{
    ASSERT(NULL != pBBlock);

    PhiOperandBox** ppRunner = m_prgpOperandBox;
    PhiOperandBox** ppEnd = m_prgpOperandBox + m_cOperands;
    while (ppRunner < ppEnd)
    {
        PhiOperandBox* pBox = *ppRunner;
        if (pBox->GetBBlock() == pBBlock)
        {
            pBox->GetOperand()->Unrealize(pBox);

            ::CopyMemory(
                ppRunner,
                ppRunner + 1,
                sizeof(PhiOperandBox*) * (ppEnd - (ppRunner + 1)) );

            m_cOperands -= 1;
            //UpdateTy();
            return;
        }
        ppRunner++;
    } // while

    CAN_NOT_HAPPEN();
} // PhiInsn::RemoveInput


// count_nvals
static bool count_nvals(
    const PhiInsn*  pPhi,
    uint*           out_nMin,
    uint*           out_nMax )
{
    uint nMin = static_cast<uint>(-1);
    uint nMax = 0;

    foreach (PhiInsn::EnumInput, oEnum, pPhi)
    {
        Values* pVx = oEnum.Get()->DynamicCast<Values>();

        uint nCur;
        {
            if (NULL == pVx)
            {
                nCur = 1;
            }
            else
            {
                ValuesInsn* pValues = pVx->GetDfn()->DynamicCast<ValuesInsn>();
                if (NULL == pValues) return false;
                nCur = pValues->GetOperandCount();
            }
        } // nCur

        nMin = min(nMin, nCur);
        nMax = max(nMax, nCur);
    } // for each input

    *out_nMin = nMin;
    *out_nMax = nMax;
    return true;
} // count_nvals


// is_fixed_receiver
static bool is_fixed_receiver(const PhiInsn*  pPhi)
{
    foreach (Values::EnumUseSite, oEnum, pPhi->GetVd())
    {
        if (! oEnum.Get()->GetInstruction()->Is<ProjectInsn>())
            { return false; }
    } // for each use
    return true;
} // is_fixed_receiver


//////////////////////////////////////////////////////////////////////
//
// PhiInsn::Simplify
//
//      BB1: VALUES %v1 <- %r2 %r3
//      BB2: VALUES %v4 <- %r5 %r6
//      BB3: PHI %v7 <- ...
//  ==>
//      BB3: PHI %r8 <- (BB1 %r2) (BB2 %r5)
//      BB3: PHI %r9 <- (BB1 %r3) (BB2 %r6)
//      BB3: VALUES %v10 <- %r8 %r9
//
//  Example: snarfed from d25-cmdl.lisp:
//    (multiple-value-bind (succeeded condition)
//        (ignore-errors (load filename :if-does-not-exist nil))
//      (cond
//        (succeeded
//          (remember-arg :load filename) )
//        ((null condition)
//          (format t "; No such file: ~S~%" filename) )
//        (t
//          (remember-arg :load filename)
//          (format t "; Failed to load ~S: ~A~%" filename condition) )) )
//
bool PhiInsn::Simplify()
{
    if (Instruction::Simplify()) return true;
    if (NULL == GetVd()) return false;

    uint nMin, nMax;
        if (! count_nvals(this, &nMin, &nMax)) return false;

    if (nMin == nMax)
    {
        // fixed number of values
    }
    else if (! is_fixed_receiver(this))
    {
        return false;
    }

    html_log_format(3, L"<b>Decompose</b> ~S~:%", this);

    Values* pVd = new Values();
    ValuesInsn* pValues = new ValuesInsn(pVd);

    // Make PHI instructions.
    {
        {
            tyIterator oIter(GetTy());
            for (uint nNth = 0; nNth < nMax; nNth++)
            {
                oIter.Next();
                Ty ty = oIter.GetTy();
                Register* pRd = new Register();
                ir_insert_insn(new PhiInsn(ty, pRd), this);
                pValues->InsertBefore(pRd, nNth);
            } // for each input
        }

        {
            Instruction* pInsn = GetNext();
            while (pInsn->Is<PhiInsn>())
            {
                pInsn = pInsn->GetNext();
            } // while

            ir_insert_insn(pValues, pInsn);
        }
    } // make

    // Populate PHI instructions.
    {
        uint nNth = 0;
        foreach (Instruction::EnumInput, oEnum, pValues)
        {
            PhiInsn* pPhi = oEnum.Get()->StaticCast<Register>()->
                GetDfn()->StaticCast<PhiInsn>();

            foreach (PhiInsn::EnumInput, oEnum, this)
            {
                PhiOperandBox* pBox = oEnum.GetBox();
                Operand* pSx = oEnum.Get();
                Values*  pVx = pSx->DynamicCast<Values>();

                if (NULL == pVx)
                {
                    if (nNth >= 1) pSx = NewLiteral(nil);
                }
                else
                {
                    ValuesInsn* pValues = pVx->GetDfn()->
                        StaticCast<ValuesInsn>();

                    if (nNth < pValues->GetOperandCount())
                    {
                        pSx = pValues->GetOperand(nNth);
                    }
                    else
                    {
                        pSx = NewLiteral(nil);
                    }
                } // if

                pPhi->AddInput(pBox->GetBBlock(), pSx);
            } // for each input

            nNth += 1;
        } // for each input
    } // populate

    ir_replace_all_users(pVd, GetOutput());
    return true;
} // PhiInsn::Simplify


//////////////////////////////////////////////////////////////////////
//
// PhiInsn::SimplifyOutputAux
//
//  o Single operand PHI instruction.   PHI %rx <= (BBn %sx) ==> %sx
//
Operand* PhiInsn::SimplifyOutputAux() const
{
    // Note: Since Phi operands are added after Phi instruction, we
    // check number of in-edges instead of number of operands.
    if (GetBBlock()->HasOnlyOnePred())
    {
        return GetSx();
    }

    return GetOutput();
} // PhiInsn::SimplifyOutputAux


//////////////////////////////////////////////////////////////////////
//
// PhiInsn::SortInput
//  Sort input in RPO.
//
void PhiInsn::SortInput()
{
    for (uint i = 0; i < m_cOperands; i++)
    {
        for (uint j = i + 1; j < m_cOperands; j++)
        {
            if (m_prgpOperandBox[i]->GetBBlock()->GetPostorder() <
                m_prgpOperandBox[j]->GetBBlock()->GetPostorder() )
            {
                swap(m_prgpOperandBox[i], m_prgpOperandBox[j]);
            }
        } // for j
    } // for i
} // PhiInsn::SortInput


//////////////////////////////////////////////////////////////////////
//
// PhiInsn::UpdateTy
//
bool PhiInsn::UpdateTy()
{
    Ty ty = nil;
    foreach (PhiInsn::EnumInput, oEnum, this)
    {
        Operand* pSx = oEnum.Get();
            if (Obj_Void == pSx) { ty = t; break; }
        {
            Output* pRx = oEnum.Get()->ToOutput();
            if (NULL != pRx && NULL == pRx->GetDfn())
            {
                format(t, L"PhiIns::UpdateTy: %r~D isn't defined~%",
                    pRx->GetName() );

                ty = ty_t;
                break;
            }
        }
        ty = ty_or(ty, pSx->GetTy());
    } // for each input

    if (equal(ty, m_ty)) return false;

    html_log_format(4, L"PhiInsn::UpdateTy: ~S:~S -> ~W~:%",
        GetBBlock(), this, ty );

    m_ty = ty;
    return true;
} // PhiInsn::UpdateTy

} // Compiler
