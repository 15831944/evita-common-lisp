#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction
// ir/ir_instruction.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_instruction.cpp#13 $
//

#include "./ir_instruction.h"

#include "../cm/cm_session.h"

#include "./ir_bblock.h"
#include "./ir_fns.h"
#include "./ir_function.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Instruction::GetFunction
//
Function*
Instruction::GetFunction() const
{
    return GetBBlock()->GetFunction();
} // Instruction::GetFunction


//////////////////////////////////////////////////////////////////////
//
// Instruction::HtmlPrint
//
void
Instruction::HtmlPrint(Val stream, bool fDef) const
{
    if (ty_void == m_ty)
    {
        html_format(stream, L"(<span class='IrOp_~A'>~A</span>",
            GetMnemonic(),
            GetMnemonic() );

        foreach (EnumInput, oEnum, this)
        {
            html_format(stream, L" ~S", oEnum.Get());
        } // for each operand

        write_string(L")", stream);
    }
    else
    {
        HtmlA(stream, fDef);

        html_format(stream, L"(<span class='IrOp_~A'>~A</span>",
            GetMnemonic(),
            GetMnemonic() );

        html_format(stream, L"</a> <i class='t'>~W</i> ", m_ty);

        if (fDef)
        {
            html_format(stream, L" ~:S", m_pOutput);
        }
        else
        {
            html_format(stream, L" ~S", m_pOutput);
        }

        html_format(stream, L" &lt;-");

        foreach (EnumInput, oEnum, this)
        {
            html_format(stream, L" ~S", oEnum.Get());
        } // for each operand

        write_string(L")", stream);
    } // if void

    if (fDef && 0 != m_rgfAttr)
    {
        html_format(stream, L" attr=~D", m_rgfAttr);
    }
} // Instruction::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
// Instruction::Identical
//
bool Instruction::Identical(const Instruction* that) const
{
    if (this == that) return true;
    if (GetOpcode() != that->GetOpcode()) return false;
    if (GetOperandCount() != that->GetOperandCount()) return false;
    if (! ty_equal(m_ty, that->m_ty)) return false;

    if (m_pOutput != that->m_pOutput)
    {
        if (! m_pOutput->Equal(that->m_pOutput)) return false;
    }

    EnumInput oEnumThat(that);
    foreach (EnumInput, oEnum, this)
    {
        if (oEnum.Get() != oEnumThat.Get())
        {
            if (! oEnum.Get()->Equal(oEnumThat.Get())) return false;
        }
    } // for each input
    return true;
} // Instruction::Identical


//////////////////////////////////////////////////////////////////////
//
// Instruction::IsUseless
//
bool
Instruction::IsUseless() const
{
    if (ty_void == m_ty)
    {
        return false;
    }

    return ! GetOutput()->HasUseSite();
} // Instruction::IsUseless


//////////////////////////////////////////////////////////////////////
//
// Instruction::Realize
//
// Description:
//  Links Use/Def-Chain
//
void
Instruction::Realize()
{
    m_pOutput->SetDfn(this);

    foreach (EnumInput, oEnum, this)
    {
        OperandBox* pBox = oEnum.GetBox();
        pBox->SetInstruction(this);
        pBox->GetOperand()->Realize(pBox);
    } // for each operand
} // Instruction::Realize


//////////////////////////////////////////////////////////////////////
//
// ReplaceLabelOperand
//
void
Instruction::ReplaceLabelOperand(
    BBlock*         pNew,
    BBlock*         pOld )
{
    ASSERT(NULL != pNew);
    ASSERT(NULL != pOld);

    if (Is<BranchInsn>())
    {
        if (GetOperand(1)->StaticCast<Label>()->GetBBlock() == pOld)
        {
            GetOperandBox(1)->SetOperand(pNew->GetLabel());
        }
        else if (GetOperand(2)->StaticCast<Label>()->GetBBlock() == pOld)
        {
            GetOperandBox(2)->SetOperand(pNew->GetLabel());
        }
    }
    else if (Is<JumpInsn>())
    {
        if (GetOperand(0)->StaticCast<Label>()->GetBBlock() == pOld)
        {
            GetOperandBox(0)->SetOperand(pNew->GetLabel());
        }
    }
} // Instruction::ReplaceLabelOperand


//////////////////////////////////////////////////////////////////////
//
// Instruction::SetOutput
//
Output*
Instruction::SetOutput(Output* pOutput)
{
    if (m_pOutput->GetDfn() == this) m_pOutput->SetDfn(NULL);

    m_pOutput = pOutput;

    if (IsRealized()) pOutput->SetDfn(this);
    if (NULL == m_pVar) m_pVar = pOutput->GetVar();

    return pOutput;
} // Instruction::SetOutput


//////////////////////////////////////////////////////////////////////
//
// Instruction::Unrealize
//
// Description:
//  Unlinks Use/Def-Chain.
//
void
Instruction::Unrealize()
{
    foreach (EnumInput, oEnum, this)
    {
        OperandBox* pBox = oEnum.GetBox();
        pBox->SetInstruction(this);
        oEnum.Get()->Unrealize(pBox);
    } // for each operand

    if (m_pOutput->GetDfn() == this)
    {
        m_pOutput->SetDfn(NULL);
    }
} // Instruction::Unrealize


ClosureInsn::ClosureInsn(
    Val ty,
    Register* pRd,
    Function* pTempl,
    Values* pVx ) :
        Two_Operands_Instruction(pTempl, pVx)
{
    ASSERT(NULL != pVx);
    m_ty = ty;
    m_pOutput = pRd;
} // ClosureInsn


LoadTimeValueInsn::LoadTimeValueInsn(
        Register*   pRd,
        Val         cookie,
        Function*   pFun,
        Val         read_only_p ) :
    Two_Operands_Instruction(NewLiteral(cookie), pFun, ty_t, pRd),
    m_read_only_p(read_only_p)
{
    ASSERT(pRd->GetStorage() == Register::Storage_LoadTimeValue);
} // LoadTimeValueInsn::LoadTimeValueInsn


OpenBlockInsn::OpenBlockInsn(
    Frame*      pFrame,
    Literal*    pName,
    BBlock*     pNonlocalXp ) :
        Two_Operands_Instruction(pName, pNonlocalXp->GetLabel())
{
    m_ty = Qt;
    m_pOutput = pFrame;
} // OpenBlockInsn


OpenCatchInsn::OpenCatchInsn(
    Frame*      pFrame,
    Operand*    pTag,
    BBlock*     pNonlocalXp ) :
        Two_Operands_Instruction(pTag, pNonlocalXp->GetLabel())
{
    m_ty = Qt;
    m_pOutput = pFrame;
} // OpenCatchInsn


OpenFinallyInsn::OpenFinallyInsn(
    Frame* pFrame,
    Function* pCleanup,
    Values* pVx ) :
        Two_Operands_Instruction(pCleanup, pVx)
{
    ASSERT(NULL != pVx);
    m_ty = Qt;
    m_pOutput = pFrame;
} // OpenFinallyInsn


TagInsn::TagInsn(
    Register*   pRd,
    Frame*      pFrame,
    BBlock*     pBBlock ) :
        Two_Operands_Instruction(pFrame, pBBlock->GetLabel())
{
    m_ty = ty_t;
    m_pOutput = pRd;
} // TagInsn::TagInsn


//////////////////////////////////////////////////////////////////////
//
// ir_insert_insn
//
Instruction*
ir_insert_insn(Instruction* pInsn, Instruction* pRef)
{
    ASSERT(NULL != pInsn);
    ASSERT(NULL != pRef);

    ASSERT(! pInsn->IsRealized());
    ASSERT(pRef->IsRealized());

    BBlock* pBBlock = pRef->GetBBlock();
    pInsn->SetParent(pBBlock);
    static_cast<BBlock::InsnList*>(pBBlock)->Insert_(pInsn, pRef);
    pInsn->Realize();

    html_log_format(6, L"insert ~S before ~S ~S~:%",
        pInsn,
        pBBlock, pRef );

    return pInsn;
} // ir_insert_insn


//////////////////////////////////////////////////////////////////////
//
// ir_move_insn
//
Instruction*
ir_move_insn(Instruction* pInsn, Instruction* pRef)
{
    ASSERT(NULL != pInsn);
    ASSERT(NULL != pRef);

    ASSERT(pInsn->IsRealized());
    ASSERT(pRef->IsRealized());

    html_log_format(6, L"move ~S ~S before ~S ~S~:%",
        pInsn->GetBBlock(), pInsn,
        pRef->GetBBlock(),  pRef );

    pInsn->Unlink_();
    BBlock* pBBlock = pRef->GetBBlock();
    pInsn->SetParent(pBBlock);
    static_cast<BBlock::InsnList*>(pBBlock)->Insert_(pInsn, pRef);
    return pInsn;
} // ir_move_insn


//////////////////////////////////////////////////////////////////////
//
// ir_remove_insn
//
Instruction*
ir_remove_insn(Instruction* pInsn)
{
    ASSERT(NULL != pInsn);

    BBlock* pBBlock = pInsn->GetBBlock();

    html_log_format(6, L"remove ~S ~S~:%", pBBlock, pInsn);

    pInsn->Unrealize();
    pInsn->SetParent(NULL);
    static_cast<BBlock::InsnList*>(pBBlock)->Remove_(pInsn);
    return pInsn;
} // ir_remove_insn


//////////////////////////////////////////////////////////////////////
//
// ir_replace_insn
//
Instruction*
ir_replace_insn(Instruction* pNew, Instruction* pOld)
{
    ASSERT(NULL != pNew);
    ASSERT(! pNew->IsRealized());
    ASSERT(NULL != pOld);

    ir_insert_insn(pNew, pOld);
    ir_remove_insn(pOld);
    return pNew;
} // ir_replace_insn


//////////////////////////////////////////////////////////////////////
//
// Instruction::Simplify
//  Simplify output and replaces all uses.
//
bool Instruction::Simplify()
{
    if (Obj_Void == GetOutput()) return false;

    Output*  pRd = GetOutput();
    Operand* pSx = SimplifyOutput();
        if (pRd == pSx) return false;

    if (NULL == GetBd() || (pSx == Bool_True || pSx == Bool_False))
    {
        ir_replace_all_users(pSx, pRd);
        return true;
    }
    html_log_format(4, L"<b>simplify</b> ~S:~S -> ~S~:%",
        GetBBlock(), this, pSx );

    Bool* pBx = pSx->StaticCast<Bool>();
    Instruction* pDfn = pBx->GetDfn();

    bool fChanged = false;
    Bool::EnumUseSite oEnum(GetBd());
    while (! oEnum.AtEnd())
    {
        OperandBox* pBox = oEnum.Get();
            oEnum.Next();

        Instruction* pUser = pBox->GetInstruction();

        Instruction* pCmp = ir_insert_insn(pDfn->Clone(), pUser);
        pBox->Replace(pCmp->GetBd());
    } // while
    return fChanged;
} // Instruction::Simplify


// Instruction::SimplifyOutput
Operand* Instruction::SimplifyOutput() const
{
    if (! m_pOutput->IsSSA()) return m_pOutput;
    return SimplifyOutputAux();
} // Instruction::SimplifyOutput


//////////////////////////////////////////////////////////////////////
//
// Instruction::simplifyBx
//
// Description:
//  Constant folding on %bx.
//
bool
Instruction::simplifyBx()
{
    Bool* pBx = GetSx()->StaticCast<Bool>();

    if (pBx == Bool_True || pBx == Bool_False)
    {
        return false;
    }

    Bool* pBy = pBx->GetDfn()->SimplifyOutput()->StaticCast<Bool>();
    if (pBy != pBx)
    {
        if (pBy != Bool_True && pBy != Bool_False)
        {
            if (! pBy->HasOnlyOneUseSite())
            {
                return false;
            }

            ir_move_insn(pBy->GetDfn(), this);
        }

        html_log_format(6, L"<b>update</b> ~S ~S => ~S~:%",
            GetBBlock(), this, pBy );

        GetOperandBox(0)->Replace(pBy);
        return true;
    } // if

    return false;
} // Instruction::simplifyBx

} // Compiler
