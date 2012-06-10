#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR - Functions
// ir/ir_fns.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_fns.cpp#17 $
//
//
//      ir_add_extra_param
//      ir_remove_useless_instructions
//
#include "./ir_defs.h"
#include "./ir_fns.h"

#include "../cm/cm_session.h"

#include "./ir_bblock.h"
#include "./ir_function.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// ir_add_extra_param
//
//  Called by:
//      ClosureFinalizer::intern_base
//      ClosureFinalizer::intern_sp
//
Register*
ir_add_extra_param(Function* pFun, Ty ty, Register* pRd)
{
    Instruction* pPrologue = pFun->GetPrologueInsn();

    Instruction* pRefInsn = NULL;

    Values* pVd = pPrologue->GetVd();
    if (NULL == pVd)
    {
        pVd = new Values();
        pPrologue->SetOutput(pVd);
        pPrologue->SetTy(ty_values_rest_t);
    }
    else
    {
        // Adjust nth operand of PROJECT instruction.
        foreach (Values::EnumUseSite, oEnum, pVd)
        {
            ProjectInsn* pProject = oEnum.Get()->GetInstruction()->
                    DynamicCast<ProjectInsn>();
                if (NULL == pProject) continue;

            uint nNth = static_cast<uint>(Fixnum::Decode_(pProject->GetLy()));

            if (nNth >= pFun->m_cExtraParams)
            {
                if (NULL == pRefInsn) pRefInsn = pProject;
                pProject->GetOperandBox(1)->Replace(NewLiteral(nNth + 1));
            }
        } // for each user
    }

    ir_insert_insn(
        new ProjectInsn(ty, pRd, pVd, pFun->m_cExtraParams),
        NULL != pRefInsn ? pRefInsn : pPrologue->GetNext() );

    pFun->m_cExtraParams += 1;

    return pRd;
} // ir_add_extra_param


//////////////////////////////////////////////////////////////////////
//
// ir_dominate_p -- return true if pInsn1 dominates pInsn2
//
bool ir_dominate_p(const Instruction* pInsn1, const Instruction* pInsn2)
{
    if (pInsn1->GetBBlock() == pInsn2->GetBBlock())
        { return pInsn1->GetIndex() <= pInsn2->GetIndex(); }

    return pInsn1->GetBBlock()->DoesDominate(pInsn2->GetBBlock());
} // ir_dominate_p


//////////////////////////////////////////////////////////////////////
//
// ir_number_instructions
//
// Description:
//  Number all instructions.
//
void ir_number_instructions(Function* pFun)
{
    ComputeDominance(pFun);

    uint nIndex = 0;
    foreach (Function::EnumBBlock_Reverse_Postorder, oEnumBB, pFun)
    {
        BBlock* pBBlock = oEnumBB.Get();
        foreach (BBlock::EnumInsn, oEnum, pBBlock)
        {
            nIndex += 100;
            oEnum.Get()->SetIndex(nIndex);
        } // for each insn
    } // for each bblock
} // ir_number_instructions


//////////////////////////////////////////////////////////////////////
//
// RemoveCfgEdge
//
CfgEdge* RemoveCfgEdge(BBlock* pCurr, BBlock* pSucc)
{
    ASSERT(NULL != pCurr);
    ASSERT(NULL != pSucc);
    CfgEdge* pEdge = FindCfgEdge(pCurr, pSucc);
    ASSERT(NULL != pEdge);
    return RemoveCfgEdge(pEdge);
} // RemoveCfgEdge


//////////////////////////////////////////////////////////////////////
//
// ir_post_dominate_p -- return true if pInsn1 dominates pInsn2
//
bool ir_post_dominate_p(const Instruction* pInsn1, const Instruction* pInsn2)
{
    if (pInsn1->GetBBlock() == pInsn2->GetBBlock())
        { return pInsn1->GetIndex() >= pInsn2->GetIndex(); }

    return pInsn1->GetBBlock()->DoesPostDominate(pInsn2->GetBBlock());
} // ir_post_dominate_p


// ir_simplify_phi
//  For values PHI instruction, all inputs of PHI instruction is single value
//  converts it to single value if user is RET.
bool ir_simplify_phi(BBlock* pBBlock)
{
    bool fChanged = false;
    foreach (BBlock::EnumInsn, oEnum, pBBlock)
    {
        PhiInsn* pPhi = oEnum.Get()->DynamicCast<PhiInsn>();
        if (NULL == pPhi) break;
        if (NULL == pPhi->GetVd()) continue;

        bool fSingle = true;
        Val ty = nil;
        foreach (Instruction::EnumInput, oEnum, pPhi)
        {
            if (oEnum.Get()->Is<Values>())
            {
                fSingle = false;
                break;
            }

            ty = ty_or(ty, oEnum.Get()->GetTy());
        } // for each input

        if (! fSingle) continue;

        Register* pRx = NULL;
        Output::EnumUseSite  oEnum(pPhi->GetVd());
        while (! oEnum.AtEnd())
        {
            Instruction* pRet = oEnum.Get()->GetInstruction()->
                    DynamicCast<RetInsn>();
                oEnum.Next();

            if (NULL == pRet) continue;

            if (NULL == pRx)
            {
                fChanged = true;
                pRx = new Register();
                ir_insert_insn(
                    new PhiInsn(ty, pRx, pPhi),
                    pBBlock->GetFirstInsn() );
            }

            pRet->GetOperandBox(0)->Replace(pRx);
        } // for each use
    } // for each PHI
    return fChanged;
} // ir_simplify_phi


//////////////////////////////////////////////////////////////////////
//
// ir_remove_useless_instructions
//
static bool ir_remove_useless_instructions_1(Function* pFun)
{
    ASSERT(NULL != pFun);

    ir_eliminate_unreachable_bblocks(pFun);

    bool fChanged = false;
    //foreach (Function::EnumBBlock_Reverse_Layout, oEnumBB, pFun)
    foreach (Function::EnumBBlock_Postorder, oEnumBB, pFun)
    {
        BBlock* pCurr = oEnumBB.Get();

        if (ir_simplify_phi(pCurr))
        {
            fChanged = true;
        }

        BBlock::EnumInsn_Reverse oEnum(pCurr);
        while (! oEnum.AtEnd())
        {
            Instruction* pInsn = oEnum.Get();
                oEnum.Next();

            if (pInsn->Simplify()) fChanged = true;

            if (pInsn->IsUseless())
            {
                if (pInsn->IsPinned())
                {
                    html_log_format(3, L"void ~S ~S~:%", pCurr, pInsn);
                    pInsn->SetVoid();
                    fChanged = true;
                }
                else
                {
                    ir_remove_insn(pInsn);
                    fChanged = true;
                } // if
            } // if
        } // for each insn
    } // for each bblock

    return fChanged;
} // ir_remove_useless_instructions_1


bool ir_remove_useless_instructions(Function* pFun)
{
    bool fChanged = false;
    uint nCount = 0;
    for (;;)
    {
        nCount += 1;
        html_log_format(2, L"[~D] Remove useless instructions in ~S~:%",
            nCount,
            pFun );

        if (! ir_remove_useless_instructions_1(pFun)) return fChanged;

        fChanged = true;
    } // for
} // ir_remove_useless_instructions


//////////////////////////////////////////////////////////////////////
//
// ir_replace_all_users
//
void
ir_replace_all_users(Operand* pNew, Output* pOld)
{
    ASSERT(NULL != pNew);
    ASSERT(NULL != pOld);

    // %b0 and %b1 don't have definition.
    //ASSERT(! pNew->IsOutput() || NULL != pNew->ToOutput()->GetDfn());

    html_log_format(4, L"replace_all_uses: ~S ~S~:%", pNew, pOld);

    Output::Enum oEnum(pOld);

    while(! oEnum.AtEnd())
    {
        OperandBox* pBox = oEnum.Get();
            oEnum.Next();

        pBox->Replace(pNew);
    } // for each box
} // ir_replace_all_users

} // Compiler
