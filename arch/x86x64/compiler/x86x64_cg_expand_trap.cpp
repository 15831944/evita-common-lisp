#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x86 - assembler
// cg/x86/x86_cg_asm_main.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_expand_trap.cpp#8 $
//
#include "./x86x64_cg_expand_trap.h"

#include "./x86x64_cg_defs.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// X86X64ExpandTrapPass::process_function
//
void
X86X64ExpandTrapPass::process_function(Function* pFun)
{
    html_log_format(2, L"<h2>Process ~S</h2>~%", pFun);

    WorkList_<Instruction> oTrapIfs;

    foreach (Function::EnumBBlock, oEnum, pFun)
    {
        BBlock* pBBlock = oEnum.Get();

        foreach (BBlock::EnumInsn, oEnum, pBBlock)
        {
            Instruction* pInsn = oEnum.Get();
            switch (pInsn->GetOpcode())
            {
            case IrOp_TRAPIF:
            case IrOp_TRAPIFNOT:
                oTrapIfs.Push(pInsn);
                break;
            } // switch opcode
        } // for each insn
    } // for bblock

    while (! oTrapIfs.IsEmpty())
    {
        Instruction* pTrapIf = oTrapIfs.Pop();
        process_TRAPIF(pTrapIf);
    } // while

    m_oErrorBlocks.MakeEmpty();
} // X86X64ExpandTrapPass::process_function


//////////////////////////////////////////////////////////////////////
//
// match_frame
//
static bool 
match_frame(const Frame* p1, const Frame* p2)
{
    while (NULL != p1 && p1->GetDfn() == NULL) p1 = p1->GetOuter();
    while (NULL != p2 && p2->GetDfn() == NULL) p2 = p2->GetOuter();
    return p1 == p2;
} // match_frame


//////////////////////////////////////////////////////////////////////
//
// get_frame
//
static Frame* get_frame(const Instruction* p)
{
    switch (p->GetOpcode())
    {
    case IrOp_TRAPIF:    return p->StaticCast<TrapIfInsn>()->GetFrame();
    case IrOp_TRAPIFNOT: return p->StaticCast<TrapIfNotInsn>()->GetFrame();
    default: CAN_NOT_HAPPEN();
    } // switch opcode
} // get_frame



//////////////////////////////////////////////////////////////////////
//
// match_trap
//
static bool 
match_trap(const Instruction* p1, const Instruction* p2)
{
    return
        p1->GetLy() == p2->GetLy() &&
        p1->GetSz()->Equal(p2->GetSz()) &&
        match_frame(get_frame(p1), get_frame(p2));
} // match_trap


//////////////////////////////////////////////////////////////////////
//
// find_trap
//
BBlock*
X86X64ExpandTrapPass::find_trap(Instruction* pTrapIf)
{
    foreach (WorkList_<Instruction>::Enum, oEnum, &m_oErrorBlocks)
    {
        Instruction* pPresent = oEnum.Get();
        if (match_trap(pTrapIf, pPresent))
        {
            return pPresent->GetExtension<BBlock>();
        }
    } // for each trap

    return NULL;
} // X86X64ExpandTrapPass::find_trap


//////////////////////////////////////////////////////////////////////
//
// X86X64ExpandTrapPass::process_TRAPIF
//
//      TRAPIF %bool name %sx*
//    ==>
//      BRANCH %bool error succ
//
//     error:
//      VALUES ty %vx <= %sx*
//      CALL name %vx
//
void
X86X64ExpandTrapPass::process_TRAPIF(Instruction* pTrapIf)
{
    BBlock* pSucc = ir_split_bblock_after(pTrapIf);

    BBlock* pTrap = find_trap(pTrapIf);

    if (NULL != pTrap)
    {
        ir_remove_insn(pTrapIf->GetVz()->GetDfn());
    }
    else
    {
        pTrap = NewBBlock();
            pSucc->GetFunction()->AppendBBlock(pTrap);

        m_oErrorBlocks.Push(pTrapIf);
        pTrapIf->SetExtension<BBlock>(pTrap);

        Instruction* pUnreachable =
            pTrap->AppendInsn(new UnreachableInsn());

        Values* pArgs = pTrapIf->GetVz();
        ir_move_insn(pArgs->GetDfn(), pUnreachable);

        insert_service(
            pTrapIf->GetSy()->StaticCast<Literal>(),
            pArgs,
            pUnreachable );

        foreach (Frame::Enum, oEnum, get_frame(pTrapIf))
        {
            Frame* pFrame = oEnum.Get();
            if (NULL == pFrame->GetDfn()) continue;
            if (pTrap->GetFunction() != pFrame->GetOwner()) continue;
            ir_insert_insn(new UseInsn(pFrame), pUnreachable);
        } // for each frame
    } // if

    Instruction* pBranch;
    {
        switch (pTrapIf->GetOpcode())
        {
        case IrOp_TRAPIF:
            pBranch = new BranchInsn(pTrapIf->GetBx(), pTrap, pSucc);
            break;

        case IrOp_TRAPIFNOT:
            pBranch = new BranchInsn(pTrapIf->GetBx(), pSucc, pTrap);
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch opcode
    } // pBranch

    ir_replace_insn(pBranch, pTrapIf);
} // X86X64ExpandTrapPass::process_TRAPIF

} // Compiler
