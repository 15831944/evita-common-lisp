#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x86 - assembler
// cg/x86/x86_cg_ensure.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_ensure.cpp#25 $
//
#include "./x86x64_cg_ensure.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::process_function
//
void
X86X64EnsurePass::process_function(Function* pFun)
{
    html_log_format(1, L"<h1>process ~S</h1>~%", pFun);

    prepare_function(pFun);

    foreach (Function::EnumBBlock, oEnumBB, pFun)
    {
        BBlock* pCurr = oEnumBB.Get();

        BBlock::EnumInsn oEnum(pCurr);
        while (! oEnum.AtEnd())
        {
            Instruction* pInsn = oEnum.Get();
                oEnum.Next();

            process_instruction(pInsn);
        } // for each insn
    } // for each bblock

    while (! m_oVdDfns.IsEmpty())
    {
        Instruction* pDfn = m_oVdDfns.Pop();

        Instruction* pRefInsn = pDfn;
        for (;;)
        {
            pRefInsn = pRefInsn->GetNext();

            unless (pRefInsn->Is<ProjectInsn>() ||
                    pRefInsn->Is<CountInsn>() )
                { break; }
        } // for

        Values::EnumUseSite oEnum(pDfn->GetVd());
        while (! oEnum.AtEnd())
        {
            Instruction* pInsn = oEnum.Get()->GetInstruction();
                oEnum.Next();

            Register* pRd = pInsn->GetRd();

            if (pInsn->Is<CountInsn>())
            {
                Physical* pPd = new Physical($rn, pRd->GetVar());

                html_log_format(3, L"change output ~S -> ~S~:%", pInsn, pPd);

                pInsn->SetOutput(pPd);

                ir_insert_insn(new CopyInsn(pRd, pPd), pRefInsn);
            }
            else
            {
                uint iNth = static_cast<uint>(
                    Fixnum::Decode_(pInsn->GetLy()) );

                if (iNth < m_pMach->m_pGprArg->m_n)
                {
                    Physical* pPd = new Physical(
                        m_pMach->m_pGprArg->m_prgn[iNth],
                        pRd->GetVar() );

                    html_log_format(3, L"change output ~S -> ~S~:%",
                        pInsn, pPd );

                    pInsn->SetOutput(pPd);

                    ir_insert_insn(new CopyInsn(pRd, pPd), pRefInsn);
                }
                else
                {
                    Register* pRx = new Register();
                        pRx->SetStorage(Register::Storage_Pseudo);

                    Instruction* pTcb = ir_insert_insn(
                        new x86x64TcbInsn(ty_ptr_t, pRx,
                            offsetof(Kernel::Thread, mv_value[iNth])  ),
                        pRefInsn );

                    ir_insert_insn(new LoadInsn(pRd, pRx), pRefInsn);

                    ir_remove_insn(pInsn);

                    pRefInsn = pTcb;
                }
            } // if
        } // for each user
    } // for each dfn
} // X86X64EnsurePass::process_function


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::insert_for_vx
//
void
X86X64EnsurePass::insert_for_vx(Operand* pCallee, Instruction* pCall)
{
    Register* pRx = new Register();
        pRx->SetStorage(Register::Storage_Pseudo);

    x86x64TcbInsn* pTcbFn = new x86x64TcbInsn(ty_ptr_function, pRx,
        offsetof(Thread, m_fn) );

    StoreInsn* pStore = new StoreInsn(ty_ptr_function, pRx, pCallee);

    Instruction* pArgs = pCall->GetVy()->GetDfn();
    if (pArgs->Is<ValuesInsn>())
    {
        ir_move_insn(pArgs, pCall);
        ir_insert_insn(pTcbFn, pArgs);
        ir_insert_insn(pStore, pArgs);
    }
    else
    {
        ir_insert_insn(pTcbFn, pCall);
        ir_insert_insn(pStore, pCall);
        patch_vx(pCall, pCall->GetOperandBox(1));
    }

    process_instruction(pStore);
} // X86X64EnsurePass::insert_for_vx


// insert_MvSaveMvRestore
static Values* insert_MvSaveMvRestore(Values* pVd, Instruction* pUser)
{
    Register* pRx = new Register();
        pRx->SetStorage(Register::Storage_Stack);
    {
        Register* pR0 = new Physical($r0);

        Instruction* pDfn = pVd->GetDfn();
        Instruction* pRef = pDfn->GetNext();

        ir_insert_insn(new MvSaveInsn(pR0, pVd), pRef);
        ir_insert_insn(new CopyInsn(pRx, pR0), pRef);
    } // pRx

    Values* pVx = new Values();
    ir_insert_insn(
        new MvRestoreInsn(pVd->GetTy(), pVx, pRx),
        pUser );

    return pVx;
} // insert_MvSaveMvRestore


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::patch_operand
//
void
X86X64EnsurePass::patch_operand(OperandBox* pBox)
{
    Operand*  pSx = pBox->GetOperand();
    Register* pRy = new Register();
    ir_insert_insn(new CopyInsn(pRy, pSx), pBox->GetInstruction());
    pBox->Replace(pRy);
} // X86X64EnsurePass::patch_operand


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::patch_vx
//  Makes EAX free for
//    o CLOSE instruction
//    o CALL+CALL
//    o RET
//
// Note: We don't patch OPENFINALLY. RA handles OPENFINALLY.
//
//      VALUEST ty %v1 <= ...
//      ...
//      THROW %r2 %v1
//    ==>
//      VALUES ty %v1 <=
//      PROJECT EAX <= %v1 0
//      COPY %r3 <= %EAX
//      ...
//      x86_VALUES %v4 <= %r3
//      THROW %r2 %v4
//
void
X86X64EnsurePass::patch_vx(Instruction* pUser, OperandBox* pBox)
{
    Values* pVd = pBox->GetOperand()->DynamicCast<Values>();
    if (NULL == pVd)
    {
        return;
    }

    if (pVd->GetDfn()->GetBBlock() != pUser->GetBBlock())
    {
        warn(L"Values go over basic block.");

        html_log_format(2, L"~S ~S uses ~S ~S~:%",
            pUser->GetBBlock(), pUser,
            pVd->GetDfn()->GetBBlock(), pVd->GetDfn() );

        return;
    } // if

    ASSERT(pUser->GetBBlock()->GetFirstInsn() != pUser);

    if (pUser->GetPrev()->GetVd() == pVd)
    {
        return;
    }

    if (pVd->GetDfn()->Is<ValuesInsn>())
    {
        // Move VALUES just before user.
        ir_move_insn(pVd->GetDfn(), pUser);
        return;
    }

    pBox->Replace(insert_MvSaveMvRestore(pVd, pUser));
} // X86X64EnsurePass::patch_vx


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::process_nonlocal_xfer
//
// Called by:
//  X86X64EnsurePass::process_RETURNFROM
//  X86X64EnsurePass::process_THROW
//
//  Rewrites to CALL
//      STORE [TCB+m_fn], %tag
//      CALL .throw %vx
void
X86X64EnsurePass::process_nonlocal_xfer(Instruction* pThrow, Val fn)
{
    html_log_format(2, L"process ~S:~S~:%", pThrow->GetBBlock(), pThrow);

    insert_for_vx(pThrow->GetSx(), pThrow);

    ir_replace_insn(
        new CallInsn(ty_void, Obj_Void, NewLiteral(fn), pThrow->GetVy()),
        pThrow );
} // X86X64EnsurePass::process_nonlocal_xfer


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::process_output
//
void
X86X64EnsurePass::process_output(Instruction* pInsn)
{
    Register* pRd = pInsn->GetRd();
        if (NULL == pRd) return;

    if (pRd->IsStackSlot())
    {
        Register* pR0 = new Register();
        pInsn->SetOutput(pR0);
        ir_insert_insn(new CopyInsn(pRd, pR0), pInsn->GetNext());
    }
} // X86X64EnsurePass::process_output


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::process_ADD
//
//      ADD ty %rd <= %rx %ry
//      ==>
//      COPY ty %ra <= %rx
//      ADD  ty %ra <= %ra %ry
//      COPY ty %rd <= %ra
void
X86X64EnsurePass::process_ADD(Instruction* pInsn)
{
    ASSERT(NULL != pInsn);

    if (pInsn->GetRd() != pInsn->GetRx())
    {
        Register* pRd = pInsn->GetRd();

        Register* pRa = new Register(pRd->GetVar(), pRd->GetClass());
            pRa->MarkNotSSA();

        Operand*  pSx = pInsn->GetSx();

        ir_insert_insn(new CopyInsn(pRa, pSx), pInsn);
        pInsn->GetOperandBox(0)->Replace(pRa);
        pInsn->SetOutput(pRa);
        ir_insert_insn(new CopyInsn(pRd, pRa), pInsn->GetNext());
    }
} // X86X64EnsurePass::process_ADD


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::process_BOX
//
//      BOX double-flaot %rd <- %fx
//      ==>
//      COPY float64      %f0 <- %fx
//      BOX double-float  %r0 <- %f0
//      COPY double-float %rd <- %r0
void
X86X64EnsurePass::process_BOX(Instruction* pBox)
{
    if (pBox->GetTy() == ty_double_float ||
        pBox->GetTy() == ty_single_float )
    {
        Register* pF0 = new Physical(Register::Class_FPR, $f0);
            ir_insert_insn(new CopyInsn(pF0, pBox->GetRx()), pBox);
            pBox->GetOperandBox(0)->Replace(pF0);

        Register* pRd = pBox->GetRd();
        Register* pR0 = new Physical(Register::Class_GPR, $r0);
            pBox->SetOutput(pR0);
        ir_insert_insn(new CopyInsn(pRd, pR0), pBox->GetNext()); 
        return;
    } // float64

    warn(L"cg-ensure: BOX doesn't support ~S.", pBox->GetTy());
}  // X86X64EnsurePass::process_BOX


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::process_COUNT
//  Moves COUNT instruction just after output instruction.
//
void
X86X64EnsurePass::process_COUNT(Instruction* pCount)
{
    Instruction* pDfn = pCount->GetVx()->GetDfn();
    if (! m_oVdDfns.Has(pDfn)) m_oVdDfns.Push(pDfn);

    Instruction* pRefInsn = pDfn->GetNext();
    if (pRefInsn != pCount) ir_move_insn(pCount, pRefInsn);
} // X86X64EnsurePass::process_COUNT


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::process_ELT
//
void
X86X64EnsurePass::process_ELT(Instruction* pElt)
{
    Operand* pSx = pElt->GetSx();
    if (! pSx->Is<Register>())
    {
        Register* pRx = new Register();
        ir_insert_insn(new CopyInsn(pRx, pSx), pElt);
        pElt->GetOperandBox(0)->Replace(pRx);
    }
} // X86X64EnsurePass::process_ELT


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::process_GO
//
void
X86X64EnsurePass::process_GO(Instruction* pGo)
{
    Values* pVy = new Values();

    ir_insert_insn(new ValuesInsn(pVy, pGo->GetRx()), pGo);

    ir_replace_insn(
        new CallInsn(ty_void, Obj_Void, NewLiteral(QDgo), pVy),
        pGo );
} // X86X64EnsurePass::process_GO


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_LOAD
//
void
X86X64EnsurePass::process_LOAD(Instruction* pLoad)
{
    Register* pRd = pLoad->GetRd();

    Ty ty = pLoad->GetTy();

    if (ty == ty_c6_stack_cell)
    {
        ASSERT(pRd->IsVirtual());
        pRd->SetStorage(Register::Storage_Pseudo);
        return;
    }

    if (ty == ty_float32 || ty == ty_float64)
    {
        FpRegister* pFd = new FpRegister(pRd->GetVar());
        pLoad->SetOutput(pFd);
        ir_replace_all_users(pFd, pRd);
    }

    process_output(pLoad);
} // X86X64EnsurePass::process_LOAD


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_MVSAVE
//
void
X86X64EnsurePass::process_MVSAVE(Instruction* pInsn)
{
    Register* pRd = pInsn->GetRd();
        ASSERT(pRd->IsVirtual());

    pRd->SetStorage(Register::Storage_Stack);

    Register* pR0 = new Physical($r0);
    pInsn->SetOutput(pR0);
    ir_insert_insn(new CopyInsn(pRd, pR0), pInsn->GetNext());
} // X86X64EnsurePass::process_MVSAVE


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_NEG
//
void
X86X64EnsurePass::process_NEG(Instruction* pNeg)
{
    Register* pRx = pNeg->GetRx();
    Register* pR2 = new Register(pRx->GetVar(), pRx->GetClass());
    ir_insert_insn(new x86x64DefInsn(pRx->GetTy(), pR2), pNeg);
    ir_replace_insn(
        new x86x64NegInsn(pNeg->GetTy(), pNeg->GetRd(), pRx, pR2),
        pNeg );
} // X86X64EnsurePass::process_NEG


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::process_PROJECT
//  Moves PROJECT instruction just after output instruction.
//
void
X86X64EnsurePass::process_PROJECT(Instruction* pProject)
{
    Instruction* pDfn = pProject->GetVx()->GetDfn();
    if (! m_oVdDfns.Has(pDfn)) m_oVdDfns.Push(pDfn);

    Instruction* pRefInsn = pDfn->GetNext();
    if (pRefInsn->Is<CountInsn>()) pRefInsn = pRefInsn->GetNext();
    while (pRefInsn->Is<ProjectInsn>())
    {
        if (pRefInsn == pProject) return;
        if (pRefInsn->GetLy() > pProject->GetLy()) break;
        pRefInsn = pRefInsn->GetNext();
    }
    ir_move_insn(pProject, pRefInsn);
} // X86X64EnsurePass::process_PROJECT


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::process_RETURNFROM
//
void
X86X64EnsurePass::process_RETURNFROM(Instruction* pReturnFrom)
{
    process_nonlocal_xfer(pReturnFrom, QDreturn_from);
} // X86X64EnsurePass::process_RETURNFROM


//////////////////////////////////////////////////////////////////////
//
//      SELECT ty %rd <= %bx %sy %sz
//      ==>
//      COPY ty %rd* <= %sy
//      COPY ty %rz  <= %sz
//      SELECT ty %rd* <= %bx %rd %rz
void
X86X64EnsurePass::process_SELECT(Instruction* pSelect)
{
    Register* pRd = pSelect->GetRd();

    if (pRd->IsStackSlot())
    {
        Register* pR2 = new Register();
        pSelect->SetOutput(pR2);
        ir_insert_insn(new CopyInsn(pRd, pR2), pSelect->GetNext());
        pRd = pR2;
    } // if

    pRd->MarkNotSSA();

    ir_insert_insn(
        new CopyInsn(pRd, pSelect->GetSy()),
        pSelect );

    pSelect->GetOperandBox(1)->Replace(pRd);

    ensure_physical(pSelect->GetOperandBox(2));
} // X86X64EnsurePass::process_SELECT


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::process_STORE
//
void
X86X64EnsurePass::process_STORE(Instruction* pStore)
{
    ensure_operand(pStore->GetOperandBox(1));
} // X86X64EnsurePass::process_STORE


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::process_THROW
//
void
X86X64EnsurePass::process_THROW(Instruction* pThrow)
{
    process_nonlocal_xfer(pThrow, QDthrow);
} // X86X64EnsurePass::process_THROW


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::process_UNBOX
//
void
X86X64EnsurePass::process_UNBOX(Instruction* pUnbox)
{
    if (pUnbox->GetTy() == ty_float32 || pUnbox->GetTy() == ty_float64)
    {
        Instruction* pNext = pUnbox->GetNext();
        if (pUnbox->GetRd()->GetSingleUser() == pNext)
        {
            switch (pNext->GetOpcode())
            {
            case IrOp_X86X64_CONVERT:
                pUnbox->GetRd()->SetStorage(Register::Storage_Pseudo);
                return;
            } // switch opcode
        } // if
    } // if float{32,64}

    if (NULL != pUnbox->GetRx()) return;
    Register* pRx = new Register();
    ir_insert_insn(new CopyInsn(pRx, pUnbox->GetSx()), pUnbox);
    pUnbox->GetOperandBox(0)->Replace(pRx);
} // X86X64EnsurePass::process_UNBOX


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::process_VALUES
//
//      VALUES ty %r1 <= var frame
//      =>
//      LOAD t %r2 <= frame
//      VALUES ty %r1 <= var %r2
//
void
X86X64EnsurePass::process_VALUES(Instruction* pValues)
{
    foreach (Instruction::EnumInput, oEnum, pValues)
    {
        OperandBox* pBox = oEnum.GetBox();
        ASSERT(! pBox->GetOperand()->Is<Frame>());
#if 0
        if (pBox->GetOperand()->Is<Frame>())
        {
            Register* pRy = new Register();
            ir_insert_insn(
                new LoadInsn(pRy, pBox->GetOperand()),
                pValues );
            pBox->Replace(pRy);
        }
#endif
    } // for each input
} // X86X64EnsurePass::process_VALUES


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::process_VARDEF
//
void X86X64EnsurePass::process_VARDEF(Instruction* pVarDef)
{
    ensure_operand(pVarDef->GetOperandBox(1));
} // X86X64EnsurePass::process_VARDEF

} // Compiler
