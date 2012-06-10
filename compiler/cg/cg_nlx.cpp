#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - Nonlocal Exit
// compiler/cg/cg_nlx.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_nlx.cpp#2 $
//
// Description:
//  TBD
//  See eval-with-restart (devel3/d25-cmdl.lisp)
//
#include "./cg_defs.h"

#include "../ir/ir_dfa.h"
#include "../ir/ir_pass.h"


namespace Compiler
{

namespace
{

//////////////////////////////////////////////////////////////////////
//
// Nonlocal Pass
//
class NlxPass : public FunctionPass
{
    private: WorkList_<Register> m_oRegs;

    // ctor
    public: NlxPass() : FunctionPass(L"CG-NLX") {}

    // Entry Point
    protected: virtual void process_function(Function* pFun)
    {
        if (! hasNonlocalExit(pFun)) return;

        html_log_format(2, L"<h2>process ~S</h2>~%", pFun);

        m_oRegs.MakeEmpty();

        ComputeLiveness(pFun);

        foreach (Function::EnumReg, oEnum, pFun)
        {
            Register* pRx = oEnum.Get();
            if (! pRx->HasIndex()) continue;

            if (! m_oRegs.Has(pRx))
            {
                m_oRegs.Push(pRx);
            }
        } // for each reg

        updateCLOSEs(pFun);
    } // process_function

    // [H]
    static bool hasNonlocalExit(const Function* pFun)
    {
        foreach (FrameList::Enum, oEnum, &pFun->m_oFrames)
        {
            Frame* pFrame = oEnum.Get();
            Instruction* pDfnI = pFrame->GetDfn();
                if (NULL == pDfnI) continue;

            switch (pDfnI->GetOpcode())
            {
            case IrOp_OPENBLOCK:
            case IrOp_OPENCATCH:
            case IrOp_OPENTAGBODY:
                return true;
            } // switch opcode
        } // for each frame
        return false;
    } // hasNonlocalExit

    // [U]
    void updateCLOSEs(Function* pFun)
    {
        foreach (FrameList::Enum, oEnum, &pFun->m_oFrames)
        {
            Frame* pFrame = oEnum.Get();
            Instruction* pDfnI = pFrame->GetDfn();
            if (NULL == pDfnI) continue;

            switch (pDfnI->GetOpcode())
            {
            case IrOp_OPENBLOCK:
            case IrOp_OPENCATCH:
                updateCLOSE(
                    pFrame,
                    pDfnI->GetSy()->StaticCast<Label>()->GetBBlock() );
                break;

            case IrOp_OPENTAGBODY:
                foreach (Frame::EnumUseSite, oEnum, pFrame)
                {
                    TagInsn* pTag = oEnum.Get()->GetInstruction()->
                            DynamicCast<TagInsn>();
                        if (NULL == pTag) continue;

                    updateCLOSE(
                        pFrame,
                        pTag->GetSy()->StaticCast<Label>()->GetBBlock() );
                } // for each tag
                break;
            }
        } // for each frame
    } // updateCLOSE

    void updateCLOSE(Frame* pFrame, BBlock* pNlx)
    {
        html_log_format(3, L"<h3>process ~S</h3>~%", pFrame->GetDfn());

        const BitVec* pLiveIn = pNlx->Extend<DataFlowBB>()->GetIn();

        foreach (Frame::EnumUseSite, oEnum, pFrame)
        {
            CloseInsn* pClose = oEnum.Get()->GetInstruction()->
                    DynamicCast<CloseInsn>();
                if (NULL == pClose) continue;

            html_log_format(4, L"<h4>process ~S</h4>~%", pClose);

            Instruction* pNext = pClose->GetNext();

            foreach (WorkList_<Register>::Enum, oEnum, &m_oRegs)
            {
                Register* pRx = oEnum.Get();

                if (pLiveIn->IsZero(pRx->GetIndex())) continue;

                if (pRx->IsVirtual())
                {
                    pRx->SetStorage(Register::Storage_Stack);
                }

                ir_insert_insn(new UseInsn(pRx), pNext);
            } // for each reg
        } // for each use
    } // updateCLOSE
}; // NlxPass
} // namespace


//////////////////////////////////////////////////////////////////////
//
// cg_pass_finalize_nlx
//
void cg_pass_finalize_nlx()
{
    NlxPass oPass;
    oPass.Run();
} // cg_pass_finalize_nlx

} // Compiler
