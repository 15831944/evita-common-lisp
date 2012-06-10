#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - Nonlocal Exit
// compiler/cg/cg_nlx.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_nlx.cpp#1 $
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

        ComputeLiveness(pFun);

        foreach (Function::EnumReg, oEnum, pFun)
        {
            Register* pRx = oEnum.Get();
            if (pRx->HasIndex())
            {
                m_oRegs.Push(pRx);
            }
        } // for each reg

        updateCLOSE(pFun);
    } // process_function

    // [H]
    static bool hasNonlocalExit(const Function* pFun)
    {
        foreach (FrameList::Enum, oEnum, &pFun->m_oFrames)
        {
            Frame* pFrame = oEnum.Get();
            Instruction* pDfn = pFrame->GetDfn();
                if (NULL == pDfn) continue;

            switch (pDfn->GetOpcode())
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
    void updateCLOSE(Function* pFun)
    {
        foreach (FrameList::Enum, oEnum, &pFun->m_oFrames)
        {
            Frame* pFrame = oEnum.Get();
            Instruction* pDfn = pFrame->GetDfn();
                if (NULL == pDfn) continue;

            switch (pDfn->GetOpcode())
            {
            case IrOp_OPENBLOCK:
            case IrOp_OPENCATCH:
                html_log_format(3, L"<h3>process ~S</h3>~%", pDfn);

                updateCLOSE1(
                    pFrame,
                    pDfn->GetSy()->StaticCast<Label>()->GetBBlock() );

            case IrOp_OPENTAGBODY:
                html_log_format(3, L"<h3>process ~S</h3>~%", pDfn);

                foreach (Frame::EnumUseSite, oEnum, pFrame)
                {
                    TagInsn* pTag = oEnum.Get()->GetInstruction()->
                            DynamicCast<TagInsn>();
                        if (NULL == pTag) continue;

                    updateCLOSE1(
                        pFrame,
                        pTag->GetSy()->StaticCast<Label>()->GetBBlock() );
                } // for each tag
                break;
            }
        } // for each frame
    } // updateCLOSE

    void updateCLOSE1(Frame* pFrame, BBlock* pNlx)
    {
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
    } // updateCLOSE1
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
