#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - opt - Call Phase
// compiler/opt/opt_call.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/opt/opt_call.cpp#5 $
//
//
// Description:
//  This file contains "Self-Tail Call Elimination" pass.
//
#include "./opt_defs.h"

#include "../ir/ir_bblock.h"
#include "../ir/ir_instruction.h"
#include "../ir/ir_pass.h"

namespace Compiler
{

namespace
{

//////////////////////////////////////////////////////////////////////
//
// Call Optimizer
//
class CallOptimizer : public FunctionPass
{
    public: CallOptimizer() : FunctionPass(L"OPT-CALL") {}

    public: virtual void process_function(Function*);

    BBlock*         m_pLoopBB;
    PrologueInsn*   m_pPrologue;
    UINT            m_cOptimizedCalls;

    void expand_call(CallInsn*);
    void expand_call_args(CallInsn*);

    BBlock*  get_loop_bblock();
    PhiInsn* get_phi_insn(UINT);
    BBlock*  setup_loop_bblock(Instruction*);

    static bool need_cleanup_p(Function*)
        { return false; }

    static bool notinline_p(Function*)
        { return false; }

    static bool mv_call_p(CallInsn* pCall)
    {
        Instruction* pInsn = 
            pCall->GetSy()->StaticCast<Values>()->GetDfn();
        return ! pInsn->Is<ValuesInsn>();
    } // mv_call_p

    static bool required_only_p(Function* pFun)
    {
        return pFun->GetArityMin() == pFun->GetArityMax();
    } // required_only_p
}; // CallOptimizer


//////////////////////////////////////////////////////////////////////
//
// CallOptimizer::process_function
//
void CallOptimizer::process_function(Function* pFun)
{
    ASSERT(NULL != pFun);

    html_log_format(1, L"<h2>Process ~S</h2>~%", pFun);

    m_pLoopBB = NULL;
    m_cOptimizedCalls = 0;

    BBlock::EnumInEdge  oEnum(pFun->GetExitBB());
    while (! oEnum.AtEnd())
    {
        BBlock* pBBlock = oEnum.GetNode();
            oEnum.Next();

        RetInsn* pRet = pBBlock->GetLastInsn()->DynamicCast<RetInsn>();

        if (NULL == pRet) continue;

        CallInsn* pCall = pBBlock->GetLastInsn()->GetPrev()->
            DynamicCast<CallInsn>();

        if (NULL == pCall) continue;
        if (pRet->GetSx() != pCall->GetOutput()) continue;

        if (pCall->GetSx() == pFun &&
            ! notinline_p(pFun) &&
            required_only_p(pFun)  &&
            ! need_cleanup_p(pFun) &&
            ! mv_call_p(pCall) )
        {
            expand_call(pCall);
        }
    } // for each pred

    if (0 == m_cOptimizedCalls)
    {
        html_log_format(1, L"<i>No change.</i>~:%");
    }
    else
    {
        html_log_format(1, L"Replaces ~D calls into jump.~:%",
            m_cOptimizedCalls );

        ir_remove_useless_instructions(pFun);
    }
} // CallOptimizer::process_function


//////////////////////////////////////////////////////////////////////
//
// CallOptimizer::expand_call
//
void CallOptimizer::expand_call(CallInsn* pCall)
{
    ASSERT(NULL != pCall);

    html_log_format(2, L"Expand self-tail-call ~S~:%", pCall);

    if (NULL == m_pLoopBB)
    {
        Function* pFun = pCall->GetSx()->StaticCast<Function>();

        m_pPrologue = pFun->GetPrologueInsn();
        m_pLoopBB   = get_loop_bblock();
    } // if

    expand_call_args(pCall);

    html_log_format(2, L"Replace ~S with JUMP.~:%", pCall->GetNext());
    ir_replace_insn(new JumpInsn(m_pLoopBB), pCall->GetNext());

    ir_remove_insn(pCall);

    m_cOptimizedCalls += 1;
} // CallOptimizer::expand_call


//////////////////////////////////////////////////////////////////////
//
// CallOptimizer::expand_call_args
//
//
//      PHI %rd <= [CallBB %sx] ...
//
void CallOptimizer::expand_call_args(CallInsn* pCall)
{
    ValuesInsn* pArgs = pCall->GetSy()->StaticCast<Values>()->
        GetDfn()->StaticCast<ValuesInsn>();

    UINT nNth = 0;
    foreach (Instruction::EnumInput, oEnum, pArgs)
    {
        Operand* pSx = oEnum.Get();

        PhiInsn* pPhi = get_phi_insn(nNth);
        if (NULL != pPhi)
        {
            log_format(3, L"[~D]~%", Fixnum::Encode(nNth));

            pPhi->StaticCast<PhiInsn>()->
                AddInput(pCall->GetBBlock(), pSx);
        } // if

        nNth += 1;
    } // for each arg
} //  CallOptimizer::expand_call_args


//////////////////////////////////////////////////////////////////////
//
//  Looking forward JUMP instruction follows PROJECT instruction
//     PROLOGUE   ty %vx <= :none
//     PROJECT ty %r0 <= %vx 0
//     PROJECT ty %r1 <= %vx 1
//     PROJECT ty %r2 <= %vx 2
//     JUMP loop
BBlock* CallOptimizer::get_loop_bblock()
{
    Instruction* pRunner = m_pPrologue;
    for (;;)
    {
        pRunner = pRunner->GetNext();

        // We've already setup loop head bblock
        switch (pRunner->GetOpcode())
        {
        case IrOp_JUMP:
            return pRunner->GetSx()->StaticCast<Label>()->GetBBlock();

        case IrOp_COUNT:
        case IrOp_PROJECT:
            break;

        default:
            return setup_loop_bblock(pRunner->GetPrev());
        } // switch opcode
    } // for each insn
} // CallOptimizer::get_loop_bblock


//////////////////////////////////////////////////////////////////////
//
// CallOptimizer::get_phi_insn
//
//  Description:
//   Returns PHI instruction for nth argument by using use sites:
//      PROLOGUE -> PROJECT -> PHI
//
PhiInsn* CallOptimizer::get_phi_insn(UINT nNth)
{
    Val nth = Fixnum::Encode(nNth);

    foreach (Values::Enum, oEnum, m_pPrologue->GetVd())
    {
        Instruction* pProj = oEnum.Get()->GetInstruction()->
            DynamicCast<ProjectInsn>();

        if (NULL != pProj &&
            pProj->GetSy()->StaticCast<Literal>()->GetDatum() == nth )
        {
            Register* pRd = pProj->GetRd();
            foreach (Register::EnumUseSite, oEnum, pRd)
            {
                PhiInsn* pInsn = oEnum.Get()->GetInstruction()->
                    DynamicCast<PhiInsn>();

                if (NULL != pInsn && pInsn->GetParent() == m_pLoopBB)
                {
                    return pInsn;
                }
            } // for each use
        } // if
    } // for each use

    return NULL;
} // CallOptimizer::get_phi_insn


//////////////////////////////////////////////////////////////////////
//
// CallOptimizer::setup_loop_bblock
//
//
//    start:
//      PROLOGUE   ty %vx <= :none
//      PROJECT ty %r0 <= %vx
//      PROJECT ty %r1 <= %vx
//      PROJECT ty %r2 <= %vx        <---- pRefInsn
//      JUMP loop
//
//    loop:
//      PHI ty %r00 <= (start %r0)
//      PHI ty %r01 <= (start %r1)
//      PHI ty %r02 <= (start %r2)
//      ...
//
BBlock* CallOptimizer::setup_loop_bblock(Instruction* pRefInsn)
{
    ASSERT(
        pRefInsn->Is<CountInsn>() ||
        pRefInsn->Is<ProjectInsn>() ||
        pRefInsn->Is<PrologueInsn>());

    html_log_format(2, L"Split ~S after ~S.~:%",
        pRefInsn->GetBBlock(),
        pRefInsn );

    BBlock* pLoopBB = ir_split_bblock_after(pRefInsn);

    html_log_format(2, L"Setup loop ~S~:%", pLoopBB);

    pRefInsn->GetBBlock()->AppendInsn(new JumpInsn(pLoopBB));

    if (m_pPrologue->GetTy() == ty_void)
    {
        return pLoopBB;
    }

    html_log_format(3, L"<ol>~%");

    // Insert PHI instruction for corresponding PROJECT instruction.
    foreach (Values::Enum, oEnum, m_pPrologue->GetVd())
    {
        Instruction* pProj = oEnum.Get()->GetInstruction();

        if (pProj->Is<ProjectInsn>() || pProj->Is<CountInsn>())
        {
            Val ty = pProj->GetTy();

            html_log_format(3, L"<li>Insert PHI for ~S => ~%", pProj);

            PhiInsn* pPhi = new PhiInsn(ty, pProj->GetRd());

            pProj->SetOutput(NewOutput(ty));

            ir_insert_insn(pPhi, pLoopBB->GetFirstInsn());

            pPhi->AddInput(pRefInsn->GetBBlock(), pProj->GetRd());

            html_log_format(3, L"~S</li>~%", pPhi);
        } // if
    } // for each use site

    html_log_format(3, L"</ol>~%");

    return pLoopBB;
} // CallOptimizer::setup_loop_bblock

} // namespace


//////////////////////////////////////////////////////////////////////
//
// optimize_call
//
void optimize_call()
{
    CallOptimizer oOptimizer;
    oOptimizer.Run();
} // optimize_call

} // Compiler
