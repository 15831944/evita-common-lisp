#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - opt - Inline Pass
// compiler/opt/opt_closure->cpp
//
// Copyright (C) 1996-2006 by Project Vogue->
// Written by Yoshifumi "VOGUE" INOUE-> (yosi@msn->com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/opt/opt_inline.cpp#14 $
//
//
// Description:
//  This file contains "Inline" pass->
//
#include "./opt_defs.h"

#include "../cm/cm_module.h"
#include "../cm/cm_session.h"

#include "../ir/ir_pass.h"

namespace Compiler
{

namespace
{

//////////////////////////////////////////////////////////////////////
//
// Inline Optimizer
//
class InlinePass : public ModulePass
{
    public: InlinePass() :
        ModulePass(L"OPT-INLINE"),
        m_pCall(NULL) {}

    WorkList_<Instruction> m_oCallSites;
    CallInsn* m_pCall;

    public: virtual void process_module(Module* pModule)
    {
        foreach (Module::EnumFunction, oEnum, pModule)
        {
            Function* pFun = oEnum.Get();
            plan_inline(pFun);
        } // for each fun

        while (! m_oCallSites.IsEmpty())
        {
            CallInsn* pCall = m_oCallSites.Pop()->StaticCast<CallInsn>();
            if (pCall->GetBBlock()->GetFunction() != pCall->GetSx())
            {
                process(pCall);
            }
        } // while
    } // process_module

    // is_called_once
    static CallInsn* called_once(Function* pCallee)
    {
        if (pCallee->HasUseSite())
        {
            // Callee is used as value.
            return NULL;
        }

        Function::EnumCallSite oEnum(pCallee);
        if (oEnum.AtEnd())
        {
            // No call site
            return NULL;
        }

        CallInsn* pCall = oEnum.Get()->GetInstruction()->
            StaticCast<CallInsn>();

        oEnum.Next();
        if (! oEnum.AtEnd()) return NULL;
        if (pCall->IsNotInline()) return NULL;

        return pCall;
    } // is_called_once

    Function* getCaller() const
        { return m_pCall->GetBBlock()->GetParent(); }

    Operand* convert_vx_to_sx(Values*);
    void plan_inline(Function*);
    void process(CallInsn*);
    void process_body(CallInsn*, Function*);
    void process_epilogue(Function*);
    void process_prologue(CallInsn*, Function*);

    void process_GO(Instruction*);
    void process_RETURNFROM(Instruction*);
    void process_UNREACHABLE(Instruction*);
}; // InlinePass


//////////////////////////////////////////////////////////////////////
//
// convert_vx_to_sx
//
Operand* InlinePass::convert_vx_to_sx(Values* pVx)
{
    Instruction* pDfn = pVx->GetDfn();

    html_log_format(3, L"convert ~S to single~:%", pDfn);

    switch (pDfn->GetOpcode())
    {
    case IrOp_CALL:
    {
        Register* pRd = new Register();
        pDfn->SetTy(ty_get_primary(pDfn->GetTy()));
        pDfn->SetOutput(pRd);
        ir_replace_all_users(pRd, pVx);
        return pRd;
    } // call

    case IrOp_MVRESTORE:
    {
        Values* pVx = pDfn->GetRx()->GetDfn()->GetVx();
        Register* pRx = new Register();
        ir_insert_insn(new ProjectInsn(
                ty_get_primary(pVx->GetTy()), pRx, pVx, 0),
            pDfn );
        return pRx;
    } // mvrestore

    case IrOp_PHI:
    {
        Register* pRd = new Register();
        pDfn->SetTy(ty_get_primary(pDfn->GetTy()));
        pDfn->SetOutput(pRd);
        ir_replace_all_users(pRd, pVx);
        foreach (PhiInsn::EnumInput, oEnum, pDfn->StaticCast<PhiInsn>())
        {
            Values* pVi = oEnum.Get()->DynamicCast<Values>();
            if (NULL != pVi)
            {
                oEnum.GetBox()->Replace(convert_vx_to_sx(pVi));
            }
        } // for each input
        return pRd;
    } // phi

    case IrOp_VALUES:
    {
        if (0 == pDfn->GetOperandCount())
        {
            return NewLiteral(nil);
        }
        else
        {
            return pDfn->GetSx();
        }
    } // values

    case IrOp_VALUESA:
    {
        switch (pDfn->GetOperandCount())
        {
        case 0:
            return NewLiteral(nil);

        case 1:
        {
            Values*      pVy = new Values();
            Register*    pRd = new Register();
            Instruction* pRef = pDfn->GetNext();

            ir_insert_insn(
                new ValuesInsn(pVy, pDfn->GetSx()),
                pRef );

            ir_insert_insn(
                new CallInsn(ty_t, pRd, NewLiteral(Qcar), pVy),
                pRef );

            return pRd;
        } // 1
        default:
            return pDfn->GetSx();
        }
    } // values*
    } // switch opcode

    CAN_NOT_HAPPEN();
} // InlinePass::convert_vx_to_sx


//////////////////////////////////////////////////////////////////////
//
// plan_inline
//
void InlinePass::plan_inline(Function* pCallee)
{
    CallInsn* pCall = called_once(pCallee);
    if (NULL != pCall)
    {
        m_oCallSites.Push(pCall);
        return;
    }

    // BUGBUG: NYI: If callee is small, we make copy of callee for each
    // call sites.
} // InlinePass::need_inline


//////////////////////////////////////////////////////////////////////
//
// InlinePass::process
//
void InlinePass::process(CallInsn* pCall)
{
    m_pCall = pCall;

    Function* pCallee = pCall->GetSx()->StaticCast<Function>();
    if (pCallee->GetArityMin() != pCallee->GetArityMax() ||
        pCallee->HasRestParam() )
    {
        return;
    }

    html_log_format(2, L"<h2>Process ~S:~S</h2>~%",
        pCall->GetBBlock(),
        pCall );

    Function* pCaller = pCall->GetBBlock()->GetFunction();

    html_log_format(2, L"&nbsp;in ~S~:%", pCaller);

    process_prologue(pCall, pCallee);
    process_epilogue(pCallee);
    process_body(pCall, pCallee);

    ir_remove_function(pCallee);

    html_log_format(3, L"<h3>Clean ~S</h3>~%", pCaller);
    ir_remove_useless_instructions(pCaller);
} // InlinePass::process


//////////////////////////////////////////////////////////////////////
//
// InlinePass::process_body
//  o Instructions
//  o Variables
//  o Frames
//
void InlinePass::process_body(CallInsn* pCall, Function* pCallee)
{
    html_log_format(3, L"<h3>Rewrite body</h3>~%");

    Function* pCaller = pCall->GetBBlock()->GetFunction();

    {
        Instruction* pRef = pCaller->GetEntryInsn()->GetNext();

        Function::EnumUpVarSite oEnum(pCallee);
        while (! oEnum.AtEnd())
        {
            Instruction* pUpVarDef = oEnum.Get()->GetInstruction();
                oEnum.Next();

            Variable* pVar = pUpVarDef->GetSx()->StaticCast<Variable>();

            Register* pCell = ir_find_variable(pCaller, pVar);

            if (NULL == pCell)
            {
                // Upvar isn't defined by caller.
                pUpVarDef->Unlink_();
                pRef->Link_(pUpVarDef);
            }
            else
            {
                // Owner of this upvar is caller. We don't need to use
                // UPVARDEF instruction.
                ir_replace_all_users(pCell, pUpVarDef->GetRd());
                ir_remove_insn(pUpVarDef);
            }
        } // for each upVar site
    }

    // Variables
    {
        Function::EnumVar oEnum(pCallee);
        while (! oEnum.AtEnd())
        {
            Variable* pVar = oEnum.Get();
                oEnum.Next();
            pCallee->RemoveVar(pVar);
            pCaller->AddVar(pVar);
        } // for each var
    } // for each Variable

    BBlock* pSucc = ir_split_bblock_after(pCall);

    BBlock* pCallBB  = pCall->GetBBlock();
    BBlock* pEntryBB = pCallee->GetEntryBB();
    BBlock* pExitBB  = pCallee->GetExitBB();

    // Frames
    if (! pCallee->m_oFrames.IsEmpty())
    {
        pCallee->m_oFrames.GetHead()->SetOuter(pCall->GetFrame());
        while (! pCallee->m_oFrames.IsEmpty())
        {
            Frame* pFrame = pCallee->m_oFrames.GetHead();
            pFrame->Unlink_();
            pFrame->SetOwner(pCaller);
            pCaller->m_oFrames.Append_(pFrame);
        } // while
    } // frame


    ir_remove_insn(pCall);

    // Move bblocks
    while (pCallee->HasBBlock())
    {
        BBlock* pBBlock = pCallee->GetEntryBB();
        pCallee->Remove(pBBlock);
        pCaller->Insert(pBBlock, pSucc);
    } // for each bblock

    // Update call graph edge
    html_log_format(3, L"<h3>Update Call Graph</h3>~%");
    #if 0
    foreach (Function::EnumInEdge, oEnum, pCallee)
    {
        CgEdge* pEdge = oEnum.Get();
        pEdge->SetTo(pCaller);
    } // for each in-edge
    #endif

    html_log_format(4, L"<ol>~%");
    {
        Function::EnumOutEdge oEnum(pCallee);
        while (! oEnum.AtEnd())
        {
            CgEdge* pEdge = oEnum.Get();
                oEnum.Next();

            html_log_format(4, L"<li>~S -> ~S: ", pCallee, pEdge->GetTo());

            static_cast<Function::EdgeOutAnchor*>(pCallee)->
                Remove_(pEdge);

            Function* pCallee2 = pEdge->GetTo();

            if (NULL != ir_find_cg_edge(pCaller, pCallee2))
            {
                html_log_format(4, L" remove");

                static_cast<Function::EdgeInAnchor*>(pCallee2)->
                    Remove_(pEdge);
            }
            else
            {
                static_cast<Function::EdgeOutAnchor*>(pCaller)->
                    Append_(pEdge);

                pEdge->SetFrom(pCaller);

                html_log_format(4, L" move");
            } // if

            html_log_format(4, L"</li>~%");
        } // for each out-edge
    }
    html_log_format(4, L"</ol>~%");

    ir_remove_insn(pEntryBB->GetFirstInsn());
    pCallBB->AppendInsn(new JumpInsn(pEntryBB));
    pExitBB->AppendInsn(new JumpInsn(pSucc));
} // InlinePass::process_body


//////////////////////////////////////////////////////////////////////
//
// InlinePass::process_epilogue
//
//  Description:
//   o Rewrite EXIT to PHI
//   o Rewrite RET to PHI operand
//   o Remove CFG edge For UNREACHABLE
//
void InlinePass::process_epilogue(Function* pCallee)
{
    BBlock* pExitBB = pCallee->GetExitBB();

    PhiInsn* pPhi;
    if (ty_void == m_pCall->GetTy())
    {
        pPhi = NULL;
    }
    else
    {
        pPhi = new PhiInsn(m_pCall->GetTy(), m_pCall->GetOutput());
    } // if

    RemoveCfgEdge(pCallee->GetEntryBB(), pExitBB);

    {
        BBlock::EnumInEdge oEnum(pExitBB);
        while (! oEnum.AtEnd())
        {
            CfgEdge* pEdge = oEnum.Get();
            BBlock* pPred  = pEdge->GetFrom();
                oEnum.Next();

            Instruction* pInsn = pPred->GetLastInsn();

            switch (pInsn->GetOpcode())
            {
            case IrOp_RET:
            {
                html_log_format(3, L"<h4>Rewrite ~S: ~S</h4>~%",
                    pInsn->GetBBlock(),
                    pInsn );

                if (NULL != pPhi)
                {
                    Operand* pSx = pInsn->GetSx();

                    if (NULL != pPhi->GetRd() && pSx->Is<Values>())
                    {
                        pSx = convert_vx_to_sx(pSx->StaticCast<Values>());
                    }

                    pPhi->AddInput(pPred, pSx);
                }
                ir_replace_insn(new JumpInsn(pExitBB), pInsn);
                break;
            } // RET

            case IrOp_JUMP:
                break;

            case IrOp_UNREACHABLE:
                process_UNREACHABLE(pInsn);
                if (pPred->GetLastInsn()->Is<UnreachableInsn>())
                {
                    ir_replace_cfg_edge_to(
                        pEdge,
                        m_pCall->GetBBlock()->GetFunction()->GetExitBB() );
                }
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch opcode
        } // while
    }

    if (NULL != pPhi)
    {
        ir_insert_insn(pPhi, pExitBB->GetFirstInsn());
    }

    ir_remove_insn(pExitBB->GetLastInsn());
} // InlinePass::process_epilogue


//////////////////////////////////////////////////////////////////////
//
// InlinePass::process_prologue
//
//  Description:
//   o Binds parameter registers to corresponding argument registers->
//   o PROJECT with VALUES
//   o PROJECT without VALUES
//
void InlinePass::process_prologue(CallInsn* pCall, Function* pCallee)
{
    Instruction* pPrologue = pCallee->GetPrologueInsn();

    if (NULL == pPrologue->GetVd())
    {
        // There is no parameter->
    }
    else
    {
        ir_replace_all_users(pCall->GetVy(), pPrologue->GetVd());
    }

    ir_remove_insn(pPrologue);
} // InlinePass::process_prologue

namespace
{

// insert_CLOSE
static void insert_CLOSE(Frame* pFrame, Instruction* pRef)
{
    Instruction* pOpen = pFrame->GetDfn();

    if (NULL == pOpen)
    {
        // The frame is removed.
        return;
    }

    ir_insert_insn(new CloseInsn(pFrame), pRef);

    // For finally frame, we call cleanup function.
    if (pOpen->Is<OpenFinallyInsn>())
    {
        Function* pFinally = pOpen->GetSx()->StaticCast<Function>();

        Values* pVx = new Values();
        ir_insert_insn(new ValuesInsn(pVx), pRef);
        ir_insert_insn(
            new CallInsn(pFinally, pVx),
            pRef );
    } // OpenFinally
} // insert_CLOSE


// Unwind callee frames
static void unwind_callee_frames(Instruction* pRef)
{
    for (;;)
    {
        Instruction* pInsn = pRef->GetNext();

        if (pInsn->Is<UseInsn>())
        {
            ASSERT(pInsn->Is<UseInsn>());

            Frame* pFrame = pInsn->GetSx()->DynamicCast<Frame>();
            if (NULL != pFrame)
            {
                insert_CLOSE(pFrame, pRef);
            }

            ir_remove_insn(pInsn);
        }
        else
        {
            ir_remove_insn(pInsn);
            break;
        }
    } // for each insn
} // unwind_calee_frames

} // namespace

//////////////////////////////////////////////////////////////////////
//
// InlinePass::process_GO
//  In callee:
//  (UPVARDEF CLOSED-CELL %r16 <= [var #:EXIT])
//  ...
//  (SLOT (PTR T) %r17 <= 'CLOSED-CELL 'VALUE %r16)
//  (LOAD T %r18 <= %r17)
//  (GO %r18)
//  (UNREACHABLE)
//
//  In caller:
//  (OPENTAGBODY T [TAGOBDY-Frame 966908] <=)
//  (TAG T %r14 <= [TAGBODY-Frame 966908] BB10)
//  (VARDEF CLOSED-CELL %r15 <= [var #:EXIT] %r14)
//
void InlinePass::process_GO(Instruction* pGo)
{
    ASSERT(NULL != pGo);

    Variable* pVar = pGo->GetRx()->
        GetDfn()->GetRx()->     // LOAD
            GetDfn()->GetRz()-> // SLOT
                GetDfn()->GetSx()->StaticCast<Variable>();

    Function* pCaller = getCaller();

    if (pVar->GetOwner() != pCaller) return;

    html_log_format(3, L"<h4>Rewrite ~S: ~S</h4>~%",
        pGo->GetBBlock(),
        pGo );

    unwind_callee_frames(pGo);

    Frame* pTagbodyFrame =
        pVar->GetDfn()->GetRy()->   // VARDEF
            GetDfn()->GetSx()->     // TAG
                StaticCast<Frame>();

    // Unwind caller's frame
    {
        foreach (Frame::Enum, oEnum, m_pCall->GetFrame())
        {
            Frame* pFrame = oEnum.Get();
            if (pFrame == pTagbodyFrame) break;
            if (pFrame->GetOwner() != pCaller) continue;

            insert_CLOSE(pFrame, pGo);
        } // for each frame
    }

    BBlock* pNonlocalXp =
        pVar->GetDfn()->GetRy()->   // VARDEF
            GetDfn()->GetSy()->     // TAG
                StaticCast<Label>()->GetBBlock();

    ir_replace_insn(new JumpInsn(pNonlocalXp), pGo);
} // InlinePass::process_GO


//////////////////////////////////////////////////////////////////////
//
// Returns local exit point bblock
//
static BBlock* getLocalXp(NonlocalInsn* pNonlocal)
{
    {
        JumpInsn* pJump = pNonlocal->GetNext()->DynamicCast<JumpInsn>();
        if (NULL != pJump)
        {
            return pJump->GetSx()->StaticCast<Label>()->GetBBlock();
        }
    }

    BBlock* pLocalXp = ir_split_bblock_after(pNonlocal);
    BBlock* pNonlocalXp = pNonlocal->GetBBlock();

    pNonlocalXp->AppendInsn(new JumpInsn(pLocalXp));

    Output* pNew = NewOutput(pNonlocal->GetTy());

    PhiInsn* pPhi = new PhiInsn(pNonlocal->GetTy(), pNew);

    ir_insert_insn(pPhi, pLocalXp->GetFirstInsn());

    ir_replace_all_users(pNew, pNonlocal->GetOutput());

    pPhi->AddInput(pNonlocalXp, pNonlocal->GetOutput());

    return pLocalXp;
} // getLocalXp


//////////////////////////////////////////////////////////////////////
//
// InlinePass::process_RETURNFROM
//
// In callee:
//  (UPVARDEF CLOSED-CELL %r17 <= [var #:FOO])
//  ...
//  (SLOT (PTR T) %r18 <= 'CLOSED-CELL 'VALUE %r17)
//  (LOAD T %r19 <= %r18)
//  (RETURNFROM %r19 %v16)
//  (UNREACHABLE)
//
// In caller:
//  (OPENBLOCK T [BLOCK-Frame FOO 966908] <= 'FOO BB7)
//  (ENCODE FIXNUM %r14 <= [BLOCK-Frame FOO 966908])
//  (VARDEF CLOSED-CELL %r15 <= [var #:FOO] %r14)
//
void InlinePass::process_RETURNFROM(Instruction* pReturnFrom)
{
    ASSERT(NULL != pReturnFrom);

    Variable* pVar = pReturnFrom->GetRx()->
        GetDfn()->GetRx()->     // LOAD
            GetDfn()->GetRz()-> // SLOT
                GetDfn()->GetSx()->StaticCast<Variable>();

    Function* pCaller = getCaller();

    if (pVar->GetOwner() != pCaller) return;

    html_log_format(3, L"<h4>Rewrite ~S: ~S</h4>~%",
        pReturnFrom->GetBBlock(),
        pReturnFrom );

    unwind_callee_frames(pReturnFrom);

    Frame* pBlockFrame =
        pVar->GetDfn()->GetRy()->   // VARDEF
            GetDfn()->GetSx()->     // Encode
                StaticCast<Frame>();

    // Unwind caller's frame
    {
        foreach (Frame::Enum, oEnum, m_pCall->GetFrame())
        {
            Frame* pFrame = oEnum.Get();
            if (pFrame == pBlockFrame) break;
            if (pFrame->GetOwner() != pCaller) continue;

            insert_CLOSE(pFrame, pReturnFrom);
        } // for each frame
    }

    BBlock* pNonlocalXp = pBlockFrame->GetDfn()->  // OPENBLOCK
        GetSy()->StaticCast<Label>()->GetBBlock();

    NonlocalInsn* pNonlocal = pNonlocalXp->GetFirstInsn()->
        DynamicCast<NonlocalInsn>();

    if (NULL == pNonlocal)
    {
        // Nonlocal Exit Point is merged or return-from doesn't
        // retrun usefule value.
        ir_replace_insn(new JumpInsn(pNonlocalXp), pReturnFrom);
    }
    else
    {
        BBlock* pLocalXp = getLocalXp(pNonlocal);

        // Update PHI
        {
            PhiInsn* pPhi = pLocalXp->GetFirstInsn()->DynamicCast<PhiInsn>();
            if (NULL != pPhi)
            {
                pPhi->AddInput(pReturnFrom->GetBBlock(), pReturnFrom->GetVy());
            }
        }

        ir_replace_insn(new JumpInsn(pLocalXp), pReturnFrom);
    }
} // InlinePass::process_RETURNFROM


//////////////////////////////////////////////////////////////////////
//
// InlinePass::process_UNREACHABLE
//
void InlinePass::process_UNREACHABLE(Instruction* pUnreachable)
{
    Instruction* pInsn = pUnreachable;
    for (;;)
    {
        pInsn = pInsn->GetPrev();
        switch (pInsn->GetOpcode())
        {
        case IrOp_CALL:
        {
            Function* pCaller = getCaller();
            CallInsn* pCall = pInsn->StaticCast<CallInsn>();
            foreach (Frame::Enum, oEnum, pCall->GetFrame())
            {
                Frame* pFrame = oEnum.Get();
                if (pFrame->GetOwner() != pCaller) break;
                ir_insert_insn(new UseInsn(pFrame), pUnreachable);
            } // for each frame
            return;
        } // call

        case IrOp_GO:
            process_GO(pInsn);
            return;

        case IrOp_RETURNFROM:
            process_RETURNFROM(pInsn);
            return;

        case IrOp_THROW:
            return;

        case IrOp_USE:
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch opcode
    } // for each insn
} // InlinePass::process_UNREACHABLE

} // namespace

//////////////////////////////////////////////////////////////////////
//
// optimize_inline
//
void
optimize_inline()
{
    // During debugging, we alway do inline->
#if 0
    OptimizeQualities* pQualities = Session::Get()->m_oOptimizeQualities;
    if (pQualities->GetDebug() > pQualities->GetSpeed())
    {
        // Use wants debug program
    }
    else if (pQualities->GetCompilationSpeed() > pQualities->GetSpeed())
    {
        // Maybe called by eval
    }
    else
#endif
    {
        InlinePass oPass;
        oPass.Run();
    }
} // optimize_inline

} // Compiler
