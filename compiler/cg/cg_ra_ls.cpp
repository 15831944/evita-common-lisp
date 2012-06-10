#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - Linear Scan Register Allocator
// cg/cg_ra_ls.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_ra_ls.cpp#24 $
//
#include "./cg_defs.h"

#include "./cg_ra_ls.h"
#include "./cg_target.h"
#include "./cg_instruction.h"

#include "../cm/cm_bitvec.h"
#include "../cm/cm_session.h"

#include "../ir/ir_dfa.h"
#include "../ir/ir_function.h"
#include "../ir/ir_pass.h"

namespace Compiler
{

using namespace LinearScanRA;

const RegSet RegisterAllocator::k_oEmptyRegSet;

// isCopyI
static bool isCopyI(Instruction* pInsn)
{
    switch (pInsn->GetOpcode())
    {
    case IrOp_COPY:
    case IrOp_PHICOPY:
    case IrOp_RELOAD:
    case IrOp_SPILL:
    case IrOp_SPLIT:
        return true;
    default:
        return false;
    } // switch opcode
} // isCopyI


// fold_copy static
Register* fold_copy(Instruction* pInsn, Register* pRx)
{
    if (pInsn->GetBBlock()->GetFirstInsn() == pInsn) return pRx;

    Instruction* pPrev = pInsn->GetPrev();

    if (isCopyI(pPrev) &&
        NULL != pPrev->GetRx() &&
        pPrev->GetRd()->Equal(pRx) )
    {
        // copy %r0 <- %r1
        // some <- ... %r0 ... ==> some <- ... %r1 ...
        if (pPrev->GetRx()->IsPhysical())
        {
            html_log_format(3, L"use ~S:~S~:%",
                pPrev->GetBBlock(),
                pPrev );

            pRx = pPrev->GetRx();
        }
    } // if

    return pRx;
} // fold_copy

// get_bblock
static BBlock* get_bblock_at(Function* pFun, uint nPosn)
{
    nPosn = (nPosn / Posn_Step) * Posn_Step;

    foreach (Function::EnumBBlock_Postorder, oEnum, pFun)
    {
        BBlock* pCurr = oEnum.Get();
        if (nPosn >= pCurr->GetFirstInsn()->GetIndex() &&
            nPosn <= pCurr->GetLastInsn()->GetIndex() )
        {
            return pCurr;
        }
    } // for each bblock
    return pFun->GetExitBB();
} // get_bblock_at


// html_dump_interval
static void html_dump_interval(Val s, Function* pFun, LiveInterval* pIntv)
{
    html_format(s, L"<tr><td>~S [~S, ~S]</td>~%",
        pIntv,
        get_bblock_at(pFun, pIntv->GetStart()),
        get_bblock_at(pFun, pIntv->GetEnd()) );

    html_format(s, L"<td>");
    foreach (LiveRangeList::Enum, oEnum, pIntv)
    {
        html_format(s, L" ~S", oEnum.Get());
    } // for each range
    html_format(s, L"</td>");

    html_format(s, L"<td>");
    foreach (UsePosnList::Enum, oEnum, pIntv)
    {
        html_format(s, L" ~S", oEnum.Get());
    } // for each range
    html_format(s, L"</td>");


    html_format(s, L"</tr>~%");
} // html_dump_interval

// html_dump_intervals
static void html_dump_intervals(Val s, Function* pFun)
{
    if (! streamp(s)) return;

    html_format(s, L"<h3>Live Interval by Parent of ~S</h3>~%", pFun);
    html_format(s, L"<table border=1>~%");
    html_format(s, L"<tr><th>Parent</th><th>Children</th></tr>~%");

    foreach (Function::EnumBBlock_Reverse_Postorder, oEnum, pFun)
    {
        BBlock* pCurr = oEnum.Get();
        foreach (BBlock::EnumInsn, oEnum, pCurr)
        {
            Instruction* pInsn = oEnum.Get();
                if (pInsn->Is<ReloadInsn>()) continue;
                if (pInsn->Is<SpillInsn>()) continue;
                if (pInsn->Is<SplitInsn>()) continue;

            Register* pRd = pInsn->GetRd();
                if (NULL == pRd) continue;
                if (pRd->GetDfn() != pInsn) continue;

            LiveInterval* pIntv = pRd->GetExtension<LiveInterval>();
                if (NULL == pIntv) continue;
                if (pIntv->IsFixed()) continue;
                if (pIntv->GetParent() != pIntv) continue;

            html_format(s, L"<tr>");

            if (NULL != pRd->GetVar())
            {
                html_format(s, L"<td>~S</td>", pRd->GetVar());
            }
            else
            {
                html_format(s, L"<td></td>");
            }

            html_format(s, L"<td><table border=1 cellpadding=2>~%");

            html_dump_interval(s, pFun, pIntv);

            foreach (LiveInterval::EnumSibling, oEnum, pIntv)
            {
                html_dump_interval(s, pFun, oEnum.Get());
            } // for each sibling

            html_format(s, L"</table></td></tr>~%");
        } // for each insn
    } // for each bblock

    html_format(s, L"</table>~%");
} // html_dump_intervals


// ra_insert_insn
//  Set index of instruction for LiveInterval::FindAt
static void ra_insert_insn(Instruction* pInsn, Instruction* pRefInsn)
{
    ir_insert_insn(pInsn, pRefInsn);
    pInsn->SetIndex(pRefInsn->GetIndex());
} // ra_insert_insn

//////////////////////////////////////////////////////////////////////
//
// AssignPass
//
class AssignPass : public SubPass
{
    private: typedef RegisterAllocator RA;

    // Entry Point
    public: static void Run(RA* pPass, Function* pFun)
    {
        AssignPass oSubPass(pPass, pFun);
            oSubPass.m_pPass = pPass;

        html_dump_intervals(pPass->GetLogStream(1), pFun);

        oSubPass.run(pFun);
    } // Run

    private: RA* m_pPass;

    // ctor
    private: AssignPass(RA* pPass, Function* pFun) :
        SubPass(pPass, L"ASSIGN", pFun),
        m_pPass(pPass) {}

    // [A]
    void assign(Function* pFun)
    {
        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* pBBlock = oEnum.Get();
            foreach (BBlock::EnumInsn, oEnum, pBBlock)
            {
                Register* pRd = oEnum.Get()->GetRd();
                    if (NULL == pRd) continue;

                LiveInterval* pLeader = pRd->GetExtension<LiveInterval>();
                    if (NULL == pLeader) continue;

                if (pLeader->GetParent() != pLeader) continue;

                assign1(pLeader);

                foreach (LiveInterval::EnumSibling, oEnum, pLeader)
                {
                    assign1(oEnum.Get());
                } // for each sibling
            } // for each insn
        } // for each bblock
    } // assign

    void assign1(LiveInterval* pIntv)
    {
        switch (pIntv->m_eStorage)
        {
        case Register::Storage_Stack:
            pIntv->SetReg(pIntv->GetSpill());
            break;

        case Register::Storage_Physical:
            pIntv->GetReg()->SetStorage(
                Register::Storage_Physical,
                pIntv->m_iStorage );
            break;

        default:
            warn(L"Can't assign register to interval for %r~D.",
                pIntv->GetReg()->GetName() );
        } // switch storage
    } // assign

    // [F]
    private: LiveInterval* findIntv(
        Register*       pR1,
        Instruction*    pI,
        uint            nPosn )
    {
        LiveInterval* pParent = pR1->GetExtension<LiveInterval>();
        if (NULL == pParent) return NULL;

        ASSERT(! pParent->IsFixed());

        unless (pParent->GetParent() == pParent)
        {
            html_log_format(1, L"<i>Broken interval ~S in ~S~:%",
                pR1,
                pI );
            warn(L"Broken interval for %r~D", pR1->GetName());
            return NULL;
        }

        LiveInterval* pIntv = pParent->FindAt(nPosn);

        if (NULL == pIntv)
        {
            html_log_format(3,
                L"<i>No interval found for ~S at ~D ~S</i>~:%",
                pR1, nPosn, pI );

            warn(L"No interval found for %r~D at ~D in BB~D.",
                pR1->GetName(),
                Fixnum::Encode(nPosn),
                pI->GetBBlock()->GetName() );
        }

        return pIntv;
    } // findIntv

    // [R]
    private: void rewriteInput(
        Instruction*    pI,
        OperandBox*     pBox,
        uint            nNth,
        uint            nFrom )
    {
        Register* pR1 = pBox->GetRx();
        if (NULL == pR1) return;
        //if (! pR1->IsVirtual()) return;

        uint nPosn = pI->GetIndex() + nFrom;
        LiveInterval* pIntv = findIntv(pR1, pI, nPosn);
        if (NULL == pIntv) return;

        Register* pR2 = pIntv->GetReg();

        if (! pI->Is<UseInsn>())
        {
            Register* pRspill = pIntv->GetSpill();
            if (NULL != pRspill && pRspill != pR2)
            {
                ASSERT(pR2->IsPhysical());

                Instruction* pRefInsn = pI;
                #if 1
                for (;;)
                {
                    if (pRefInsn->GetBBlock()->GetFirstInsn() == pRefInsn)
                    {
                        break;
                    }

                    Instruction* pPrev = pRefInsn->GetPrev();

                    if (pPrev->GetIndex() != pRefInsn->GetIndex())
                    {
                        break;
                    }

                    if (pPrev->Is<SplitInsn>())
                    {
                        break;
                    }

                    pRefInsn = pPrev;
                } // for
                #endif

                if (pI->GetBBlock()->GetFirstInsn() != pI &&
                    pI->GetPrev()->Is<SpillInsn>() &&
                    pI->GetPrev()->GetRd() == pRspill &&
                    pI->GetPrev()->GetRx() == pR2 )
                {
                    // fold SPILL+RELOAD
                    html_log_format(3, L"fold relod ~S:~D: ~S~:%",
                        pI->GetBBlock(),
                        pI->GetIndex(),
                        pI->GetPrev() );
                }
                else if (! m_pPass->NeedPhysical(pI, nNth))
                {
                    pR2 = pRspill;
                }
                else
                {
                    html_log_format(3, L"reload ~S:~D: ~S~:%",
                        pI->GetBBlock(),
                        pI->GetIndex(),
                        pI );
                    ra_insert_insn(new ReloadInsn(pR2, pRspill), pRefInsn);
                } // if
            } // if
        }

        if (pR2 != pR1)
        {
            html_log_format(3,
                L"<b>change input</b> ~S:~D:~S: ~S => ~S~:%",
                pI->GetBBlock(),
                pI->GetIndex(),
                pI,
                pR1,
                pR2 );

            pBox->Replace(pR2);
        }
    } // rewriteInput

    // rewriteOutput
    void rewriteOutput(Instruction* pI)
    {
        Register* pR1 = pI->GetRd();
        if (NULL == pR1) return;

        uint nPosn = pI->GetIndex() + Posn_Output;
        LiveInterval* pIntv = findIntv(pR1, pI, nPosn);
        if (NULL == pIntv) return;

        Register* pRspill = pIntv->GetSpill();

    #if 0
        // Note: Output fuse increase code size.
        //  1,492,144 -> 1492,384
        if (NULL != pRspill && ! m_pPass->NeedPhysicalOutput(pI))
        {
            pI->SetOutput(pRspill);
            return;
        }
    #endif

        Register* pR2 = pIntv->GetReg();

        if (pR2 != pR1)
        {
            html_log_format(3,
                L"<font color='red'>change output</font> of ~S:~D:~S to ~S~:%",
                pI->GetBBlock(),
                pI->GetIndex(),
                pI,
                pR2 );

            if (NULL != pR2->GetDfn()) pR2->MarkNotSSA();

            pI->SetOutput(pR2);

            // Update Use-Def Link.
            Register::Enum oEnum(pR1);
            while (! oEnum.AtEnd())
            {
                OperandBox* pBox = oEnum.Get();
                    oEnum.Next();

                if (pBox->GetInstruction()->GetIndex() >= nPosn)
                {
                    pR1->Remove_(pBox);
                    pR2->Append_(pBox);
                }
            } // for each use site of pR1
        } // if

        if (NULL != pRspill && pRspill != pR2)
        {
            ASSERT(pR2->IsPhysical());

            if (NULL != pRspill->GetDfn()) pRspill->MarkNotSSA();

            html_log_format(3, L"spill ~S:~D: ~S~:%",
                pI->GetBBlock(),
                pI->GetIndex(),
                pI );

            Instruction* pSpill = ir_insert_insn(
                new SpillInsn(pI->GetTy(), pRspill, pR2),
                pI->GetNext() );

            pSpill->SetIndex(pI->GetIndex());
        }
    } // rewriteOutput

    void run(Function* pFun)
    {
        assign(pFun);

        foreach (Function::EnumBBlock_Reverse_Postorder, oEnumBB, pFun)
        {
            BBlock* pBBlock = oEnumBB.Get();

            BBlock::EnumInsn  oEnum(pBBlock);
            while (! oEnum.AtEnd())
            {
                Instruction* pI = oEnum.Get();
                    oEnum.Next();

                switch (pI->GetOpcode())
                {
                case IrOp_SPLIT:
                {
                    LiveInterval* pIntv = pI->GetRd()->
                        GetExtension<LiveInterval>();

                    if (pIntv->IsSpilled())
                    {
                        ir_remove_insn(pI);
                        continue;
                    }

                    rewriteInput(
                        pI,
                        pI->GetOperandBox(0),
                        0,
                        Posn_Split - 1 );
                    break;
                } // SPLIT

                default:
                {
                    rewriteOutput(pI);
                    uint nNth = 0;
                    foreach (Instruction::EnumInput, oEnum, pI)
                    {
                        rewriteInput(pI, oEnum.GetBox(), nNth, Posn_Input);
                        nNth += 1;
                    } // for each input
                } // default
                } // switch opcode

                if (isCopyI(pI))
                {
                    if (pI->GetRd() == pI->GetRx())
                    {
                        ir_remove_insn(pI);
                    }
                    else if (pBBlock->GetFirstInsn() != pI)
                    {
                        Instruction* pPrev = pI->GetPrev();
                        if (isCopyI(pPrev) &&
                            pPrev->GetRd() == pI->GetRx() &&
                            pPrev->GetRx() == pI->GetRd() )
                        {
                            // copy %r1 <= %r2
                            // copy %r2 <= %r1  === useless
                            ir_remove_insn(pI);
                        }
                    }
                } // is_copy

            } // for each instruction
        } // for each bblock
    } // run
}; // AssignPass

//////////////////////////////////////////////////////////////////////
//
// Live Interval BuildPass
//
// Note: We assume all registers have definition instruction and at
// least one use instruction. See set_start method.
//
// Example:
//          BB.first         BB.last
//      v1: >----------------<
//      v2:
//
//      v1: >-------------U--<
//      v2: >-----------<               used
//
//      v1: >-------------U--<
//      v2:        >----<               defined
//
//
class BuildPass : public SubPass
{
    // Entry Point
    public: static bool Run(RegisterAllocator* pPass, Function* pFun)
    {
        BuildPass oBuildPass(pPass, pFun);
            bool fHasFPR = oBuildPass.run(pFun);

        html_dump_intervals(pPass->GetLogStream(1), pFun);

        return fHasFPR;
    } // Run

    private: WorkList_<Register>    m_oRegs;
    private: RegisterAllocator*     m_pPass;

    private: BuildPass(RegisterAllocator* pPass, Function* pFun) :
        SubPass(pPass, L"BUILD", pFun),
        m_pPass(pPass) {}

    // run - main loop of building live interval
    bool run(Function* pFun)
    {
        bool fHasFPR = prepare(pFun);
        build(pFun);
        return fHasFPR;
    } // Build

    // build
    void build(Function* pFun)
    {
        foreach (Function::EnumBBlock_Postorder, oEnum, pFun)
        {
            DataFlowBB* pCurr = oEnum.Get()->Extend<DataFlowBB>();

            uint nStart = pCurr->GetFirstInsn()->GetIndex() +
                Posn_LiveIn;

            // Add ranges for LiveOut registers. We assume LiveOut registers
            // are live thorugh or defined in another block.
            html_log_format(2, L"LiveOut(~S)={", pCurr);
            {
                LPCWSTR pwszFormat = L"~S";

                uint nPosn = pCurr->GetLastInsn()->GetIndex() +
                    Posn_LiveOutEnd;

                const BitVec* pLiveOut = pCurr->GetOut();

                foreach (WorkList_<Register>::Enum, oEnum, &m_oRegs)
                {
                    Register* pRx = oEnum.Get();

                    // BUGBUG: We should have mapping from index to
                    // register for speed.
                    if (pLiveOut->IsOne(pRx->GetIndex()))
                    {
                        html_log_format(2, pwszFormat, pRx);
                        pwszFormat = L" ~S";

                        addRange(pRx, nStart, nPosn);
                    }
                } // for each reg
            }
            log_format(2, L"}<br/>~%");

            foreach (BBlock::EnumInsn_Reverse, oEnum, pCurr)
            {
                Instruction* pInsn = oEnum.Get();

                uint nPosn = pInsn->GetIndex();

                {
                    Register* pRd = pInsn->GetRd();
                    if (NULL != pRd && pRd->IsVirtual())
                    {
                        setStart(pRd, nPosn + Posn_Output);

                        addUsePosn(
                            pRd,
                            nPosn + Posn_Output,
                            m_pPass->NeedPhysicalOutput(pInsn) );
                    } // if
                }

                uint nNth = 0;
                foreach (Instruction::EnumInput, oEnum, pInsn)
                {
                    Register* pRx = oEnum.GetReg();

                    if (NULL != pRx)
                    {
                        if (pRx->IsPseudo())
                        {
                            pRx = m_pPass->MapPseudo(pInsn, pRx);
                        }

                        if (NULL != pRx && pRx->IsVirtual())
                        {
                            addRange(
                                pRx,
                                nStart,
                                nPosn + Posn_InputEnd );

                            addUsePosn(
                                pRx,
                                nPosn + Posn_Input,
                                m_pPass->NeedPhysical(pInsn, nNth) );
                        } // if
                    } // if
                    nNth += 1;
                } // for each input
            } // for each instruction
        } // for each bblock
    } // build

    // addRange
    void addRange(Register* pRx, uint nStart, uint nEnd)
    {
        ASSERT(NULL != pRx && pRx->IsVirtual());
        ASSERT(nStart < nEnd);

        LiveInterval* pIntv = pRx->GetExtension<LiveInterval>();
        pIntv->AddRange(nStart, nEnd);
    } // addRange

    // addUsePosn
    void addUsePosn(Register* pRx, uint nPosn, bool fPhysical)
    {
        LiveInterval* pIntv = pRx->GetExtension<LiveInterval>();
        ASSERT(pIntv->GetReg() == pRx);
        pIntv->AddUsePosn(nPosn, fPhysical);
    } // addUsePosn

    // prepare
    bool prepare(Function* pFun)
    {
        bool fHasFPR = false;

        foreach (Function::EnumReg, oEnum, pFun)
        {
            Register* pRx = oEnum.Get();

            if (pRx->GetClass() == Register::Class_FPR)
            {
                fHasFPR = true;
            }

            switch (pRx->GetStorage())
            {
            case Register::Storage_Virtual:
                new LiveInterval(pRx);
                m_oRegs.Push(pRx);
                break;

            case Register::Storage_Stack:
                m_pPass->AddSpill(pRx->GetTy(), pRx);
                break;
            } // switch storage
        } // for each register

        return fHasFPR;
    } // prepare

    // setStart
    void setStart(Register* pRd, uint nStart)
    {
        ASSERT(NULL != pRd && pRd->IsVirtual());

        LiveInterval* pIntv = pRd->GetExtension<LiveInterval>();
        ASSERT(pIntv->HasRange());
            ASSERT(pIntv->GetFirstRange()->GetEnd() >= nStart);

        pIntv->GetFirstRange()->SetStart(nStart);
    } // setStart
}; // BuildPass

//////////////////////////////////////////////////////////////////////
//
// BuildFixedPass
//  Build fixed live intervals.
//
class BuildFixedPass : public SubPass
{
    RegisterAllocator* m_pPass;

    public: static void Run(RegisterAllocator* pPass, Function* pFun)
    {
        BuildFixedPass oPass(pPass, pFun);
            oPass.run(pFun);
    } // Run

    BuildFixedPass(RegisterAllocator* pPass, Function* pFun) :
        SubPass(pPass, L"FIXED", pFun),
        m_pPass(pPass) {}


    void html_dump_intervals(
        Val             s,
        Function*       pFun,
        const RegSet*   pRegSet,
        const char16*   pwsz )
    {
        html_format(s, L"<h2>~A Intervals</h2>", pwsz);
        html_format(s, L"<table border='1'>~%");
        html_format(s, L"<tr>");
        html_format(s, L"  <th>Interval</th>");
        html_format(s, L"  <th>Range</th>");
        html_format(s, L"  <th>Use Point</th>");
        html_format(s, L"</tr>");

        foreach (RegSet::Enum, oEnum, pRegSet)
        {
            int rx = oEnum.Get();
            LiveInterval* pIntv = m_pPass->GetFixed(rx);
            if (pIntv->HasRange())
            {
                html_dump_interval(s, pFun, pIntv);
            }
        } // for each reg

        html_format(s, L"</table>~%");
    } // html_dump_interval

    // prepare
    void prepare()
    {
        foreach (RegSet::Enum, oEnum, m_pPass->m_pMach->m_pGprAll)
            { prepare1(Register::Class_GPR, oEnum.Get()); }

        foreach (RegSet::Enum, oEnum, m_pPass->m_pMach->m_pFprAll)
            { prepare1(Register::Class_FPR, oEnum.Get()); }
    } // prepare

    void prepare1(Register::Class eClass, int rx)
    {
        LiveInterval* pIntv = new LiveInterval(rx);
        Register* pRx = new Physical(eClass, rx);
        pRx->MarkNotSSA();
        pIntv->SetReg(pRx);
        m_pPass->SetFixed(rx, pIntv);
    } // prepare1

    // run
    void run(Function* pFun)
    {
        prepare();
        m_pPass->BuildFixedInterval(pFun);

        Val s = m_pPass->GetLogStream(1);
        if (nil != s)
        {
            html_dump_intervals(s, pFun, m_pPass->m_pMach->m_pGprAll, L"GPR");
            html_dump_intervals(s, pFun, m_pPass->m_pMach->m_pFprAll, L"FPR");
        }
    } // run
}; // BuildFixedPass

//////////////////////////////////////////////////////////////////////
//
// CheckPass
//
class CheckPass : public SubPass
{
    RegisterAllocator* m_pPass;

    private: CheckPass(RegisterAllocator* pPass, Function* pFun) :
        SubPass(pPass, L"*Check", pFun), m_pPass(pPass) {}

    // Entry Point
    public: static void Run(RegisterAllocator* pPass, Function* pFun)
    {
        if (! Session::Get()->IsVerify()) return;

        CheckPass oSubPass(pPass, pFun);
        oSubPass.run(pFun);
    } // Run

    // [C]
    void check1(
        const char16*   pwszLive,
        BBlock*         pCurr,
        const BitVec*   pLive,
        Register*       pR0,
        uint            nPosn )
    {
        if (pR0->GetRep() != pR0) return;
        if (! pR0->HasIndex()) return;

        LiveInterval* pParent = pR0->GetExtension<LiveInterval>();
            if (NULL == pParent) return;

        if (! pLive->IsOne(pR0->GetIndex())) return;

        LiveInterval* pIntv = pParent->GetParent()->FindAt(nPosn);
            if (NULL == pIntv)
            {
                warn(L"RA-LS: ~A: %r~D doesn't have interval at ~D.",
                    make_string(pwszLive),
                    pR0->GetName(),
                    Fixnum::Encode(nPosn) );
                return;
            }

        Register* pR1 = pIntv->GetReg();
        if (! pR1->IsPhysical()) return;

        int iReg = pR1->GetLocation();
        int nPresent = m_pPass->GetPosn(iReg);

        if (0 == nPresent)
        {
            m_pPass->SetPosn(iReg, pR1->GetNum());
        }
        else if (nPresent != pR1->GetNum())
        {
            html_log_format(1,
                L"~A(~S): ~S conflicts %r~D~:%",
                pwszLive,
                pCurr,
                pR1,
                nPresent );

            warn(L"RA-LS: ~A: %r~D conflicts at BB~D.",
                make_string(pwszLive),
                pR1->GetName(),
                pCurr->GetName() );
        }
    } // check1

    // check
    void check(
        const char16*   pwszLive,
        BBlock*         pCurr,
        const BitVec*   pLive,
        uint            nPosn )
    {
        foreach (RegSet::Enum, oEnum, m_pPass->m_pMach->m_pRegAll)
        {
            m_pPass->SetPosn(oEnum.Get(), 0);
        } // for each reg

        foreach (Function::EnumReg, oEnum, m_pFun)
        {
            check1(pwszLive, pCurr, pLive, oEnum.Get(), nPosn);
        } // for each reg

        foreach (RegSet::Enum, oEnum, m_pPass->m_pMach->m_pRegAll)
        {
            m_pPass->SetPosn(oEnum.Get(), 0);
        } // for each reg
    } // check

    // run
    void run(Function* pFun)
    {
        uint nPosn = 0;
        foreach (Function::EnumBBlock_Reverse_Postorder, oEnum, pFun)
        {
            DataFlowBB* pCurr = oEnum.Get()->Extend<DataFlowBB>();

            nPosn += Posn_Step;

            check(
                L"LiveIn",
                pCurr,
                pCurr->GetIn(),
                nPosn + Posn_LiveIn );

            nPosn = pCurr->GetLastInsn()->GetIndex();

            check(
                L"LiveOut",
                pCurr,
                pCurr->GetOut(),
                nPosn + Posn_LiveOut );
        } // for each bblock
    } // run
}; // CheckPass

//////////////////////////////////////////////////////////////////////
//
// CleanPass
//
class CleanPass : public SubPass
{
    // Entry Point
    public: static void Run(RegisterAllocator* pPass, Function* pFun)
    {
        CleanPass oSubPass(pPass, pFun);
            oSubPass.run(pFun);

        CheckPass::Run(pPass, pFun);
    } // Run

    private: WorkList_<Register>    m_oSpilledRegs;
    private: RegisterAllocator*     m_pPass;

    private: CleanPass(RegisterAllocator* pPass, Function* pFun) :
        SubPass(pPass, L"CLEAN", pFun), m_pPass(pPass) {}

    // run
    void run(Function* pFun)
    {
        BitVec* pLive = pFun->GetEntryBB()->Extend<DataFlowBB>()->GetIn();

        foreach (Function::EnumReg, oEnum, pFun)
        {
            Register* pRx = oEnum.Get();
            LiveInterval* pIntv = pRx->GetExtension<LiveInterval>();
            if (NULL == pIntv) continue;
            if (pIntv->IsSpilled())
            {
                m_oSpilledRegs.Push(pRx);
            }
        } // for each reg

        foreach (Function::EnumBBlock, oEnumBB, pFun)
        {
            DataFlowBB* pCurr = oEnumBB.Get()->Extend<DataFlowBB>();

            pLive->Copy(pCurr->GetOut());

            foreach (WorkList_<Register>::Enum, oEnum, &m_oSpilledRegs)
            {
                Register* pRx = oEnum.Get();
                pLive->SetZero(pRx->GetIndex());
            } // for each spilled reg

            BBlock::EnumInsn_Reverse oEnum(pCurr);
            while (! oEnum.AtEnd())
            {
                Instruction* pInsn = oEnum.Get();
                    oEnum.Next();

                // process output
                {
                    Register* pRd = pInsn->GetRd();
                    if (NULL != pRd && pRd->IsPhysical())
                    {
                        if (pLive->IsOne(pRd->GetIndex()))
                        {
                            pLive->SetZero(pRd->GetIndex());
                        }
                        else
                        {
                            if (isCopyI(pInsn))
                            {
                                ir_remove_insn(pInsn);
                                continue;
                            }
                        }
                    } // if
                }

                if (! oEnum.AtEnd() && isCopyI(pInsn))
                {
                    // MOV [ESP+8], EAX => MOV [ESP+8],EAX
                    // MOV EAX, [ESP+8] => MOV EAX, EAX
                    Instruction* pPrev = oEnum.Get();
                    if (isCopyI(pPrev))
                    {
                        if (pInsn->GetSx()->Equal(pPrev->GetRd()))
                        {
                            if (NULL != pPrev->GetRx() &&
                                pPrev->GetRx()->IsPhysical() )
                            {
                                pInsn->GetOperandBox(0)->Replace(
                                    pPrev->GetSx() );
                            }
                        }
                    }
                } // if

                foreach (Instruction::EnumInput, oEnum, pInsn)
                {
                    Register* pRx = oEnum.Get()->DynamicCast<Register>();
                    if (NULL == pRx) continue;
                    if (pRx->IsPhysical())
                    {
                        pLive->SetOne(pRx->GetIndex());
                    }
                } // for each operand
            } // for each insn
        } // for each bblock
    } // run
}; // CleanPass

//////////////////////////////////////////////////////////////////////
//
// LivenessPass
//
class LivenessPass : public SubPass
{
    private: LivenessPass(Pass* pPass, Function* pFun) :
        SubPass(pPass, L"*Liveness", pFun) {}

    public: static void Run(Pass* pPass, Function* pFun)
    {
        LivenessPass oPass(pPass, pFun);
        ComputeLiveness(pFun);
    } // Run
}; // LivenessPass

//////////////////////////////////////////////////////////////////////
//
// LiveInterval::AddUsePosn
//
void
LiveInterval::AddUsePosn(uint nPosn, bool fPhysical)
{
    if (UsePosnList::IsEmpty())
    {
        UsePosnList::Prepend_(new UsePosn(nPosn, fPhysical));
    }
    else if (UsePosnList::GetHead()->GetPosn() != nPosn)
    {
        UsePosnList::Prepend_(new UsePosn(nPosn, fPhysical));
    }
    else if (fPhysical)
    {
        UsePosnList::GetHead()->SetPhysical(true);
    } // if
} // LiveInterval::AddUsePosn


//////////////////////////////////////////////////////////////////////
//
// LiveInterval::FindAt
//
// Description:
//  Finds live interval of pRd at nPosn.
//
LiveInterval*
LiveInterval::FindAt(uint nPosn)
{
    ASSERT(GetParent() == this);

    if (IsPointIn(nPosn))
    {
        return this;
    }

    foreach (LiveInterval::EnumSibling, oEnum, this)
    {
        LiveInterval* pSibling = oEnum.Get();
        if (pSibling->IsPointIn(nPosn))
        {
            return pSibling;
        }
    } // for each sibling

    return NULL;
} // LiveInterval::FindAt


//////////////////////////////////////////////////////////////////////
//
// LiveInterval::HtmlPrint
//
void
LiveInterval::HtmlPrint(Val stream, bool) const
{
    switch (m_eStorage)
    {
    case Register::Storage_Physical:
        html_format(stream, L"~S[~D, ~D]/~W",
            m_pReg,
            GetStart(),
            GetEnd(),
            cm_get_target()->GetPhysicalName(m_iStorage) );
        break;

    case Register::Storage_Stack:
        html_format(stream, L"~S[~D, ~D]/s~D",
            m_pReg,
            GetStart(),
            GetEnd(),
            m_iStorage );
        break;

    case Register::Storage_Virtual:
        html_format(stream, L"~S[~D, ~D]",
            m_pReg,
            GetStart(),
            GetEnd() );
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch storage

    if (NULL != m_pReg->GetVar())
    {
        html_format(stream, L"/~S", m_pReg->GetVar());
    }
} // LiveInterval::HtmlPrint

//////////////////////////////////////////////////////////////////////
//
// LiveIntervall::NextUseAfter
//
// Description:
//  Returns the first use position of this interval after nPosn (inclusive)
//
uint
LiveInterval::NextUseAfter(uint nPosn) const
{
    foreach (UsePosnList::Enum, oEnum, this)
    {
        UsePosn* pUsePosn = oEnum.Get();
        if (pUsePosn->GetPhysical() && pUsePosn->GetPosn() >= nPosn)
        {
            return pUsePosn->GetPosn();
        }
    } // for each use posn

    return Posn_Max;
} // LiveInterval::NextUseAfter

//////////////////////////////////////////////////////////////////////
//
// number_instructions
//  Number instructions in RPO. We count SLOT+LOAD or SLOT+STORE as
//  one instruciton instead of two instruction. Since, we don't want
//  to insert instruction between SLOT+LOAD or SLOT+STORE.
//
class NumberPass : public SubPass
{
    // Run - Entry Point
    public: static void Run(Pass* pPass, Function* pFun)
    {
        NumberPass oPass(pPass, pFun);
        oPass.run(pFun);
    } // Run

    // NumberPass ctor
    private: NumberPass(Pass* pPass, Function* pFun) :
        SubPass(pPass, L"NUMBER", pFun) {}

    // run
    void run(Function* pFun)
    {
        uint nPosn = 0;
        foreach (Function::EnumBBlock_Reverse_Postorder, oEnum, pFun)
        {
            BBlock* pBBlock = oEnum.Get();
            foreach (BBlock::EnumInsn, oEnum, pBBlock)
            {
                Instruction* pInsn = oEnum.Get();

                switch (pInsn->GetOpcode())
                {
                case IrOp_LOAD:
                case IrOp_STORE:
                    if (pInsn->GetRx()->IsPseudo() &&
                        pInsn->GetRx()->GetDfn() == pInsn->GetPrev() )
                    {
                        break;
                    }
                    goto inc_posn;

                inc_posn:
                default:
                    nPosn += Posn_Step;
                    break;
                } // switch opcode

                pInsn->SetIndex(nPosn);
            } // for each instruction
        } // for each bblock
    } // run
}; // NumberPass

//////////////////////////////////////////////////////////////////////
//
// ResolvePass
//
class ResolvePass : public SubPass
{
    public: static void Run(RegisterAllocator* pPass, Function* pFun)
    {
        ResolvePass oSubPass(pPass, pFun);
        oSubPass.run(pFun);

        CheckPass::Run(pPass, pFun);
    } // Run

    private: CopyTask::WorkList     m_oCopy;
    private: WorkList_<Register>    m_oRegs;
    private: RegisterAllocator*     m_pPass;

    // ctor
    private: ResolvePass(RegisterAllocator* pPass, Function* pFun) :
        SubPass(pPass, L"RESOLVE", pFun), m_pPass(pPass) {}

    // [P]
    private: void processBblock(DataFlowBB* pCurr)
    {
        foreach (BBlock::EnumOutEdge, oEnum, pCurr)
        {
            DataFlowBB* pSucc = oEnum.Get()->GetTo()->
                Extend<DataFlowBB>();

            foreach (WorkList_<Register>::Enum, oEnum, &m_oRegs)
            {
                Register* pRx = oEnum.Get();

                if (0 == pCurr->GetOut()->IsOne(pRx->GetIndex())) continue;
                if (0 == pSucc->GetIn()->IsOne(pRx->GetIndex())) continue;

                processReg(pRx, pCurr, pSucc);
            } // for each reg

            if (! m_oCopy.IsEmpty())
            {
                processCopies(pCurr, pSucc);
                m_pPass->ResetPosn();
            }
        } // for each succ
    } // processBblock

    void processCopies(BBlock* pCurr, BBlock* pSucc)
    {
        Instruction* pRefInsn;
        {
            if (pSucc->HasOnlyOnePred())
            {
                pRefInsn = pSucc->GetFirstInsn();
                while (pRefInsn->Is<PhiInsn>())
                {
                    pRefInsn = pRefInsn->GetNext();
                } // while
            }
            else if (pCurr->HasOnlyOneSucc())
            {
                pRefInsn = pCurr->GetLastInsn();
            }
            else
            {
                html_log_format(0,
                    L"NYI: resolve on critical edge ~S->~S.~:%",
                    pCurr,
                    pSucc );

                warn(L"RA-LS: NYI: resolve on critical edge BB~D->BB~D.",
                    pCurr->GetName(),
                    pSucc->GetName() );

                m_oCopy.MakeEmpty();
                return;
            }
        } // pRefInsn

        CopyTask::WorkList oPendings;
        CopyTask::WorkList oReadies;

        // [2] Set up work list of initial copies
        while (! m_oCopy.IsEmpty())
        {
            CopyTask* pTask = m_oCopy.Pop();
            if (0 == use_count(pTask->m_pRd))
            {
                oReadies.Push(pTask);
            }
            else
            {
                oPendings.Push(pTask);
            }
        } // while

        // [3] Iterate over the worklist, inserting copies
        {
            CopyTask::WorkList  oTemps;
            CopyTask::WorkList* pReadies  = &oReadies;
            CopyTask::WorkList* pPendings = &oPendings;
            CopyTask::WorkList* pTemps    = &oTemps;

            for (;;)
            {
                while (! pReadies->IsEmpty())
                {
                    CopyTask* pTask = pReadies->Pop();
                    Register* pRd = pTask->m_pRd;
                    Register* pRx = pTask->m_pRx;

                    ra_insert_insn(
                        new CopyInsn(ty_of(pRd), pRd, pRx),
                        pRefInsn );

                    dec_use_count(pRx);

                    ASSERT(pTemps->IsEmpty());
                    while (! pPendings->IsEmpty())
                    {
                        CopyTask* pTask = pPendings->Pop();
                        if (0 == use_count(pTask->m_pRd))
                        {
                            pReadies->Push(pTask);
                        }
                        else
                        {
                            pTemps->Push(pTask);
                        }
                    } // while

                    swap(pPendings, pTemps);
                } // while

                if (pPendings->IsEmpty())
                {
                    // All tasks are finished or no more pending tasks.
                    break;
                }

                // Free %rd since %rd is source other tasks
                {
                    CopyTask* pTask = pPendings->Pop();

                    Register* pRd = pTask->m_pRd;
                        ASSERT(use_count(pRd) >= 1);
                        pRd->MarkNotSSA();

                    Register* pRx = pTask->m_pRx;
                        ASSERT(use_count(pRx) >= 1);
                        pRx->MarkNotSSA();

                    Pseudo* pQd = new Pseudo();
                    ra_insert_insn(new SwapInsn(pQd, pRd, pRx), pRefInsn);

                    ir_insert_insn(
                        new SwapGetInsn(ty_of(pRd), pRd, pQd, pRx),
                        pRefInsn );

                    ir_insert_insn(
                        new SwapGetInsn(ty_of(pRx), pRx, pQd, pRd),
                        pRefInsn );

                    foreach (CopyTask::WorkList::Enum, oEnum, pPendings)
                    {
                        CopyTask* pPending = oEnum.Get();
                        if (eq_reg(pPending->m_pRx, pRd))
                        {
                            pPending->m_pRx = pRx;
                            dec_use_count(pRd);
                            inc_use_count(pRx);
                        }
                    } // for each task

                    ASSERT(pTemps->IsEmpty());
                    while (! pPendings->IsEmpty())
                    {
                        CopyTask* pPending = pPendings->Pop();
                        if (eq_reg(pPending->m_pRd, pPending->m_pRx))
                        {
                            // ignore
                        }
                        else if (0 == use_count(pPending->m_pRd))
                        {
                            pReadies->Push(pPending);
                        }
                        else
                        {
                            pTemps->Push(pPending);
                        }
                    } // while

                    swap(pPendings, pTemps);
                } // free
            } // for
        }
    } // processCopies

    void processReg(Register* pReg, BBlock* pPred, BBlock* pSucc)
    {
        LiveInterval* pParent = pReg->GetExtension<LiveInterval>();
            if (NULL == pParent) return;

        pParent = pParent->GetParent();

        if (pParent->IsSpilled()) return;

        LiveInterval* pPredIntv = pParent->FindAt(
            pPred->GetLastInsn()->GetIndex() + Posn_LiveOut );

        ASSERT(pPredIntv->IsPhysical());

        LiveInterval* pSuccIntv = pParent->FindAt(
            pSucc->GetFirstInsn()->GetIndex() + Posn_LiveIn );

        if (NULL == pSuccIntv)
        {
            html_log_format(2,
                L"~S->FindAt(~S:~D) failed.~:%",
                pParent,
                pSucc->GetFirstInsn()->GetBBlock(),
                pSucc->GetFirstInsn()->GetIndex() + Posn_LiveIn );

            warn(L"No interval for %r~D at BB~D.",
                pParent->GetReg()->GetName(),
                pSucc->GetName() );
            return;
        }

        ASSERT(pSuccIntv->IsPhysical());

        if (pPredIntv->m_iStorage == pSuccIntv->m_iStorage)
        {
            return;
        }

        if (m_oCopy.IsEmpty())
        {
            html_log_format(3, L"<h3>resolve ~S &lt- ~S</h3>~%",
                pSucc, pPred );
        }

        html_log_format(3, L"resolve ~S <- ~S~:%",
            pSuccIntv, pPredIntv );

        Register* pRd = pSuccIntv->GetReg();
        Register* pRx = pPredIntv->GetReg();

        inc_use_count(pRx);

        m_oCopy.Push(new CopyTask(pRd, pRx));
    } // processReg

    // [R]
    void run(Function* pFun)
    {
        m_pPass->ResetPosn();

        foreach (Function::EnumReg, oEnum, pFun)
        {
            Register* pRx = oEnum.Get();

            if (pRx->GetRep() != pRx) continue;
            if (! pRx->HasIndex()) continue;

            m_oRegs.Push(pRx);
        } // for each reg

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            processBblock(oEnum.Get()->Extend<DataFlowBB>());
        } // for each bblock
    } // run

    static bool eq_reg(Register* pRx, Register* pRy)
        { return pRx->GetLocation() == pRy->GetLocation(); }

    void dec_use_count(Register* pRx)
        { m_pPass->SetPosn(pRx->GetLocation(), use_count(pRx) - 1); }

    void inc_use_count(Register* pRx)
        { m_pPass->SetPosn(pRx->GetLocation(), use_count(pRx) + 1); }

    int use_count(Register* pRx)
        { return m_pPass->GetPosn(pRx->GetLocation()); }

    Val ty_of(Register* pRx)
        { return pRx->GetRep()->GetTy(); }
}; // ResolvePass

//////////////////////////////////////////////////////////////////////
//
// RenamePass
//  Rename all physical registers to representative.
//
class RenamePass : public SubPass
{

    // Run
    public: static void Run(RegisterAllocator* pPass, Function* pFun)
    {
        RenamePass oPass(pPass, pFun);
            oPass.run();
    } // Run

    RegisterAllocator* m_pPass;

    RenamePass(RegisterAllocator* pPass, Function* pFun) :
        SubPass(pPass, L"RENAME", pFun), m_pPass(pPass) {}

    // run
    void run()
    {
        foreach (Function::EnumBBlock, oEnumBB, m_pFun)
        {
            DataFlowBB* pCurr = oEnumBB.Get()->Extend<DataFlowBB>();

            m_pPass->ResetMap();

            BBlock::EnumInsn oEnum(pCurr);

            while (! oEnum.AtEnd())
            {
                Instruction* pInsn = oEnum.Get();
                    oEnum.Next();

                process_instruction(pInsn);
            } // for each bblock
        } // for each bblock
    } // run

    // process_instruction - rename and copy propagation
    void process_instruction(Instruction* pInsn)
    {
        foreach (Instruction::EnumInput, oEnum, pInsn)
        {
            update_input(oEnum.GetBox());
        } // for each operand

        update_output(pInsn);

        if (isCopyI(pInsn))
        {
            if (pInsn->GetRd() == pInsn->GetRx())
            {
                ir_remove_insn(pInsn);
            }
            else if (pInsn->GetBBlock()->GetFirstInsn() != pInsn)
            {
                Instruction* pPrev = pInsn->GetPrev();
                if (isCopyI(pPrev) &&
                    pPrev->GetRd() == pInsn->GetRx() &&
                    NULL != pPrev->GetRx() &&
                    pPrev->GetRx()->IsPhysical() )
                {
                    pInsn->GetOperandBox(0)->Replace(pPrev->GetRx());
                }
            } // if
        } // if copy_insn
    } // process_instruction

    // rename
    Register* rename(Register* pRx)
    {
        if (NULL == pRx) return NULL;

        switch (pRx->GetStorage())
        {
        case Register::Storage_Physical:
            return m_pPass->GetFixed(pRx->GetLocation())->GetReg();

        case Register::Storage_Stack:
            LiveInterval* pIntv = pRx->GetExtension<LiveInterval>();
                if (NULL == pIntv) return NULL;
            return pIntv->GetSpill();
        } // switch storage

        return NULL;
    } // rename

    // update_input
    void update_input(OperandBox* pBox)
    {
        Register* pRx = rename(pBox->GetRx());
        if (NULL != pRx) pBox->Replace(pRx);
    } // update_input

    // update_output
    void update_output(Instruction* pInsn)
    {
        Register* pRd = rename(pInsn->GetRd());
        if (NULL != pRd) pInsn->SetOutput(pRd);
    } // update_output
}; // RenamePass

//////////////////////////////////////////////////////////////////////
//
// ScanPass
//
class ScanPass : public SubPass
{
    private: typedef RegisterAllocator RA;

    // Entry Point
    public: static void Run(
        RA*                 pPass,
        Function*           pFun,
        Register::Class     eClass,
        const RegSet*       pAllocable )
    {
        ScanPass oScanPass(pPass, pFun, eClass, pAllocable);
        oScanPass.run();
    } // Run

    private: RegisterAllocator*  m_pPass;
    private: const RegSet*       m_pAllocable;
    private: Register::Class     m_eClass;

    private: LiveIntervalList    m_oUnhandledIntvs;
    private: LiveIntervalList    m_oActiveIntvs;
    private: LiveIntervalList    m_oInactiveIntvs;
    private: LiveIntervalList*   m_pHandledIntvs;

    private: ScanPass(
        RA*                 pPass,
        Function*           pFun,
        Register::Class     eClass,
        const RegSet*       pAllocable ) :
            SubPass(pPass, L"SCAN", pFun),
            m_pPass(pPass),
            m_eClass(eClass),
            m_pAllocable(pAllocable) {}

    // add_unhandled
    //  Insert pChild into unhandled list in order of increasing start
    //  position.
    private: void add_unhandled(LiveInterval* pChild)
    {
        LiveIntervalList* pUnhandled = &m_oUnhandledIntvs;
        LiveInterval* pRef = NULL;
        foreach (LiveIntervalList::Enum, oEnum, pUnhandled)
        {
            LiveInterval* pIntv = oEnum.Get();

            if (pIntv->GetStart() > pChild->GetStart())
            {
                pRef = pIntv;
                break;
            }
        } // for each intv

        pUnhandled->Insert_(pChild, pRef);
    } // add_unhandled

    // alloc_with_spill
    private: bool alloc_with_spill(LiveInterval* pCurr)
    {
        foreach (RegSet::Enum, oEnum, m_pAllocable)
        {
            int rx = oEnum.Get();
            m_pPass->SetPosn(rx, Posn_Max);
        } // for each intv

        uint nCurr = pCurr->GetStart();
        uint nUsePosn = pCurr->NextUseAfter(nCurr);

        html_log_format(4, L"<b>NextUseAfter(~S, ~D)=~D</b>~:%",
            pCurr, nCurr, nUsePosn );

        if (Posn_Max == nUsePosn)
        {
            // pCurr doesn't need physical register.
            assign_spill_slot(pCurr);
            return false;
        }

        // Not allocable
        html_log_format(4, L"<ol>");

        foreach (RegSet::Enum, oEnum, m_pPass->GetNonAllocable(pCurr))
        {
            int rx = oEnum.Get();

            html_log_format(4, L"<li>Not allocable ~W</li>~%",
                cm_get_target()->GetPhysicalName(rx) );

            m_pPass->SetPosn(rx, 0);
        } // for each intv

        html_log_format(4, L"</ol>");

        // For each active intv
        html_log_format(4, L"<ol>");

        foreach (LiveIntervalList::Enum, oEnum, &m_oActiveIntvs)
        {
            LiveInterval* pIntv = oEnum.Get();

            uint nPosn = pIntv->NextUseAfter(nCurr);

            html_log_format(4,
                L"<li><i>active</i> NextUseAfter(~S, ~D)=~D</li>~%",
                pIntv,
                nCurr,
                nPosn );

            if (m_pPass->GetPosn(pIntv->m_iStorage) > nPosn)
            {
                m_pPass->SetPosn(pIntv->m_iStorage, nPosn);
            }
        } // for each intv

        html_log_format(4, L"</ol>");

        // For each inactive intv overlapped with current
        html_log_format(4, L"<ol>");

        foreach (LiveIntervalList::Enum, oEnum, &m_oInactiveIntvs)
        {
            LiveInterval* pIntv = oEnum.Get();

            if (! pIntv->IsOverlapped(pCurr))
            {
                html_log_format(4,
                    L"<li><i>inactive</i> ~S isn't overlapped.</li>~%",
                    pIntv );
                continue;
            }

            uint nPosn = pIntv->NextUseAfter(nCurr);

            html_log_format(4,
                L"<li><i>inactive</i> NextUseAfter(~S, ~D)=~D</li>~%",
                pIntv,
                nCurr,
                nPosn );

            if (m_pPass->GetPosn(pIntv->m_iStorage) > nPosn)
            {
                m_pPass->SetPosn(pIntv->m_iStorage, nPosn);
            }
        } // for each intv

        html_log_format(4, L"</ol>");

        int  nCanReg = m_pAllocable->m_n;
        uint nCanPosn = 0;

        // Pick a register which is used at the largest position.
        foreach (RegSet::Enum, oEnum, m_pAllocable)
        {
            int  rx = oEnum.Get();
            uint nPosn = m_pPass->GetPosn(rx);

            if (nCanPosn < nPosn)
            {
                nCanPosn = nPosn;
                nCanReg  = rx;
            }
        } // for each physical

        if (0 != nCanPosn && nUsePosn != nCanPosn)
        {
            html_log_format(4, L"<b>candidate ~W canPosn=~D</b>~:%",
                cm_get_target()->GetPhysicalName(nCanReg),
                nCanPosn );
        }
        else
        {
            warn(L"No physical registers are free.");
            return true;
        }

        if (nUsePosn > nCanPosn)
        {
            // All other intervals are used before pCurr
            //  - assign spill slot to pCurr
            //  - split pCurr before its first use position that
            //    required regiser
            //
            //  pCurr:    >-----------U--------
            //  nCanReg:  >--------U-----------
            split(pCurr, nUsePosn);
            assign_spill_slot(pCurr);
            return false;
        }
        else
        {
            // Assign register to part of pCurr, from pCurr->Start to
            // nCanPosn.
            //  pCurr:   >--U--------------
            //  nCanReg: >------U----------

            uint nSplitAt = pCurr->GetStart();

            if (nSplitAt % Posn_Step != Posn_LiveIn)
            {
                nSplitAt = (nSplitAt / Posn_Step) * Posn_Step + Posn_Split;
            }

            // split active interval at start.
            foreach (LiveIntervalList::Enum, oEnum, &m_oActiveIntvs)
            {
                LiveInterval* pIntv = oEnum.Get();
                    ASSERT(Register::Storage_Physical== pIntv->m_eStorage);

                if (pIntv->m_iStorage == nCanReg)
                {
                    if (pIntv->IsFixed())
                    {
                        // split pCurr at conflict
                    }
                    else if (pIntv->GetStart() == nSplitAt)
                    {
                        html_log_format(3, L"undo ~S~%", pIntv);
                        pIntv->m_eStorage = Register::Storage_Physical;
                        pIntv->Unlink_();
                        add_unhandled(pIntv);
                    }
                    else
                    {
                        split_aux(pIntv, nSplitAt);
                    }
                    break;
                } // if
            } // for each active

            {
                LiveInterval* pFixed = m_pPass->GetFixed(nCanReg);
                uint nConflictAt = pCurr->IntersectionPosn(pFixed);
                if (Posn_Max != nConflictAt)
                {
                    html_log_format(3,
                        L"~S and ~S conflict at ~D.~:%",
                        pCurr,
                        pFixed,
                        nConflictAt );

                    split_aux(pCurr, nConflictAt);
                } // if
            }

            // split any inactive interval for reg at the end of its
            // lifetime hole
            foreach (LiveIntervalList::Enum, oEnum, &m_oInactiveIntvs)
            {
                LiveInterval* pIntv = oEnum.Get();
                    ASSERT(Register::Storage_Physical== pIntv->m_eStorage);

                if (pIntv->IsFixed()) continue;

                if (pIntv->m_iStorage == nCanReg)
                {
                    if (pIntv->GetStart() == nSplitAt)
                    {
                        warn(L"Can't split %r~D.",
                            pIntv->GetReg()->GetName() );
                    }
                    else
                    {
                        split_aux(pIntv, nSplitAt);
                    }
                }
            } // for each intv

            assign_physical(pCurr, nCanReg);
            return true;
        } // if
    } // alloc_with_spill

    // alloc_without_spill
    //  Computes free until position
    private: bool alloc_without_spill(LiveInterval* pCurr)
    {
        foreach (RegSet::Enum, oEnum, m_pAllocable)
        {
            int rx = oEnum.Get();
            m_pPass->SetPosn(rx, Posn_Max);
        } // for each intv

        html_log_format(4, L"<ol>~%");
        foreach (RegSet::Enum, oEnum, m_pPass->GetNonAllocable(pCurr))
        {
            int rx = oEnum.Get();

            html_log_format(4, L"<li class=r>Not allocable ~W</li>~%",
                cm_get_target()->GetPhysicalName(rx) );

            m_pPass->SetPosn(rx, 0);
        } // for each intv
        html_log_format(4, L"</ol>~%");

        html_log_format(4, L"<ol>~%");
        foreach (LiveIntervalList::Enum, oEnum, &m_oActiveIntvs)
        {
            LiveInterval* pIntv = oEnum.Get();
            int rx = pIntv->m_iStorage;

            html_log_format(4,
                L"<li><i>active</i> FreeUntil(~S)=0</li>~%",
                pIntv );

            m_pPass->SetPosn(rx, 0);
        } // for each intv
        html_log_format(4, L"</ol>~%");

        html_log_format(4, L"<ol>~%");
        foreach (LiveIntervalList::Enum, oEnum, &m_oInactiveIntvs)
        {
            LiveInterval* pIntv = oEnum.Get();
            int rx = pIntv->m_iStorage;

            uint nMin = m_pPass->GetPosn(rx);

            // We can't allocate active register.
            if (0 == nMin) continue;

            if (Posn_Max == pIntv->IntersectionPosn(pCurr))
            {
                html_log_format(4,
                    L"<li><i>inactive</i> ~S isn't overlapped.</li>~%",
                    pIntv );
                continue;
            }

            uint nPosn = pIntv->FreeUntil(pCurr->GetStart());

            html_log_format(4,
                L"<li><i>inactive</i> FreeUntil(~S)=~D min=~D</li>~%",
                pIntv, nPosn, nMin );

            if (nMin > nPosn)
            {
                m_pPass->SetPosn(rx, nPosn);
            }
        } // for each intv
        html_log_format(4, L"</ol>~%");

        int  nCanReg = m_pAllocable->m_n;
        uint nCanPosn = 0;

        if (pCurr->GetParent() != pCurr &&
            pCurr->GetParent()->IsPhysical() )
        {
            nCanReg  = pCurr->GetParent()->m_iStorage;
            nCanPosn = m_pPass->GetPosn(nCanReg);
            if (nCanPosn <= pCurr->GetStart() + Posn_Step ||
                nCanPosn == pCurr->GetStart() )
            {
                nCanReg = m_pAllocable->m_n;
                nCanPosn = 0;
            }
        } // if

        if (0 == nCanPosn)
        {
            // Pick the largest free_until_posn
            foreach (RegSet::Enum, oEnum, m_pAllocable)
            {
                int rx = oEnum.Get();
                uint nPosn = m_pPass->GetPosn(rx);

                if (nCanPosn < nPosn && nPosn > pCurr->GetStart())
                {
                    nCanReg  = rx;
                    nCanPosn = nPosn;
                }
            } // for

            if (0 == nCanPosn)
            {
                // no available register
                html_log_format(4, L"<font color='red'>Need spill</font>~:%");
                return false;
            }
        } // if

        html_log_format(4, L"assign w/o spill: ~W@~D~:%",
            cm_get_target()->GetPhysicalName(nCanReg),
            nCanPosn );

        if (pCurr->GetEnd() > nCanPosn)
        {
            // S---R---E ==> S---R R---E
            split(pCurr, nCanPosn);
        }

        assign_physical(pCurr, nCanReg);

        return true;
    } // alloc_without_spill

    // assign_physical
    private: void assign_physical(LiveInterval* pIntv, int iReg)
    {
        html_log_format(4, L" <b>assign</b> ~W to ~S~:%",
            cm_get_target()->GetPhysicalName(iReg),
            pIntv );

        pIntv->m_eStorage = Register::Storage_Physical;
        pIntv->m_iStorage = iReg;
    } // assign_physical

    // assign_spill_slot
    private: void assign_spill_slot(LiveInterval* pIntv)
    {
        Register* pRspill = pIntv->GetSpill();
        if (NULL == pRspill)
        {
            Register* pRx = pIntv->GetParent()->GetReg();

            pRspill = new Register(pRx->GetVar(), pRx->GetClass());

            pRspill->SetRep(pRx);

            pIntv->SetSpill(pRspill);

            m_pPass->AddSpill(pRx->GetTy(), pRspill);
        } // if

        html_log_format(4, L" <b>assign</b>"
            L" <font color='red'>~S</font> to ~S~:%",
            pRspill,
            pIntv );

        pIntv->m_eStorage = Register::Storage_Stack;
        pIntv->m_iStorage = pRspill->GetLocation();
    } // assign_spill_slot

    // get_insn_at
    //  Returns instruction at nPosn.
    private: Instruction* get_insn_at(uint nPosn)
    {
        nPosn = (nPosn / Posn_Step) * Posn_Step;

        foreach (Function::EnumBBlock_Postorder, oEnum, m_pFun)
        {
            BBlock* pCurr = oEnum.Get();
            if (nPosn >= pCurr->GetFirstInsn()->GetIndex() &&
                nPosn <= pCurr->GetLastInsn()->GetIndex() )
            {
                foreach (BBlock::EnumInsn, oEnum, pCurr)
                {
                    Instruction* pInsn = oEnum.Get();
                    if (pInsn->GetIndex() == nPosn)
                    {
                        return pInsn;
                    }
                } // for each insn
                break;
            }
        } // for each bblock
        CAN_NOT_HAPPEN();
    } // get_insn_at

    //////////////////////////////////////////////////////////////////////
    //
    // list_intervals
    //  List live intervals by increasing start position.
    //
    private: void list_intervals()
    {
        foreach (Function::EnumBBlock_Reverse_Postorder, oEnum, m_pFun)
        {
            BBlock* pBBlock = oEnum.Get();
            foreach (BBlock::EnumInsn, oEnum, pBBlock)
            {
                Instruction* pInsn = oEnum.Get();
                Register* pRd = pInsn->GetRd();
                    if (NULL == pRd) continue;
                    if (! pRd->IsVirtual()) continue;
                    if (pRd->GetClass() != m_eClass) continue;

                LiveInterval* pIntv = pRd->GetExtension<LiveInterval>();

                if (m_oUnhandledIntvs.IsEmpty() ||
                    m_oUnhandledIntvs.GetTail()->GetStart() <
                        pIntv->GetStart() )
                {
                    m_oUnhandledIntvs.Append_(pIntv);
                }
            } // for each instruction
        } // for each bblock
    } // list_intervals

    // run
    private: void run()
    {
        LiveIntervalList* pActiveIntvs    = &m_oActiveIntvs;
        LiveIntervalList* pInactiveIntvs  = &m_oInactiveIntvs;

        prepare();

        while (! m_oUnhandledIntvs.IsEmpty())
        {
            LiveInterval* pCurr = m_oUnhandledIntvs.Pop_();
            uint nPosn = pCurr->GetStart();


            if (pCurr->GetParent() == pCurr)
            {
                html_log_format(3, L"<h3>process ~S</h3>~%", pCurr);
            }
            else
            {
                html_log_format(3, L"<h3>process child ~S of ~S</h3>~%",
                    pCurr, pCurr->GetParent() );
            }

            // Update active interval list
            {
                LiveIntervalList::Enum oEnum(pActiveIntvs);

                while (! oEnum.AtEnd())
                {
                    LiveInterval* pIntv = oEnum.Get();
                        oEnum.Next();

                    ASSERT(Register::Storage_Physical == pIntv->m_eStorage);
                    ASSERT(NULL != pIntv->GetReg());

                    if (pIntv->GetEnd() <= nPosn)
                    {
                        html_log_format(4, L"finish active ~S~:%", pIntv);
                        pActiveIntvs->Remove_(pIntv);
                        //m_pHandledIntvs->Append_(pIntv);
                    }
                    else if (! pIntv->IsPointIn(nPosn))
                    {
                        html_log_format(4, L"inactivate ~S~:%", pIntv);
                        pActiveIntvs->Remove_(pIntv);
                        pInactiveIntvs->Append_(pIntv);
                    }
                } // while
            } // for active intv

            // Update inactive interval list
            {
                LiveIntervalList::Enum oEnum(pInactiveIntvs);

                while (! oEnum.AtEnd())
                {
                    LiveInterval* pIntv = oEnum.Get();
                        oEnum.Next();

                    ASSERT(Register::Storage_Physical == pIntv->m_eStorage);
                    ASSERT(NULL != pIntv->GetReg());

                    if (pIntv->GetEnd() <= nPosn)
                    {
                        html_log_format(4, L"finish inactive ~S~:%", pIntv);
                        pInactiveIntvs->Remove_(pIntv);
                        //m_pHandledIntvs->Append_(pIntv);
                    }
                    else if (pIntv->IsPointIn(nPosn))
                    {
                        html_log_format(4, L"activate inactives ~S~:%", pIntv);
                        pInactiveIntvs->Remove_(pIntv);
                        pActiveIntvs->Append_(pIntv);
                    }
                } // while
            } // for active intv

            if (alloc_without_spill(pCurr) || alloc_with_spill(pCurr))
            {
                html_log_format(4, L"*activate ~S~:%", pCurr);
                pActiveIntvs->Append_(pCurr);
            }
            else
            {
                html_log_format(4, L"*finish ~S~:%", pCurr);
                //m_pHandledIntvs->Append_(pCurr);
            }
        } // while

        #if 0
        {
            LiveIntervalList::Enum oEnum(pActiveIntvs);

            while (! oEnum.AtEnd())
            {
                LiveInterval* pIntv = oEnum.Get();
                    oEnum.Next();

                pActiveIntvs->Remove_(pIntv);
                //m_pHandledIntvs->Append_(pIntv);
            } // while
        }

        {
            LiveIntervalList::Enum oEnum(pInactiveIntvs);

            while (! oEnum.AtEnd())
            {
                LiveInterval* pIntv = oEnum.Get();
                    oEnum.Next();

                pInactiveIntvs->Remove_(pIntv);
                //m_pHandledIntvs->Append_(pIntv);
            } // while
        }
        #endif
    } // run

    // prepare
    private: void prepare()
    {
        list_intervals();

        foreach (RegSet::Enum, oEnum, m_pAllocable)
        {
            int rx = oEnum.Get();

            LiveInterval* pIntv = m_pPass->GetFixed(rx);

            if (NULL == pIntv->GetReg())
            {
                pIntv->SetReg(new Physical(m_eClass, rx));
            }

            if (! pIntv->HasRange())
            {
                pIntv->AddRange(new LiveRange(Posn_Max, Posn_Max));
            }

            m_oInactiveIntvs.Append_(pIntv);
        } // for each reg
    } // prepare

    // split
    private: void split(LiveInterval* pIntv, uint nSplitAt)
    {
        // Adjust split position.
        // Note: We'll process splitted interval after interval for output
        // even if splitted interval starts before output interval.
        if (nSplitAt % Posn_Step != Posn_LiveIn)
        {
            nSplitAt = (nSplitAt / Posn_Step) * Posn_Step + Posn_Split;
        }

        split_aux(pIntv, nSplitAt);
    } // split

    // split_aux
    private: void split_aux(LiveInterval* pIntv, uint nSplitAt)
    {
        ASSERT(! pIntv->IsFixed());
        if (pIntv->GetStart() == nSplitAt)
        {
            html_log_format(1, L"<div class='r'>"
                L"No available register: ~S</div>~%", pIntv );
            warn(L"No available register.");
            return;
        }

        html_log_format(4, L" <b>split</b> ~S at ~D.~:%", pIntv, nSplitAt);

        #if _DEBUG
        {
            if (nil != m_pPass->GetLogStream(4))
            {
                foreach (LiveRangeList::Enum, oEnum, pIntv)
                {
                    html_log_format(4, L" ~S", oEnum.Get());
                } // for each range
                html_log_format(4, L"<br/>~%");
            } // if
        }
        #endif // _DEBUG

        if (nSplitAt < pIntv->GetStart())
        {
            html_log_format(5, L"split point is before start.~:%");
            return;
        }

        if (nSplitAt >= pIntv->GetEnd())
        {
            html_log_format(5, L"split point is after end.~:%");
            return;
        }

        LiveInterval* pParent = pIntv->GetParent();
        LiveInterval* pChild = new LiveInterval(pParent);

        // Move ranges to pChild from pParent
        bool fConnect = false;
        {
            while (pIntv->HasRange())
            {
                LiveRange* pRange = pIntv->GetLastRange();

                if (pRange->GetEnd() <= nSplitAt)
                {
                    // S---E---P
                    break;
                }

                if (pRange->GetStart() < nSplitAt)
                {
                    // S----P---E => S---P P---E
                    uint nEnd = pRange->GetEnd();
                    pRange->SetEnd(nSplitAt);
                    pChild->AddRange(nSplitAt, nEnd);
                    fConnect = true;
                    break;
                }

                pIntv->LiveRangeList::Remove_(pRange);
                pChild->AddRange(pRange);
            } // while
        } // move ranges

        // Move UsePosn to pChild from pIntv
        {
            while (! pIntv->UsePosnList::IsEmpty())
            {
                UsePosn* pUsePosn = pIntv->UsePosnList::GetTail();
                if (pUsePosn->GetPosn() < nSplitAt)
                {
                    break;
                }

                pIntv->UsePosnList::Remove_(pUsePosn);
                pChild->UsePosnList::Prepend_(pUsePosn);
            } // while
        }

        add_unhandled(pChild);

        html_log_format(3,
            L"<b style='color:red'>results</b> ~S"
            L" ~S"
            L" fConnect=~D"
            L" nSplitAt=~D"
            L" pParent->IsSpilled=~D~:%",
            pIntv, pChild,
            fConnect,
            nSplitAt,
            pParent->IsSpilled() );

        // Insert copy from sibling to child
        if (! pParent->IsSpilled() &&
            fConnect &&
            nSplitAt % Posn_Step != Posn_LiveIn )
        {
            Register* pRd = pChild->GetReg();
                pRd->SetRep(pParent->GetReg());

            SplitInsn* pSplit = new SplitInsn(
                pParent->GetReg()->GetTy(), pRd,
                pParent->GetReg() );

            ra_insert_insn(pSplit, get_insn_at(nSplitAt));
        } // if
    } // split
}; // ScanPass

//////////////////////////////////////////////////////////////////////
//
// RegisterAllocator::initOnFunction
//
//  Description:
//    Per-Function initialization.
void
RegisterAllocator::initOnFunction(Function*)
{
    m_iSpillSlot = 0;
} // RegisterAllocator::initOnFunction


//////////////////////////////////////////////////////////////////////
//
// RegisterAllocator::process_intervals
//
void
RegisterAllocator::process_function(Function* pFun)
{
    initOnFunction(pFun);

    NumberPass::Run(this, pFun);

    LivenessPass::Run(this, pFun);

    bool fHasFPR = BuildPass::Run(this, pFun);
    unless (Session::Get()->CanContinue()) return;

    BuildFixedPass::Run(this, pFun);

    ScanPass::Run(this, pFun, Register::Class_GPR, m_pGprAlloc);
    unless (Session::Get()->CanContinue()) return;

    if (fHasFPR)
    {
        ScanPass::Run(this, pFun, Register::Class_FPR, m_pFprAlloc);
        unless (Session::Get()->CanContinue()) return;
    }

    AssignPass::Run(this, pFun);
    unless (Session::Get()->CanContinue()) return;

    ResolvePass::Run(this, pFun);
    unless (Session::Get()->CanContinue()) return;

    CleanPass::Run(this, pFun);
    unless (Session::Get()->CanContinue()) return;

    RenamePass::Run(this, pFun);
    unless (Session::Get()->CanContinue()) return;

    // BUGBUG: We should compute size of frame in portable way.
    pFun->SetFrameSize(static_cast<uint>(m_iSpillSlot * sizeof(Val)));

    AssignFrameOffset(pFun);

    ir_remove_useless_instructions(pFun);
} // RegisterAllocator::process_function


// RegisterAllocator::AddSpill
void RegisterAllocator::AddSpill(Ty ty, Register* pRx)
{
    pRx->SetStorage(
        Register::Storage_Stack,
        sizeof(Val) * m_iSpillSlot );

    m_iSpillSlot += SizeOfSpill(ty);
} // RegisterAllocator::AddSpill

} // Compiler
