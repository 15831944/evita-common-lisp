#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - assembler for CICS
// cg/cg_asm_cics.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_asm_cics.cpp#4 $
//
#include "./cg_asm_cics.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// CgCicsAsmPass::copy_codevec
//
void CgCicsAsmPass::copy_codevec(uint8* pbCodeVec)
{
    uint nAddr = 0;
    uint ofsCode = 0;

    foreach (SpanList::Enum, oEnum, m_oContext.GetSpans())
    {
        Span* pSpan = oEnum.Get();

        uint cbSpan = pSpan->m_nAddr - nAddr;

        ::CopyMemory(
            pbCodeVec + nAddr,
            m_oContext.GetBuffer(ofsCode),
            cbSpan );

        nAddr   = pSpan->m_nAddr;
        ofsCode = pSpan->m_ofsCode;
    } // for each span

    ::CopyMemory(
        pbCodeVec + nAddr,
        m_oContext.GetBuffer(ofsCode),
        m_oContext.GetAddress() - nAddr );

} // CgCicsAsmPass::copy_codevec


//////////////////////////////////////////////////////////////////////
//
// CgCicsAsmPass::remember_jump
//
void CgCicsAsmPass::emit_jump(BBlock* pBBlock, uint nLongOp, uint nShortOp)
{
    ASSERT(NULL != pBBlock);
    ASSERT(nLongOp != nShortOp);

    uint nAddr   = m_oContext.GetAddress();
    uint cbLong  = nLongOp > 0xFF ? 6 : 5;
    uint cbShort = 2;
    bool fLongForm = false;

    {
        AsmBBlock* pAsmBB = pBBlock->GetExtension<AsmBBlock>();
        if (NULL != pAsmBB)
        {
            int iRel = pAsmBB->GetAddress() - (nAddr + 2);
            if (iRel < -128 || iRel > 127)
            {
                fLongForm = true;
            }
        } // if
    }

    //fLongForm = true;

    m_oContext.Advance(fLongForm ? cbLong : cbShort, cbLong);

    m_oContext.GetSpans()->Append_(
        new JumpSpan(
            m_oContext.GetAddress(),
            m_oContext.GetOffset(),
            pBBlock,
            nLongOp,
            nShortOp,
            fLongForm ) );
} // CgCicsAsmPass::emit_jump


//////////////////////////////////////////////////////////////////////
//
// Fix Spans
//
// Description:
//  Fixes address of labels and form of span dependent instructions.
//
void
CgCicsAsmPass::fix_spans(Function* pFun)
{
    class Resolver
    {
        Context* m_pContext;

        public: Resolver(Context* pContext) :
            m_pContext(pContext) {}

        public: void Run()
        {
            compute();

            foreach (SpanList::Enum, oEnum, m_pContext->GetSpans())
            {
                Span* pSpan = oEnum.Get();

                switch (pSpan->GetKind())
                {
                case Span::Kind_Jump:
                {
                    JumpSpan* pJump = pSpan->StaticCast<JumpSpan>();

                    uint nTarget = pJump->GetTarget();
                        reinterpret_cast<JumpSpan*>(pSpan)->m_pBBlock;

                    uint cbLong = pJump->m_nLongOp > 0xFF ? 6 : 5;
                    uint ofsInsn= pJump->m_ofsCode - cbLong;

                    if (! pJump->m_fLongForm)
                    {
                        m_pContext->PatchU8(
                            ofsInsn,
                            static_cast<uint8>(pJump->m_nShortOp) );

                        m_pContext->PatchS8(
                            ofsInsn + 1,
                            static_cast<int8>(nTarget - pSpan->m_nAddr) );
                    }
                    else if (pJump->m_nLongOp <= 0xFF)
                    {
                        m_pContext->PatchU8(
                            ofsInsn,
                            static_cast<uint8>(pJump->m_nLongOp) );

                        m_pContext->PatchS32(
                            ofsInsn + 1,
                            nTarget - pSpan->m_nAddr );
                    }
                    else
                    {
                        m_pContext->PatchU8(
                            ofsInsn,
                            static_cast<uint8>(pJump->m_nLongOp >> 8) );

                        m_pContext->PatchU8(
                            ofsInsn + 1,
                            static_cast<uint8>(pJump->m_nLongOp) );

                        m_pContext->PatchS32(
                            ofsInsn + 2,
                            nTarget - pSpan->m_nAddr );
                    } // if short form
                    break;
                } // Kind_Jump

                default:
                    CAN_NOT_HAPPEN();
                } // switch kind
            } // for each span
        } // Run

        // compute
        void compute()
        {
            WorkList_<Span> oWorkList;
                foreach (SpanList::Enum, oEnum, m_pContext->GetSpans())
                {
                    oWorkList.Push(oEnum.Get());
                } // for each span

            while (! oWorkList.IsEmpty())
            {
                Span* pSpan = oWorkList.Pop();

                switch (pSpan->GetKind())
                {
                case Span::Kind_Jump:
                {
                    JumpSpan* pJump = pSpan->StaticCast<JumpSpan>();

                    if (! pJump->m_fLongForm)
                    {
                        int iRel = pJump->GetTarget() - pJump->m_nAddr;
                        if (iRel < -128 || iRel > 127)
                        {
                            add_crossing(&oWorkList, pJump->m_nAddr);

                            uint nInc = pJump->m_nLongOp > 0xFF ? 4 : 3;
                            pJump->m_fLongForm = true;
                            update_labels(pJump->m_nAddr, nInc);
                        }
                    } // if
                    break;
                } // Span::Kind_Jump

                default:
                    CAN_NOT_HAPPEN();
                } // switch eKind
            } // for each span
        } // compute

        // add_crossing
        void add_crossing(WorkList_<Span>* pWorkList, uint nAddr)
        {
            foreach (SpanList::Enum, oEnum, m_pContext->GetSpans())
            {
                Span* pSpan = oEnum.Get();
                if (pWorkList->Has(pSpan))
                {
                    continue;
                }

                JumpSpan* pJump = pSpan->DynamicCast<JumpSpan>();

                if (NULL == pJump)
                {
                    pWorkList->Push(pSpan);
                }
                else
                {
                    if (pJump->m_nAddr < nAddr &&
                        pJump->GetTarget() >= nAddr )
                    {
                        pWorkList->Push(pSpan);
                    }
                    else if (pJump->m_nAddr >= nAddr &&
                             pJump->GetTarget() < nAddr )
                    {
                        pWorkList->Push(pSpan);
                    }
                } // if
            } // for each span
        } // add_crossing

        // update_labels
        //  Updates spans, labels, annons after nAddr.
        void update_labels(uint nAddr, uint nInc)
        {
            m_pContext->IncAddress(nInc);

            foreach (SpanList::Enum, oEnum, m_pContext->GetSpans())
            {
                Span* pSpan = oEnum.Get();
                if (pSpan->m_nAddr >= nAddr)
                {
                    pSpan->m_nAddr += nInc;
                }
            } // for each span

            foreach (
                Function::EnumBBlock_Reverse,
                oEnum,
                m_pContext->GetFunction() )
            {
                AsmBBlock* pAsmBB = oEnum.Get()->GetExtension<AsmBBlock>();
                if (pAsmBB->GetAddress() < nAddr)
                {
                    break;
                }

                pAsmBB->IncAddress(nInc);
            } // for each bblock

            foreach (AsmFunction::EnumAnnon, oEnum, m_pContext->GetFunction())
            {
                AsmAnnon* pAnnon = oEnum.Get();
                if (pAnnon->GetAddress() >= nAddr)
                {
                    pAnnon->IncAddress(nInc);
                }
            } // for each annon
        } // convert_to_long_form
    }; // Resolver

    Resolver oResolver(&m_oContext);
        oResolver.Run();

    foreach (Function::EnumBBlock, oEnum, pFun)
    {
        BBlock* pBBlock = oEnum.Get();
        int diff = pBBlock->GetExtension<AsmBBlock>()->GetAddress() -
            pBBlock->GetFirstInsn()->GetIndex();
        if (0 == diff) continue;
        foreach (BBlock::EnumInsn, oEnum, pBBlock)
        {
            Instruction* pInsn = oEnum.Get();
            pInsn->SetIndex(pInsn->GetIndex() + diff);
        } // for each insn
    } // for each bblock
} // CgCicsAsmPass::fix_spans

} // Compiler
