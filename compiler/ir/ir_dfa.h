//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR - Data Flow Analysis
// cm_base_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_dfa.h#2 $
//
#if !defined(INCLUDE_compiler_ir_dfa_h)
#define INCLUDE_compiler_ir_dfa_h

#include "../cm/cm_bitvec.h"
#include "../cm/cm_session.h"
#include "../cm/cm_fns.h"
#include "./ir_bblock.h"

namespace Compiler
{

// DataFlowData
class DataFlowData : public Atom
{
    public: BitVec* m_pIn;
    public: BitVec* m_pKill;
    public: BitVec* m_pOut;

    public: DataFlowData()
    {
        m_pIn   = BitVec::Null();
        m_pKill = BitVec::Null();
        m_pOut  = BitVec::Null();
    } // DataFlowData

    public: void Init(Session*, UINT);
}; // DataFlowData


//////////////////////////////////////////////////////////////////////
//
// DataFlowBB
//
class DataFlowBB : public BBlock
{
    public: DataFlowData* GetDataFlowData()
        { return m_pDFData; }

    public: void InitDataFlow(Session* pSession, UINT cRegs)
    {
        if (NULL == m_pDFData) m_pDFData = new DataFlowData();

        m_pDFData->m_pIn   = m_pDFData->m_pIn->Adjust(pSession, cRegs);
        m_pDFData->m_pKill = m_pDFData->m_pKill->Adjust(pSession, cRegs);
        m_pDFData->m_pOut  = m_pDFData->m_pOut->Adjust(pSession, cRegs);

        m_pDFData->m_pIn->FillZero();
        m_pDFData->m_pKill->FillZero();
        m_pDFData->m_pOut->FillZero();
    } // InitDataFlow

    // In
    public: BitVec* GetIn()
        { return m_pDFData->m_pIn; }

    public: UINT IsIn(UINT n) const
        { return m_pDFData->m_pIn->IsOne(n); }

    public: void SetIn(UINT n)
        { m_pDFData->m_pIn->SetOne(n); }

    public: void ClearIn(UINT n)
        { m_pDFData->m_pIn->SetZero(n); }

    public: BitVec* SetIn(BitVec* pNew)
    {
        BitVec* pCur = m_pDFData->m_pIn;
        m_pDFData->m_pIn = pNew;
        return pCur;
    } // SetIn

    // Kill
    public: BitVec* GetKill()
        { return m_pDFData->m_pKill; }

    public: UINT IsKill(UINT n) const
        { return m_pDFData->m_pKill->IsOne(n); }

    public: void SetKill(UINT n)
        { m_pDFData->m_pKill->SetOne(n); }

    public: void ClearKill(UINT n)
        { m_pDFData->m_pKill->SetZero(n); }

    public: BitVec* SetKill(BitVec* pNew)
    {
        BitVec* pCur = m_pDFData->m_pKill;
        m_pDFData->m_pKill = pNew;
        return pCur;
    } // SetKill

    // Out
    public: BitVec* GetOut()
        { return m_pDFData->m_pOut; }

    public: UINT IsOut(UINT n) const
        { return m_pDFData->m_pOut->IsOne(n); }

    public: void SetOut(UINT n)
        { m_pDFData->m_pOut->SetOne(n); }

    public: void ClearOut(UINT n)
        { m_pDFData->m_pOut->SetZero(n); }

    public: BitVec* SetOut(BitVec* pNew)
    {
        BitVec* pCur = m_pDFData->m_pOut;
        m_pDFData->m_pOut = pNew;
        return pCur;
    } // SetOut
}; // DataFlowBB

} // Compiler

#endif // !defined(INCLUDE_compiler_ir_dfa_h)
