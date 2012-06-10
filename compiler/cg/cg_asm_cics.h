//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - Assembler
// cg/cg_asm_cics.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_asm_cics.h#3 $
//
#if !defined(INCLUDE_compiler_cg_asm_cics_h)
#define INCLUDE_compiler_cg_asm_cics_h

#include "./cg_asm.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// CgCicsAsmPass
//
class CgCicsAsmPass : public CgBaseAsmPass
{
    class JumpSpan : public Span
    {
        public: static Kind GetKind_() { return Kind_Jump; }

        public: BBlock* m_pBBlock;  // jump target
        public: uint    m_nLongOp;
        public: uint    m_nShortOp;
        public: bool    m_fLongForm;

        public: JumpSpan(
                uint    nAddr,
                uint    ofsCode,
                BBlock* p,
                uint    nLongOp,
                uint    nShortOp,
                bool    fLongForm ) :
            Span(GetKind_(), nAddr, ofsCode),
            m_pBBlock(p),
            m_nLongOp(nLongOp),
            m_nShortOp(nShortOp),
            m_fLongForm(fLongForm) {}

        public: uint GetTarget() const
            { return m_pBBlock->GetExtension<AsmBBlock>()->GetAddress(); }
    }; // JumpSpan

    public: CgCicsAsmPass(LPCWSTR pwsz) : CgBaseAsmPass(pwsz) {}

    protected: void copy_codevec(uint8*);
    protected: void emit_jump(BBlock*, uint, uint);
    protected: void fix_spans(Function*);
}; // CgCicsAsmPass

} // Compiler

#endif // !defined(INCLUDE_compiler_cg_asm_cics_h)
