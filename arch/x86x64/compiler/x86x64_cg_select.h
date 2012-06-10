//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - x86x64 - select Pass
// arch/x86x64/compiler/x86x64_cg_select.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_select.h#12 $
//
#if !defined(INCLUDE_arch_x86x64_compiler_cg_select_h)
#define INCLUDE_arch_x86x64_compiler_cg_select_h

#include "./x86x64_cg_pass.h"


namespace Compiler
{

class X86X64SelectPass : public FunctionPass
{
    protected: typedef Kernel::Frame::Type FrameType;

    private: Register*      m_pSP;
    private: const Mach*    m_pMach;

    // ctor
    protected: X86X64SelectPass(const char16* pwsz, const Mach* pMach) :
        m_pMach(pMach),
        m_pSP(NULL),
        FunctionPass(pwsz) {}

    protected: virtual void process_instruction(Instruction*) = 0;

    protected: virtual void process_function(Function*);

    protected: void process_compare(Instruction*, Tttn);

    protected: void process_BOUND(Instruction*);
    protected: void process_BOX(Instruction*);
    protected: void process_CALL(Instruction*);
    protected: void process_CLOSE(Instruction*);
    protected: void process_CLOSURE(Instruction*);
    protected: void process_ELT(Instruction*);
    protected: void process_KEYSUPPLIED(Instruction*);
    protected: void process_KEYVAL(Instruction*);
    protected: void process_OPENBIND(Instruction*);
    protected: void process_OPENBLOCK(Instruction*);
    protected: void process_OPENCATCH(Instruction*);
    protected: void process_OPENFINALLY(Instruction*);
    protected: void process_OPENSIMPLE(Instruction*);
    protected: void process_OPENTAGBODY(Instruction*);
    protected: void process_SHR(Instruction*);
    protected: void process_SLOT(Instruction*);
    protected: void process_UNBOX(Instruction*);
    protected: void process_UPVARDEF(Instruction*);
    protected: void process_VALUESA(Instruction*);
    protected: void process_VARDEF(Instruction*);

    protected: void process_EQ(Instruction* p)
        { process_compare(p, tttn_E); }

    protected: void process_GE(Instruction* p)
        { process_compare(p, tttn_GE); }

    protected: void process_GT(Instruction* p)
        { process_compare(p, tttn_G); }

    protected: void process_LE(Instruction* p)
        { process_compare(p, tttn_LE); }

    protected: void process_LT(Instruction* p)
        { process_compare(p, tttn_L); }

    protected: void process_NE(Instruction* p)
        { process_compare(p, tttn_NE); }

    protected: void process_TLV(Instruction* pInsn)
        { pInsn->GetRd()->SetStorage(Register::Storage_Pseudo); }

    protected: void process_VARREF(Instruction* pInsn)
        { pInsn->GetRd()->SetStorage(Register::Storage_Pseudo); }

    // [G]
    private: Register*  get_sp(Function*);

    // [I]
    protected: void insertActivateFrame(Frame*, Instruction*);
    protected: void insertOpenFrame(FrameType, Frame*, Instruction*);
    protected: void insertXferFame(FrameType, Frame*, Instruction*);
}; // X64X86SelectPass

} // Compiler

#endif //!defined(INCLUDE_arch_x86x64_compiler_cg_select_h)
