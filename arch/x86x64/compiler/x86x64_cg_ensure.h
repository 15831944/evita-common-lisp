//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - x86x64 - Ensure Pass
// arch/x86x64/compiler/x86x64_cg_ensure.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_ensure.h#16 $
//
#if !defined(INCLUDE_arch_x86x64_compiler_cg_ensure_h)
#define INCLUDE_arch_x86x64_compiler_cg_ensure_h

#include "../../../compiler/ir/ir_pass.h"

#include "./x86x64_cg_instruction.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass
//
// Description:
//  Ensure instruction operations satisfying x86 ISA restrictions:
//
// Note: You SHOULD NOT introduce new instruction. If you insert
// instruction, you MUST follow what ENSURE pass does.
//
class X86X64EnsurePass : public FunctionPass
{

    const Mach* m_pMach;

    protected: X86X64EnsurePass(const char16* pwsz, const Mach* pMach) :
        m_pMach(pMach),
        FunctionPass(pwsz) {}

    protected: WorkList_<Instruction> m_oVdDfns;

    protected: virtual void process_function(Function*);

    protected: virtual void ensure_operand(OperandBox*) = 0;
    protected: virtual void ensure_physical(OperandBox*) = 0;
    protected: virtual void prepare_function(Function*) = 0;
    protected: virtual void process_instruction(Instruction*) = 0;

    void insert_for_vx(Operand*, Instruction*);
    void patch_operand(OperandBox*);
    void patch_vx(Instruction*, OperandBox*);
    void process_output(Instruction*);
    void process_nonlocal_xfer(Instruction*, Val);

    // IR instructions processed by EnsurePass
    void process_BOX(Instruction*);
    void process_CALL(Instruction*);
    void process_COUNT(Instruction*);
    void process_ELT(Instruction*);
    void process_GO(Instruction*);
    void process_LOAD(Instruction*);
    void process_MVSAVE(Instruction*);
    void process_NEG(Instruction*);
    void process_PROJECT(Instruction*);
    void process_RETURNFROM(Instruction*);
    void process_SELECT(Instruction*);
    void process_STORE(Instruction*);
    void process_THROW(Instruction*);
    void process_UNBOX(Instruction*);
    void process_VALUES(Instruction*);
    void process_VARDEF(Instruction*);

    // Rewrite three operands to two operands.
    void process_ADD(Instruction*);
    DEFPROC_SAME(DIV,    ADD);
    DEFPROC_SAME(MUL,    ADD);
    DEFPROC_SAME(SUB,    ADD);

    DEFPROC_SAME(SHL,    ADD);
    DEFPROC_SAME(SHR,    ADD);

    DEFPROC_SAME(LOGAND, ADD);
    DEFPROC_SAME(LOGIOR, ADD);
    DEFPROC_SAME(LOGXOR, ADD);

    void process_VECREF(Instruction* pVecRef)
        { pVecRef->GetRd()->SetStorage(Register::Storage_Pseudo); }

    void process_VARSLOT(Instruction* pVarSlot)
        { pVarSlot->GetRd()->SetStorage(Register::Storage_Pseudo); }
}; // X86X64EnsurePass

} // Compiler

#endif //!defined(INCLUDE_arch_x86x64_compiler_cg_ensure_h)
