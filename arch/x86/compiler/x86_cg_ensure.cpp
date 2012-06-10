#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x86 - assembler
// cg/x86/x86_cg_ensure.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/compiler/x86_cg_ensure.cpp#10 $
//
#include "../../x86x64/compiler/x86x64_cg_ensure.h"

#include "./x86_cg_defs.h"

#include "./x86_cg_instruction.h"

namespace Compiler
{

using namespace X86;

class X86EnsurePass : public X86X64EnsurePass
{
    Physical* m_pRTrue;

    public: X86EnsurePass() :
        X86X64EnsurePass(L"X86-ENSURE", &X86Mach::ISA) {}

    virtual void ensure_operand(OperandBox*);
    virtual void ensure_physical(OperandBox*);

    virtual void prepare_function(Function*)
        { m_pRTrue = NULL; }

    virtual void process_instruction(Instruction* pInsn)
    {
        foreach (Instruction::EnumInput, oEnum, pInsn)
        {
            OperandBox* pBox = oEnum.GetBox();
            Operand* pSx = pBox->GetOperand();

            if (pSx == Obj_True)
            {
                if (NULL == m_pRTrue)
                {
                    m_pRTrue = new Physical($rtcb);
                    ir_insert_insn(
                        new x86x64DefInsn(m_pRTrue),
                        pInsn->GetBBlock()->GetFunction()->
                            GetEntryBB()->GetLastInsn() );
                } // if

                pBox->Replace(m_pRTrue);
            } // if
        } // for each input

        InsnProcT pfn = k_rgpInsnProc[pInsn->GetOpcode()];
        (this->*pfn)(pInsn);
    } // process_instruction

    void process_CALL(Instruction*);
    void process_SLOT(Instruction*);

    void process_X86X64_LEA2(Instruction* p)
        { ensure_operand(p->GetOperandBox(0)); }

    void process_X86X64_LEA3(Instruction* p)
    {
        ensure_operand(p->GetOperandBox(0));
        ensure_operand(p->GetOperandBox(2));
    } // process_X86X64_LEA3

    // Dispatch table
    typedef void (X86EnsurePass::*InsnProcT)(Instruction*);
    static const InsnProcT k_rgpInsnProc[IrOp_MAX_1 + 1];
}; // X86EnsurePass


const X86EnsurePass::InsnProcT
X86EnsurePass::k_rgpInsnProc[IrOp_MAX_1 + 1] =
{
    #define DEFIROP(mp_name) &X86EnsurePass::process_##mp_name,
    #include "./x86_cg_opcode.inc"
}; // k_rgpInsnProc


//////////////////////////////////////////////////////////////////////
//
// X86EnsurePass::ensure_operand
//
void
X86EnsurePass::ensure_operand(OperandBox* pBox)
{
    Operand* pSx = pBox->GetOperand();
    switch (pSx->GetKind())
    {
    case Operand::Kind_Register:
        switch (pSx->StaticCast<Register>()->GetStorage())
        {
        case Register::Storage_LoadTimeValue:
        case Register::Storage_Stack:
            break;
        default:
            return;
        } // swtich storeage
        break;
    default:
        return;
    } // swtich operand

    patch_operand(pBox);
} // X86EnsurePass::ensure_operand


//////////////////////////////////////////////////////////////////////
//
// X86EnsurePass::ensure_physical
//
void
X86EnsurePass::ensure_physical(OperandBox* pBox)
{
    Operand* pSx = pBox->GetOperand();
    switch (pSx->GetKind())
    {
    case Operand::Kind_Register:
        switch (pSx->StaticCast<Register>()->GetStorage())
        {
        case Register::Storage_LoadTimeValue:
        case Register::Storage_Stack:
            break;
        default:
            return;
        } // swtich storeage
        break;
    default:
        break;
    } // swtich operand

    patch_operand(pBox);
} // X86EnsurePass::ensure_physical


//////////////////////////////////////////////////////////////////////
//
// X64EnsurePass::process_CALL
//
//  Makes register callee to [TCB+m_fn]
//
//  [1] Insert set callee before VALUES:
//      STORE [TCB+m_fn] %callee
//      VALUES ty %vx <= ...
//      CALL ty %vd <= .funcall %vx
//
//  [2] Restore values before CALL:
//      CALL ty %v1 <= ...
//      PROJECT ty EAX <= %v1 0
//      COPY %r1 <= EAX
//      STORE [TCB+m_fn] %callee
//      x86_VALUES %v2 <= %r1
//      CALL ty %vd <= .funcall %v2
//
void
X86EnsurePass::process_CALL(Instruction* pCall)
{
    Register* pCallee = pCall->GetRx();
    if (NULL != pCallee)
    {
        html_log_format(2, L"process ~S~:%", pCall);

        insert_for_vx(pCallee, pCall);

        pCall->GetOperandBox(0)->Replace(NewLiteral(QDfuncall));
    } // if
} // X86EnsurePass::process_CALL


//////////////////////////////////////////////////////////////////////
//
// X86X64EnsurePass::process_SLOT
//      SLOT %q1 <= class slot-name literal
//  Since we don't support code annotation of arbitrary offset.
void
X86EnsurePass::process_SLOT(Instruction* pSlot)
{
    pSlot->GetRd()->SetStorage(Register::Storage_Pseudo);

    switch (pSlot->GetSz()->GetKind())
    {
    case Operand::Kind_Register:
        switch (pSlot->GetRz()->GetStorage())
        {
        case Register::Storage_LoadTimeValue:
            break;
        case Register::Storage_Stack:
            if (pSlot->GetLx() == Qc6_stack_cell) return;
            break;
        default:
            return;
        } // switch storage
        break;

    case Operand::Kind_Function:
        break;

    case Operand::Kind_Literal:
    {
        Val klass = pSlot->GetLx();
        Val slot  = pSlot->GetLy();

        if (Qsymbol     == klass && Qfunction == slot) return;
        if (Qvalue_cell == klass && Qvalue    == slot) return;
        if (Qsetf_cell  == klass && Qfunction == slot) return;
        break;
    } // literal

    default:
        CAN_NOT_HAPPEN();
    } // switch

    patch_operand(pSlot->GetOperandBox(2));
} // X86EnsurePass::process_SLOT


//////////////////////////////////////////////////////////////////////
//
// Code Generator Entry Point
//
void x86_ensure_operands()
{
    X86EnsurePass oPass;
    oPass.Run();
} // x86_ensure_operands

} // Compiler
