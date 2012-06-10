#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x64 - assembler
// cg/x64/x64_cg_ensure.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/compiler/x64_cg_ensure.cpp#12 $
//
#include "../../x86x64/compiler/x86x64_cg_ensure.h"

#include "./x64_cg_defs.h"

#include "./x64_cg_instruction.h"

namespace Compiler
{

using namespace X64;

class X64EnsurePass : public X86X64EnsurePass
{
    Register* m_pRTrue;
    Register* m_pRNil;

    public: X64EnsurePass() :
        m_pRNil(NULL),
            X86X64EnsurePass(L"X64-ENSURE", &X64Mach::ISA) {}

    virtual void ensure_operand(OperandBox*);
    virtual void ensure_physical(OperandBox*);

    void ensure_operands(Instruction*);
    void introduce_constant_register(Instruction*);

    virtual void prepare_function(Function*)
        { m_pRTrue = NULL; m_pRNil  = NULL; }

    virtual void process_instruction(Instruction* pInsn)
    {
        introduce_constant_register(pInsn);
        ensure_operands(pInsn);

        InsnProcT pfn = k_rgpInsnProc[pInsn->GetOpcode()];
        (this->*pfn)(pInsn);
    } // process_instruction

    void process_CALL(Instruction*);
    void process_SLOT(Instruction*);
    void process_STACKDEF(Instruction*);

    void process_X86X64_LEA2(Instruction* p)
        { ensure_operand(p->GetOperandBox(0)); }

    void process_X86X64_LEA3(Instruction* p)
    { 
        ensure_operand(p->GetOperandBox(0)); 
        ensure_operand(p->GetOperandBox(2));
    } // process_X86X64_LEA3

    // Dispatch table
    typedef void (X64EnsurePass::*InsnProcT)(Instruction*);
    static const InsnProcT k_rgpInsnProc[IrOp_MAX_1 + 1];
}; // X64EnsurePass


const X64EnsurePass::InsnProcT
X64EnsurePass::k_rgpInsnProc[IrOp_MAX_1 + 1] =
{
    #define DEFIROP(mp_name) &X64EnsurePass::process_##mp_name,
    #include "./x64_cg_opcode.inc"
}; // k_rgpInsnProc


// ensure_operand
void X64EnsurePass::ensure_operand(OperandBox* pBox)
{
    Operand* pSx = pBox->GetOperand();

    switch (pSx->GetKind())
    {
    case Operand::Kind_Register:
        switch (pSx->StaticCast<Register>()->GetStorage())
        {
        case Register::Storage_Closed:
        case Register::Storage_LoadTimeValue:
        case Register::Storage_Stack:
            break;
        default:
            return;
        } // switch storage
        break;

    case Operand::Kind_Integer:
        if (is_32bit(pSx->StaticCast<Integer>()->GetValue())) return;
        break;

    case Operand::Kind_Literal:
        if (is_32bit(pSx->StaticCast<Literal>()->GetDatum())) return;
        break;

    case Operand::Kind_Label:   // for OPENBLOCK, OPENCATCH, TAG
    case Operand::Kind_TlvName:
        return;
    } // switch operand

    Instruction* pInsn = pBox->GetInstruction();
    Instruction* pRefInsn = pInsn;

    if (pInsn->Is<StoreInsn>() && pInsn->GetRx()->Is<Pseudo>())
    {
        pRefInsn = pInsn->GetRx()->GetDfn();
    }

    html_log_format(3, L"patch literal operand: ~S:~S~:%",
        pInsn->GetBBlock(), pInsn );

    Register* pRy = new Register();
    ir_insert_insn(new CopyInsn(pRy, pSx), pRefInsn);
    pBox->Replace(pRy);
} // X64EnsurePass::ensure_operand


// X64EnsurePass::ensure_operands
void X64EnsurePass::ensure_operands(Instruction* pInsn)
{
    switch (pInsn->GetOpcode())
    {
    case IrOp_ADD:         case IrOp_SUB:
    case IrOp_LOGAND:      case IrOp_LOGIOR:   case IrOp_LOGXOR:
    case IrOp_STORE:       case IrOp_VARDEF:
    case IrOp_X86X64_CMP:  case IrOp_X86X64_TEST:
        ensure_operand(pInsn->GetOperandBox(1));
        break;

    case IrOp_COPY:    case IrOp_PHICOPY: case IrOp_SIGMA:
    case IrOp_CLOSURE: case IrOp_OPENFINALLY:
        ensure_operand(pInsn->GetOperandBox(0));
        break;
    } // switch opcode
} // X64EnsurePass::ensure_operands



// X64EnsurePass::ensure_physical
void X64EnsurePass::ensure_physical(OperandBox* pBox)
{
    Operand* pSx = pBox->GetOperand();

    switch (pSx->GetKind())
    {
    case Operand::Kind_Register:
        switch (pSx->StaticCast<Register>()->GetStorage())
        {
        case Register::Storage_Physical:
        case Register::Storage_Virtual:
            return;
        } // switch storage
        break;
    } // switch operand

    Instruction* pInsn = pBox->GetInstruction();
    Instruction* pRefInsn = pInsn;

    if (pInsn->Is<StoreInsn>() && pInsn->GetRx()->Is<Pseudo>())
    {
        pRefInsn = pInsn->GetRx()->GetDfn();
    }

    html_log_format(3, L"patch literal operand: ~S:~S~:%",
        pInsn->GetBBlock(), pInsn );

    Register* pRy = new Register();
    ir_insert_insn(new CopyInsn(pRy, pSx), pRefInsn);
    pBox->Replace(pRy);
} // X64EnsurePass::ensure_physical


// X64EnsurePass::introduce_constant_register
//  Replace generaized true and nil with physical register.
void X64EnsurePass::introduce_constant_register(Instruction* pInsn)
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
        }
        else if (pSx == Obj_Nil)
        {
            if (NULL == m_pRNil)
            {
                m_pRNil = new Physical($rnil);
                ir_insert_insn(
                    new x86x64DefInsn(m_pRNil),
                    pInsn->GetBBlock()->GetFunction()->
                        GetEntryBB()->GetLastInsn() );
            } // if

            pBox->Replace(m_pRNil);
        }
    } // for each operand
} // X64EnsurePass::replace_boolean


//////////////////////////////////////////////////////////////////////
//
// X64EnsurePass::process_CALL
//
//  Makes register callee to $rfn
//
//  Insert set callee before VALUES:
//      COPY t $rfn %callee
//      VALUES ty %vx <= ...
//      USE $rfn
//      CALL ty %vd <= .funcall %vx
//
void
X64EnsurePass::process_CALL(Instruction* pCall)
{
    Register* pRx = pCall->GetRx();
    if (NULL == pRx) return;

    html_log_format(2, L"process ~S~:%", pCall);

    Values* pVy = pCall->GetVy();
    Instruction* pArgs = pVy->GetDfn();

    if (pArgs->GetBBlock() != pCall->GetBBlock())
    {
        warn(L"Values go over basic block.");
    }

    Physical* pRfn = new Physical($rfn);

    ir_insert_insn(new CopyInsn(ty_t, pRfn, pRx), pCall);

    if (pArgs->Is<ValuesInsn>())
    {
        ir_move_insn(pArgs, pCall);
    }

    ir_insert_insn(new UseInsn(pRfn), pCall);
    pCall->GetOperandBox(0)->Replace(NewLiteral(QDfuncall));
} // X64EnsurePass::process_CALL


//////////////////////////////////////////////////////////////////////
//
// X64SelectPass::process_SLOT
//
//  Owner:
//      VARDEF  stack-cell %r1 <= var %r2
//      SLOT    (ptr t)    %r3 <= stack-cell value %r1
//
//      VARREF  literal-cell %r1 <= %r2 %q3
//      SLOT    (ptr t)      %r4 <= literal-cell value %r1
//     ==>
//      VARSLOT (ptr t)      %r4 <= %r2 %q3
//      (VARREF instruction will be removed.)
//
void X64EnsurePass::process_SLOT(Instruction* pSlot)
{
    Val klass = pSlot->GetLx();
    Val slot  = pSlot->GetLy();

    Register* pRd = pSlot->GetRd();
        pRd->SetStorage(Register::Storage_Pseudo);

    if (Qc6_literal_cell == klass) return;
    if (Qc6_stack_cell == klass) return;

    if (pSlot->GetSz()->Is<Register>())
    {
        #if 0
        if (Qclosed_cell == klass && Qvalue == slot)
        {
            if (pSlot->GetRz()->IsClosed())
            {
                pRd->SetStorage(Register::Storage_Virtual);
            }
        }
        #endif

        ensure_operand(pSlot->GetOperandBox(2));
        return;
    }

    if ((Qsymbol     == klass && Qfunction == slot) ||
        (Qvalue_cell == klass && Qvalue    == slot) ||
        (Qsetf_cell  == klass && Qfunction == slot) )
    {
        pRd->SetStorage(Register::Storage_Virtual);
        return;
    }

    Register* pRz = new Register();
    ir_insert_insn(
        new CopyInsn(pRz, pSlot->GetSz()),
        pSlot );
    pSlot->GetOperandBox(2)->Replace(pRz);
} // X64EnsurePass::process_SLOT


//////////////////////////////////////////////////////////////////////
//
// X64SelectPass::process_STACKDEF
//
void X64EnsurePass::process_STACKDEF(Instruction* pStackDef)
{
    Register* pRy = pStackDef->GetRy();
    if (Register::Storage_Closed == pRy->GetStorage())
    {
        Pseudo* pQ0 = new Pseudo();
        ir_insert_insn(
            new SlotInsn(ty_ptr_t, pQ0,
                NewLiteral(Qc6_literal_cell),
                NewLiteral(Qvalue),
                pRy ),
            pStackDef );

        Register* pRx = new Register();
        ir_insert_insn(new LoadInsn(pRx, pQ0), pStackDef);

        pStackDef->GetOperandBox(1)->Replace(pRx);
    }
} // // X64SelectPass::process_STACKDEF


//////////////////////////////////////////////////////////////////////
//
// Code Generator Entry Point
//
void x64_ensure_operands()
{
    X64EnsurePass oPass;
    oPass.Run();
} // x64_ensure_operands

} // Compiler
