#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - x64 - instrution selection
// cg/x64/x64_cg_select.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/compiler/x64_cg_select.cpp#8 $
//
//
// BUGBUG: We should expand OPENxxx and CLOSE in X64SelectPass instead of
// assembler. Post RA is another place for expanding OPENxxx and CLOSE.
//
#include "../../x86x64/compiler/x86x64_cg_select.h"
#include "./x64_cg_instruction.h"

//#include "./x64_cg_operand.h"
//#include "./x64_cg_target.h"

//#include "../../../compiler/ir/ir_pass.h"
//#include "../../../compiler/ir/ir_function.h"

namespace Compiler
{

using namespace X64;


//////////////////////////////////////////////////////////////////////
//
// Instruction X64SelectPass
//
class X64SelectPass : public X86X64SelectPass
{
    public: X64SelectPass() : X86X64SelectPass(L"X64-SELECT", &X64Mach::ISA) {}

    protected: virtual void process_instruction(Instruction* pInsn)
    {
        InsnProcT pfn = k_rgpInsnProc[pInsn->GetOpcode()];
        (this->*pfn)(pInsn);
    } // process_instruction

    //void process_UNBOX(Instruction*);

    typedef void (X64SelectPass::*InsnProcT)(Instruction*);
    static const InsnProcT k_rgpInsnProc[IrOp_MAX_1 + 1];

    //void process_SLOT(Instruction*);
}; // X64SelectPass

const X64SelectPass::InsnProcT
X64SelectPass::k_rgpInsnProc[IrOp_MAX_1 + 1] =
{
    #define DEFIROP(mp_name) &X64SelectPass::process_##mp_name,
    #include "./x64_cg_opcode.inc"
}; // k_rgpInsnProc


#if 0
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
void X64SelectPass::process_SLOT(Instruction* pSlot)
{
    Val klass = pSlot->GetLx();

    Register* pRd = pSlot->GetRd();

    if (Qc6_literal_cell == klass || Qc6_stack_cell == klass)
    {
        Instruction* pInsn = pSlot->GetRz()->GetDfn();

        if (pInsn->Is<LoadInsn>())
        {
            pInsn = pInsn->GetRx()->GetDfn();
        }

        if (pInsn->Is<VarRefInsn>())
        {
            VarRefInsn* pVarRef = pInsn->StaticCast<VarRefInsn>();

            ir_replace_insn(
                new VarSlotInsn(pSlot->GetTy(), pRd,
                    pVarRef->GetRx(), pVarRef->GetRy() ),
                pSlot );
        } // if
    }
    else if (Qsymbol == klass)
    {
        if (pSlot->GetSz()->Is<Register>())
        {
            Register* pRx = new Register();
            ir_insert_insn(
                new LogAndInsn(ty_int32, pRx, pSlot->GetRz(), NewInteger(~15)),
                pSlot );
            pSlot->GetOperandBox(2)->Replace(pRx);
        }
    }
} // X64SelectPass::process_SLOT
#endif


#if 0
//////////////////////////////////////////////////////////////////////
//
// X64SelectPass::process_UNBOX
//
//      UNBOX int{8,16,32}  => SHR
//      UNBOX uint{8,16,32} => SHR
//
void X64SelectPass::process_UNBOX(Instruction* pUnbox)
{
    Ty ty = pUnbox->GetTy();
    if (ty == ty_int8  || ty == ty_uint8 ||
        ty == ty_int16 || ty == ty_uint16 ||
        ty == ty_int32 || ty == ty_uint32 )
    {
        Register* pRd = pUnbox->GetRd();
        Operand*  pSx = pUnbox->GetSx();
        switch (pSx->GetKind())
        {
        case Operand::Kind_Integer:
            ir_replace_all_users(pSx, pRd);
            break;

        case Operand::Kind_Literal:
            ir_replace_all_users(NewInteger(Fixnum::Decode_(pUnbox->GetLx())), pRd);
            break;

        case Operand::Kind_Register:
            ir_replace_insn(
                new ShrInsn(ty, pRd, pSx, NewInteger(Fixnum::TagBits)),
                pUnbox );
            break;

        default:
            warn(L"Unexpected operand of UNBOX.");
            break;
        } // switch operand
    }
} // X64SelectPass::process_UNBOX
#endif


//////////////////////////////////////////////////////////////////////
//
// Code Generator Entry Point
//
void
x64_pass_select_instruction()
{
    X64SelectPass oPass;
    oPass.Run();
} // x64_pass_select_instruction

} // Compiler
