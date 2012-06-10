#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - x86 - instrution selection
// cg/x86/x86_cg_select.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/compiler/x86_cg_select.cpp#7 $
//
//
// BUGBUG: We should expand OPENxxx and CLOSE in X86SelectPass instead of
// assembler. Post RA is another place for expanding OPENxxx and CLOSE.
//
#include "../../x86x64/compiler/x86x64_cg_select.h"
//#include "./x86_cg_instruction.h"
//#include "./x86_cg_operand.h"
//#include "./x86_cg_target.h"

//#include "../../../compiler/ir/ir_pass.h"
//#include "../../../compiler/ir/ir_function.h"

namespace Compiler
{

using namespace X86;


//////////////////////////////////////////////////////////////////////
//
// Instruction X86SelectPass
//
class X86SelectPass : public X86X64SelectPass
{
    public: X86SelectPass() :
        X86X64SelectPass(L"X86-SELECT", &X86Mach::ISA) {}

    protected: virtual void process_instruction(Instruction* pInsn)
    {
        InsnProcT pfn = k_rgpInsnProc[pInsn->GetOpcode()];
        (this->*pfn)(pInsn);
    } // process_instruction

    //void process_UNBOX(Instruction*);

    typedef void (X86SelectPass::*InsnProcT)(Instruction*);
    static const InsnProcT k_rgpInsnProc[IrOp_MAX_1 + 1];
}; // X86SelectPass

const X86SelectPass::InsnProcT
X86SelectPass::k_rgpInsnProc[IrOp_MAX_1 + 1] =
{
    #define DEFIROP(mp_name) &X86SelectPass::process_##mp_name,
    #include "./x86_cg_opcode.inc"
}; // k_rgpInsnProc

#if 0
//////////////////////////////////////////////////////////////////////
//
// X86SelectPass::process_UNBOX
//
//      UNBOX int{8,16}  => SHR
//      UNBOX uint{8,16} => SHR
//
void X86SelectPass::process_UNBOX(Instruction* pUnbox)
{
    Ty ty = pUnbox->GetTy();
    if (ty == ty_int8  || ty == ty_uint8 ||
        ty == ty_int16 || ty == ty_uint16 )
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
} // X86SelectPass::process_UNBOX
#endif

//////////////////////////////////////////////////////////////////////
//
// Code Generator Entry Point
//
void
x86_pass_select_instruction()
{
    X86SelectPass oPass;
    oPass.Run();
} // x86_pass_select_instruction

} // Compiler
