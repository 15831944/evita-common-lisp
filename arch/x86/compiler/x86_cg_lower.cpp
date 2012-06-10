#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x86 - Lower Pass
// arch/x86/compiler/x86_cg_lower.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/compiler/x86_cg_lower.cpp#5 $
//
#include "../../x86x64/compiler/x86x64_cg_lower.h"

namespace Compiler
{

using namespace X86;


//////////////////////////////////////////////////////////////////////
//
// Instruction X86LowerPass
//
class X86LowerPass : public X86X64LowerPass
{
    public: X86LowerPass() : X86X64LowerPass(L"X86-LOWER") {}

    protected: virtual void process_instruction(Instruction* pInsn)
    {
        InsnProcT pfn = k_rgpInsnProc[pInsn->GetOpcode()];
        (this->*pfn)(pInsn);
    } // process_instruction

    //void process_UNBOX(Instruction*);

    typedef void (X86LowerPass::*InsnProcT)(Instruction*);
    static const InsnProcT k_rgpInsnProc[IrOp_MAX_1 + 1];

    void process_UNBOX(Instruction*);
}; // X86LowerPass

const X86LowerPass::InsnProcT
X86LowerPass::k_rgpInsnProc[IrOp_MAX_1 + 1] =
{
    #define DEFIROP(mp_name) &X86LowerPass::process_##mp_name,
    #include "./x86_cg_opcode.inc"
}; // k_rgpInsnProc


//////////////////////////////////////////////////////////////////////
//
// x86LowerPass::process_UNBOX
//
//  UNBOX float32 %f1 <- {single-float-literal}
//      => ENCODE float32 %f1 <- int32
//
//  UNBOX float64 %f1 <- {double-float-literal}
//      => ENCODE float64 %f1 <- int32      if lower 32bit is zero.
void
X86LowerPass::process_UNBOX(Instruction* pUnbox)
{
    unless (pUnbox->GetSx()->Is<Literal>()) return;

    Ty ty = pUnbox->GetTy();
    Val val = pUnbox->GetLx();

    if (ty == ty_float32)
    {
        Val flt = funcall(Qcoerce, val, Qsingle_float);

        int32* p = reinterpret_cast<int32*>(
            &flt->Decode<SingleFloat>()->m_flt );

        Register* pRx = new Register();
        ir_insert_insn(
            new CopyInsn(ty_int32, pRx, NewInteger(*p)),
            pUnbox );

        ir_replace_insn(
            new x86x64EncodeInsn(ty_float32, pUnbox->GetRd(), pRx),
            pUnbox );
        return;
    } // float32

    if (ty == ty_float64)
    {
        Val flt = funcall(Qcoerce, val, Qdouble_float);

        int32* p = reinterpret_cast<int32*>(
            &flt->Decode<DoubleFloat>()->m_dbl );

        if (p[0] == 0)
        {
            Register* pRx = new Register();
            ir_insert_insn(
                new CopyInsn(ty_int32, pRx, NewInteger(p[1])),
                pUnbox );

            ir_replace_insn(
                new x86x64EncodeInsn(ty_float64, pUnbox->GetRd(), pRx),
                pUnbox );
            return;
        } // if lower is zero
    } // float64
} // X86LowerPass::process_UNBOX


//////////////////////////////////////////////////////////////////////
//
// Code Generator Entry Point
//
void
x86_pass_lower()
{
    X86LowerPass oPass;
    oPass.Run();
} // x86_pass_lower

} // Compiler
