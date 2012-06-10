#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x64 - Lower Pass
// arch/x64/compiler/x64_cg_lower.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/compiler/x64_cg_lower.cpp#5 $
//
#include "../../x86x64/compiler/x86x64_cg_lower.h"

namespace Compiler
{

using namespace X64;


//////////////////////////////////////////////////////////////////////
//
// Instruction X64LowerPass
//
class X64LowerPass : public X86X64LowerPass
{
    public: X64LowerPass() : X86X64LowerPass(L"X64-LOWER") {}

    protected: virtual void process_instruction(Instruction* pInsn)
    {
        InsnProcT pfn = k_rgpInsnProc[pInsn->GetOpcode()];
        (this->*pfn)(pInsn);
    } // process_instruction

    //void process_UNBOX(Instruction*);

    typedef void (X64LowerPass::*InsnProcT)(Instruction*);
    static const InsnProcT k_rgpInsnProc[IrOp_MAX_1 + 1];

    void process_UNBOX(Instruction*);
}; // X64LowerPass

const X64LowerPass::InsnProcT
X64LowerPass::k_rgpInsnProc[IrOp_MAX_1 + 1] =
{
    #define DEFIROP(mp_name) &X64LowerPass::process_##mp_name,
    #include "./x64_cg_opcode.inc"
}; // k_rgpInsnProc


//////////////////////////////////////////////////////////////////////
//
// x64LowerPass::process_UNBOX
//
//  UNBOX float32 %f1 <- {single-float-literal}
//      => ENCODE float32 %f1 <- int32
void
X64LowerPass::process_UNBOX(Instruction* pUnbox)
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

        int64* p = reinterpret_cast<int64*>(
            &flt->Decode<DoubleFloat>()->m_dbl );

        Register* pRx = new Register();
        ir_insert_insn(
            new CopyInsn(ty_int64, pRx, NewInteger(*p)),
            pUnbox );

        ir_replace_insn(
            new x86x64EncodeInsn(ty_float64, pUnbox->GetRd(), pRx),
            pUnbox );
        return;
    } // float64
} // X64LowerPass::process_UNBOX


//////////////////////////////////////////////////////////////////////
//
// Code Generator Entry Point
//
void
x64_pass_lower()
{
    X64LowerPass oPass;
    oPass.Run();
} // x64_pass_lower

} // Compiler
