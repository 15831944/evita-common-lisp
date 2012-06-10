#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x64
// cg/x64/x64_cg_lisp.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/compiler/x64_cg_target.cpp#17 $
//
#include "./x64_cg_defs.h"
#include "./x64_cg_target.h"

#include "../../../compiler/cm/cm_session.h"

#include "../kernel/x64_ke_frame.h"

namespace Compiler
{

using namespace X64;


//////////////////////////////////////////////////////////////////////
//
// Physical Register Name Table
//
static Val s_rgPhysical[4][32];

const Mach* X64Target::km_pMach = &X64::X64Mach::ISA;


//////////////////////////////////////////////////////////////////////
//
// X64Target constructor
//
X64Target::X64Target() :
    Target(Kx64, &X64::X64Mach::ISA)
{
    #define setRegName(mp_reg, mp_name) \
        s_rgPhysical[mp_reg >> 8][mp_reg & 31] = \
            intern(mp_name, PACKAGE_keyword)

    #define defphy(mp_name) setRegName(Gpr_##mp_name, L## #mp_name)

    #define defphy1(mp_A) \
        defphy(mp_A##L); defphy(mp_A##X); \
        defphy(E##mp_A##X); defphy(R##mp_A##X);

    #define defphy16(mp_16) \
        defphy(mp_16##L);   defphy(mp_16##); \
        defphy(E##mp_16##); defphy(R##mp_16##);

    #define defphy64(mp_64) \
        defphy(mp_64##L); defphy(mp_64##W); \
        defphy(mp_64##D); defphy(mp_64);

    // rax, rbx, rcx, rdx
    defphy1(A); defphy1(B); defphy1(C); defphy1(D);

    // rsi, rdi, rbp, rsp
    defphy16(SI); defphy16(DI); defphy16(BP); defphy16(SP);

    // r8, r9, ..., r15
    defphy64(R8);  defphy64(R9);  defphy64(R10); defphy64(R11);
    defphy64(R12); defphy64(R13); defphy64(R14); defphy64(R15);

    setRegName($rnil, L"RNIL");

    setRegName(Fpr_XMM0, L"XMM0"); setRegName(Fpr_XMM1, L"XMM1");
    setRegName(Fpr_XMM2, L"XMM2"); setRegName(Fpr_XMM3, L"XMM3");
    setRegName(Fpr_XMM4, L"XMM4"); setRegName(Fpr_XMM5, L"XMM5");
    setRegName(Fpr_XMM6, L"XMM6"); setRegName(Fpr_XMM7, L"XMM7");

    setRegName(Fpr_XMM8,  L"XMM8");  setRegName(Fpr_XMM9,  L"XMM9");
    setRegName(Fpr_XMM10, L"XMM10"); setRegName(Fpr_XMM11, L"XMM11");
    setRegName(Fpr_XMM12, L"XMM12"); setRegName(Fpr_XMM13, L"XMM13");
    setRegName(Fpr_XMM14, L"XMM14"); setRegName(Fpr_XMM15, L"XMM15");

    // Note: We don't use AH, BH, CH, DH
} // X64Target::Init


//////////////////////////////////////////////////////////////////////
//
// X64Target::ComputeFrameSize
//
uint X64Target::ComputeFrameSize(Frame* pFrame) const
{
    ASSERT(NULL != pFrame);

    if (pFrame->GetKind() == Qlet)
    {
        OpenBindInsn* pOpen = pFrame->GetDfn()->
            StaticCast<OpenBindInsn>();

        uint cBinds = pOpen->GetOperandCount();
        return BindFrame::ComputeSize(cBinds);
    }

    if (pFrame->GetKind() == Qblock)
    {
        return BlockFrame::ComputeSize();
    }

    if (pFrame->GetKind() == Qcatch)
    {
        return CatchFrame::ComputeSize();
    }

    if (pFrame->GetKind() == Kcode)
    {
        return sizeof(GcDisableFrame);
    }

    if (pFrame->GetKind() == Kfinally)
    {
        OpenFinallyInsn* pOpen = pFrame->GetDfn()->
            StaticCast<OpenFinallyInsn>();

        ValuesInsn* pArgs = pOpen->GetVy()->GetDfn()->
            StaticCast<ValuesInsn>();

        uint cArgs = pArgs->GetOperandCount();
        return FinallyFrame::ComputeSize(cArgs);
    }

    if (pFrame->GetKind() == Qtagbody)
    {
        return TagbodyFrame::ComputeSize(pFrame->GetCount());
    }

    if (pFrame->GetKind() == Qt)
    {
        return 0;
    }

    CAN_NOT_HAPPEN();
} // X64Target::ComputeFrameSize


//////////////////////////////////////////////////////////////////////
//
// X64Target::GetPhysicalName
//
Val X64Target::GetPhysicalName(Int iReg) const
{
    ASSERT((iReg >> 8) <= 3);
    return s_rgPhysical[iReg >> 8][iReg & 31];
} // X64Target::GetPhysicalName


extern void optimize_clean();

extern void cg_convert_SSA_to_CFG();
extern void cg_layout_bblocks();
extern void cg_pass_ensure_bool();
extern void cg_pass_expand_cast();
extern void cg_pass_expand_typep();
extern void cg_pass_finalize_nlx();

extern void x64_allocate_register();
extern void x64_assemble();
extern void x64_ensure_operands();
extern void x64_pass_expand_trap();
extern void x64_pass_finalize_upvar();
extern void x64_pass_expand_typep();
extern void x64_pass_lower();
extern void x64_pass_select_instruction();

typedef void (*PassFn)();

static const PassFn k_rgpPass[] =
{
    x64_pass_finalize_upvar,
    cg_pass_finalize_nlx,
    cg_pass_expand_cast,
    cg_pass_ensure_bool,
    cg_pass_expand_typep,

    x64_pass_expand_typep,
    x64_pass_select_instruction,
    x64_pass_lower,
    cg_convert_SSA_to_CFG,
    x64_pass_expand_trap,
    x64_ensure_operands,

    // RA-LS requires no critical edge.
    x64_allocate_register,

    // Remove block introduced by removing critical edge and hoist PHI+RET
    optimize_clean,
    cg_layout_bblocks,
    x64_assemble,
}; // k_rgpPass


//////////////////////////////////////////////////////////////////////
//
// X64Target::Generate
//
void
X64Target::Generate()
{
    Session* pSession = Session::Get();

    for (
        const PassFn* pRunner = &k_rgpPass[0];
        pRunner < &k_rgpPass[lengthof(k_rgpPass)];
        pRunner++ )
    {
        (*pRunner)();
        if (! pSession->CanContinue()) break;
    } // for each pass
} // X64Target::Generate


// BUGBUG: We move cg_init another place.
void cg_init()
{
    static Target* s_pTarget;
    if (NULL == s_pTarget)
    {
        s_pTarget = new X64Target();
    }
} // Compiler::init_target

} // Compiler
