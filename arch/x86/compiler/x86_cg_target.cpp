#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x86
// cg/x86/x86_cg_lisp.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/compiler/x86_cg_target.cpp#15 $
//
#include "./x86_cg_defs.h"
#include "./x86_cg_target.h"

#include "../../../compiler/cm/cm_session.h"

#include "../kernel/x86_ke_frame.h"

namespace Compiler
{

using namespace X86;


//////////////////////////////////////////////////////////////////////
//
// Physical Register Name Table
//
static Val s_rgPhysical[3][16];


const Mach* X86Target::km_pMach = &X86::X86Mach::ISA;

//////////////////////////////////////////////////////////////////////
//
// X86Target constructor
//
X86Target::X86Target() :
    Target(Kx86, &X86::X86Mach::ISA)
{
    #define setname(mp_reg, mp_name) \
        s_rgPhysical[mp_reg >> 4][mp_reg & 15] = \
            intern(L## mp_name, PACKAGE_keyword)

    #define defphy(mp_name) \
        setname(Gpr_##mp_name, #mp_name)

    #define defphy1(mp_A) \
        defphy(mp_A##L); defphy(mp_A##H); \
        defphy(mp_A##X); defphy(E##mp_A##X);

    #define defphy16(mp_16) \
        defphy(mp_16##); defphy(E##mp_16##);

    // rax, rbx, rcx, rdx
    defphy1(A); defphy1(B); defphy1(C); defphy1(D);

    // rsi, rdi, rbp, rsp
    defphy16(SI); defphy16(DI); defphy16(BP); defphy16(SP);

    setname(Fpr_XMM0, "XMM0");
    setname(Fpr_XMM1, "XMM1");
    setname(Fpr_XMM2, "XMM2");
    setname(Fpr_XMM3, "XMM3");
    setname(Fpr_XMM4, "XMM4");
    setname(Fpr_XMM5, "XMM5");
    setname(Fpr_XMM6, "XMM6");
    setname(Fpr_XMM7, "XMM7");
} // X86Target::Init


//////////////////////////////////////////////////////////////////////
//
// X86Target::ComputeFrameSize
//
uint X86Target::ComputeFrameSize(Frame* pFrame) const
{
    ASSERT(NULL != pFrame);

    if (pFrame->GetKind() == Qlet)
    {
        OpenBindInsn* pOpen = pFrame->GetDfn()->
            StaticCast<OpenBindInsn>();

        UINT cBinds = pOpen->GetOperandCount();

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

        UINT cArgs = pArgs->GetOperandCount();

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

    COMPILER_INTERNAL_ERROR();
    return 0;
} // X86Target::ComputeFrameSize


//////////////////////////////////////////////////////////////////////
//
// X86Target::GetPhysicalName
//
Val X86Target::GetPhysicalName(Int iReg) const
{
    ASSERT((iReg >> 4) <= 2);
    return s_rgPhysical[iReg >> 4][iReg & 15];
} // X86Target::GetPhysicalName


extern void optimize_clean();

extern void cg_convert_SSA_to_CFG();
extern void cg_layout_bblocks();
extern void cg_pass_ensure_bool();
extern void cg_pass_expand_cast();
extern void cg_pass_expand_elt_and_slot();
extern void cg_pass_expand_typep();
extern void cg_pass_finalize_nlx();

extern void x86_allocate_register();
extern void x86_assemble();
extern void x86_ensure_operands();
extern void x86_pass_expand_trap();
extern void x86_pass_expand_typep();
extern void x86_pass_finalize_upvar();
extern void x86_pass_lower();
extern void x86_pass_select_instruction();

typedef void (*PassFn)();

static const PassFn k_rgpPass[] =
{
    x86_pass_finalize_upvar,
    cg_pass_finalize_nlx,
    cg_pass_expand_cast,
    cg_pass_ensure_bool,
    cg_pass_expand_typep,

    x86_pass_expand_typep,
    x86_pass_select_instruction,
    x86_pass_lower,
    cg_convert_SSA_to_CFG,
    x86_pass_expand_trap,
    x86_ensure_operands,

    // RA-LS requires no critical edge.
    x86_allocate_register,

    // Remove block introduced by removing critical edge and hoist PHI+RET
    optimize_clean,
    cg_layout_bblocks,
    x86_assemble,
}; // k_rgpPass


//////////////////////////////////////////////////////////////////////
//
// X86Target::Generate
//
void
X86Target::Generate()
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
} // X86Target::Generate


// BUGBUG: We move cg_init another place.
void cg_init()
{
    static Target* s_pTarget;
    if (NULL == s_pTarget)
    {
        s_pTarget = new X86Target();
    }
} // Compiler::init_target

} // Compiler
