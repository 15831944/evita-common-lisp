#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x86 - Finalize Closure
// arch/x86/compiler/x86_cg_closure.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/compiler/x86_cg_upvar.cpp#4 $
//
#include "./x86_cg_defs.h"
#include "./x86_cg_instruction.h"
#include "./x86_cg_target.h"

#include "../../../compiler/cg/cg_upvar.h"

namespace Compiler
{

using namespace X86;

namespace
{

//////////////////////////////////////////////////////////////////////
//
// X86UpVarPass
//
class X86UpVarPass : public UpVarPass
{
    public: X86UpVarPass() :
        UpVarPass(
            L"X86-UPVAR",
            m_rgpExtra,
            m_rgpVector ) {}

    protected: virtual void analyze_by_target(Module*) {}

    Operand* m_rgpExtra[X86Mach::Multiple_Values_Limit];
    Operand* m_rgpVector[X86Mach::Multiple_Values_Limit];
}; // X86UpVarPass

} // namespace

//////////////////////////////////////////////////////////////////////
//
// x86_finalze_closure_pass
//
void x86_pass_finalize_upvar()
{
    X86UpVarPass oPass;
    oPass.Run();
} // x86_pass_finalize_upvar

} // Compiler
