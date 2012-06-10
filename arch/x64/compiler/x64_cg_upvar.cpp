#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x64 - Finalize Closure
// arch/x64/compiler/x64_cg_closure.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/compiler/x64_cg_upvar.cpp#5 $
//
#include "./x64_cg_defs.h"
#include "./x64_cg_instruction.h"
#include "./x64_cg_target.h"

#include "../../../compiler/cg/cg_upvar.h"

namespace Compiler
{

using namespace X64;

namespace
{

//////////////////////////////////////////////////////////////////////
//
// X64UpVarPass
//
class X64UpVarPass : public UpVarPass
{
    public: X64UpVarPass() :
        UpVarPass(
            L"X64-UPVAR",
            m_rgpExtra,
            m_rgpVector ) {}

    protected: virtual void analyze_by_target(Module*) {}

    Operand* m_rgpExtra[X64Mach::Multiple_Values_Limit];
    Operand* m_rgpVector[X64Mach::Multiple_Values_Limit];
}; // X64UpVarPass

} // namespace

//////////////////////////////////////////////////////////////////////
//
// x64_finalze_closure_pass
//
void x64_pass_finalize_upvar()
{
    X64UpVarPass oPass;
    oPass.Run();
} // x64_pass_finalize_upvar

} // Compiler
