//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x86 - target
// cg/x86/x86_cg_target.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/compiler/x86_cg_target.h#5 $
//
#if !defined(INCLUDE_compiler_cg_x86_target_h)
#define INCLUDE_compiler_cg_x86_target_h

#include "../../../compiler/cm/cm_target.h"
#include "../kernel/x86_ke_mach.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// X86Target
//
class X86Target : public Target, public X86::X86Mach
{
    public: X86Target();

    public: virtual uint    ComputeFrameSize(Frame*) const;
    public: virtual void    Generate();
    public: virtual Val     GetPhysicalName(Int) const;

    public: virtual uint ComputeFrameSize(uint cbData) const
        { return cbData + 4; }  // +4 for RA

    public: static const Mach*  km_pMach;
}; // X86Target

} // Compiler

#endif //!defined(INCLUDE_compiler_cg_x86_target_h)
