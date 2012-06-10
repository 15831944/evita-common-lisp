//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x64 - target
// arch/x64/compiler/x64_cg_target.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/compiler/x64_cg_target.h#5 $
//
#if !defined(INCLUDE_compiler_cg_x64_target_h)
#define INCLUDE_compiler_cg_x64_target_h

#include "../../../compiler/cm/cm_target.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// X64Target
//
class X64Target : public Target, public X64::X64Mach
{
    public: X64Target();

    public: virtual uint    ComputeFrameSize(Frame*) const;
    public: virtual void    Generate();
    public: virtual Val     GetPhysicalName(Int) const;

    public: virtual uint ComputeFrameSize(uint cbData) const
    {
        uint cbFrame = cbData + 8;  // +8 for RA
             cbFrame += cbFrame & 15;
        return cbFrame;
    } // ComputeFrameSize

    public: static const Mach*  km_pMach;
}; // X64Target

} // Compiler

#endif //!defined(INCLUDE_compiler_cg_x64_target_h)
