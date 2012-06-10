//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - target
// cm_target.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_target.h#4 $
//
#if !defined(INCLUDE_compiler_cm_target_h)
#define INCLUDE_compiler_cm_target_h

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Target
//
class Target
{
    public: virtual uint    ComputeFrameSize(uint) const = 0;
    public: virtual uint    ComputeFrameSize(Frame*) const = 0;
    public: virtual void    Generate() = 0;
    public: virtual Val     GetPhysicalName(Int) const = 0;
    public: virtual uint    GetSP() const { return m_pMach->m_$sp; }

    protected: static Target* sm_pTargets;

    protected: Target*          m_pPrev;
    protected: Val              m_name;
    protected: const Mach*      m_pMach;

    public: Target(Val name, const Mach* pMach) :
        m_name(name),
        m_pPrev(sm_pTargets),
        m_pMach(pMach)
    {
        sm_pTargets = this;
    } // Target

    public: static Target* Get();
}; // Target

} // Compiler

#endif //!defined(INCLUDE_compiler_cm_target_h)
