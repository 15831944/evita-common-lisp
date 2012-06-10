//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86x64 Machine Dependent Gcmap
// arch/kernel/x86x64_ke_gcmap.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/kernel/x86x64_ke_gcmap.h#3 $
//
#if !defined(INCLUDE_arch_x86x64_kernel_x86x64_ke_gcmap_h)
#define INCLUDE_arch_x86x64_kernel_x86x64_ke_gcmap_h

#if MACH == MACH_x86
    #include "../../x86/kernel/x86_ke_gcmap.h"
#elif MACH == MACH_x64
    #include "../../x64/kernel/x64_ke_gcmap.h"
#else
    #error "MACH must be either MACH_x86 or MACH_x64"
#endif // MACH == MACH_x86

namespace Kernel
{

class FunctionFrame;

//////////////////////////////////////////////////////////////////////
//
// EnumFrameVar
//  Enumerates stack slot of specified function frame.
//
class EnumFrameVar
{
    Val*            m_pStart;
    Val*            m_pRunner;
    uint32          m_nGcDesc;
    const uint32*   m_pGcDesc;
    uint            m_fContinue;
    uint            m_cBits;

    public: struct Slot
    {
        Val m_name;
        Val m_ofs;
        Val m_datum;
        Slot(Val name, Val ofs, Val datum) :
            m_name(name), m_ofs(ofs), m_datum(datum) {}
    }; // Slot

    public: EnumFrameVar(FunctionFrame*);
    public: bool AtEnd() const { return NULL == m_pGcDesc; }
    public: Slot Get() const;

    public: void Next()
    {
        ASSERT(! AtEnd());
        m_pRunner++;
        m_nGcDesc >>= 1;
        m_cBits -= 1;
        next();
    } // Next
    void next();
}; // EnumFrameVar
} // Kernel


#endif //!defined(INCLUDE_arch_x86x64_kernel_x86x64_ke_gcmap_h)
