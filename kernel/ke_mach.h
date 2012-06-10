//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Target Machine
// ke_mach.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_mach.h#3 $
//
#if !defined(INCLUDE_kernel_mach_h)
#define INCLUDE_kernel_mach_h

namespace Kernel
{

//////////////////////////////////////////////////////////////////////
//
// RegSet
//
struct RegSet
{
    uint        m_n;
    const uint* m_prgn;

    public: uint GetLength() const
        { return m_n; }

    public: uint Get(uint i) const
        { ASSERT(i < m_n); return m_prgn[i]; }

    public: class Enum
    {
        const uint* m_pnEnd;
        const uint* m_pn;

        public: Enum(const RegSet* pRegSet) :
            m_pn(pRegSet->m_prgn),
            m_pnEnd(pRegSet->m_prgn + pRegSet->m_n) {}

        public: bool AtEnd() const { return m_pn == m_pnEnd; }
        public: uint Get() const { ASSERT(! AtEnd()); return *m_pn; }
        public: void Next() { ASSERT(! AtEnd()); m_pn++; }
    }; // EnumReg
}; // RegSet


#define DefineRegSet(mp_Name) \
    const RegSet k_o##mp_Name = { lengthof(k_reg##mp_Name), k_reg##mp_Name }


//////////////////////////////////////////////////////////////////////
//
// Machine Instruction Set Architecture
//
struct Mach
{
    enum { Isa_RISC, Isa_CICS } m_eISA;

    int             m_$sp;
    const RegSet*   m_pRegAll;
    const RegSet*   m_pGprAll;
    const RegSet*   m_pFprAll;

    // Calling Convention
    int             m_$rn;
    const RegSet*   m_pVolatile;
    const RegSet*   m_pGprArg;
    const RegSet*   m_pFprArg;

    // Save Registers at Interrupt
    const RegSet*   m_pGprSave;
    const RegSet*   m_pFprSave;
}; // Mach

} // Kernel

#endif //!defined(INCLUDE_kernel_mach_h)
