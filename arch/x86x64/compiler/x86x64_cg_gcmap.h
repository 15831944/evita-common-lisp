//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - GC Map Construction
// arch/x86x64/compiler/x86x64_cg_GcMap.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_gcmap.h#7 $
//
#if !defined(INCLUDE_arch_x86x64_compiler_x86x64_cg_GcMap_h)
#define INCLUDE_arch_x86x64_compiler_x86x64_cg_GcMap_h

#include "./x86x64_cg_pass.h"

#include "../kernel/x86x64_ke_gcmap.h"

#include "../../../compiler/cm/cm_bitvec.h"
#include "../../../compiler/cm/cm_target.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// GC Map Factory
//
class GcMapFactory
{
    public: struct GcDesc;

    struct Entry : Atom, DLinkSite_<Entry>
    {
        GcDesc*     m_pDesc;
        uint        m_ofsCode;

        Entry(uint ofsCode = 0) :
            m_ofsCode(ofsCode),
            m_pDesc(NULL) {}
    }; // Entry

    typedef DLinkAnchor_<Entry> EntryList;

    struct GcDesc : Atom
    {
        public: virtual void HtmlPrint(Val, bool) const;

        public: static uint Hash(GcDescType e, const BitVec* bv)
            {  return e ^ bv->Hash(); }

        BitVec*     m_pBitVec;
        uint        m_ofsDesc;
        uint        m_nHashCode;
        GcDescType  m_eType;

        GcDesc(uint ofs, GcDescType e, BitVec* bv) :
            m_nHashCode(Hash(e, bv)),
            m_eType(e), m_pBitVec(bv),
            m_ofsDesc(ofs) {}
    }; // GcDesc

    EntryList m_oEntries;

    class HashTable
    {
        public: uint GetCount() const { return m_nCount; }
        public: void Init(uint);
        public: GcDesc* Get(uint, GcDescType, const BitVec*) const;
        public: void Put(GcDesc*);

        void put(GcDesc*);

        struct Slot : Atom { GcDesc* m_p; };

        uint    m_cAlloc;
        uint    m_nCount;
        Slot*   m_prgoSlot;

        public: HashTable() :
            m_cAlloc(0), m_nCount(0), m_prgoSlot(NULL) {}
    }; // HashTable

    HashTable m_oHashTable;

    // EnumEntry
    class EnumEntry : public EntryList::Enum
    {
        public: EnumEntry(const GcMapFactory* p) :
            EntryList::Enum(&p->m_oEntries) {}
    }; // EnumEntry

    // EnumEntry_Reverse
    class EnumEntry_Reverse : public EntryList::Enum_Reverse
    {
        public: EnumEntry_Reverse(const GcMapFactory* p) :
            EntryList::Enum_Reverse(&p->m_oEntries) {}
    }; // EnumEntry

    ////////////////////////////////////////////////////////////
    //
    // BitStream
    //
    class BitStream
    {
        uint32* m_p;
        uint32  m_word;
        uint    m_nbits;
        uint    m_cb;

        public: BitStream(uint32* p) :
            m_p(p),
            m_nbits(0),
            m_word(0),
            m_cb(0) {}

        public: void Emit(int f)
        {
            ASSERT(m_nbits <= 31);
            if (f) m_word |= 1 << m_nbits;
            m_nbits += 1;
        } // Emit

        public: uint Flush(uint bit0 = 0)
        {
            if (0 != m_nbits)
            {
                if (NULL != m_p) *m_p++ = m_word | bit0;
                m_cb += 4;
                m_word = 0;
                m_nbits = 0;
            }

            return m_cb;
        } // Flush

        public: bool IsStart() const
            { return 32 == m_nbits; }
    }; // BitStream
}; // GcMapFactory


//////////////////////////////////////////////////////////////////////
//
// X86X64GcMapPass class
//
// Description:
//  Computes and serialize GC Map.
//
class X86X64GcMapPass :
    public SubPass,
    public GcMapFactory
{
    public: const RegSet* m_pAllRegs;
    public: const RegSet* m_pArgRegs;

    public: Pass* m_pPass;

    public: uint m_cEntries;
    public: uint m_cSegments;
    public: uint m_ofsDesc;

    const uint* m_pReg2Idx;
    uint        m_cReg2Idx;

    X86X64GcMapPass(
            Pass*           pPass,
            const char16*   pwsz,
            const RegSet*   pGprAll,
            const RegSet*   pGprArg,
            const uint*     pReg2Idx,
            uint            cReg2Idx ) :
        SubPass(pPass, pwsz),
        m_pPass(pPass),
        m_cEntries(0),
        m_cSegments(1),
        m_ofsDesc(0),
        m_pAllRegs(pGprAll), m_pArgRegs(pGprArg),
        m_pReg2Idx(pReg2Idx), m_cReg2Idx(cReg2Idx) {}

    public: uint Compute(Function*);
    public: void Serialize(uint32*) const;

    public: uint map_to_index(uint) const;
    public: uint map_to_index(Register*, bool = false) const;
    public: uint map_to_index(Instruction*, uint) const;
    public: Entry* intern(uint, GcDescType, const BitVec*);

    uint serialize(uint32*, const GcDesc*) const;
}; // X86X64GcMapPass


//////////////////////////////////////////////////////////////////////
//
// X86X64GcMapPass_ class
//  Provies machine register information.
//
template<class Target_>
class X86X64GcMapPass_ : public X86X64GcMapPass
{
    public: X86X64GcMapPass_(Pass* pPass) :
        X86X64GcMapPass(
            pPass,
            L"*Gc Map",
            Target_::km_pMach->m_pGprAll,
            Target_::km_pMach->m_pGprArg,
            Target_::k_rgnGcMapReg2Idx,
            lengthof(Target_::k_rgnGcMapReg2Idx)) {}
}; // X86X64GcMapPass_

} // Compiler

#endif //!defined(INCLUDE_arch_x86x64_compiler_x86x64_cg_GcMap_h)
