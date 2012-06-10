//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Generic Machine Dependent Gcmap
// arch/generic/kernel/gen_ke_gcmap.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/generic/kernel/gen_ke_gcmap.h#5 $
//
#if !defined(INCLUDE_arch_Generic_kernel_Gen_ke_gcmap_h)
#define INCLUDE_arch_Generic_kernel_Gen_ke_gcmap_h

namespace Kernel
{
    enum GcDescType
    {
        GcDesc_General = 0,
        GcDesc_StdCall = 1,
    }; // GcDescType
} // Kernel

namespace Generic
{

//////////////////////////////////////////////////////////////////////
//
// GCMap For GC At Safe-Point
//
//    GC Map                    Segment Table
//    +------------------+      +----------------+
//    |                  |      |  entry list_0  |
//    |  segment table   |      +----------------+
//    |                  |      |  entry list_1  |
//    +------------------+      +----------------+
//    |                  |            ...
//    | entry list  pool |      +----------------+
//    |                  |      |  entry list_n-1|
//    +------------------+      +----------------+
//    |                  |
//    | description pool |
//    |                  |
//    +------------------+
//
//     Entry List           Entry
//    +------------+        +-------------+-----------+
//    |  #entries  |        | Desc Offset | IP offset |
//    +------------+        +-------------+-----------+
//    |   entry_0  |             16bit        16bit
//    +------------+
//    |   entry_1  |        Desc Offset = origin is start of GC Map
//    +------------+        IP offset   = origin is start of 64KB segment,
//          ...                           or low 16 bit of address.
//    +------------+
//    |   entry_n-1|
//    +------------+
// 
//     GC Description
//    +----------------------+--+-+     Bit[0] = continue-p
//    |       data           |TT|1|     TT = type of GC description.
//    +----------------------+--+-+         00 = general
//            ....                          01 = stdcall
//    +----------------------+--+-+         10 = extcall
//    |       data              |0|         11 = extcall3
//    +----------------------+--+-+
//


//////////////////////////////////////////////////////////////////////
//
// GcMap class
//
//          +-----------------------+
//  m_p ->  |   list of segment[0]  |
//          +-----------------------+
//          |   list of segment[1]  |
//          +-----------------------+
//                    ...
//          +-----------------------+
//          |   list of segment[n-1]|
//          +-----------------------+
//          |    number of entry    |   entry list
//          +-----------------------+
//          |        entry[0]       | 
//          +-----------------------+
//          |        entry[1]       | 
//          +-----------------------+
//                    ...
//          +-----------------------+
//          |    number of entry    |   entry list
//          +-----------------------+
//          |        entry[0]       | 
//          +-----------------------+
//          |        entry[1]       | 
//          +-----------------------+
//                    ...
//
template<uint t_cb, uint t_ra_ofs>
class GcMap_
{
    const uint32*   m_p;    // start address of GC Map
    uint            m_cb;   // size of code vector

    static const uint km_cbSegment = t_cb * 64 * 1024;

    // GcMap constructor
    public: GcMap_() {}
    public: GcMap_(const uint32* p, uint cb) { Init(p, cb); }

    // Init
    public: void Init(const uint32* p, uint cb)
    {
        m_p  = p;
        m_cb = cb;
    } // Init

    // FindByRA
    public: const uint32* FindByRA(UInt ra) const
    {
        uint ofs = static_cast<uint>(ra - t_ra_ofs);
        uint nSegment = ofs / km_cbSegment;
        const uint32* pEntry = m_p + m_p[nSegment];
        uint cEntries = *pEntry;
            if (0 == cEntries) return NULL;

        pEntry++;

        ofs %= km_cbSegment;
        ofs /= t_cb;
            if (ofs > pEntry[cEntries-1]) return NULL;

        int low  = 0;
        int high = cEntries - 1;

        while (low <= high)
        {
            int guess = (high + low) / 2;
            uint present = GetOffset(pEntry[guess]);
            if (ofs == present) return GetDesc(pEntry[guess]);
            if (ofs <  present) high = guess - 1;    // search before guess
            else low = guess + 1;   // search afer guess
        } // while

        return NULL;
    } // FindByRA

    // FindAfter - returns gc point after ofs or equal to ofs.
    // Called by: Thread::Interrupt
    public: uint FindAfter(uint ofs) const
    {
        uint nSegment = ofs / km_cbSegment;
        ofs %= km_cbSegment;
        ofs /= t_cb;

        uint nSegmentEnd = m_cb / km_cbSegment + 1;

        const uint32* pEntry = m_p + m_p[nSegment];

        uint cEntries = *pEntry;
            pEntry++;

        if (cEntries >= 1 && ofs < GetOffset(pEntry[cEntries-1]))
        {
            while (cEntries >= 1)
            {
                const uint32* pRunner = pEntry + cEntries / 2;
                uint present = GetOffset(*pRunner);
                if (present == ofs) return *pRunner;

                if (ofs > present)
                {
                    // BUGBUG: Can we be more smart?
                    for (;;)
                    {
                        pRunner++;
                        if (GetOffset(*pRunner) >= ofs)
                        {
                            return *pRunner;
                        }
                    } // for
                } // if

                pEntry = pRunner;
                cEntries /= 2;
            } // while
        }
        else
        {
            for (;;)
            {
                nSegment += 1;
                if (nSegment == nSegmentEnd) break;
                const uint32* pEntry = m_p + m_p[nSegment];
                if (0 != *pEntry)
                {
                    return pEntry[1];
                }
            } // for
        } // if
        return 0;
    } // FindAfter

    public: uint GetOffset(uint cookie) const
        { return (cookie & 0xFFFF) * t_cb; }

    public: const uint32* GetDesc(uint cookie) const
        { return m_p + (cookie >> 16); }

    // Enum
    public: class Enum
    {
        const uint32*   m_p;
        const uint32*   m_list;
        const uint32*   m_entry;
        uint            m_nSegEnd;
        uint            m_cEntries;
        uint            m_nSegIdx;

        public: Enum() {}
        public: Enum(const GcMap_<t_cb, t_ra_ofs>* p) { Init(p); }

        public: void Init(const GcMap_<t_cb, t_ra_ofs>* p)
        {
            m_p         = p->m_p;
            m_nSegEnd   = (p->m_cb + km_cbSegment - 1) / km_cbSegment;
            m_nSegIdx   = 0;
            m_list      = m_p;
            m_entry     = m_p + *m_list + 1;
            m_cEntries  = m_entry[-1];
            if (0 == m_cEntries) next();
        } // Init

        public: bool AtEnd() const
            { return m_nSegIdx == m_nSegEnd && 0 == m_cEntries; }

        public: uint GetOffset() const
        {
            ASSERT(! AtEnd());
            return m_nSegIdx * km_cbSegment + (*m_entry & 0xFFFF) * t_cb;
        } // GetOffset

        public: const uint32* GetDesc() const
            { ASSERT(! AtEnd()); return m_p + (*m_entry >> 16); }

        public: void Next()
            { ASSERT(! AtEnd()); m_cEntries -= 1; next(); }

        void next()
        {
            while (0 == m_cEntries)
            {
                m_nSegIdx += 1;
                if (m_nSegIdx == m_nSegEnd) return;
                m_list++;
                m_entry = m_p + *m_list;
                m_cEntries = *m_entry;
            } // while

            m_entry++;
        } // next
    }; // Enum
}; // GcMap_

}; // Generic

#endif //!defined(INCLUDE_arch_Generic_kernel_Gen_ke_gcmap_h)
