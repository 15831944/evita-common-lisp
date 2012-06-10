//////////////////////////////////////////////////////////////////////////////
//
// evcl - Genesis - Disasembler for CICS
// arch/generic/gs_25_disasm_cics.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/generic/genesis/gen_gs_25_disasm_cics.h#4 $
//
// Note:
//  You must include {arch}_ke_layout.h for FunObj.
//
#if !defined(INCLUDE_arch_gen_gs_25_disasm_cics_h)
#define INCLUDE_arch_gen_gs_25_disasm_cics_h

namespace Genesis
{

// CICSDisassembler
class CICSDisassembler
{
    protected: struct CICSContext
    {
        const uint8*        m_pbRunner;
        const uint8*        m_pbEnd;
        const uint8*        m_pbStart;
        FunObj::EnumAnnon   m_oEnumAnnon;

        // Context constructor
        CICSContext(Val fun) :
            m_oEnumAnnon(fun->Decode<FunObj>())
        {
            FunObj* pFun = fun->Decode<FunObj>();

            m_pbStart   = pFun->GetCodeVec();
            m_pbEnd     = m_pbStart + pFun->GetDesc()->m_cbCodeVec;
                ASSERT(m_pbEnd > m_pbStart);

            m_pbRunner  = m_pbStart;
        } // CICSContext

        bool AtEnd() const
            { return m_pbRunner >= m_pbEnd; }

        uint8 FetchU8(Int ofs)
            { return m_pbStart[ofs]; }

        Int GetAddress() const
            { return reinterpret_cast<Int>(m_pbRunner); }

        // FetchAnnon
        FunObj::Annon::Type FetchAnnon()
        {
            if (m_oEnumAnnon.AtEnd())
            {
                return FunObj::Annon::Type_MAX_1;
            }

            FunObj::Annon oAnnon = m_oEnumAnnon.Get();

            if (GetOffset() != oAnnon.m_ofs)
            {
                return FunObj::Annon::Type_MAX_1;
            }

            return oAnnon.m_eType;
        } // FetchAnnon

        // GetAnnon
        FunObj::Annon::Type GetAnnon()
        {
            FunObj::Annon::Type eType = FetchAnnon();

            if (FunObj::Annon::Type_MAX_1 != eType)
            {
                m_oEnumAnnon.Next();
            }

            return eType;
        } // GetAnnon

        Int GetEnd() const
            { return reinterpret_cast<Int>(m_pbEnd); }

        uint GetLength() const
            { return static_cast<uint>(m_pbEnd - m_pbStart); }

        uint GetOffset() const
            { return static_cast<uint>(m_pbRunner - m_pbStart); }

        Int GetStart() const
            { return reinterpret_cast<Int>(m_pbStart); }

        // ReadS16
        int16 ReadS16()
        {
            int l = ReadU8();
            int h = ReadS8();
            return static_cast<int16>((h << 8) | l);
        } // ReadS16

        // ReadS32
        int32 ReadS32()
        {
            int32 l = ReadU16();
            int32 h = ReadS16();
            return (h << 16) | l;
        } // ReadS32

        // ReadS64
        int64 ReadS64()
        {
            int64 l = ReadU32();
            int64 h = ReadS32();
            return (h << 32) | l;
        } // ReadS64

        // ReadS8
        int8 ReadS8()
        {
            // For broken instruction stream
            if (AtEnd())
            {
                return 0;
            }
            const int8* p = reinterpret_cast<const int8*>(m_pbRunner);
            m_pbRunner++;
            return *p;
        } // ReadS8

        // ReadU16
        uint16 ReadU16()
        {
            uint l = ReadU8();
            uint h = ReadU8();
            return static_cast<uint16>((h << 8) | l);
        } // ReadU16

        // ReadU32
        uint32 ReadU32()
        {
            uint32 l = ReadU16();
            uint32 h = ReadU16();
            return (h << 16) | l;
        } // ReadU32

        // ReadU64
        uint64 ReadU64()
        {
            uint64 l = ReadU32();
            uint64 h = ReadU32();
            return (h << 32) | l;
        } // ReadU64

        // ReadU8
        uint8 ReadU8()
        {
            // For broken instruction stream
            if (AtEnd())
            {
                return 0;
            }
            return *m_pbRunner++;
        } // ReadU8

        // Rewind
        void Rewind()
        {
            m_pbRunner = m_pbStart;
            m_oEnumAnnon.Reset();
        } // Rewind

        // Unread
        void Unread()
        {
            ASSERT(m_pbRunner > m_pbStart);
            --m_pbRunner;
        } // Unread
    }; // CICSContext


    protected: Val  m_cvars;
    protected: Val  m_labels;

    protected: CICSDisassembler() :
        m_cvars(nil),
        m_labels(nil) {}

    // get_labeled
    Val get_labeled(CICSContext* pContext)
    {
        Int nIP = pContext->GetOffset();
        Int nLabel;
        while (nil != m_labels)
        {
            nLabel = Fixnum::Decode_(car(m_labels));
            if (nIP < nLabel) break;
            m_labels = cdr(m_labels);
            if (nIP == nLabel) return Character::Encode('L');
        } // while
        return Character::Encode(' ');
    } // get_labeled

    // insert_label
    void insert_label(CICSContext* pContext, Int nAddr)
    {
        if (nAddr < 0 || static_cast<UInt>(nAddr) > pContext->GetLength())
        {
            return;
        }

        Val last = nil;
        Val runner = m_labels;
        Val addr = Fixnum::Encode(nAddr);
        while (nil != runner)
        {
            if (car(runner) == addr) return;
            if (cmp_xx(car(runner), addr) > 0) break;

            last = runner;
            runner = cdr(runner);
        } // while

        if (nil == last)
        {
            m_labels = cons(addr, m_labels);
        }
        else
        {
            setf_cdr(
                cons(addr, cdr(last)),
                last );
        } // if
    } // insert_label

    // ldb
    protected: static uint ldb(uint nSize, uint nPosn, uint nVal)
        { return (nVal >> nPosn) & ((1 << nSize) - 1); }

}; // CICSDisassembler

} // Genesis

#endif //!defined(INCLUDE_arch_gen_gs_25_disasm_cics_h)
