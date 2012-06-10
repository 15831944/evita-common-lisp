//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x64 Machine Dependent Layout
// arch/kernel/x64_ke_layout.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/kernel/x64_ke_layout.h#2 $
//
#if !defined(INCLUDE_arch_x64_kernel_x64_ke_layout_h)
#define INCLUDE_arch_x64_kernel_x64_ke_layout_h

#include "../../../kernel/ke_layout.h"

namespace Kernel
{
    struct DllEntry;
} // Kernel

namespace X64
{

using namespace Kernel;

//////////////////////////////////////////////////////////////////////
//
// Function
//
class Function : public Kernel::NativeCodeFunction
{
    public: static uint64 const Cookie = 0x0F0BCEF4;

    ////////////////////////////////////////////////////////////
    //
    // Annon
    //
    //  other than Annon_Data:
    //      <----         28           --><-4->
    //      +----------------------------+----+
    //      |        offset              | at |
    //      +----------------------------+----+
    //
    //
    //  Annon_Data
    //      <----         28           --><-4->
    //      +----------------------------+----+
    //      |        offset              | at |
    //      +----------------------------+----+
    //      |        length              | dt |
    //      +----------------------------+----+
    //
    //
    //  ClosedLit
    //      template:   index in fixnum
    //      closure:    value
    //  ClosedRef
    //      template:   index in fixnum
    //      closure:    offset of ValueCell.m_value
    //
    public: struct Annon
    {
        public: enum Type
        {
            Type_LispVal       = 0,
            Type_NamedCallee   = 1,
            Type_LocalCallee   = 2,
            Type_Callee        = 3, // for discriminator
            Type_SymFun        = 4,
            Type_SymVal        = 5,
            Type_SymSetf       = 6,
            Type_Delta2        = 7,

            Type_TlvOffset     = -8,    // 8

            Type_ExitPoint     = -7,    // 9

            Type_ClosedLit     = -6,    // 10
            Type_ClosedVar     = -5,    // 11

            Type_DllLink       = -4,    // 12
            Type_Data          = -3,    // 13

            Type_Label         = -2,    // 14
            Type_15            = -1,    // 15

            Type_MAX_1         = 16,
        }; // Type

        Type    m_eType : 4;
        uint    m_ofs   : 28;

        Annon(Type e, uint ofs) : m_eType(e), m_ofs(ofs) {}
    }; // Annon

    // FrameType
    public: enum FrameType
    {
        FrameType_Fixed     = Fixnum::One * 0,  // 0000
        FrameType_Restify   = Fixnum::One * 1,  // 0100
        FrameType_Mask      = Fixnum::One * 1,  // 0100
    }; // FrameType

    // Desc
    public: struct Desc
    {
        uint32  m_ofsGcMap;
        uint32  m_ofsAnnon;
        uint32  m_cbCodeVec;
        uint32  m_nFrame;

        public: uint GetFrameSize() const
            { return m_nFrame >> 4; }

        public: FrameType GetFrameType() const
            { return static_cast<FrameType>(m_nFrame & FrameType_Mask); }

        public: void SetFrame(FrameType eFrame, uint cbFrame)
        {
            ASSERT(8 == cbFrame || cbFrame >= 16);
            ASSERT(8 == cbFrame || 0 == cbFrame % 16);

            m_nFrame = static_cast<uint32>(eFrame | (cbFrame << 4));
        } // SetFrame
    }; // Desc

    // DataType
    public: enum DataType
    {
        DataType_Int8       = 0,
        DataType_Int16      = 1,
        DataType_Int32      = 2,

        DataType_UInt8      = 3,
        DataType_UInt16     = 4,
        DataType_UInt32     = 5,

        DataType_Float32    = 6,
        DataType_Float64    = 7,

        DataType_Label      = 8,
        DataType_LispVal    = 9,

        DataType_NotUsed10  = 10,
        DataType_NotUsed11  = 11,
        DataType_NotUsed12  = 12,
        DataType_NotUsed13  = 13,
        DataType_NotUsed14  = 14,
        DataType_NotUsed15  = 15,

        DataType_MAX_1      = 16,
    }; // DataType

    // GetAnnon
    Annon* GetAnnon() const
    {
        return reinterpret_cast<Annon*>(
            reinterpret_cast<uint8*>(const_cast<Function*>(this)) +
            GetDesc()->m_ofsAnnon );
    } // GetAnnon

    // GetAnnonSize
    uint32 GetAnnonSize() const
        { return GetDesc()->m_ofsGcMap - GetDesc()->m_ofsAnnon; }

    // GetCodeSize
    uint GetCodeSize() const
        { return GetDesc()->m_cbCodeVec; }

    // GetCodeVec
    uint8* GetCodeVec() const
        { return reinterpret_cast<uint8*>( const_cast<Function*>(this) + 1); }

    // GetDesc
    Desc* GetDesc() const
    {
        return reinterpret_cast<Desc*>(
            reinterpret_cast<uint8*>(const_cast<Function*>(this)) +
            m_cbFunction -
            sizeof(Desc) );
    } // GetDesc

    // GetFrameSize
    uint GetFrameSize() const
        { return GetDesc()->GetFrameSize(); }

    // GetFrameType
    FrameType GetFrameType() const
        { return GetDesc()->GetFrameType(); }

    // GetGcMap
    uint32* GetGcMap() const
    {
        return reinterpret_cast<uint32*>(
            reinterpret_cast<uint8*>(const_cast<Function*>(this)) +
            GetDesc()->m_ofsGcMap );
    } // GetGcMap

    // GetGcMapSize
    uint GetGcMapSize() const
    {
        return static_cast<uint>(
            reinterpret_cast<Int>(GetDesc()) -
            reinterpret_cast<Int>(GetGcMap()) );
    } // GetGcMapSize

    // FetchCallee
    public: Val FetchCallee(uint ofs) const
    {
        Int iCallee = FetchRel(ofs);
        iCallee -= sizeof(Function);
        iCallee |= Arch::Tag_Function;
        return FromInt<Val_>(iCallee);
    } // FetchCallee

    // FetchDllEntry
    public: DllEntry* FetchDllEntry(uint ofs) const
        { return reinterpret_cast<DllEntry*>(FetchRel(ofs)); }

    // FetchExitPoint
    public: Val FetchExitPoint(uint ofs) const
        { return FromInt<Val_>(FetchU32(ofs)); }

    // FetchRel
    public: Int FetchRel(uint ofs) const
    {
        Int iRel = FetchS32(ofs);
        Int iAbs = reinterpret_cast<Int>(
            (GetCodeVec() + ofs + 4) + iRel );
        return iAbs;
    } // FetchCallee

    // FetchS32
    public: int32 FetchS32(uint ofs) const
    {
        return static_cast<int32>(FetchU32(ofs));
    } // FetchS32

    // FetchS64
    public: int64 FetchS64(uint ofs) const
    {
        return static_cast<int64>(FetchU64(ofs));
    } // FetchS64

    // FetchU32
    public: uint32 FetchU32(uint ofs) const
    {
        const uint8* pb = reinterpret_cast<uint8*>(GetCodeVec() + ofs);
        uint32 n;
            n  = pb[3]; n <<= 8;
            n |= pb[2]; n <<= 8;
            n |= pb[1]; n <<= 8;
            n |= pb[0];
        return n;
    } // FetchU32

    // FetchU64
    public: uint64 FetchU64(uint ofs) const
    {
        const uint8* pb = reinterpret_cast<uint8*>(GetCodeVec() + ofs);
        uint64 ull;
            ull  = pb[7]; ull <<= 8;
            ull |= pb[6]; ull <<= 8;
            ull |= pb[5]; ull <<= 8;
            ull |= pb[4]; ull <<= 8;
            ull |= pb[3]; ull <<= 8;
            ull |= pb[2]; ull <<= 8;
            ull |= pb[1]; ull <<= 8;
            ull |= pb[0];
        return ull;
    } // FetchU64

    public: Int FetchSn(uint ofs) const
        { return FetchS64(ofs); }

    public: UInt FetchUn(uint ofs) const
        { return FetchU64(ofs); }

    // FetchVal
    public: Val FetchVal(uint ofs) const
        { return FromInt<Val_>(FetchU64(ofs)); }

    // PatchCallee
    public: void PatchCallee(uint ofs, Val fun)
    {
        PatchRel(
            ofs,
            reinterpret_cast<Int>(fun->Decode<Function>()->GetCodeVec()) );
    } // PatchCallee

    // PatchDllEntry
    public: void PatchDllEntry(uint ofs, DllEntry* pEntry)
        { PatchRel(ofs, reinterpret_cast<Int>(pEntry)); }

    // PatchExitPoint
    public: void PatchExitPoint(uint ofs, Val label)
    {
        ASSERT(label->Is<Fixnum>());
        ASSERT(static_cast<UInt>(label->ToInt()) <= 0xFFFFFFFF);
        PatchU32(ofs, static_cast<uint32>(label->ToInt()));
    } // PatchExitPoint

    // PatchRel
    public: void PatchRel(uint ofs, Int iAbs)
    {
        Int iRel =
            iAbs - 
            reinterpret_cast<Int>(GetCodeVec() + ofs + 4);
        PatchS32(ofs, static_cast<uint32>(iRel));
    } // PatchRel

    // PatchS32
    public: void PatchS32(uint ofs, int32 i)
      { PatchU32(ofs, static_cast<uint32>(i)); }

    // PatchU32
    public: void PatchU32(uint ofs, uint32 n)
    {
        uint8* pb = GetCodeVec() + ofs;
        pb[0] = static_cast<uint8>(n);
        pb[1] = static_cast<uint8>(n >> 8);
        pb[2] = static_cast<uint8>(n >> 16);
        pb[3] = static_cast<uint8>(n >> 24);
    } // PatchU32

    public: void PatchU64(uint ofs, uint64 n)
    {
        uint8* pb = GetCodeVec() + ofs;
        pb[0] = static_cast<uint8>(n);
        pb[1] = static_cast<uint8>(n >> 8);
        pb[2] = static_cast<uint8>(n >> 16);
        pb[3] = static_cast<uint8>(n >> 24);
        pb[4] = static_cast<uint8>(n >> 32);
        pb[5] = static_cast<uint8>(n >> 40);
        pb[6] = static_cast<uint8>(n >> 48);
        pb[7] = static_cast<uint8>(n >> 56);
    } // PatchU64

    // PatchS8
    public: void PatchS8(uint ofs, int8 i)
      { PatchU8(ofs, static_cast<uint8>(i)); }

    // PatchU8
    public: void PatchU8(uint ofs, uint8 n)
    {
        uint8* pb = GetCodeVec() + ofs;
        pb[0] = n;
    } // PatchU8

    // PatchUn
    public: void PatchUn(uint ofs, UInt n)
        { PatchU64(ofs, n); }

    // PatchUn
    public: void PatchUn(uint ofs, void* pv)
        { PatchUn(ofs, reinterpret_cast<UInt>(pv)); }

    // PatchSn
    public: void PatchSn(uint ofs, Int n)
        { PatchU64(ofs, n); }

    // PatchVal
    public: void PatchVal(uint ofs, Val val)
        { PatchU64(ofs, static_cast<uint64>(val->ToInt())); }

    // EnumAnnon
    public: class EnumAnnon
    {
        Annon*  m_pRunner;
        Annon*  m_pEnd;
        Annon*  m_pStart;

        public: EnumAnnon(Function* pFun)
        {
            m_pRunner = pFun->GetAnnon();
            m_pEnd    = reinterpret_cast<Annon*>(
                reinterpret_cast<uint8*>(m_pRunner) + pFun->GetAnnonSize() );
            m_pStart = m_pRunner;
        } // EnumAnnon

        public: bool AtEnd() const
            { return m_pRunner >= m_pEnd; }

        public: Annon Get() const
        {
            ASSERT(! AtEnd());
            return *m_pRunner;
        } // Get

        public: void Next()
        {
            ASSERT(! AtEnd());
            if (Annon::Type_Data == Get().m_eType)
            {
                m_pRunner += 2;
            }
            else
            {
                m_pRunner++;
            }
        } // Next

        public: void Reset()
            { m_pRunner = m_pStart; }
    }; // EnumAnnon
}; // Function

CASSERT(32 == sizeof(Function));
CASSERT( 4 == sizeof(Function::Annon));

//////////////////////////////////////////////////////////////////////
//
// Function Frames
//
// Fixed Frame:
//              +----------------+
//       ESP -> |  RA of current |          mod 16 = 8
//  (low)       +----------------+
//              |    slot[0]     |
//              +----------------+
//                    ...
//              +----------------+
//              | slot[cSlots-1] |
//              +----------------+
//              | pad for align  |
//              +----------------+
//              |  RA of caller  |          mod 16 = 8
//              +----------------+
//  (high)
//
// Restify Frame:
//              +----------------+
//       rsp -> |  RA of current |          mod 16 = 8
//  (low)       +----------------+
//              |    slot[0]     |
//              +----------------+
//                    ...
//              +----------------+
//              | slot[cSlots-1] |
//              +----------------+
//              | pad for align  |
//              +----------------+
//              |    RP  o-------+------+
//              +----------------+      |
//              |    rest[0]     |      |   mod 16 = 0
//              +----------------+      |
//              |    cdr[0]      |      |
//              +----------------+      |
//              |    rest[1]     |      |
//              +----------------+      |
//              |    cdr[1]      |      |
//              +----------------+      |
//                    ...               |
//              +----------------+      |
//              | rest[cRests-1] |      |
//              +----------------+      |
//              |    nil         |      |
//              +----------------+      |
//              |  pad for align |      |   mod 16 = 0
//              +----------------+      |
//              |  RA of caller  |  <---+
//              +----------------+
//  (high)
//

}; // X64

namespace Kernel
{
    class FunObj : public X64::Function {};
}; // Kernel

#endif //!defined(INCLUDE_arch_x64_kernel_x64_ke_layout_h)
