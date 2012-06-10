//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86 Machine Dependent Layout
// arch/kernel/x86_ke_layout.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/kernel/x86_ke_layout.h#2 $
//
#if !defined(INCLUDE_arch_x86_kernel_x86_ke_layout_h)
#define INCLUDE_arch_x86_kernel_x86_ke_layout_h

#include "../../../kernel/ke_layout.h"

namespace Kernel
{
    struct DllEntry;
} // Kernel

namespace X86
{

using namespace Kernel;

//////////////////////////////////////////////////////////////////////
//
// Function
//
class Function : public Kernel::NativeCodeFunction
{
    public: static uint32 const Cookie = 0x0F0BCEF4;

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
        FrameType_NotUsed1  = Fixnum::One * 2,  // 1000
        FrameType_NotUsed2  = Fixnum::One * 3,  // 1100
        FrameType_Mask      = Fixnum::One * 3,  // 1100
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
            ASSERT(cbFrame >= 4);
            ASSERT(0 == cbFrame % 4);
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

    // GetCodeVec
    uint8* GetCodeVec() const
        { return reinterpret_cast<uint8*>(const_cast<Function*>(this) + 1); }

    // GetCodeSize
    uint GetCodeSize() const
        { return GetDesc()->m_cbCodeVec; }

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
        Int iRel = FetchS32(ofs);

        Int iCallee = reinterpret_cast<Int>(
            (GetCodeVec() + ofs + 4) + iRel );
        iCallee -= sizeof(Function);
        iCallee |= Arch::Tag_Function;
        return FromInt<Val_>(iCallee);
    } // FetchCallee

    // FetchDllEntry
    public: DllEntry* FetchDllEntry(uint ofs) const
        { return reinterpret_cast<DllEntry*>(FetchUn(ofs)); }

    // FetchExitPoint
    public: Val FetchExitPoint(uint ofs) const
        { return FetchVal(ofs); }

    // FetchS32
    public: int32 FetchS32(uint ofs) const
        { return static_cast<int32>(FetchU32(ofs)); }

    // FetchU32
    public: uint32 FetchU32(uint ofs) const
    {
        const uint8* pb = reinterpret_cast<uint8*>(GetCodeVec() + ofs);
        uint32 iVal =
            (pb[3] << 24) |
            (pb[2] << 16) |
            (pb[1] <<  8) |
            (pb[0] <<  0);
        return iVal;
    } // FetchU32

    // FetchSn
    public: Int FetchSn(uint ofs) const
        { return static_cast<Int>(FetchS32(ofs)); }

    // FetchUn
    public: UInt FetchUn(uint ofs) const
        { return static_cast<UInt>(FetchU32(ofs)); }

    // FetchVal
    public: Val FetchVal(uint ofs) const
        { return FromInt<Val_>(FetchU32(ofs)); }

    // PatchCallee
    public: void PatchCallee(uint ofs, Val fun)
    {
        Int iRel =
            fun->Decode<Function>()->GetCodeVec() - 
            reinterpret_cast<uint8*>(GetCodeVec() + ofs + 4);

        PatchU32(ofs, static_cast<uint32>(iRel));
    } // PatchCallee

    // PatchExitPoint
    public: void PatchExitPoint(uint ofs, Val val)
        { ASSERT(val->Is<Fixnum>()); PatchVal(ofs, val); }

    // PatchDllEntry
    public: void PatchDllEntry(uint ofs, DllEntry* pEntry)
        { PatchUn(ofs, reinterpret_cast<UInt>(pEntry)); }

    // PatchVal
    public: void PatchVal(uint ofs, Val val)
        { PatchUn(ofs, val->ToInt()); }

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
        { PatchU32(ofs, static_cast<uint32>(n)); }

    // PatchUn
    public: void PatchUn(uint ofs, void* pv)
        { PatchUn(ofs, reinterpret_cast<UInt>(pv)); }

    // PatchSn
    public: void PatchSn(uint ofs, Int n)
        { PatchU32(ofs, static_cast<uint32>(n)); }

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
                reinterpret_cast<Int>(m_pRunner) + pFun->GetAnnonSize() );
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

CASSERT(16 == sizeof(Function));
CASSERT( 4 == sizeof(Function::Annon));

//////////////////////////////////////////////////////////////////////
//
// Function Frames
//
// Fixed Frame:
//              +----------------+
//       ESP -> |  RA of current |
//  (low)       +----------------+
//              |    slot[0]     |
//              +----------------+
//                    ...
//              +----------------+
//              | slot[cSlots-1] |
//              +----------------+
//              |  RA of caller  |
//              +----------------+
//  (high)
//
// Restify Frame:
//              +----------------+
//       ESP -> |  RA of current |          mod 8 = 4
//  (low)       +----------------+
//              |    slot[0]     |
//              +----------------+
//                    ...
//              +----------------+
//              | slot[cSlots-1] |
//              +----------------+
//              |    RP  o-------+------+
//              +----------------+      |
//              |    pad[0]      |      |
//              +----------------+      |
//              |    rest[0]     |      |   mod 8 = 0
//              +----------------+      |
//              |    next        |      |
//              +----------------+      |
//              |    rest[1]     |      |
//              +----------------+      |
//              |    next        |      |
//              +----------------+      |
//                    ...               |
//              +----------------+      |
//              | rest[cRests-1] |      |
//              +----------------+      |
//              |    nil         |      |
//              +----------------+      |
//              |  RA of caller  |  <---+
//              +----------------+
//  (high)
//

}; // X86

namespace Kernel
{
    class FunObj : public X86::Function {};
}; // Kernel

#endif //!defined(INCLUDE_arch_x86_kernel_x86_ke_layout_h)
