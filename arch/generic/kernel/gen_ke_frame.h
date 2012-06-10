//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Generic Machine Dependent Frame
// arch/generic/kernel/gen_ke_frame.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/generic/kernel/gen_ke_frame.h#2 $
//
#if !defined(INCLUDE_arch_Generic_kernel_Gen_ke_frame_h)
#define INCLUDE_arch_Generic_kernel_Gen_ke_frame_h

#include "../../../kernel/ke_frame.h"

namespace Generic
{

using namespace Kernel;

//////////////////////////////////////////////////////////////////////
//
// XferFrame
//
class XferFrame : public Frame
{
    // public: Frame*   m_pOuter;   // [0]
    // public: Type     m_eType;    // [1]
    public: Val     m_sp;           // [2]
    public: Val     m_fn;           // [3]
    public: Val     m_ip;           // [4]
    public: Val     m_name;         // [5]
    public: Val     m_n;            // [6]
    public: Val     mv_value[3];    // [7]
                                    // [8]
                                    // [9]

    public: XferFrame(Type eType) :
        Frame(eType) {}

    public: bool IsAbandoned() const { return Fixnum::Encode(0) == m_sp; }
    public: void MakeAbandoned() { m_sp = Fixnum::Encode(0); }

    public: void __declspec(noreturn) __fastcall Transfer(Thread*);
}; // XferFrame


//////////////////////////////////////////////////////////////////////
//
// BlockFrame
//
class BlockFrame : public XferFrame
{
    public: static Type GetType_() { return Type_Block; }

    public: BlockFrame() :
        XferFrame(GetType_()) {}

    public: static uint ComputeSize()
        { return sizeof(BlockFrame); }
}; // BlockFrame


//////////////////////////////////////////////////////////////////////
//
// CatchFrame
//
class CatchFrame : public XferFrame
{
    public: static Type GetType_() { return Type_Catch; }

    public: CatchFrame() :
        XferFrame(GetType_()) {}

    public: static uint ComputeSize()
        { return sizeof(CatchFrame); }
}; // CatchFrame


//////////////////////////////////////////////////////////////////////
//
// FinallyFrame
//
class FinallyFrame : public Frame
{
    public: static Type GetType_() { return Type_Finally; }

    // public: Frame*   m_pOuter;   // [0]
    // public: Type     m_eType;    // [1]
    public: Val     m_sp;           // [2]
    public: Val     m_fn;           // [3]
    public: Val     m_n;            // [4]
    public: Val     mv_arg[1];      // [5]

    public: FinallyFrame() :
        Frame(GetType_()) {}

    public: static uint ComputeSize(uint cArgs)
    {
        return sizeof(FinallyFrame) +
            sizeof(Val) * static_cast<int>(cArgs - 1);
    } // ComputeSize

    public: void __fastcall Unwind(Thread*);
}; // FinallyFrame


//////////////////////////////////////////////////////////////////////
//
// TagbodyFrame
//
class TagbodyFrame : public Frame
{
    public: static Type GetType_() { return Type_Tagbody; }

    public: struct Tag { Val m_ofs; };

    // public: Frame*   m_pOuter;   // [0]
    // public: Type     m_eType;    // [1]
    public: Val     m_sp;           // [2]
    public: Val     m_fn;           // [3]
    public: Val     m_n;            // [4]
    public: Tag     m_rgoTag[1];    // [5]

    public: bool IsAbandoned() const { return Fixnum::Encode(0) == m_sp; }
    public: void MakeAbandoned() { m_sp = Fixnum::Encode(0); }

    public: TagbodyFrame() :
        Frame(GetType_()) {}

    public: static uint ComputeSize(UINT cTags)
    {
        return sizeof(TagbodyFrame) +
            sizeof(Val) * static_cast<int>(cTags - 1);
    } // ComputeSize

    public: bool HasTag(Tag* pTag) const
    {
        return pTag >= &m_rgoTag[0] &&
               pTag <  &m_rgoTag[Fixnum::Decode_(m_n)];
    } // HasTag

    public: void __declspec(noreturn) __fastcall Transfer(Thread*, Tag*);
}; // TagbodyFrame

}; // Generic

#endif //!defined(INCLUDE_arch_Generic_kernel_Gen_ke_frame_h)
