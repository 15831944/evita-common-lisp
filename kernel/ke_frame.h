//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Frame
// ke_frame.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_frame.h#7 $
//
#if !defined(INCLUDE_kernel_frame_h)
#define INCLUDE_kernel_frame_h

#include "./ke_layout.h"

namespace Kernel
{

class Area;
class ObStackArea;

class Frame;
    class BindFrame;
    class FromForeignFrame;
    class FunctionFrame;
    class GcDisableFrame;
    class ObStackFrame;
    class ToForeignFrame;

//////////////////////////////////////////////////////////////////////
//
// Base frame
//
class Frame : public AsInt
{
    public: enum Type
    {
        Type_Bind           = 0x424E4400,   // BND
        Type_Block          = 0x424C4B00,   // BLK
        Type_Catch          = 0x43415400,   // CAT
        Type_Finally        = 0x464C5900,   // FLY
        Type_FromForeign    = 0x46724600,   // FrF
        Type_Function       = 0x46756E00,   // Fun
        Type_GcDisable      = 0x47634400,   // GcD
        Type_GcRoot         = 0x47635200,   // GcR
        Type_ObStack        = 0x4F625300,   // ObS
        Type_ToKernel       = 0x4B724E00,   // KrN
        Type_Tagbody        = 0x54425900,   // TBY
        Type_ToForeign      = 0x546F4600,   // ToF
    }; // Type

    public: Frame*  m_pOuter;
        public: Frame* GetOuter() const
            { return m_pOuter; }

    public: Val m_type;
        public: Type GetType() const
            { return static_cast<Type>(m_type->ToInt()); }

    public: Frame(Type eType) :
        m_type(reinterpret_cast<Val>(eType)),
        m_pOuter(NULL) {}

    public: template<class T> T* DynamicCast()
    {
        if (! Is<T>())
        {
            return NULL;
        }
        return reinterpret_cast<T*>(this);
    } // DynamicCast

    public: template<class T> T* StaticCast()
    {
        ASSERT(Is<T>());
        return reinterpret_cast<T*>(this);
    } // StaticCast

    public: template<class T> bool Is() const
    {
        return GetType() == T::GetType_();
    } // Is
}; // Frame


//////////////////////////////////////////////////////////////////////
//
// Bind Frame
//          +--------------+
//          | m_pOuter     |  [0]   +0  +0
//          +--------------+
//          | Type_Bind    |  [1]   +4  +8
//          +--------------+
//          | m_cbFrame    |  [2]   +8  +16
//          +--------------+
//          | m_name[0]    |  [3]   +12 +20
//          +--------------+
//          | m_value[0]   |  [4]   +26 +28
//          +--------------+
//              ....
//          +--------------+
//          | m_name[k-1]  |  [k*2+3]
//          +--------------+
//          | m_value[k-1] |  [k*2+4]
//          +--------------+
//              where k is number of bindings.
//
class BindFrame : public Frame
{
    //public: Frame*    m_pOuter;       // [0]
    //public: Type*     m_eType;        // [1]
    public: size_t      m_cbFrame;      // [2]

    public: static Type GetType_() { return Type_Bind; }

    public: BindFrame(UINT cBinds) :
        Frame(GetType_())
    {
        m_cbFrame = ComputeSize(cBinds);
    } // BindFrame

    public: static uint ComputeSize(UINT cBinds)
        { return sizeof(BindFrame) + sizeof(Entry) * cBinds; }

    public: size_t GetSize() const { return m_cbFrame; }

    public: void Bind(UINT, Val, Val);
    public: void Unwind(Thread*);

    public: struct Entry
    {
        Val m_name;
        Val m_value;
    }; // Entry

    public: class Enum
    {
        protected: Entry*  m_pRunner;
        protected: Entry*  m_pEnd;

        public: Enum(BindFrame* pFrame) :
            m_pRunner(reinterpret_cast<Entry*>(pFrame + 1)),
            m_pEnd(reinterpret_cast<Entry*>(
                reinterpret_cast<BYTE*>(pFrame) + pFrame->m_cbFrame )) {}

        public: bool AtEnd() const
            { return m_pRunner >= m_pEnd; }

        public: Entry* Get() const
            { ASSERT(! AtEnd()); return m_pRunner; }

        public: void Next()
            { ASSERT(! AtEnd()); m_pRunner++; }
    }; // Enum
}; // BindFrame


//////////////////////////////////////////////////////////////////////
//
// FromForeign Frame
//
//
//              function frame
//                   of
//              lisp function
//                  ...
//          +-------------------+
//          |  RA of CallLisp   |  <= This must be placed here.
//          +-------------------+
//          | m_pOuter          |  [0]   +0  +0
//          +-------------------+
//          | Type_FromForeign  |  [1]   +4  +8
//          +------------------+
//
//  Pushed by CallLisp
//
class FromForeignFrame : public Frame
{
    public: static Type GetType_() { return Type_FromForeign; }
    private: FromForeignFrame() : Frame(GetType_()) {}
}; // FromForeignFrame


//////////////////////////////////////////////////////////////////////
//
// Function Frame
//
//          +-----------+
//          | callee RA |
//          +-----------+
//              ....      <= some implementaiton uses slots between
//              ...          RA and local[0].
//          +-----------+
//          | local[0]  | <= m_pval
//          +-----------+
//              ...
//          +-----------+
//          | caller RA | <= m_pvCallerRA
//          +-----------+
//
class FunctionFrame : public Frame
{
    public: static Type GetType_() { return Type_Function; }
    public: FunctionFrame() : Frame(GetType_()) {}

    public: Val     m_fn;
    public: Val     m_ip;
    public: Val*    m_pval;
    public: void*   m_pvRA;
    public: void*   m_pvCallerRA;
}; // FunctionFrame


//////////////////////////////////////////////////////////////////////
//
// GcDisableFrame
//
class GcDisableFrame : public Frame
{
    public: static Type GetType_() { return Type_GcDisable; }
    public: GcDisableFrame() : Frame(GetType_()) {}
}; // GcDisableFrame


//////////////////////////////////////////////////////////////////////
//
// GcRootFrame
//
class GcRootFrame : public Frame
{
    public: Val m_value;
        public: Val Get() const { return m_value; }
        public: Val Set(Val x)  { return m_value = x; }
    public: static Type GetType_() { return Type_GcRoot; }
    public: GcRootFrame() : Frame(GetType_()) {}
}; // GcRootFrame


//////////////////////////////////////////////////////////////////////
//
// ObStack Frame
//
//          +--------------+
//          | m_pOuter     |  [0]   +0  +0
//          +--------------+
//          | Type_ObStack |  [1]   +4  +8
//          +--------------+
//          | m_pArea      |  [2]   +8  +16
//          +--------------+
//          | m_ofsFree    |  [3]   +12 +20
//          +--------------+
//
class ObStackFrame : public Frame
{
    public: static Type GetType_() { return Type_ObStack; }

    public: ObStackArea*    m_pArea;    // [2]
    public: OffsetT         m_ofsFree;  // [3]

    public: ObStackFrame(ObStackArea* pArea, OffsetT ofs) :
        Frame(GetType_()),
        m_pArea(pArea),
        m_ofsFree(ofs) {}

    public: void Unwind(Thread*);
}; // ObStackFrame



//////////////////////////////////////////////////////////////////////
//
// ToForeign Frame
//
//            foreign function
//                  frame
//          +-------------------+
//          |   RA of wrapper   |
//          +-------------------+
//            ... arguments ...     Arguments for foreign function
//          +-------------------+
//          |    m_pOuter       |  [0] +0  +0
//          +-------------------+
//          |   Type_ToForeign  |  [1] +4  +8
//          +-------------------+
//          |   Size of Args    |  [2] +8  +16
//          +-------------------+
//                  ....
//          +-------------------+
//          |    RA of caller   |
//          +-------------------+
//
//  Pushed by wrapper function.
//
class ToForeignFrame : public Frame
{
    public: static Type GetType_() { return Type_ToForeign; }
    private: ToForeignFrame() : Frame(GetType_()) {}
    protected: ToForeignFrame(Type eType) : Frame(eType) {}

    public: UInt m_cbArgs;

    public: UInt* GetRaPtr() const
    {
        return reinterpret_cast<UInt*>(
            ToInt() - m_cbArgs - sizeof(UInt) );
    } // GetRaPtr
}; // ToForeignFrame


// ToKernelFrame
class ToKernelFrame : public ToForeignFrame
{
    public: static Type GetType_() { return Type_ToKernel; }
    private: ToKernelFrame() : ToForeignFrame(GetType_()) {}
}; // ToKernel

} // Kernel

#endif //!defined(INCLUDE_kernel_frame_h)
