//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Memory Manager
// kernel/ke_memory.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_memory.h#5 $
//
#if !defined(INCLUDE_kernel_memory_h)
#define INCLUDE_kernel_memory_h

#include "./ke_arch.h"

namespace Kernel
{

class Area;
class Memory;
class ObStackArea;


//////////////////////////////////////////////////////////////////////
//
// Area
//
class Area : public AsInt
{
    friend class Memory;

    public: Area*   m_pSelf;         // [0] +0       +0
    public: Area*   m_pPrev;         // [1] +4       +8
    public: Area*   m_pNext;         // [2] +8       +16

    public: OffsetT  m_cbArea;       // [3] +12      +24
    public: OffsetT  m_nFlags;       // [4] +16      +32
    public: OffsetT  m_ofsFree;      // [5] +20      +40
    public: OffsetT  m_ofsScan;      // [6] +24      +48
    public: OffsetT  m_thread;       // [7] +28      +56

    public: enum ScanType
    {
        // Aged
        ScanType_None       = 0x00000000,

        ScanType_AgedMin    = 0x00010000,
            ScanType_BinObj     = 0x00010000,
            ScanType_Cons       = 0x00020000,   // no header
            ScanType_Function   = 0x00030000,
            ScanType_Record     = 0x00040000,
            ScanType_5          = 0x00050000,
            ScanType_6          = 0x00060000,
            ScanType_Large      = 0x00070000,
            ScanType_Weak       = 0x00080000,
        ScanType_AgedMax    = 0x0008FFFF,

        // No age
        ScanType_9          = 0x00090000,
        ScanType_DllLink    = 0x000A0000,
        ScanType_HashTable  = 0x000B0000,
        ScanType_RS         = 0x000C0000,

        // Outside of lisp heap
        ScanType_GcAnchor   = 0x000D0000,
        ScanType_ObStack    = 0x000E0000,
        ScanType_Thread     = 0x000F0000,
    }; // ScanType

    public: enum Space
    {
        ToSpace     = 0x00,
        FromSpace   = 0x80,
    }; // Direction

    public: enum Age
    {
        //Age_Max     = 127,
        Age_Max     = 31,

        Age_Static  = 31,
        Age_System  = 30,
        Age_NoAge   = 30,
        Age_ScanMax = 29,
        Age_Min     = 0,
    }; // Age

    public: enum Flags
    {
        Flags_AgeMask       = 0x0000007F,
        Flags_DirectionMask = 0x00000080,
        Flags_ScanMask      = 0x7FFF0000,
    }; // Flags

    protected: Area() {}

    public: void* operator new(size_t, void* pv) { return pv; }

    public: Area(ScanType, size_t, uint = 0);

    public: void* Alloc(size_t);

    public: static Area* GetEmpty()
    {
        static uint8 rgbArea[sizeof(Area)];
        return reinterpret_cast<Area*>(rgbArea);
    } // GetEmpty


    public: uint GetAge() const
        { return static_cast<uint>(m_nFlags & Flags_AgeMask); }
    
    public: template<class T> T* GetFree() const
        { return reinterpret_cast<T*>(GetStart() + m_ofsFree); }

    public: Area* GetNext() const
        { return m_pNext; }

    public: template<class T> T* GetTop() const
        { return reinterpret_cast<T*>(const_cast<Area*>(this) + 1); }

    public: template<class T> T* GetBtm() const
        { return reinterpret_cast<T*>(GetEnd()); }

    public: ScanType GetType() const
    {
        return static_cast<ScanType>(m_nFlags & Flags_ScanMask);
    } // GetType

    public: LPCWSTR GetTypeName() const
    {
        static const LPCWSTR k_rgwszType[16] =
        {
            L"none",        // 0
            L"binobj",      // 1
            L"cons",        // 2
            L"function",    // 3
            L"record",      // 4
            L"5",           // 5
            L"6",           // 6
            L"large",       // 7
            L"weak",        // 8

            L"9",           // 9
            L"dllcall",     // 10
            L"sxhash",      // 11
            L"rs",          // 12

            L"GcAnchor",    // 13
            L"obstack",     // 14
            L"thread",      // 15
        }; // k_rgwszType
        return k_rgwszType[GetType() >> 16];
    } // GetTypeName

    public: uint8* GetStart() const
        { return reinterpret_cast<uint8*>(const_cast<Area*>(this)); }

    public: uint8* GetEnd() const
        { return GetStart() + m_cbArea; }

    ////////////////////////////////////////////////////////////
    //
    // GC
    //
    public: template<class T> T* GetScan() const
        { return reinterpret_cast<T*>(GetStart() + m_ofsScan); }

    public: bool IsAged() const
    {
        return
            m_nFlags >= ScanType_AgedMin &&
            m_nFlags <= ScanType_AgedMax;
    } // IsAged

    // EnumObject
    class EnumObject
    {
        protected: Val* m_pval;
        protected: Val* m_pend;
        protected: uint m_nTag;
        protected: uint m_nAlign;

        public: EnumObject(Area*);

        public: bool AtEnd() const
            { return m_pend == m_pval; }

        public: Val Get() const;

        public: Val* GetPtr() const
            { ASSERT(! AtEnd()); return m_pval; }

        public: void Next();
    }; // EnumObject

    ////////////////////////////////////////////////////////////
    //
    // Customize
    //
    public: template<class T> bool Is() const
        { return T::getType_() == GetType(); }

    public: template<class T> T* StaticCast()
        { return reinterpret_cast<T*>(this); }
}; // Area


//////////////////////////////////////////////////////////////////////
//
// GcAnchor
//
struct GcAnchor
{
    Val m_value;
}; // GcAnchor

class GcAnchorArea : public Area
{
    public: static ScanType GetType_()
        { return ScanType_GcAnchor; }

    public: GcAnchorArea() :
        Area(GetType_(), Arch::Param_AllocUnit)
    {
        m_ofsScan = 0;
    } // GcAnchorArea

    public: GcAnchor* Alloc(Val);
    public: void Free(GcAnchor*);

    public: class EnumAnchor
    {
        private: GcAnchor* m_pEnd;
        private: GcAnchor* m_pRunner;

        public: EnumAnchor(Area* p) :
            m_pEnd(p->GetBtm<GcAnchor>()),
            m_pRunner(p->GetTop<GcAnchor>()) {}

        public: bool AtEnd() const { return m_pRunner == m_pEnd; }
        public: GcAnchor* Get() const { ASSERT(! AtEnd()); return m_pRunner; }
        public: void Next() { ASSERT(! AtEnd()); m_pRunner++; }
    }; // EnumAnchor
}; // GcAnchorArea


//////////////////////////////////////////////////////////////////////
//
// ObStackArea
//
class ObStackArea : public Area
{
    public: static ScanType GetType_()
        { return ScanType_ObStack; }

    public: ObStackArea(size_t cbArea = 0) :
        Area(GetType_(), cbArea) {}

    public: void* Alloc(size_t);
}; // ObStackArea


//////////////////////////////////////////////////////////////////////
//
// Memory
//
class Memory
{
    protected: static const ptrdiff_t Param_Start
        = Arch::Param_Start;

    public: static const size_t Param_AllocUnit =
        Arch::Param_AllocUnit;

    protected: static uint8* sm_pbStart;
    protected: static uint8* sm_pbEnd;
    protected: static uint8* sm_pbCommit;
    protected: static Area*  sm_pFreeArea;

    protected: static GcAnchorArea*  sm_pGcAnchorArea;

    // [A]
    public: static Area*
        AllocCodeArea(Thread*, Area::ScanType, size_t, uint = 0);

    public: static Area*
        AllocDataArea(
            Thread*,
            Area::ScanType,
            size_t = sizeof(Area),
            uint = 0 );

    public: static GcAnchor*
        AllocGcAnchor(Val);

    public: static ObStackArea*
        AllocObStackArea(size_t);

    public: static void*
        AllocThread(size_t);

    // [G]
    static void* getFreeArea(size_t);

    static GcAnchorArea* GetGcAnchorArea()
        { return sm_pGcAnchorArea; }

    // [F]
    static void FreeGcAnchor(GcAnchor*);

    // [L]
    public: static HRESULT Load(LPCWSTR);

    // [R]
    public: static void ResetWriteWatch();

    // [S]
    public: static HRESULT Save(HANDLE, size_t*);

    public: static HRESULT Save(HANDLE h)
        { size_t cb; return Save(h, &cb); }

    public: static void Start(size_t);

    // [V]
    public: static bool Verify();

    ////////////////////////////////////////////////////////////
    //
    // Allocation Statistics
    //
    public: static size_t sm_cbAllocSoFar;

    ////////////////////////////////////////////////////////////
    //
    // Area Mapping
    //
    public: static bool IsHeap(void* pv)
    {
        uint8* pb = reinterpret_cast<uint8*>(pv);
        return pb >= sm_pbStart && pb < sm_pbCommit;
    } // IsHeap

    public: static void AddArea(Area*);
    public: static void AddFreeArea(Area*);
    public: static void AddFreeArea(Thread*, Area*);

    public: static Area** sm_ppAreaMap;
    public: static Area*  sm_rgpAreaByAge[Area::Age_Max + 1];
    public: static Area*  sm_rgpRSArea[Area::Age_Max + 1];

    public: static Area* MapToArea(Val);

    public: static Area* MapToArea(void* pv)
    {
        if (! Memory::IsHeap(pv)) return NULL;
        Int idx = mapToIndex(pv);
        return sm_ppAreaMap[idx];
    } // MapToArea

    static Int mapToIndex(void* pv)
    {
        ASSERT(pv >= sm_pbStart);
        ASSERT(pv <= sm_pbEnd);
        uint8* pb = reinterpret_cast<uint8*>(pv);
        Int idx = (pb - sm_pbStart) >> 16;
        return idx;
    } // mapToIndex

    static void setAreaMap(Area*);

    // GetCommit
    public: static Area* GetCommit()
        { return reinterpret_cast<Area*>(sm_pbCommit); }

    // GetEnd
    public: static Area* GetEnd()
        { return reinterpret_cast<Area*>(sm_pbEnd); }

    // GetStart
    public: static Area* GetStart()
        { return reinterpret_cast<Area*>(sm_pbStart); }

    // EnumAllArea
    public: class EnumAreaAll
    {
        Area* m_pRunner;
        public: EnumAreaAll(Area* p) :
            m_pRunner(p) {}
        public: bool AtEnd() const
            { return m_pRunner >= GetCommit(); }
        public: Area* Get() const
            { ASSERT(! AtEnd()); return m_pRunner; }
        public: void Next()
            { m_pRunner = reinterpret_cast<Area*>(m_pRunner->GetEnd()); }
    }; // EnumAreaAll

    // EnumArea
    public: class EnumArea : public Enum_<Area>
        { public: EnumArea(Area* p) : Enum_<Area>(p) {} };

    // EnumGeneration
    public: class EnumGeneration
    {
        uint m_nAgeMax;
        uint m_nAge;
        Area* m_pRunner;

        public: EnumGeneration(uint);

        public: bool AtEnd() const
            { return NULL == m_pRunner && m_nAge == m_nAgeMax; }

        public: Area* Get() const
            { ASSERT(! AtEnd()); return m_pRunner; }

        public: void Next();
    }; // EnumGeneration
}; // Memory

} // Kernel

#endif //!defined(INCLUDE_kernel_memory_h)
