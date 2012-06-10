#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Garbage Collector
// kernel/ke_gc.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_gc.cpp#22 $
//
#include "./ke_gc.h"

#include "./ke_executive.h"
#include "./ke_frame.h"
#include "./ke_memory.h"
#include "./ke_thread.h"
#include "./ke_weak.h"

#include "../mini/mini_lisp.h"

namespace Kernel
{

uint Gc::sm_nAgeScanMax;
Thread* Gc::sm_pThread;

Gc::ForwardCell::ForwardCell(Val x)
{
    m_cookie = QQforward_cell_marker;
    m_val = x;
} // ForwardCell


// Gc::ForwardCell::DynamicCast
Gc::ForwardCell* Gc::ForwardCell::DynamicCast(Val x)
{
    ForwardCell* p = reinterpret_cast<ForwardCell*>(x->GetPtr());
    if (p->m_cookie != QQforward_cell_marker)
    {
        return NULL;
    }

    return p;
} // Gc::ForwardCell::DynamicCast

// Gc::ForwardCell::StaticCast
Gc::ForwardCell* Gc::ForwardCell::StaticCast(Val x)
{
    ForwardCell* p = DynamicCast(x);
        ASSERT(NULL != p);
    return p;
} // Gc::ForwardCell::StaticCast

namespace
{

// forward_cell_p
static bool forward_cell_p(Val x)
    { return Gc::ForwardCell::DynamicCast(x) != NULL; }


//////////////////////////////////////////////////////////////////////
//
// FromSpace
//
class FromSpace
{
    size_t m_cbSize;
        public: size_t GetSize() const { return m_cbSize; }

    Area*  m_rgpArea[Area::Age_Max + 1];

    public: FromSpace() : m_cbSize(0)
    {
        ::memcpy(
            m_rgpArea,
            Memory::sm_rgpAreaByAge,
            sizeof(Area*) * (Gc::sm_nAgeScanMax + 1) );

        foreach (Memory::EnumGeneration, oEnum, Gc::sm_nAgeScanMax)
        {
            mark_fromspace(oEnum.Get());
        } // for each area

        VAR_Aweak_areaA->Decode<ValueCell>()->
            m_value = Fixnum::Encode(NULL);

        for (uint nAge = 0; nAge <= Gc::sm_nAgeScanMax; nAge++)
        {
            Memory::sm_rgpAreaByAge[nAge] = NULL;
        } // for age
    } // FromSpace

    public: ~FromSpace()
    {
        for (uint nAge = 0; nAge <= Gc::sm_nAgeScanMax; nAge++)
        {
            free_areas(m_rgpArea[nAge]);
        } // for each age
    } // ~FromSpace

    void free_areas(Area* pArea)
    {
        while (NULL != pArea)
        {
            GC_PRINTF(L"%p...%p %X\r\n",
                pArea,
                pArea->GetEnd(),
                pArea->m_nFlags );

            Area* pNext = pArea->m_pNext;
            Memory::AddFreeArea(pArea);
            pArea = pNext;
        } // for each area
    } // free_areas

    void mark_fromspace(Area* pArea)
    {
        m_cbSize += pArea->m_ofsFree - sizeof(Area);

        pArea->m_nFlags |= Area::FromSpace;
        GC_PRINTF(L"%p...%p %X\r\n",
            pArea,
            pArea->GetEnd(),
            pArea->m_nFlags );
    } // mark_fromspace
}; // FromSpace


//////////////////////////////////////////////////////////////////////
//
// ToSpace
//
class ToSpace
{
    enum AllocType
    {
        AllocType_BinObj    = 0,
        AllocType_Cons      = 1,
        AllocType_Function  = 2,
        AllocType_Record    = 3,
        AllocType_Symbol    = 4,

        AllocType_Max_1,
    }; // AllocType

    static size_t sm_cbAlloc;
        public: static size_t GetSize() { return sm_cbAlloc; }

    static Area* sm_rgpArea[Area::Age_Max][AllocType_Max_1];

    public: ToSpace()
    {
        for (uint nAge = 1; nAge < Area::Age_Max; nAge++)
        {
            for (uint nAlloc = 0; nAlloc < AllocType_Max_1; nAlloc++)
            {
                sm_rgpArea[nAge][nAlloc] = Area::GetEmpty();
            } // for alloc
        } // for age
    } // ToSpace

    public: ~ToSpace()
    {
        foreach (Memory::EnumGeneration, oEnum, Gc::sm_nAgeScanMax)
        {
            Area* pArea = oEnum.Get();
            GC_PRINTF(L" reset area %p\r\n", pArea);
            ASSERT(pArea->m_ofsScan == pArea->m_ofsFree);
            pArea->m_ofsScan = sizeof(Area);
        } // for each area
    } // ~ToSpace

    public: static void* Alloc(
        const Area* pFromArea,
        size_t      cbObject )
    {
        uint nAge = pFromArea->GetAge() + 1;
            ASSERT(nAge < Area::Age_Max);

        sm_cbAlloc += cbObject;

        AllocType eAlloc;
        {
            switch (pFromArea->GetType())
            {
            case Area::ScanType_BinObj:
                eAlloc = AllocType_BinObj;
                break;

            case Area::ScanType_Cons:
                eAlloc = AllocType_Cons;
                break;

            case Area::ScanType_Function:
                eAlloc = AllocType_Function;
                break;

            case Area::ScanType_Record:
                eAlloc = AllocType_Record;
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch area type
        } // eAlloc

        Area* pArea = sm_rgpArea[nAge][eAlloc];

        for (;;)
        {
            void* pv = pArea->Alloc(cbObject);
            if (NULL != pv)
            {
                return pv;
            } // if

            pArea = getArea(pFromArea->GetType(), nAge, cbObject);

            sm_rgpArea[nAge][eAlloc] = pArea;

            GC_PRINTF(L"%p %06x from.flags=%06x\r\n",
                pArea, pArea->m_nFlags,
                pFromArea->m_nFlags );
        } // for
    } // Alloc

    // getArea
    static Area* getArea(Area::ScanType eType, uint nAge, size_t cbObject)
    {
        if (nAge > Gc::sm_nAgeScanMax)
        {
            foreach (
                Memory::EnumArea,
                oEnum,
                Memory::sm_rgpAreaByAge[nAge] )
            {
                Area* pArea = oEnum.Get();
                if (pArea->GetType() == eType &&
                    pArea->m_ofsFree + cbObject <= pArea->m_cbArea )
                {
                    return pArea;
                }
            } // for each area
        } // if

        Area* pArea;

        if (Area::ScanType_Function == eType)
        {
            pArea = Memory::AllocCodeArea(
                Gc::sm_pThread,
                eType,
                cbObject,
                nAge );
        }
        else
        {
            pArea = Memory::AllocDataArea(
                Gc::sm_pThread,
                eType,
                cbObject,
                nAge );
        } // if

        return pArea;
    } // getArea
}; // ToSpace

size_t ToSpace::sm_cbAlloc;
Area* ToSpace::sm_rgpArea[Area::Age_Max][AllocType_Max_1];


//////////////////////////////////////////////////////////////////////
//
// RsManager
//
// Description:
//  A manager of old-to-young pointer hash-table or RS area.
//
// Note: RS stands for remembered set.
//
class RsManager
{
    static Area* sm_rgpRSArea[Area::Age_Max + 1];

    public: static Area* Get(uint nAge)
        { return sm_rgpRSArea[nAge]; }

    public: static void Reset(uint nAge)
        { sm_rgpRSArea[nAge] = NULL; }

    public: RsManager()
    {
        ASSERT(sizeof(sm_rgpRSArea) == sizeof(Memory::sm_rgpRSArea));


        for (uint nAge = 1; nAge <= Gc::sm_nAgeScanMax; nAge += 1)
        {
            Area* pArea = Memory::sm_rgpRSArea[nAge];
            if (NULL != pArea) Memory::AddFreeArea(pArea);
        } // for nAge

        for (
            uint nAge = Gc::sm_nAgeScanMax + 1;
            nAge <= Area::Age_Max;
            nAge += 1 )
        {
            sm_rgpRSArea[nAge] = Memory::sm_rgpRSArea[nAge];
        } // for nAge

        ::ZeroMemory(
            Memory::sm_rgpRSArea,
            sizeof(Memory::sm_rgpRSArea) );

        #if _DEBUG
        {
            for (uint n = 0; n < lengthof(sm_rgpRSArea); n++)
            {
                Area* pArea = sm_rgpRSArea[n];
                if (NULL != pArea) Verify(pArea);
            } // for n
        }
        #endif // _DEBUG
    } // RsManager

    // Find
    public: static bool Find(Val* pval)
    {
        Val x = Fixnum::Encode(pval);

        Area* pArea = Memory::MapToArea(pval);
        Area* pRS = Memory::sm_rgpRSArea[pArea->GetAge()];
        Int iHashCode = hash(x);

        Entry* pTop    = pRS->GetTop<Entry>();
        Entry* pEnd    = pRS->GetBtm<Entry>();
        Entry* pStart  = pTop + iHashCode % (pEnd - pTop);
        Entry* pRunner = pStart;

        do
        {
            if (x == pRunner->m_val) return true;
            if (Fixnum::Encode(0) == pRunner->m_val) return false;

            pRunner++;

            if (pRunner >= pEnd) pRunner = pTop;
        } while (pRunner != pStart);
        return false;
    } // Find

    // Register
    public: static void Register(uint nAge, Val x)
    {
        Area* pArea = Memory::sm_rgpRSArea[nAge];
        if (NULL == pArea)
        {
            pArea = allocRS(nAge, sizeof(Entry));
        }
        else if (pArea->m_ofsFree * 70 > pArea->m_cbArea * 100 )
        {
            pArea = rehash(nAge, pArea);
        } // if

        put(pArea, x);
    } // Register

    // Verify
    public: static void Verify(Area* pArea)
    {
        ASSERT(Area::ScanType_RS == pArea->GetType());
        size_t cb = sizeof(Area);
        for (
            Val* pRunner = pArea->GetTop<Val>();
            pRunner < pArea->GetBtm<Val>();
            pRunner++ )
        {
            if (Fixnum::Encode(0) != *pRunner) cb += sizeof(Val);
        } // for

        if (cb != pArea->m_ofsFree)
        {
            Debugger::Fail(
                L"RS count mismatched\r\n"
                L"  pArea   %p\r\n"
                L"  expect  %u\r\n"
                L"  got     %u\r\n",
                pArea,
                pArea->m_ofsFree,
                cb );
        } // if
    } // Verify

    // Entry
    public: struct Entry
    {
        Val m_val;
    }; // Entry

    public: class EnumEntry
    {
        size_t  m_cb;
        Entry*  m_pRunner;
        Entry*  m_pEnd;

        public: EnumEntry(Area* pArea) :
            m_cb(pArea->m_ofsFree - sizeof(Area)),
            m_pRunner(pArea->GetTop<Entry>()),
            m_pEnd(pArea->GetBtm<Entry>())
        {
            if (! AtEnd()) next();
        } // EnumEntry

        public: bool AtEnd() const { return 0 == m_cb; }
        public: Entry* Get() const { ASSERT(! AtEnd()); return m_pRunner; }

        public: void Next()
        {
            ASSERT(! AtEnd());
            m_cb -= sizeof(Entry);
            if (! AtEnd())
            {
                m_pRunner++;
                next();
            }
        } // Next

        void next()
        {
            while (Fixnum::Encode(0) == m_pRunner->m_val)
            {
                ASSERT(m_pRunner < m_pEnd);
                m_pRunner++;
            } // while
        } // next
    }; // EnumEntry

    // allocRS - allocates RS area.
    static Area* allocRS(uint nAge, size_t cb)
    {
        Area* pArea = Memory::AllocDataArea(
            Gc::sm_pThread,
            Area::ScanType_RS,
            cb,
            nAge );

        return Memory::sm_rgpRSArea[nAge] = pArea;
    } // allocRS

    // hash
    static Int hash(Val x)
    {
        Int iHashCode = x->ToInt();
        iHashCode = (iHashCode >> 16) ^ (iHashCode & 0xFFFF);
        return iHashCode;
    } // hash

    // put
    static void put(Area* pArea, Val x)
    {
        ASSERT(0 != x);

        Int iHashCode = hash(x);

        Entry* pTop    = pArea->GetTop<Entry>();
        Entry* pEnd    = pArea->GetBtm<Entry>();
        Entry* pStart  = pTop + iHashCode % (pEnd - pTop);
        Entry* pRunner = pStart;

        do
        {
            if (x == pRunner->m_val)
            {
                return;
            }

            if (Fixnum::Encode(0) == pRunner->m_val)
            {
                pRunner->m_val = x;

                pArea->m_ofsFree += sizeof(Entry);
                return;
            } // if

            pRunner++;

            if (pRunner >= pEnd) pRunner = pTop;
        } while (pRunner != pStart);
        CAN_NOT_HAPPEN();
    } // put

    // rehash
    static Area* rehash(uint nAge, Area* pOldArea)
    {
        Area* pArea = allocRS(nAge, pOldArea->m_cbArea + sizeof(Entry));

        foreach (EnumEntry, oEnum, pOldArea)
        {
            Entry* pEntry = oEnum.Get();
            put(pArea, pEntry->m_val);
        } // for each entry

        Memory::AddFreeArea(pOldArea);

        return pArea;
    } // rehash
}; // RsManager

Area* RsManager::sm_rgpRSArea[Area::Age_Max + 1];


//////////////////////////////////////////////////////////////////////
//
// Verify
//
class Verify
{
    bool m_fVerify;

    public: Verify() :
        m_fVerify(true)
    {
        verify_heap();

        foreach (Executive::EnumThread, oEnum, Executive::Get())
        {
            verify_thread(oEnum.Get());
        } // for each thread
    } // Verify

    // verify_heap
    void verify_heap()
    {
        foreach (Memory::EnumAreaAll, oEnum, Memory::GetStart())
        {
            Area* pArea = oEnum.Get();
            switch (pArea->GetType())
            {
            case Area::ScanType_Cons:
                verify_area(pArea);
                break;

            case Area::ScanType_Function:
                foreach (Area::EnumObject, oEnum, pArea)
                {
                    Val x = oEnum.Get();

                    ASSERT(x->Decode<Funcallable>()->m_classd->Is<ClassD>());
                } // for each object
                break;

            case Area::ScanType_Record:
                foreach (Area::EnumObject, oEnum, pArea)
                {
                    Val x = oEnum.Get();

                    //GC_PRINTF(L"%p cb=%u\r\n", x, x->GetSize());

                    ASSERT(x->Is<Record>());
                    ASSERT(x->Decode<Record>()->m_classd->Is<ClassD>());
                } // for each object
                verify_area(pArea);
                break;

            case Area::ScanType_RS:
                RsManager::Verify(pArea);
                break;
            } // switch area type
        } // for each area
    } // Verify

    // verify_area
    void verify_area(Area* pArea)
    {
        foreach (Area::EnumObject, oEnum, pArea)
        {
            Val x = oEnum.Get();
            Val* pstart = reinterpret_cast<Val*>(x->GetPtr());
            Val* pend   = pstart + x->GetSize() / sizeof(Val);

            char16 wsz[100];

            for (Val* pRunner = pstart; pRunner < pend; pRunner++)
            {
                ::wsprintf(wsz, L"Area %p", pArea);

                Area* pObjArea = verify_cell(pRunner, wsz);
                    if (NULL == pObjArea) continue;

                if (pArea->GetAge() > pObjArea->GetAge())
                {
                    if (! RsManager::Find(pRunner))
                    {
                        Debugger::Fail(
                            L"Old-to-Young pointer isn't in RS:\r\n"
                            L"  pArea    %p %06X\r\n"
                            L"  pCell    %p\r\n"
                            L"  pObjArea %p %06X\r\n"
                            L"  obj      %p\r\n",
                            pArea, pArea->m_nFlags,
                            pRunner,
                            pObjArea, pObjArea->m_nFlags,
                            *pRunner );
                    }
                } // if
            } // for each slot
        } // for each object
    } // verify_area

    // verify_cell
    Area* verify_cell(Val* pCell, const char16* pwsz)
    {
        Area* pObjArea = Memory::MapToArea(*pCell);
            if (NULL == pObjArea) return NULL;

        if (pObjArea->GetType() == Area::ScanType_None)
        {
            Debugger::Fail(
                L"Point to free area:\r\n"
                L"  %s\r\n"
                L"  pCell    %p\r\n"
                L"  pObjArea %p %06X\r\n"
                L"  obj      %p\r\n",
                pwsz,
                pCell,
                pObjArea, pObjArea->m_nFlags,
                *pCell );
        } // if

        return pObjArea;
    } // verify_cell

    // verify_range
    void verify_range(Val* pStart, Val* pEnd, const char16* pwsz)
    {
        for (Val* pRunner = pStart; pRunner < pEnd; pRunner++)
        {
            verify_cell(pRunner, pwsz);
        } // for each cell
    } // verify_range

    // verify_thread
    void verify_thread(Thread* pThread)
    {
        char16 wsz[100];

        ::wsprintf(wsz, L"thread.mv_value %p", pThread);

        for (uint i = 0; i < lengthof(pThread->mv_value); i++)
        {
            ::wsprintf(wsz, L"thread.mv_value %p %d/%d",
                pThread, i, lengthof(pThread->mv_value) );

            verify_cell(&pThread->mv_value[i], wsz);
        } // for i

        {
            Int cTlvs = Fixnum::Decode_(
                svref(VAR(Atlv_vectorA), Fixnum::Encode(0)) );

            for (Int i = 1; i <= cTlvs; i++)
            {
                ::wsprintf(wsz, L"thread.mv_tlv %p %d/%d",
                    pThread, i, cTlvs );

                verify_cell(&pThread->mv_tlv[i], wsz);
            } // for i
        }
    } // verify_thread
}; // Verify


//////////////////////////////////////////////////////////////////////
//
// is_gc_ready
//
static bool is_gc_ready(Thread* pRunner)
{
    foreach (Thread::EnumFrame, oEnum, pRunner)
    {
        Frame* pFrame = oEnum.Get();
        switch (pFrame->GetType())
        {
        case Frame::Type_GcDisable:
        case Frame::Type_ToForeign:
            return false;
        } // switch type
    } // for each frame

    return true;
} // is_gc_ready


// is_area_modified
static bool is_area_modified(Area* pArea)
{
    switch (pArea->GetType())
    {
    case Area::ScanType_Cons:
    case Area::ScanType_Function:
    case Area::ScanType_Record:
        break;

    default:
        return false;
    } // switch area type

    for (
        uint8* pbRunner = pArea->GetStart();
        pbRunner < pArea->GetEnd();
        pbRunner += Memory::Param_AllocUnit )
    {
        void*     rgpvWritten[16];
        ULONG_PTR cWrittens = lengthof(rgpvWritten);
        ULONG     cbPageSize;

        UINT nRet = ::GetWriteWatch(
            0,
            pbRunner,
            Memory::Param_AllocUnit,
            rgpvWritten,
            &cWrittens,
            &cbPageSize );
        if (0 != nRet)
        {
            Debugger::Fail(L"GetWriteWatch");
        }

        GC_PRINTF(L"  %p %06X %p cWrittens=%u cbPageSize=%u\r\n",
            pArea,
            pArea->m_nFlags,
            pbRunner,
            cWrittens,
            cbPageSize );

        if (cWrittens >= 1) return true;
    } // for

    return false;
} // is_area_modified

} // namespace


// age_of
int Gc::age_of(void* pv)
    { return Memory::MapToArea(pv)->GetAge(); }


// Gc::from_space_p
bool Gc::from_space_p(Val x)
{
    Area* pArea = Memory::MapToArea(x);
    if (NULL == pArea) return false;
    if (pArea->GetAge() > Gc::sm_nAgeScanMax) return false;
    return 0 != (pArea->m_nFlags & Area::FromSpace);
} // from_space_p


//////////////////////////////////////////////////////////////////////
//
// Memory::EnumGeneration::EnumGeneration
//
Memory::EnumGeneration::EnumGeneration(uint nAgeMax) :
    m_nAgeMax(nAgeMax),
    m_nAge(0)
{
    m_pRunner = sm_rgpAreaByAge[m_nAge];
    while (NULL == m_pRunner)
    {
        if (m_nAgeMax == m_nAge) break;
        m_nAge += 1;
        m_pRunner = sm_rgpAreaByAge[m_nAge];
    } // while
} // EnumGeneration


//////////////////////////////////////////////////////////////////////
//
// Memory::EnumGeneration::Next
//
void Memory::EnumGeneration::Next()
{
    ASSERT(! AtEnd());

    m_pRunner = m_pRunner->m_pNext;
    while (NULL == m_pRunner)
    {
        if (m_nAgeMax == m_nAge) break;
        m_nAge += 1;
        m_pRunner = sm_rgpAreaByAge[m_nAge];
    } // while
} // Next


//////////////////////////////////////////////////////////////////////
//
// Gc::IsGarbage
//
bool Gc::IsGarbage(Val x)
{
    Area* pArea = Memory::MapToArea(x);
    if (NULL == pArea)
    {
        return false;
    }

    if (pArea->GetAge() > sm_nAgeScanMax)
    {
        return false;
    }

    if (0 == (pArea->m_nFlags & Area::FromSpace))
    {
        return false;
    }

    return NULL == ForwardCell::DynamicCast(x);
} // Gc::IsGarbage


//////////////////////////////////////////////////////////////////////
//
// Gc::phaseScanGcAnchor
//
void Gc::phaseScanGcAnchor()
{
    GC_PRINTF(L"scan Gc Anchor\r\n");

    foreach (Memory::EnumArea, oEnum, Memory::GetGcAnchorArea())
    {
        Area* pArea = oEnum.Get();
        foreach (GcAnchorArea::EnumAnchor, oEnum, pArea)
        {
            GcAnchor* pGcAnchor = oEnum.Get();
            pGcAnchor->m_value = moveObject(pGcAnchor->m_value);
        } // for each anchor
    } // for each area
} // Gc::phaseScanGcAnchor


//////////////////////////////////////////////////////////////////////
//
// Gc::phaseScanTospace
//
void Gc::phaseScanTospace()
{
    GC_PRINTF(L"scan tospace\r\n");

    bool fMore = true;
    while (fMore)
    {
        fMore = false;

        foreach (Memory::EnumGeneration, oEnum, sm_nAgeScanMax + 1)
        {
            Area* pArea = oEnum.Get();
                ASSERT(0 == (pArea->m_nFlags & Area::FromSpace));

            if (pArea->m_ofsScan < pArea->m_ofsFree)
            {
                fMore = true;
                scanArea(pArea);
            } // for each object
        } // for each area
    } // while
} // Gc::phaseScanTospace


//////////////////////////////////////////////////////////////////////
//
// Gc::phaseScanRS
//
void Gc::phaseScanRS()
{
    for (uint nAge = sm_nAgeScanMax + 1; nAge <= Area::Age_Max; nAge += 1)
    {
        Area* pArea = RsManager::Get(nAge);
        if (NULL == pArea) continue;

        ASSERT(Area::ScanType_RS == pArea->GetType());

        GC_PRINTF(L" RS %p %X count=%u\r\n",
            pArea,
            pArea->m_nFlags,
            (pArea->m_ofsFree - sizeof(Area)) / sizeof(RsManager::Entry) );

        foreach (RsManager::EnumEntry, oEnum, pArea)
        {
            RsManager::Entry* pEntry = oEnum.Get();
            if (pEntry->m_val->Is<Fixnum>())
            {
                Val* pval = pEntry->m_val->StaticCast<Val>();
                updateCell(nAge, pval);
            }
            else if (pEntry->m_val->Is<Funcallable>())
            {
                scanFunObj(nAge, pEntry->m_val);
            }
            else
            {
                CAN_NOT_HAPPEN();
            }
        } // for each entry

        Memory::AddFreeArea(pArea);
        RsManager::Reset(nAge);
    } // for each age
} // Gc::phaseScanRS


//////////////////////////////////////////////////////////////////////
//
// Gc::phaseUpdateOld
//
// See also: Gc::UpdateRS
//
// Description:
//  Scans updated old generation.
//
void Gc::phaseUpdateOld()
{
    for (uint nAge = sm_nAgeScanMax + 1; nAge <= Area::Age_Max; nAge += 1)
    {
        foreach (Memory::EnumArea, oEnum, Memory::sm_rgpAreaByAge[nAge])
        {
            Area* pArea = oEnum.Get();

            if (! is_area_modified(pArea)) continue;

            GC_PRINTF(L" Area %p...%p %X is modified.\r\n",
                pArea->GetStart(),
                pArea->GetEnd(),
                pArea->m_nFlags );

            pArea->m_ofsScan = sizeof(Area);
            scanArea(pArea);
        } // for each area
    } // for each age

    Memory::ResetWriteWatch();
} // Gc::phaseUpdateOld


//////////////////////////////////////////////////////////////////////
//
// Gc::remember
//
void Gc::remember(Val* pval)
    { Gc::remember(age_of(pval), pval); }

void Gc::remember(uint nAge, Val* pval)
{
    if (IsOldToYoung(nAge, *pval))
    {
        RsManager::Register(nAge, Fixnum::Encode(pval));
    }
} // Gc::remember


//////////////////////////////////////////////////////////////////////
//
// Gc::rememberFunObj
//
void Gc::rememberFunObj(uint nAge, Val fn)
    { RsManager::Register(nAge, fn); }


//////////////////////////////////////////////////////////////////////
//
// Gc::resolveForward
//
Val Gc::resolveForward(Val x)
{
    Area* pArea = Memory::MapToArea(x);
    if (NULL == pArea)
    {
        return x;
    }

    ForwardCell* pForward = ForwardCell::DynamicCast(x);
    if (NULL != pForward)
    {
        return pForward->Get();
    }

    return x;
} // Gc::resolveForward


//////////////////////////////////////////////////////////////////////
//
// Gc::Run
//
bool Gc::Run(uint nAgeScanMax, size_t* out_cbBefore, size_t* out_cbAfter)
{
    foreach (Executive::EnumThread, oEnum, Executive::Get())
    {
        Thread* pThread = oEnum.Get();
        if (! is_gc_ready(pThread))
        {
            *out_cbBefore = *out_cbAfter = 0;
            return false;
        }
    } // for each thread

    GC_PRINTF(L"===== START ========================================\r\n");

    #if GC_DEBUG
    {
        foreach (Thread::EnumFrame, oEnum, Thread::Get())
        {
            FunctionFrame* pFrame = oEnum.Get()->DynamicCast<FunctionFrame>();
            if (NULL == pFrame) continue;

            dbg_format(L"~X~X ~S +~X~%",
                Fixnum::Encode(reinterpret_cast<Int>(pFrame) >> 4),
                Fixnum::Encode(reinterpret_cast<Int>(pFrame) & 15),
                pFrame->m_fn,
                pFrame->m_ip );
        } // for each frame
    }
    #endif

    Memory::Verify();

    foreach (Executive::EnumThread, oEnum, Executive::Get())
    {
        Thread* pThread = oEnum.Get();
        pThread->ResetAlloc();

        for (
            Area* pArea = pThread->m_pObStackArea;
            NULL != pArea;
            pArea = pArea->m_pNext )
        {
            pArea->m_ofsScan = sizeof(Area);
        } // for each area
    } // for each thread

    sm_pThread = Thread::Get();
    sm_nAgeScanMax = min(max(nAgeScanMax, 3), Area::Age_ScanMax);

    {
        RsManager  oRsManager;
        FromSpace  oFromSpace;
        ToSpace    oToSpace;

        *out_cbBefore = oFromSpace.GetSize();

        foreach (Executive::EnumThread, oEnum, Executive::Get())
        {
            Thread* pThread = oEnum.Get();
            updateThread(pThread);
        } // for each thread

        phaseScanGcAnchor();
        phaseScanRS();
        phaseUpdateOld();

        phaseScanTospace();

        foreach (Memory::EnumGeneration, oEnum, Area::Age_Max)
        {
            Area* pArea = oEnum.Get();
                ASSERT(0 == (pArea->m_nFlags & Area::FromSpace));

            if (pArea->GetType() == Area::ScanType_Weak)
            {
                scanWeakArea(pArea);
            }
        } // for each area

        phaseScanTospace();

        fixObjTab();

        *out_cbAfter = oToSpace.GetSize();
    }

    Memory::sm_cbAllocSoFar = 0;

    Verify();

    phaseInvokeFinalization();

    GC_PRINTF(L"===== END  ========================================\r\n");

    return true;
} // Gc::Run


//////////////////////////////////////////////////////////////////////
//
// Gc::scanArea
//
void Gc::scanArea(Area* pArea)
{
    GC_PRINTF(L"scanArea %p %X %d/%d\r\n",
        pArea,
        pArea->m_nFlags,
        pArea->m_ofsScan,
        pArea->m_ofsFree );

    class EnumObject_Scan : public Area::EnumObject
    {
        public: EnumObject_Scan(Area* pArea) :
            Area::EnumObject(pArea)
        {
            ASSERT(pArea->GetTop<Val>() == m_pval);
            m_pval = pArea->GetScan<Val>();
        } // EnumObject_Scan
    }; // EnumObject_Scan

    OffsetT ofsFree = pArea->m_ofsFree;

    uint nAge = pArea->GetAge();
    switch (pArea->GetType())
    {
    case Area::ScanType_Cons:
        updateRange(nAge, pArea->GetScan<Val>(), pArea->GetFree<Val>());
        break;

    case Area::ScanType_BinObj:
        // nothing to do
        break;

    case Area::ScanType_Function:
        foreach (EnumObject_Scan, oEnum, pArea)
        {
            scanFunObj(nAge, oEnum.Get());
        } // for each object
        break;

    case Area::ScanType_ObStack:
        foreach (EnumObject_Scan, oEnum, pArea)
        {
            scanObject(nAge, oEnum.Get());
        } // for each object
        break;

    case Area::ScanType_Record:
        foreach (EnumObject_Scan, oEnum, pArea)
        {
            scanObject(nAge, oEnum.Get());
        } // for each object
        break;

    case Area::ScanType_Weak:
        //scanWeakArea(pArea);
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch area type

    pArea->m_ofsScan = ofsFree;
} // Gc::scanArea


//////////////////////////////////////////////////////////////////////
//
// Gc::scanObject
//
// BUGBUG: NYI: Foramt_Mixed and Format_InstanceMixed.
//
void Gc::scanObject(uint nAge, Val x)
{
    switch (x->GetTag4())
    {
    case_Tag_Record:
    {
        Record* p = x->Decode<Record>();

        ClassD* pClassD = updateCell(nAge, &p->m_classd)->
            Decode<ClassD>();

        switch (pClassD->m_format->ToInt())
        {
        case ClassD::Format_BinVec:
        case ClassD::Format_String:
        case ClassD::Format_BinFixed:
            break;

        default:
            updateRange(
                nAge,
                x->Decode<Record>(),
                reinterpret_cast<Val*>(
                    x->Decode<Record>()->ToInt() + getObjectSize(x) ) );
            break;
        } // switch format
        break;
    } // record

    case Val_::Tag_Function:
        scanFunObj(nAge, x);
        break;

    case_Tag_Cons:
        updateRange(
            nAge,
            x->Decode<Cons>(),
            reinterpret_cast<Val*>(
                x->Decode<Cons>()->ToInt() + sizeof(Cons) ) );
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch tag
} // Gc::scanObject


//////////////////////////////////////////////////////////////////////
//
// Gc::updateRange
//
void Gc::updateRange(Val* pStart, Val* pEnd)
{
    //GC_PRINTF(L"  %p...%p\r\n", pStart, pEnd);

    for (Val* pRunner = pStart; pRunner < pEnd; pRunner++)
    {
        *pRunner = moveObject(*pRunner);
    } // for
} // updateRange


//////////////////////////////////////////////////////////////////////
//
// Gc::updateThread
//
void Gc::updateThread(Thread* pThread)
{
    GC_PRINTF(L"%p\r\n", pThread);

    beforeUpdateThread(pThread);

    pThread->m_fn = moveObject(pThread->m_fn);

    {
        int n = static_cast<int>(Fixnum::Decode_(pThread->m_n));

        updateRange(
            &pThread->mv_value[0],
            &pThread->mv_value[n] );

        ::ZeroMemory(
            &pThread->mv_value[n],
            sizeof(Val) * (lengthof(pThread->mv_value) - n) );
    }

    Int cTlvs;
    {
        Val tlv_vector = resolveForward(
            resolveForward(VAR_Atlv_vectorA)->Decode<ValueCell>()->
                m_value );

        cTlvs = Fixnum::Decode_(
            tlv_vector->Decode<SimpleVector>()->mv_element[0] );
    } // cTlvs

    GC_PRINTF(L" update TLV #tlvs=%d\r\n", cTlvs);

    updateRange(
        &pThread->mv_tlv[1],
        &pThread->mv_tlv[cTlvs+1] );

    foreach (Thread::EnumFrame, oEnum, pThread)
    {
        Frame* pFrame = oEnum.Get();

        #if GC_DEBUG
        {
            GC_PRINTF(L"%p %c%c%c outer=%p\r\n",
                pFrame->Is<FunctionFrame>() ?
                    pFrame->StaticCast<FunctionFrame>()->m_pvRA :
                    pFrame,
                (pFrame->GetType() >> 24) & 255,
                (pFrame->GetType() >> 16) & 255,
                (pFrame->GetType() >>  8) & 255,
                pFrame->GetOuter() );
        }
        #endif // GC_DEBUG

        if (updateFrame(pThread, pFrame)) continue;

        switch (pFrame->GetType())
        {
        case Frame::Type_GcRoot:
            pFrame->StaticCast<GcRootFrame>()->m_value =
                moveObject(pFrame->StaticCast<GcRootFrame>()->m_value);
            break;
        } // switch type
    } // for each frame

    // BUGBUG: We should do mark-and-sweep for object stack instead
    // of area scan.
    GC_PRINTF(L" update obstack\r\n", pThread);

    foreach (Memory::EnumArea, oEnum, pThread->m_pObStackArea)
    {
        scanArea(oEnum.Get());
    } // for each area

    afterUpdateThread(pThread);
} // Gc::updateThread


//////////////////////////////////////////////////////////////////////
//
// getObjectSize
//
// Note: We can't use Val_::GetSize, since classd may be forwarded.
//
size_t Gc::getObjectSize(Val x)
{
    switch (x->GetTag4())
    {
    case_Tag_Record:
    {
        Record* p = x->Decode<Record>();
        ClassD* pClassD = resolveForward(p->m_classd)->Decode<ClassD>();
        return pClassD->ComputeSize(p);
    } // record

    case Val_::Tag_Function:
        return x->Decode<Funcallable>()->m_cbFunction;

    case_Tag_Cons:
        return sizeof(Cons);

    default:
        CAN_NOT_HAPPEN();
    } // switch tag
} // Gc::getObjectSize


//////////////////////////////////////////////////////////////////////
//
// Gc::updateCell
//
Val Gc::updateCell(uint nAge, Val* pval)
{
    Val obj = moveObject(*pval);
    *pval = obj;
    remember(nAge, pval);
    return obj;
} // Gc::updateCell


//////////////////////////////////////////////////////////////////////
//
// Gc::updateRange
//
void Gc::updateRange(uint nAge, Val* pStart, Val* pEnd)
{
    //GC_PRINTF(L"  %p...%p\r\n", pStart, pEnd);

    for (Val* pRunner = pStart; pRunner < pEnd; pRunner++)
    {
        updateCell(nAge, pRunner);
    } // for
} // Gc::updateRange


// move_function
static Val move_function(Area* pArea, Val x, void* pvx)
{
    size_t cb = x->Decode<Funcallable>()->m_cbFunction;

    void* pvy = ToSpace::Alloc(pArea, cb);
        ::memcpy(pvy, pvx, cb);

    Int tag = x->ToInt() - reinterpret_cast<Int>(pvx);
    Val y = reinterpret_cast<Val>(reinterpret_cast<Int>(pvy) + tag);

    // Update relative addresses in function object.
    Gc::prepareFunObj(y, x);

    new(pvx) Gc::ForwardCell(y);
    return y;
} // move_function


// move_instance
//  Moves instance and storage.
static Val move_instance(Area* pArea, Val x, size_t cb)
{
    Instance* px  = x->Decode<Instance>();

    Val y;

    if (forward_cell_p(px->m_storage))
    {
        // A storage of instance is used by another.
        cb = sizeof(Instance);
        void* pvy = ToSpace::Alloc(pArea, cb);
            ::memcpy(pvy, px, cb);

        y = reinterpret_cast<Instance*>(pvy)->Encode();
    }
    else
    {
        void* pvy = ToSpace::Alloc(pArea, cb);
            ::memcpy(pvy, px, sizeof(Instance));

        Instance* py  = reinterpret_cast<Instance*>(pvy);

        Storage*  pxs = reinterpret_cast<Storage*>(px->m_storage->GetPtr());
        Storage*  pys = reinterpret_cast<Storage*>(py + 1);
            ::memcpy(pys, pxs, cb - sizeof(Instance));

        py->m_storage = pys->Encode();

        new(pxs) Gc::ForwardCell(pys->Encode());

        y = py->Encode();
    }

    return y;
} // move_instance


// move_list
//  For improving memory access locality, we move list once.
//  Conses in list must be a same age.
#if 1
static Val move_list(Area* pArea, Val x)
{
    Cons* py = reinterpret_cast<Cons*>(ToSpace::Alloc(pArea, sizeof(Cons)));

    Val y = py->Encode();

    int nAge = pArea->GetAge();

    Cons* px = x->Decode<Cons>();

    for (;;)
    {
        py->m_car = px->m_car;
        Val next  = px->m_cdr;
        py->m_cdr = next;

        new(px) Gc::ForwardCell(py->Encode());

        if (! consp(next)) break;

        px = next->Decode<Cons>();

        Gc::ForwardCell* p = Gc::ForwardCell::DynamicCast(next);

        if (NULL != p)
        {
            py->m_cdr = p->Get();
            break;
        }

        if (Gc::age_of(px) != nAge) break;

        Cons* pNext = reinterpret_cast<Cons*>(
            ToSpace::Alloc(pArea, sizeof(Cons)) );

        py->m_cdr = pNext->Encode();
        py = pNext;
    } // for

    return y;
} // move_list
#else
static Val move_list(Area* pArea, Val x)
{
    Cons* py = reinterpret_cast<Cons*>(ToSpace::Alloc(pArea, sizeof(Cons)));

    Val y = py->Encode();


    Cons* px = x->Decode<Cons>();

    py->m_car = px->m_car;
    py->m_cdr = px->m_cdr;

    new(px) Gc::ForwardCell(py->Encode());

    return y;
} // move_list
#endif


// move_record
static Val move_record(Area* pArea, Val x, void* pvx)
{
    Record* p = x->Decode<Record>();
    ClassD* pClassD = Gc::resolveForward(p->m_classd)->Decode<ClassD>();

    Val y;

    if (pClassD->m_format->ToInt() == ClassD::Format_Instance)
    {
        y = move_instance(pArea, x, pClassD->ComputeSize());
    }
    else if (Area::ScanType_Weak == pArea->GetType())
    {
        size_t cb = pClassD->ComputeSize(p);
        y = Gc::copyWeakObject(pvx, cb);
    }
    else
    {
        size_t cb = pClassD->ComputeSize(p);
        void* pvy = ToSpace::Alloc(pArea, cb);
            ::memcpy(pvy, pvx, cb);

        Int tag = x->ToInt() - reinterpret_cast<Int>(pvx);
        y = reinterpret_cast<Val>(reinterpret_cast<Int>(pvy) + tag);
    } // if

    new(pvx) Gc::ForwardCell(y);
    return y;
} // move_record


//////////////////////////////////////////////////////////////////////
//
// Gc::moveObject
//
Val Gc::moveObject(Val x)
{
    if (! from_space_p(x))
    {
        return x;
    }

    ForwardCell* pForward = ForwardCell::DynamicCast(x);
    if (NULL != pForward)
    {
        return pForward->Get();
    }

    void* pvx = x->GetPtr();

    Area* pArea = Memory::MapToArea(pvx);

    switch (x->GetTag4())
    {
    case_Tag_Cons:
        return move_list(pArea, x);

    case Val_::Tag_Function:
        return move_function(pArea, x, pvx);

    case_Tag_Record:
        return move_record(pArea, x, pvx);

    default:
        CAN_NOT_HAPPEN();
    } // switch tag
} // Gc::moveObject


//////////////////////////////////////////////////////////////////////
//
// Gc::UpdateRS
//
// See alos: phaseUpdateOld
//
void Gc::UpdateRS()
{
    Gc::sm_pThread = Thread::Get();

    for (uint nAge = 1; nAge <= Area::Age_Max; nAge += 1)
    {
        foreach (Memory::EnumArea, oEnum, Memory::sm_rgpAreaByAge[nAge])
        {
            Area* pArea = oEnum.Get();

            if (! is_area_modified(pArea)) continue;

            GC_PRINTF(L" Area %p...%p %X is modified.\r\n",
                pArea->GetStart(),
                pArea->GetEnd(),
                pArea->m_nFlags );

            updateRS_area(pArea);
        } // for each area
    } // for each age

    Memory::ResetWriteWatch();
} // Gc::UpdateRS


// Gc::updateRS_area
void Gc::updateRS_area(Area* pArea)
{
    uint nAge = pArea->GetAge();

    switch (pArea->GetType())
    {
    case Area::ScanType_Cons:
        for (
            Val* pval = pArea->GetTop<Val>();
            pval < pArea->GetFree<Val>();
            pval++ )
        {
            remember(nAge, pval);
        } // for
        break;

    case Area::ScanType_Function:
        foreach (Area::EnumObject, oEnum, pArea)
        {
            updateRS_funobj(nAge, oEnum.Get());
        } // for each object
        break;

    case Area::ScanType_Record:
        foreach (Area::EnumObject, oEnum, pArea)
        {
            updateRS_record(nAge, oEnum.Get());
        } // for each object
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch area type
} // Gc::updateRS_area


// Gc::IsOldToYoung
bool Gc::IsOldToYoung(uint nAge, Val x)
{
    switch (x->GetTag4())
    {
    case_Tag_Immediate:
         return false;
    } // switch tag

    Area* pObjArea = Memory::MapToArea(x->GetPtr());
    return NULL != pObjArea && nAge > pObjArea->GetAge();
} // Gc::IsOldToYoung


void Gc::updateRS_record(uint nAge, Val x)
{
    Val* pstart = reinterpret_cast<Val*>(
        x->GetPtr() );

    Val* pend   = reinterpret_cast<Val*>(
        reinterpret_cast<Int>(pstart) + x->GetSize() );

    for (Val* pRunner = pstart; pRunner < pend; pRunner++)
    {
        remember(nAge, pRunner);
    } // for
} // Gc::updateRS_record


//////////////////////////////////////////////////////////////////////
//
// collect_garbage
//
bool collect_garbage(size_t* out_cbBefore, size_t* out_cbAfter)
{
    const uint k_DefaultGcScanMax = 3;
    return Gc::Run(k_DefaultGcScanMax, out_cbBefore, out_cbAfter);
} // collect_garbage


//////////////////////////////////////////////////////////////////////
//
// collect_garbage
//
bool collect_garbage(uint nAge, size_t* out_cbBefore, size_t* out_cbAfter)
{
    return Gc::Run(nAge, out_cbBefore, out_cbAfter);
} // collect_garbage


//////////////////////////////////////////////////////////////////////
//
// update_remembered_set
//
// For save_image
//
void update_remembered_set()
{
    Gc::UpdateRS();
} // update_remembered_set

} // Kernel


namespace MiniLisp
{

const size_t k_cbGcThreshold = 1024 * 1024 * 4;

//////////////////////////////////////////////////////////////////////
//
// MiniThread::GcFence
//
static uint s_fGC = 0;

Val MiniThread::GcFence(Val obj)
{
    if (! s_fGC && Memory::sm_cbAllocSoFar >= k_cbGcThreshold)
    {
        s_fGC = 1;

        m_n = Fixnum::Encode(1);
        mv_value[0] = obj;

        size_t cbBefore, cbAfter;

        if (collect_garbage(&cbBefore, &cbAfter))
        {
            obj = mv_value[0];

            format(TLV(Aterminal_ioA),
                L"~&; GC Fence ~:D => ~:D~%",
                Fixnum::Encode(cbBefore),
                Fixnum::Encode(cbAfter) );
        } // if

        s_fGC = 0;
    } // if

    return obj;
} // MiniThread::GcFence

} // MiniLisp
