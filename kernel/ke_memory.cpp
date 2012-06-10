#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Memory Manager
// kernel/ke_memory.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_memory.cpp#15 $
//
#include "./ke_memory.h"

#include "./ke_layout.h"    // Latch
#include "./ke_host.h"      // Align
#include "./ke_thread.h"    // For Verify

#include "../mini/mini_lisp.h"

#undef REPORT_HRESULT
#undef REPORT_WIN32_ERROR
#define REPORT_HRESULT      __noop
#define REPORT_WIN32_ERROR  (void)

namespace Kernel
{

uint8* Memory::sm_pbStart;
uint8* Memory::sm_pbEnd;
uint8* Memory::sm_pbCommit;
Area*  Memory::sm_pFreeArea;
GcAnchorArea*  Memory::sm_pGcAnchorArea;

Area** Memory::sm_ppAreaMap;
Area*  Memory::sm_rgpAreaByAge[Area::Age_Max + 1];
Area*  Memory::sm_rgpRSArea[Area::Age_Max + 1];

size_t Memory::sm_cbAllocSoFar;

Latch __declspec(align(Kernel_Arch_Record_Align))
    g_oAreaLatch;

size_t g_cbFree;


//////////////////////////////////////////////////////////////////////
//
// ClassD::ComputeSize
//
size_t ClassD::ComputeSize(Val len) const
{
    Int iLength = Fixnum::Decode_(len);
        // limit length for debugging
        ASSERT(iLength >= 0 && iLength < 65536 * sizeof(Val));

    size_t cbObject;

    switch (m_format->ToInt())
    {
    case Format_Array:
        cbObject = offsetof(Array, mv_dimension) + sizeof(Val) * iLength;
        break;

    case Format_BinVec:
        cbObject = sizeof(DataVector);
        cbObject += CEILING(iLength * Fixnum::Decode_(m_format_param), 8);
        break;

    case Format_String:
        iLength += 1;   // for tail zero.
        cbObject = sizeof(DataVector);
        cbObject += CEILING(iLength * 16, 8);
        break;

    case Format_Storage:
        cbObject = offsetof(Storage, mv_element) + sizeof(Val) * iLength;
        break;

    case Format_Vector:
        cbObject = sizeof(DataVector) + sizeof(Val) * iLength;
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch format

    return ROUNDUP(cbObject, Val_::Record_Align);
} // ClassD::ComputeSize


//////////////////////////////////////////////////////////////////////
//
// ClassD::ComputeRecordSize
//
size_t ClassD::ComputeSize(const Record* p) const
{
    switch (m_format->ToInt())
    {
    case Format_Array:
        return ComputeSize(reinterpret_cast<const Array*>(p)->m_rank);

    case Format_BinFixed:
    case Format_Fixed:
    case Format_Mixed:
    case Format_Structure:
        return ComputeSize();

    case Format_BinVec:
    case Format_Vector:
    case Format_String:
        return ComputeSize(reinterpret_cast<const DataVector*>(p)->m_length);

    case Format_Instance:
        return sizeof(Instance);

    case Format_Storage:
    {
        size_t cb = reinterpret_cast<const Storage*>(p)->
                        m_storaged->Decode<ClassD>()->m_format_misc->ToInt();
            cb += offsetof(Storage, mv_element);
        return ROUNDUP(cb, Val_::Record_Align);
    } // Storage

    default:
        CAN_NOT_HAPPEN();
    } // switch format
} // ClassD::ComputeRecordSize


//////////////////////////////////////////////////////////////////////
//
// Area::EnumObject constructor
//
Area::EnumObject::EnumObject(Area* pArea) :
    m_pval(pArea->GetTop<Val>()),
    m_pend(pArea->GetFree<Val>())
{
    m_nAlign = 0;
    switch (pArea->GetType())
    {
    case ScanType_Cons:
        m_nTag = Cons::Tag;
        break;

    case ScanType_ObStack:
        m_nAlign = Arch::ObStack_Align;
        m_nTag = Record::Tag;
        break;

    case ScanType_BinObj:
    case ScanType_Record:
    case ScanType_Weak:
        m_nTag = Record::Tag;
        break;

    case ScanType_Function:
        m_nTag = Funcallable::Tag;
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch area type
} // Area::EnumObject::EnumObject


//////////////////////////////////////////////////////////////////////
//
// Area::EnumObject::Get
//
Val Area::EnumObject::Get() const
{
    ASSERT(! AtEnd());
    uint nTag = m_nTag;
    if (Record::Tag == nTag)
    {
        switch ((*m_pval)->Decode<ClassD>()->m_format->ToInt())
        {
        case ClassD::Format_Function:
        case ClassD::Format_FuncallableInstance:
            nTag = Funcallable::Tag;
            break;
        } // switch format
    }

    return reinterpret_cast<Val>(
        reinterpret_cast<Int>(m_pval) + nTag );
} // Area::EnumObject::Get


//////////////////////////////////////////////////////////////////////
//
// Area::EnumObject::Next
//
void
Area::EnumObject::Next()
{
    size_t cb = Get()->GetSize();
        ASSERT(0 != cb);

    if (0 != m_nAlign) cb = ROUNDUP(cb, m_nAlign);
    m_pval += cb / sizeof(Val);
} // Area::EnumObject::Next


//////////////////////////////////////////////////////////////////////
//
// Area constructor
Area::Area(
    Area::ScanType  eType,
    size_t          cbArea,
    uint            nAge )
{
    m_pSelf     = this;
    m_cbArea    = static_cast<OffsetT>(cbArea);
    m_nFlags    = eType | nAge;
    m_ofsFree   = sizeof(Area);
    m_ofsScan   = sizeof(Area);
    m_pNext     = NULL;
    m_pPrev     = NULL;
} // Area::Area


//////////////////////////////////////////////////////////////////////
//
// Allocate Object
//
void*
Area::Alloc(
    size_t  cbAlloc )
{
    ASSERT(0 == cbAlloc % Arch::Cons_Align);

    if (m_ofsFree + cbAlloc > m_cbArea)
    {
        return NULL;
    }

    void* pvObject = reinterpret_cast<uint8*>(this) + m_ofsFree;
    m_ofsFree += static_cast<uint32>(cbAlloc);
    return pvObject;
} // Area::Alloc


//////////////////////////////////////////////////////////////////////
//
// Allocate Area
//
Area*
Memory::AllocCodeArea(
    Thread* pThread,
    Area::ScanType,
    size_t  cbData,
    uint    nAge )
{
    ASSERT(NULL != pThread);

    g_oAreaLatch.Lock(pThread, Kexclusive);

    size_t cbArea = cbData + sizeof(Area);
        cbArea = ROUNDUP(cbArea, Param_AllocUnit);

    void* pvArea = getFreeArea(cbArea);

    if (NULL == pvArea)
    {
        if (sm_pbCommit + cbArea >= sm_pbEnd)
        {
            g_oAreaLatch.Unlock();
            Debugger::Fail(L"sm_pbCommit + cbArea >= sm_pbEnd");
            // NOTREACHED
        }

        pvArea = ::VirtualAlloc(
            sm_pbCommit,
            cbArea,
            MEM_COMMIT,
            PAGE_EXECUTE_READWRITE );

        if (NULL == pvArea)
        {
            g_oAreaLatch.Unlock();
            Debugger::Fail(L"VirtualAlloc(MEM_COMMIT)");
            // NOTREACHED
        }

        sm_pbCommit += cbArea;
    }
    else
    {
        DWORD dwOldProtect;
        BOOL fSucceeded = ::VirtualProtect(
            pvArea,
            cbArea,
            PAGE_EXECUTE_READWRITE,
            &dwOldProtect );
        if (! fSucceeded)
        {
            g_oAreaLatch.Unlock();
            Debugger::Fail(L"VirtualProtect");
            // NOTREACHED
        }
    } // if

    sm_cbAllocSoFar += cbArea;

    Area* pArea = new(pvArea) Area(Area::ScanType_Function, cbArea, nAge);

    AddArea(pArea);

    g_oAreaLatch.Unlock();

    DEBUG_PRINTF(L"%p %06X %u\r\n", pArea, pArea->m_nFlags, pArea->m_cbArea);
    return pArea;
} // Memory::AllocCodeArea


//////////////////////////////////////////////////////////////////////
//
// Allocate Data Area
//
Area*
Memory::AllocDataArea(
    Thread*         pThread,
    Area::ScanType  eType,
    size_t          cbData,
    uint            nAge )
{
    ASSERT(NULL != pThread);

    size_t cbArea = cbData + sizeof(Area);
        cbArea = ROUNDUP(cbArea, Param_AllocUnit);

    g_oAreaLatch.Lock(pThread, Kexclusive);

    void* pvArea = getFreeArea(cbArea);

    if (NULL == pvArea)
    {
        if (sm_pbCommit + cbArea >= sm_pbEnd)
        {
            g_oAreaLatch.Unlock();
            Debugger::Fail(L"sm_pbCommit + cbArea >= sm_pbEnd\r\n");
            // NOTREACHED
        }

        pvArea = ::VirtualAlloc(
            sm_pbCommit,
            cbArea,
            MEM_COMMIT,
            PAGE_READWRITE );

        if (NULL == pvArea)
        {
            g_oAreaLatch.Unlock();
            error(Qstorage_condition);
        }

        sm_pbCommit += cbArea;
    } // if

    sm_cbAllocSoFar += cbArea;

    Area* pArea = new(pvArea) Area(eType, cbArea, nAge);
    AddArea(pArea);

    g_oAreaLatch.Unlock();

    DEBUG_PRINTF(L"%p %06X %u\r\n", pArea, pArea->m_nFlags, pArea->m_cbArea);
    return pArea;
} // Memory::AllocDataArea


//////////////////////////////////////////////////////////////////////
//
// GcAnchorArea::Alloc
//
GcAnchor* GcAnchorArea::Alloc(Val val)
{
    if (0 != m_ofsScan)
    {
        GcAnchor* pGcAnchor = GetScan<GcAnchor>();

        m_ofsScan = static_cast<OffsetT>(reinterpret_cast<Int>(
            pGcAnchor->m_value ) );

        pGcAnchor->m_value = val;

        return pGcAnchor;
    } // if

    OffsetT ofsNext = m_ofsFree + sizeof(GcAnchor);
    if (ofsNext >= m_cbArea) return NULL;
    GcAnchor* pGcAnchor = GetFree<GcAnchor>();
    m_ofsFree = ofsNext;
    pGcAnchor->m_value = val;
    return pGcAnchor;
} // GcAnchorArea::Alloc


//////////////////////////////////////////////////////////////////////
//
// Allocate Gc Anchor
//
GcAnchor* Memory::AllocGcAnchor(Val val)
{
    for (;;)
    {
        if (NULL != sm_pGcAnchorArea)
        {
            GcAnchor* pGcAnchor = sm_pGcAnchorArea->Alloc(val);
            if (NULL != pGcAnchor)
            {
                return pGcAnchor;
            }
        } // if

        void* pvArea = ::VirtualAlloc(
            NULL,
            Param_AllocUnit,
            MEM_COMMIT,
            PAGE_READWRITE );
        if (NULL == pvArea)
        {
            DWORD dwError = ::GetLastError();
            Debugger::Printf(L"VirtualAlloc: dwError=%u\n", dwError);
            error(Qstorage_condition);
        }

        GcAnchorArea* pArea = new(pvArea) GcAnchorArea();

        pArea->m_pNext = sm_pGcAnchorArea;

        if (NULL != sm_pGcAnchorArea)
        {
            sm_pGcAnchorArea->m_pPrev = pArea;
        }

        sm_pGcAnchorArea = pArea;
    } // for
} // Memory::AllocGcAnchor


//////////////////////////////////////////////////////////////////////
//
// Allocate ObStack Area
//
ObStackArea*
Memory::AllocObStackArea(
    size_t  cbData )
{
    size_t cbArea = cbData + sizeof(Area);
        cbArea = ROUNDUP(cbArea, Param_AllocUnit);

    void* pvArea = ::VirtualAlloc(
        NULL,
        cbArea,
        MEM_COMMIT,
        PAGE_EXECUTE_READWRITE );

    if (NULL == pvArea)
    {
        DWORD dwError = ::GetLastError();
        Debugger::Printf(L"VirtualAlloc: dwError=%u\n", dwError);
        error(Qstorage_condition);
    }

    ObStackArea* pArea = new(pvArea) ObStackArea(cbArea);
    return pArea;
} // Memory::AllocObStackArea


//////////////////////////////////////////////////////////////////////
//
// Allocate Thread
//
void*
Memory::AllocThread(
    size_t  cbAlloc )
{
    cbAlloc += Host::Thread_Lead_Extra;
    cbAlloc = ROUNDUP(cbAlloc, Param_AllocUnit);

    void* pvArea = ::VirtualAlloc(
        NULL,
        cbAlloc,
        MEM_COMMIT,
        PAGE_READWRITE );

    if (NULL == pvArea)
    {
        error(Qstorage_condition);
    }

    return reinterpret_cast<uint8*>(pvArea) + Host::Thread_Lead_Extra;
} // Memory::AllocThread


//////////////////////////////////////////////////////////////////////
//
// Memory::AddArea
//
void
Memory::AddArea(Area* pArea)
{
    ASSERT(NULL != pArea);
    ASSERT(Area::ScanType_None != pArea->GetType());

    setAreaMap(pArea);

    // Link to age list
    {
        Area** ppAnchor = NULL;

        if (pArea->GetType() == Area::ScanType_RS)
        {
            ppAnchor = &sm_rgpRSArea[pArea->GetAge()];
        }
        else if (pArea->IsAged())
        {
            ppAnchor = &sm_rgpAreaByAge[pArea->GetAge()];
        } // link

        if (NULL != ppAnchor)
        {
            Area* pHead = *ppAnchor;
            if (NULL != pHead)
            {
                ASSERT(pHead != pArea);
                ASSERT(pHead->GetAge() == pArea->GetAge());

                pArea->m_pNext = pHead;
                pHead->m_pPrev = pArea;
            }
            *ppAnchor = pArea;
        } // if
    } // link
} // Memory::AddArea


//////////////////////////////////////////////////////////////////////
//
// Add Free Area
//
void
Memory::AddFreeArea(Thread* pThread, Area* pArea)
{
    g_oAreaLatch.Lock(pThread, Kexclusive);
    AddFreeArea(pArea);
    g_oAreaLatch.Unlock();
} // Memory::AddFreeArea


//////////////////////////////////////////////////////////////////////
//
// Add Free Area
//
void
Memory::AddFreeArea(Area* pArea)
{
    ASSERT(NULL != pArea);

    DEBUG_PRINTF(L"%p size=%u\r\n", pArea, pArea->m_cbArea);

    size_t cbArea = pArea->m_cbArea;

    unless (0 == cbArea % Memory::Param_AllocUnit)
    {
        Debugger::Fail(L"Try to free broken area@%p(cb=%d)\n",
            pArea,
            pArea->m_cbArea );
    }

    g_cbFree += cbArea;

    ::ZeroMemory(pArea, pArea->m_cbArea);

    if (Area::ScanType_Function == pArea->GetType())
    {
        DWORD dwOldProtect;
        BOOL fSucceeded = ::VirtualProtect(
            pArea,
            pArea->m_cbArea,
            PAGE_READWRITE,
            &dwOldProtect );
        if (! fSucceeded)
        {
            DWORD dwError = ::GetLastError();
            REPORT_WIN32_ERROR("VirtualProtect", dwError);
            Debugger::Fail(L"VirtualProtect");
            // NOTREACHED
        }
    } // if

    // coalesce to previous
    {
        Area* pPrev = MapToArea(pArea - 1);
        if (NULL != pPrev && pPrev->GetType() == Area::ScanType_None)
        {
            pPrev->m_cbArea = static_cast<OffsetT>(
                pPrev->m_cbArea + cbArea );

            Area* pNext = MapToArea(pPrev->GetEnd());
            if (NULL != pNext && pNext->GetType() == Area::ScanType_None)
            {
                // pArea is between pPrev and pNext.
                pPrev->m_cbArea += pNext->m_cbArea;
                pPrev->m_pNext = pNext->m_pNext;
                if (pNext->m_pNext != NULL)
                {
                    pNext->m_pNext->m_pPrev = pPrev;
                } // if

                ::ZeroMemory(pNext, sizeof(Area));
            } // if

            setAreaMap(pPrev);
            return;
        } // if
    } // previous

    pArea->m_pSelf  = pArea;
    pArea->m_cbArea = static_cast<OffsetT>(cbArea);

    // coalesce to next
    {
        Area* pNext = MapToArea(pArea->GetEnd());
        if (NULL != pNext && pNext->GetType() == Area::ScanType_None)
        {
            pArea->m_cbArea += pNext->m_cbArea;
            pArea->m_nFlags = Area::ScanType_None;

            pArea->m_pNext = pNext->m_pNext;
            pArea->m_pPrev = pNext->m_pPrev;

            if (pArea->m_pNext != NULL)
            {
                pArea->m_pNext->m_pPrev = pArea;
            } // if

            if (pArea->m_pPrev != NULL)
            {
                pArea->m_pPrev->m_pNext= pArea;
            }
            else
            {
                sm_pFreeArea = pArea;
            } // if

            ::ZeroMemory(pNext, sizeof(Area));

            setAreaMap(pArea);
            return;
        } // if
    } // next

    // Find insert position
    {
        Area* pLast = NULL;
        Area* pRunner;
        for (
            pRunner = sm_pFreeArea;
            NULL != pRunner;
            pRunner = pRunner->m_pNext )
        {
            if (pArea < pRunner)
            {
                // Insert pArea before pRunner.
                pRunner->m_pPrev = pArea;
                break;
            } // if

            pLast = pRunner;
        } // for each area

        // Append pArea to free list.
        if (NULL == pLast)
        {
            sm_pFreeArea = pArea;
        }
        else
        {
            pLast->m_pNext = pArea;
        }

        pArea->m_pNext = pRunner;
        pArea->m_pPrev = pLast;
    }

    setAreaMap(pArea);
} // Memory::AddFreeArea


//////////////////////////////////////////////////////////////////////
//
// Memory::FreeGcAnchor
//
void GcAnchorArea::Free(GcAnchor* pGcAnchor)
{
    pGcAnchor->m_value = reinterpret_cast<Val>(static_cast<Int>(m_ofsScan));

    m_ofsScan = static_cast<OffsetT>(
        reinterpret_cast<Int>(pGcAnchor) - ToInt() );
} // GcAnchorArea::Free

void Memory::FreeGcAnchor(GcAnchor* pGcAnchor)
{
    GcAnchorArea* pArea = reinterpret_cast<GcAnchorArea*>(
        reinterpret_cast<Int>(pGcAnchor) & ~Param_AllocUnit );

    pArea->Free(pGcAnchor);
} // Memroy::FreeGcAnchor


//////////////////////////////////////////////////////////////////////
//
// Memory::getFreeArea
//
void* Memory::getFreeArea(size_t cbArea)
{
    size_t cbSmall = 0;

    for (
        Area* pRunner = sm_pFreeArea;
        NULL != pRunner;
        pRunner = pRunner->m_pNext )
    {
        if (pRunner->m_cbArea == cbArea)
        {
            // Unlink pRunner from free area list
            if (NULL == pRunner->m_pPrev)
            {
                sm_pFreeArea = pRunner->m_pNext;
            }
            else
            {
                pRunner->m_pPrev->m_pNext = pRunner->m_pNext;
            }

            if (NULL != pRunner->m_pNext)
            {
                pRunner->m_pNext->m_pPrev = pRunner->m_pPrev;
            }

            g_cbFree -= cbArea;

            return pRunner;
        } // if

        if (pRunner->m_cbArea > cbArea)
        {
            // Returns bottom of pRunner.
            pRunner->m_cbArea = static_cast<OffsetT>(
                pRunner->m_cbArea - cbArea );

            DEBUG_PRINTF(L"extract from free area\r\n");

            g_cbFree -= cbArea;

            return pRunner->GetBtm<Area>();
        } // if

        cbSmall += pRunner->m_cbArea;
    } // for each area

    ASSERT(0 == g_cbFree - cbSmall);
    return NULL;
} // Memory::getFreeArea


//////////////////////////////////////////////////////////////////////
//
// Memory::ResetWriteWatch
//
void
Memory::ResetWriteWatch()
{
    ::ResetWriteWatch(
        sm_pbStart,
        sm_pbCommit - sm_pbStart );
} // Memory::ResetWriteWatch


// Set area map
void 
Memory::setAreaMap(Area* pArea)
{
    Int iStart = mapToIndex(pArea->GetStart());
    Int iEnd   = mapToIndex(pArea->GetEnd());
    for (Int iIndex = iStart; iIndex < iEnd; iIndex++)
    {
        sm_ppAreaMap[iIndex] = pArea;
    } // for i
} // set area map


//////////////////////////////////////////////////////////////////////
//
// Start memory manager
//
void
Memory::Start(size_t cbAlloc)
{
    sm_pbStart = reinterpret_cast<uint8*>(::VirtualAlloc(
        reinterpret_cast<void*>(Param_Start),
        cbAlloc,
        MEM_RESERVE | MEM_WRITE_WATCH,
        PAGE_EXECUTE_READWRITE ) );

    if (NULL == sm_pbStart)
    {
        return;
    }

    sm_pbCommit = sm_pbStart;
    sm_pbEnd = sm_pbStart + cbAlloc;

    ::ZeroMemory(sm_rgpAreaByAge, sizeof(sm_rgpAreaByAge));

    // Initialize memory area latch
    {
        g_oAreaLatch.m_thread     = nil;
        g_oAreaLatch.m_state      = nil;
        g_oAreaLatch.m_name       = nil;
        g_oAreaLatch.m_lock_count = Fixnum::Encode(0);
        g_oAreaLatch.m_spinlock   = nil;
    }

    sm_ppAreaMap = reinterpret_cast<Area**>(::VirtualAlloc(
        NULL,
        CEILING(sm_pbEnd - sm_pbStart, Param_AllocUnit) * sizeof(Area*),
        MEM_COMMIT,
        PAGE_READWRITE ) );
    if (NULL == sm_ppAreaMap)
    {
        return;
    }
} // Memory::Start


#if SIZEOF_VAL == 4
    #define UNINIT_MARKER ((Val) (UInt) 0xCCCCCCCCu)
#elif SIZEOF_VAL == 8
    #define UNINIT_MARKER ( (Val) 0xCCCCCCCCCCCCCCCCull )
#else
    #error Unsupported SIZEOF_VAL
#endif


//////////////////////////////////////////////////////////////////////
//
// verify
//
bool Memory::Verify()
{
    bool fVerify = true;

    foreach (Memory::EnumAreaAll, oEnum, Memory::GetStart())
    {
        Area* pArea = oEnum.Get();
        switch (pArea->GetType())
        {
        case Area::ScanType_Cons:
        case Area::ScanType_Function:
            for (
                Val* pval = pArea->GetTop<Val>();
                pval < pArea->GetBtm<Val>();
                pval++ )
            {
                Val x = *pval;
                ASSERT(x != UNINIT_MARKER);
            } // for
            continue;

        case Area::ScanType_Record:
            foreach (Area::EnumObject, oEnum, pArea)
            {
                Val x = oEnum.Get();
                ASSERT(x != UNINIT_MARKER);
                ASSERT(x->Is<Record>());

                //DEBUG_PRINTF(L"%p cb=%u\r\n", x, x->GetSize());

                ASSERT(x->Decode<Record>()->m_classd->Is<ClassD>());
                ClassD* pClassD = x->Decode<Record>()->
                    m_classd->Decode<ClassD>();
                switch (pClassD->m_format->ToInt())
                {
                case ClassD::Format_BinFixed:
                case ClassD::Format_BinVec:
                case ClassD::Format_String:
                    Debugger::Fail(L"Record area %p contains binary.",
                        pArea );
                } // switch format
            } // for each object
            break;
        } // switch area type
    } // for each area

    return fVerify;
} // Memory::Verify


//////////////////////////////////////////////////////////////////////
//
// ObStackArea::Alloc
//
void*
ObStackArea::Alloc(size_t cbAlloc)
{
    ASSERT(0 == cbAlloc % Arch::ObStack_Align);
    if (m_ofsFree + cbAlloc < m_cbArea)
    {
        void* pv = reinterpret_cast<uint8*>(this) + m_ofsFree;
        m_ofsFree = static_cast<OffsetT>(m_ofsFree + cbAlloc);
        return pv;
    }

    return NULL;
} // ObStackArea::Alloc

} // Kernel
