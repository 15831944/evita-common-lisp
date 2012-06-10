#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - sxhash table
// kernel/ke_sxhash.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_sxhash.cpp#10 $
//
// Description:
//  This file contains implementation of kernel object hash-table for
//  sxhash of aggragate objects such as list, structure and so on.
//
#include "./ke_sxhash.h"

#include "./ke_thread.h"
#include "./ke_gc.h"

namespace Kernel
{

const uint RehashThreshold = 65;


ObjTab* ObjTab::sm_pObjTab;
Val     ObjTab::sm_random;

namespace
{

static Val const FreeMarker    = reinterpret_cast<Val>(Fixnum::One * 0);
static Val const RemovedMarker = reinterpret_cast<Val>(Fixnum::One * 1);
static const Int MaxHashCode   = HashTable::MaxHashCode;

//////////////////////////////////////////////////////////////////////
//
// Latch for ObjTab
//
Latch __declspec(align(Kernel_Arch_Record_Align))
    g_oSxHashLatch;

typedef ObjTab::Slot Slot;

//////////////////////////////////////////////////////////////////////
//
// EnumSlot
//
class EnumSlot
{
    protected: Slot*    m_pRunner;
    protected: Slot*    m_pEnd;
    protected: size_t   m_cb;
    protected: ObjTab*  m_pObjTab;

    // ctor
    public: EnumSlot(ObjTab* p) :
        m_pObjTab(p),
        m_pRunner(p->GetTop<Slot>() - 1),
        m_pEnd(p->GetBtm<Slot>()),
        m_cb(p->m_ofsFree - sizeof(Area))
    {
        next();
    } // EnumSlot

    // [A]
    public: bool AtEnd() const
    { 
        return m_cb == 0;
    } // AtEnd

    // [G]
    public: Slot* Get() const
    {
        ASSERT(! AtEnd());
        ASSERT(m_pRunner < m_pEnd);
        return m_pRunner;
    } // Get

    // [N]
    public: void Next()
    { 
        ASSERT(! AtEnd()); 
        m_cb -= sizeof(Slot);
        next();
    } // EnumSlot::Next

    private: void next()
    {
        if (AtEnd()) return;
        for (;;)
        {
            m_pRunner++;

            unless (m_pRunner < m_pEnd)
            {
                Debugger::Fail(
                    L"Overrun: sxhashtab: %p...%p size=%d/%d"
                    L" runner=%p rest=%d\n",
                    m_pObjTab->GetTop<Slot>(),
                    m_pObjTab->GetBtm<Slot>(),
                    m_pObjTab->m_ofsFree,
                    m_pObjTab->m_cbArea,
                    m_pRunner,
                    m_cb );
            } // unless

            if (m_pRunner->m_obj > RemovedMarker) break;
        } // for
    } // next

    // [R]
    public: void Remove()
    {
        Slot* pNext = m_pRunner + 1;
        if (pNext >= m_pObjTab->GetBtm<Slot>())
        {
            pNext = m_pObjTab->GetTop<Slot>();
        }

        Slot* pPrev = m_pRunner - 1;
        if (pPrev < m_pObjTab->GetTop<Slot>())
        {
            pPrev = m_pEnd - 1;
        } // if

        if (pNext->m_obj == FreeMarker && pPrev->m_obj == FreeMarker)
        {
            // Removing object isn't in collision chain.
            m_pRunner->m_obj = FreeMarker;
        }
        else
        {
            // Removing object in in collision chain.
            m_pRunner->m_obj = RemovedMarker;
        } // if

        m_pObjTab->m_ofsFree -= sizeof(Slot);
    } // Remove
}; // EnumSlot

// isSxHashable
inline bool isSxHashable(Val x)
{
    switch (x->GetTag4())
    {
    case_Tag_Cons:
    case_Tag_Record:
    case Val_::Tag_Function:
        return true;

    default:
        return false;
    } // switch tag
} // isSxHashable

} // namespace

//////////////////////////////////////////////////////////////////////
//
// ObjTab::Init
//
void ObjTab::Init(ObjTab* pObjTab)
{
    sm_pObjTab = pObjTab;
    sm_random  = Fixnum::Encode(::GetTickCount() & MaxHashCode);

    // Initialize memory area latch
    {
        g_oSxHashLatch.m_thread     = nil;
        g_oSxHashLatch.m_state      = nil;
        g_oSxHashLatch.m_name       = nil;
        g_oSxHashLatch.m_lock_count = Fixnum::Encode(0);
        g_oSxHashLatch.m_spinlock   = nil;
    }
} // ObjTab::Init


//////////////////////////////////////////////////////////////////////
//
// ObjTab::Intern
//
ObjTab::Slot* ObjTab::Intern(Thread* pThread, Val x)
{
    if (m_ofsFree * 100 < m_cbArea * RehashThreshold)
    {
        return Intern(x, nil);
    }

    return Rehash(pThread, m_cbArea)->Intern(x, nil);
} // ObjTab::Intern

//////////////////////////////////////////////////////////////////////
//
// ObjTab::Intern
//
ObjTab::Slot* ObjTab::Intern(Val x, Val hash_code)
{
    unless (isSxHashable(x))
    {
        Debugger::Fail(L"Can't insert %p into sxhash table.",  x);
    } // unless

    Slot* pTop    = GetTop<Slot>();
    Slot* pBtm    = GetBtm<Slot>();
    Slot* pStart  = pTop + x->ToInt() % (pBtm - pTop);
    Slot* pRunner = pStart;
    Slot* pHome   = NULL;
    do
    {
        if (x == pRunner->m_obj)
        {
            // Relocate to removed slot.
            if (pHome != NULL)
            {
                pHome->m_obj = x;
                pHome->m_hash_code = pRunner->m_hash_code;

                // We are in collision chain. So, we must put RemovedMarker
                // instead of FreeMarker.
                pRunner->m_obj = RemovedMarker;
                return pHome;
            } // if
            return pRunner;
        } // if

        if (FreeMarker == pRunner->m_obj)
        {
            if (pHome == NULL) pHome = pRunner;
            break;
        } // if

        if (RemovedMarker == pRunner->m_obj)
        {
            if (pHome == NULL) pHome = pRunner;
        } // if

        pRunner++;

        if (pRunner == pBtm) pRunner = pTop;
    } while (pRunner != pStart);

    if (pHome == NULL)
    {
        Debugger::Fail(L"sxhhash %p overflow\n", this);
    } // if

    if (hash_code == Qfind) return NULL;

    if (hash_code == nil)
    {
        hash_code = random();
    } // if

    pHome->m_obj = x;
    pHome->m_hash_code = hash_code;
    m_ofsFree += sizeof(Slot);
    return pHome;
} // ObjTab::Intern

//////////////////////////////////////////////////////////////////////
//
// ObjTab::random
//
Val ObjTab::random()
{
    Int nHashCode = Fixnum::Decode_(sm_random);
        nHashCode = nHashCode * 1664525 + 1013904223;
        nHashCode = (nHashCode + 1) & MaxHashCode;

    return sm_random = Fixnum::Encode(nHashCode);
} // ObjTab::random

//////////////////////////////////////////////////////////////////////
//
// ObjTab::Rehash
//
ObjTab* ObjTab::Rehash(Thread* pThread, size_t cbObjTab)
{
    ObjTab* pOldTab = sm_pObjTab;

    ObjTab* pNewTab = reinterpret_cast<ObjTab*>(
        Memory::AllocDataArea(
            pThread,
            Area::ScanType_HashTable,
            cbObjTab,
            Area::Age_System ) );

    foreach (EnumSlot, oEnum, pOldTab)
    {
        Slot* pSlot = oEnum.Get();

        unless (isSxHashable(pSlot->m_obj))
        {
            Debugger::Fail(L"Broken ObjTab slot @ %p(%p, %p).\n",
                pSlot,
                pSlot->m_obj,
                pSlot->m_hash_code );
        } // unless

        pNewTab->Intern(pSlot->m_obj, pSlot->m_hash_code);
    } // for each entry

    Memory::AddFreeArea(pThread, pOldTab);

    return sm_pObjTab = pNewTab;
} // ObjTab::Reahash

void ObjTab::Lock(Thread* pThread)
    { g_oSxHashLatch.Lock(pThread, Kexclusive); }

void ObjTab::Unlock()
    { g_oSxHashLatch.Unlock(); }

//////////////////////////////////////////////////////////////////////
//
// Gc::fixObjTab
//
void Gc::fixObjTab()
{
    class EnumSlot2 : public EnumSlot
    {
        public: EnumSlot2(ObjTab* p) : EnumSlot(p) {}

        public: void Provision(Slot* pSlot)
        {
            if (pSlot > m_pRunner)
            {
                // New slot is after m_pRunner. So, we'll see this object
                // again.
                m_cb += sizeof(Slot);
            }
        } // Provision

        public: size_t GetRest() const { return m_cb; }
    }; // EnumSlot2

    ObjTab* pObjTab = ObjTab::Get();

    foreach (EnumSlot, oEnum, pObjTab)
    {
        Slot* pSlot = oEnum.Get();
        if (IsGarbage(pSlot->m_obj))
        {
            // The object is now garbage. We discard this slot.
            oEnum.Remove();
        } // if
    } // for each slot

    if (pObjTab->m_cbArea > Memory::Param_AllocUnit)
    {
        size_t cbLives = pObjTab->m_ofsFree;
        size_t cbArea = Memory::Param_AllocUnit;
        while (cbLives * 100 > cbArea * RehashThreshold)
        {
            cbArea += Memory::Param_AllocUnit;
        } // while

        if (cbArea != pObjTab->m_cbArea)
        {
            // Shrink ObjTab
            foreach (EnumSlot, oEnum, pObjTab)
            {
                Slot* pSlot = oEnum.Get();
                ForwardCell* p = ForwardCell::DynamicCast(pSlot->m_obj);
                if (p != NULL)
                {
                    pSlot->m_obj = p->Get();
                } // if
            } // for each slot

            ASSERT(cbArea < pObjTab->m_cbArea);
            ObjTab::Rehash(sm_pThread, cbArea - sizeof(Area));
            return;
        } // if
    } // if large

    // In-Place Update
    size_t cbScan = sizeof(Area);
    size_t cbProv = 0;
    foreach (EnumSlot2, oEnum, pObjTab)
    {
        Slot* pSlot = oEnum.Get();

        cbScan += sizeof(Slot);

        unless (isSxHashable(pSlot->m_obj))
        {
            Debugger::Fail(
                L"sxhash [4] %p %d/%d scan=%d+%d rest=%d\n",
                pObjTab,
                pObjTab->m_ofsFree, pObjTab->m_cbArea,
                cbScan, cbProv,
                oEnum.GetRest() );
        } // unless

        ForwardCell* p = ForwardCell::DynamicCast(pSlot->m_obj);

        if (p != NULL)
        {
            if (p->Get() == pSlot->m_obj)
            {
                Debugger::Fail(L"sxhash [5] %p broken forward cell\n",
                    pObjTab );
            } // if

            Val obj = p->Get();
            Val hash_code = pSlot->m_hash_code;

            // The object is relocated. We relocate the object in
            // ObjTab too.
            oEnum.Remove();

            Slot* pNewSlot = pObjTab->Intern(obj, hash_code);

            oEnum.Provision(pNewSlot);
            if (pNewSlot > pSlot) cbProv += sizeof(Slot);
        } // if
    } // for each slot
} // Gc::fixObjTab

//////////////////////////////////////////////////////////////////////
//
// Thread::sxhash
//
Val Thread::SxHash(Val x)
{
    ASSERT(isSxHashable(x));

    ObjTab::Get()->Lock(this);

    ObjTab::Slot* pSlot = ObjTab::Get()->Intern(this, x);

    ObjTab::Get()->Unlock();

    ASSERT(pSlot->m_obj == x);
    ASSERT(pSlot->m_hash_code >= Fixnum::Encode(1));

    return pSlot->m_hash_code;
} // Thread::SxHash

} // Kernel
