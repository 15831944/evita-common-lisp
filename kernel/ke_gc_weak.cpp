#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Garbage Collector
// kernel/ke_gc.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_gc_weak.cpp#6 $
//
#include "./ke_gc.h"

#include "./ke_weak.h"
#include "./ke_finalize.h"

#include "./ke_memory.h"
#include "./ke_support.h"

namespace Kernel
{

//////////////////////////////////////////////////////////////////////
//
// Gc::copyWeakObject
//
Val Gc::copyWeakObject(void* pvx, size_t cb)
{
    Val y;

    WeakVectorLeader* p = reinterpret_cast<WeakVectorLeader*>(pvx) - 1;
    if (p->m_classd == CLASSD_weak_vector_leader)
    {
        cb += sizeof(WeakVectorLeader);

        WeakVectorLeader* q = reinterpret_cast<WeakVectorLeader*>(
            Weak::Alloc(cb) );

        ::memcpy(q, p, cb);

        y = reinterpret_cast<Val>(
                reinterpret_cast<Int>(q + 1) + WeakVectorLeader::Tag );
    }
    else
    {
        void* pvy = Weak::Alloc(cb);
        ::memcpy(pvy, pvx, cb);
        y = reinterpret_cast<Val>(
            reinterpret_cast<Int>(pvy) + WeakPointer::Tag );
    } // if

    return y;
} // Gc::copyWeakObject


namespace
{

// fix_caller_set
static void fix_caller_set(void* pv)
{
    CallerSet* p = reinterpret_cast<CallerSet*>(pv);

    Val* pend = p->mv_element + Fixnum::Decode_(p->mv_element[0]);

    Val* pdst = p->mv_element + 1;
    for (
        Val* prunner = p->mv_element + 1;
        prunner < pend;
        prunner++ )
    {
        if (! Gc::IsGarbage(*prunner))
        {
            *pdst++ = Gc::moveObject(*prunner);
        }
    } // for d

    p->mv_element[0] = Fixnum::Encode(pdst - p->mv_element);
    while (pdst < pend) *pdst++ = Fixnum::Encode(0);
} // fix_caller_set


// fix_htb_key
//  If key is a garbage, remove it.
static void fix_htb_key(void* pv)
{
    WeakVector* p = reinterpret_cast<WeakVector*>(pv);
    foreach (HashTableImpl::Enum, oEnum, p->Encode())
    {
        if (Gc::IsGarbage(oEnum.GetKey()))
        {
            oEnum.Remove();
        }
        else
        {
            oEnum.SetKey(Gc::moveObject(oEnum.GetKey()));
            oEnum.SetVal(Gc::moveObject(oEnum.GetVal()));
        }
    } // for each slot
} // fix_htb_key


// fix_htb_value
//  If value is a garbage, nilfy it.
static void fix_htb_value(void* pv)
{
    WeakVector* p = reinterpret_cast<WeakVector*>(pv);
    foreach (HashTableImpl::Enum, oEnum, p->Encode())
    {
        if (Gc::IsGarbage(oEnum.GetVal()))
        {
            oEnum.SetVal(nil);
        }
        else
        {
            oEnum.SetKey(Gc::moveObject(oEnum.GetKey()));
            oEnum.SetVal(Gc::moveObject(oEnum.GetVal()));
        }
    } // for each slot
} // fix_htb_value


// fix_htb_both
//  If either key or value is a garbage, remove it.
static void fix_htb_both(void* pv)
{
    WeakVector* p = reinterpret_cast<WeakVector*>(pv);
    foreach (HashTableImpl::Enum, oEnum, p->Encode())
    {
        if (Gc::IsGarbage(oEnum.GetKey()) || Gc::IsGarbage(oEnum.GetVal()))
        {
            oEnum.Remove();
        }
        else
        {
            oEnum.SetKey(Gc::moveObject(oEnum.GetKey()));
            oEnum.SetVal(Gc::moveObject(oEnum.GetVal()));
        }
    } // for each slot
} // fix_htb_both


// fix_htb_either
//  o If key is a garbage, remove it.
//  o If val is a garbage, nilfy it.
//  o otherwise update key and val.
static void fix_htb_either(void* pv)
{
    WeakVector* p = reinterpret_cast<WeakVector*>(pv);
    foreach (HashTableImpl::Enum, oEnum, p->Encode())
    {
        if (Gc::IsGarbage(oEnum.GetKey()))
        {
            oEnum.Remove();
        }
        else if (Gc::IsGarbage(oEnum.GetVal()))
        {
            oEnum.SetVal(nil);
        }
        else
        {
            oEnum.SetKey(Gc::moveObject(oEnum.GetKey()));
            oEnum.SetVal(Gc::moveObject(oEnum.GetVal()));
        }
    } // for each slot
} // fix_htb_either


static void fix_package(void* pv)
{
    WeakVector* p = reinterpret_cast<WeakVector*>(pv);
    foreach (PackageImpl::Enum, oEnum, p->Encode())
    {
        Val sym = oEnum.Get();
        //if (Gc::IsGarbage(sym) && nil == sym->Decode<Symbol>()->m_function)
        if (Gc::IsGarbage(sym))
        {
            #if defined(_DEBUG)
            {
                Val package = Gc::resolveForward(
                    sym->Decode<Symbol>()->m_package );

                Val names = Gc::resolveForward(
                    package->Decode<Package>()->m_names );

                Val name = Gc::resolveForward(
                    names->Decode<Cons>()->m_car );

                DEBUG_PRINTF(L" dead[%d] %ls %ls %p fun=%p\r\n",
                    oEnum.GetRef() -
                        reinterpret_cast<PackageImpl::Slot*>(p->mv_element),
                    Gc::resolveForward(name)->
                        Decode<SimpleString>()->GetElements(),
                    Gc::resolveForward(sym->Decode<Symbol>()->m_name)->
                        Decode<SimpleString>()->GetElements(),
                    sym,
                    sym->Decode<Symbol>()->m_function );
            }
            #endif // defined(_DEBUG)

            ASSERT(nil == sym->Decode<Symbol>()->m_function);

            oEnum.Remove();
        }
        else
        {
            sym = Gc::moveObject(sym);
            oEnum.SetKey(sym);

            #if 0
            {
                DEBUG_PRINTF(L"alive[%d] %ls %ls %p fun=%p\r\n",
                    oEnum.GetRef() -
                        reinterpret_cast<PackageImpl::Slot*>(p->mv_element),
                    sym->Decode<Symbol>()->m_package->
                        Decode<Package>()->m_names->
                            Decode<Cons>()->m_car->
                                Decode<SimpleString>()->GetElements(),
                    sym->Decode<Symbol>()->m_name->
                        Decode<SimpleString>()->GetElements(),
                    sym,
                    sym->Decode<Symbol>()->m_function );
            }
            #endif
        }
    } // for each entry
} // fix_package

// fix_vector
static void fix_vector(Val x)
{
    WeakVectorLeader* p = x->Decode<WeakVectorLeader>();
    switch (p->m_kind - nil)
    {
    case Qfuncall - nil:
        fix_caller_set(p + 1);
        break;

    case Kkey - nil:
        fix_htb_key(p + 1);
        break;

    case Kvalue - nil:
        fix_htb_value(p + 1);
        break;

    case Kboth - nil:
        fix_htb_both(p + 1);
        break;

    case Keither - nil:
        fix_htb_either(p + 1);
        break;

    case Qpackage - nil:
        fix_package(p + 1);
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch kind
} // weak_vector_leader


// updateFinalization
static void updateFinalization(Val x)
{
    Finalization* p = x->Decode<Finalization>();
    Gc::moveObject(p->m_function);

    if (Gc::from_space_p(p->m_object))
    {
        Gc::ForwardCell* pForward = Gc::ForwardCell::DynamicCast(
            p->m_object );

        if (NULL != pForward)
        {
            // An object is survived.
            p->m_object = pForward->Get();
            Gc::moveObject(p->m_state);
        }
        else
        {
            // An object is a garbage.
            p->m_object = Gc::moveObject(p->m_object);
            if (p->m_state == Kscheduled) p->m_state  = nil;
        }
    }
} // updateFinalization

} // namespace


//////////////////////////////////////////////////////////////////////
//
// Gc::updateWeakArea
//
void Gc::scanWeakArea(Area* pArea)
{
    OffsetT ofsFree = pArea->m_ofsFree;

    DEBUG_PRINTF(L"%p %X %u/%u\r\n",
        pArea,
        pArea->GetType(),
        pArea->m_ofsFree,
        pArea->m_cbArea );

    foreach (Area::EnumObject, oEnum, pArea)
    {
        Val x = oEnum.Get();
        switch (CLASSD_INDEX_OF(x->Decode<Record>()->m_classd))
        {
        case CLASSD_(finalization):
            updateFinalization(x);
            break;

        case CLASSD_(weak_pointer):
        {
            WeakPointer* p = x->Decode<WeakPointer>();
            if (Gc::IsGarbage(p->m_value))
            {
                p->m_value = nil;
            }
            break;
        } // weak_pointer

        case CLASSD_(weak_vector_leader):
            fix_vector(x);
            break;

        case CLASSD_(simple_vector):
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch classd
    } // for each object

    pArea->m_ofsScan = ofsFree;
} // Gc::scanWeakArea

} // Kernel
