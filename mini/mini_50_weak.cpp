#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 50 Extensions - Weak Objects
// mini/mini_50_weak.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_50_weak.cpp#3 $
//
#include "./mini_lisp.h"

#include "../kernel/ke_memory.h"
#include "../kernel/ke_weak.h"

namespace MiniLisp
{

namespace 
{
    class Record : public Kernel::Record
    {
        public: Record(Val classd)
            { m_classd = classd; }
    };
} // namespace

// allocate_weakobj
//  See Also: MiniThread::AllocRecord
Val allocate_weakobj(Val classd)
{
    ClassD* pClassD = classd->Decode<ClassD>();
    size_t cbObject = pClassD->ComputeSize();
    void* pvWeakObj = Weak::Alloc(cbObject);
    Record* pWeakObj = new(pvWeakObj) Record(classd);
    Val weakobj = pWeakObj->Encode();
        ASSERT(weakobj->GetSize() == cbObject);
    return weakobj;
} // allocate_weakobj


// make_weak_vector
Val make_weak_vector(Val length, Val kind)
{
    if (Qfuncall   == kind ||
        Kkey     == kind ||
        Kvalue   == kind ||
        Keither  == kind ||
        Kboth    == kind ||
        Qvector    == kind ||
        Qpackage   == kind )
    {
        // ok
    }
    else
    {
        error(make_type_error(kind, list(Qmember,
            Kkey, Kvalue,
            Kboth, Keither,
            Qfuncall,
            Qvector,
            Qpackage )) );
    }

    size_t cbObject =
        sizeof(WeakVectorLeader) +
        CLASSD_simple_vector->Decode<ClassD>()->ComputeSize(length);

    WeakVectorLeader* pLeader = new(Weak::Alloc(cbObject))
        WeakVectorLeader(kind);

    pLeader++;
    new(pLeader) WeakVector(length);

    return pLeader->Encode();
} // make_weak_vector


// make_call_set
Val make_caller_set(Val length)
{
    Val caller_set = make_weak_vector(length, Qfuncall);
    setf_svref(Fixnum::Encode(1), caller_set, Fixnum::Encode(0));
    return caller_set;
} // make_caller_set

} // MiniLisp
