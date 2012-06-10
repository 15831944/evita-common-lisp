#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Direct Call Manager
// kernel/ke_direct_call.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_50_lock.cpp#3 $
//
#include "./mini_lisp.h"


namespace MiniLisp
{

//////////////////////////////////////////////////////////////////////
//
// lock_latch
//
Val lock_latch(Val latch, Val mode)
{
    ASSERT(latch_p(latch));
    ASSERT(Kshared == mode || Kexclusive == mode);

    latch->Decode<Latch>()->Lock(MiniThread::Get(), mode);
    return latch;
} // lock_latch


//////////////////////////////////////////////////////////////////////
//
// make_latch
//
Val make_latch(Val name)
{
    check_type(name, symbol);

    Val latch = MiniThread::Get()->AllocRecord(CLASSD_latch);
    Latch* pLatch = latch->Decode<Latch>();
        pLatch->m_thread     = nil;
        pLatch->m_state      = nil;
        pLatch->m_name       = name;
        pLatch->m_lock_count = Fixnum::Encode(0);
        pLatch->m_spinlock   = nil;
    return latch;
} // make_latch


//////////////////////////////////////////////////////////////////////
//
// unlock_latch
//
Val unlock_latch(Val latch)
{
    ASSERT(latch_p(latch));

    latch->Decode<Latch>()->Unlock();
    return latch;
} // unlock_latch

} // MiniLisp
