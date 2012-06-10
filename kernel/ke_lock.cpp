#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - lock
// kernel/ke_lock.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_lock.cpp#5 $
//
#include "./ke_layout.h"

#include "./ke_spinlock.h"
#include "./ke_thread.h"

#include "../mini/mini_lisp.h"

#include "./ke_gc.h"

namespace Kernel
{

#if ! EVCL_BOOT
void dump_stack(Thread* p)
{
    Val n = Fixnum::Encode(0);

    foreach (Thread::EnumFrame, oEnum, p)
    {
        Frame* pFrame = oEnum.Get();
        if (pFrame->GetType() == Frame::Type_Function)
        {
            dbg_format(L"~D ~S~%",
                n, pFrame->StaticCast<FunctionFrame>()->m_fn );

            n = add_xx(n, 1);
        } // if
    } // for each frame
} // dump_stack
#endif // ! EVCL_BOOT


// Latch::Lock
void Latch::Lock(Thread* p, Val mode)
{
    ASSERT(nil == p->m_waiting);

    SpinLock* pSpinLock = SpinLock::Convert(&m_spinlock);

    for (;;)
    {
        pSpinLock->Lock();

        if (nil == m_state)
        {
            ASSERT(Fixnum::Encode(0) == m_lock_count);
            ASSERT(nil == m_thread);

            m_state = Kexclusive == mode ? p->Encode() : mode;
            m_lock_count = Fixnum::Encode(1);
            break;
        } // if

        if (Kshared == m_state)
        {
            if (Kshared == mode)
            {
                m_lock_count = add_xx(m_lock_count, 1);
                break;
            }
        }
        else if (p->Encode() == m_state)
        {
            #if ! EVCL_BOOT
                //dump_stack(p);
            #endif // ! EVCL_BOOT

            m_lock_count = add_xx(m_lock_count, 1);
            break;
        }
        else
        {
            CAN_NOT_HAPPEN();
        }

        p->m_waiting = Encode();
        p->m_next_waiter = m_thread;

        pSpinLock->Unlock();

        p->Suspend();
    } // forever

    pSpinLock->Unlock();
} // Latch::Lock


// Latch::Unlock
void Latch::Unlock()
{
    SpinLock* pSpinLock = SpinLock::Convert(&m_spinlock);

    Val waiter;
    {
        pSpinLock->Lock();

        ASSERT(Fixnum::Encode(0) != m_lock_count);

        m_lock_count = sub_xx(m_lock_count, 1);

        if (Fixnum::Encode(0) != m_lock_count)
        {
            waiter = nil;
        }
        else
        {
            m_state = nil;
            waiter = m_thread;
            m_thread = nil;
        }

        pSpinLock->Unlock();
    } // waiter

    while (nil != waiter)
    {
        Thread* pWaiter = reinterpret_cast<Thread*>(waiter);
        waiter = pWaiter->WakeUp(Encode());
    } // while
} // Latch::Unlock

} // Kernel
