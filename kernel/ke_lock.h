//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Memory Manager
// kernel/ke_lock.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_lock.h#2 $
//
#if !defined(INCLUDE_kernel_lock_h)
#define INCLUDE_kernel_lock_h

namespace Kernel
{

class LatchLock
{
    Latch* m_pLatch;

    public: LatchLock(Thread* pThread, Latch* pLatch, Val mode) :
            m_pLatch(pLatch)
        { m_pLatch->Lock(pThread, mode); }

    public: ~LatchLock()
        { m_pLatch->Unlock(); }
}; // LatchLock


} // Kernel

#endif //!defined(INCLUDE_kernel_lock_h)
