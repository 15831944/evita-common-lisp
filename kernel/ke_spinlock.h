//////////////////////////////////////////////////////////////////////////////
//
// evcl - Kernel - Spinlock
// ke_spinlock.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_spinlock.h#2 $
//
#if !defined(INCLUDE_kernel_ke_spinlock_h)
#define INCLUDE_kernel_ke_spinlock_h

namespace Kernel
{

//////////////////////////////////////////////////////////////////////
//
// Spin Lock
//
class SpinLock
{
    protected: mutable Val m_locked;
    protected: static UINT sm_nSpinCount;

    public: static SpinLock* Convert(Val* p_spinlock)
    {
        ASSERT(nil == *p_spinlock);
        return reinterpret_cast<SpinLock*>(p_spinlock);
    } // Convert

    public: static void Init()
    {
        SYSTEM_INFO oInfo;
            ::GetSystemInfo(&oInfo);

        if (oInfo.dwNumberOfProcessors >= 2)
        {
            sm_nSpinCount = 4000;
        }
        else
        {
            sm_nSpinCount = 0;
        }
    } // Init

    public: void Lock() const
    {
        if (TryLock())
        {
            return;
        }

        UINT nSpinCount = sm_nSpinCount;

        if (nSpinCount >= 1)
        {
            while (nSpinCount >= 1)
            {
                if (TryLock())
                {
                    return;
                }

                #if _M_IX86
                    // PAUSE instruction for Pentium 4 or grater.
                    __asm
                    {
                        __emit 0xF3;
                        __emit 0x90;
                    } // _asm
                #endif //_M_IX86

                nSpinCount -= 1;
            } // while
        }

        for (;;)
        {
            ::SwitchToThread();

            if (TryLock())
            {
                return;
            }
        } // for
    } // Lock

    public: bool TryLock() const
    {
        return nil == ::InterlockedCompareExchangePointer(
            reinterpret_cast<void**>(&m_locked),
            Klocked,
            nil );
    } // TryLock

    public: void Unlock() const
    {
        ASSERT(Klocked == m_locked);
        m_locked = nil;
    } // Unlock

    // Scope - Helper class
    public: class Scope
    {
        protected: SpinLock* m_pLock;

        public: Scope(SpinLock* pLock)
        {
            ASSERT(NULL != pLock);
            m_pLock = pLock;
            m_pLock->Lock();
        } // Scope

        public: ~Scope()
        {
            m_pLock->Unlock();
        } // ~Scope
    }; // Scope
}; // SpinLock


}; // Kernel

#endif //!defined(INCLUDE_kernel_ke_spinlock_h)
