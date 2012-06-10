//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 50 Extensions - Lock
// mini/mini_50_lock.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_50_lock.h#2 $
//
#if !defined(INCLUDE_mini_50_lock_h)
#define INCLUDE_mini_50_lock_h

namespace MiniLisp
{
    bool latch_p(Val x);
    bool mutex_p(Val x);
    Val latch_state(Val x);

    Val lock_latch(Val, Val);
    Val make_latch(Val);
    Val unlock_latch(Val);
    Val try_lock_latch(Val, Val);

    Val lock_mutex(Val);
    Val make_mutex(Val);
    Val unlock_mutext(Val);
    Val try_lock_mutex(Val);
} // MiniLisp

namespace MiniLisp
{
    // WithLatch
    class WithLatch
    {
        protected: Val      m_latch;

        public: WithLatch(Val latch, Val mode) :
            m_latch(latch)
        {
            lock_latch(m_latch, mode);
        } // WithLatch

        public: ~WithLatch()
            { unlock_latch(m_latch); }
    }; // WithLatch

    #if defined(EVCL_BOOT)
        #define with_shared_latch(mp_latch)
        #define with_exclusive_latch(mp_latch)
        #define assert_latch_locked(mp_latch)
    #else // defined(EVCL_BOOT)
        #define with_shared_latch(mp_latch) \
            WithLatch oWithLatch(mp_latch, Kshared)

        #define with_exclusive_latch(mp_latch) \
            WithLatch oWithLatch(mp_latch, Kexclusive)

        #define assert_latch_locked(mp_latch) \
            ASSERT(mp_latch->Decode<Latch>()->IsLocked());
    #endif // defined(EVCL_BOOT)
} // MiniLisp

#endif //!defined(INCLUDE_mini_50_lock_h)
