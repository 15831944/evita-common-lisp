//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - pre-compiled header
// kernel_kernel.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_executive.h#2 $
//
#if !defined(INCLUDE_kernel_kernel_h)
#define INCLUDE_kernel_kernel_h

namespace Kernel
{

class Thread;

//////////////////////////////////////////////////////////////////////
//
// Executive
//
class Executive
{
    public: static void Start(size_t);
    public: Thread* m_pFirstThread;
    public: Thread* m_pLastThread;

    public: static Executive* Get()
        { return sm_pExecutive; }

    protected: static Executive* sm_pExecutive;

    public: Executive() :
        m_pFirstThread(NULL),
        m_pLastThread(NULL) {}

    public: Thread* AddThread(Thread*);

    public: class EnumThread
    {
        protected: Thread* m_pRunner;
        public: EnumThread(Executive* p) :
            m_pRunner(p->m_pFirstThread) {}
        public: bool AtEnd() const { return NULL == m_pRunner; }
        public: Thread* Get() const { ASSERT(! AtEnd()); return m_pRunner; }
        public: void Next();
    }; // EnumThread
}; // Executive

} // Kernel

#endif //!defined(INCLUDE_kernel_kernel_h)
