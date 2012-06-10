#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - lisp
// genesis/geneis_lisp.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_executive.cpp#3 $
//

#include "./ke_executive.h"
#include "./ke_memory.h"
#include "./ke_thread.h"

namespace Kernel
{

static Executive s_oExecutive;
Executive* Executive::sm_pExecutive = &s_oExecutive;

void Executive::EnumThread::Next()
    { ASSERT(! AtEnd()); m_pRunner = m_pRunner->m_pNextThread; }


//////////////////////////////////////////////////////////////////////
//
// Executive::AddThread
//
Thread* Executive::AddThread(Thread* pThread)
{
    if (NULL == m_pFirstThread)
    {
        m_pFirstThread = pThread;
        m_pLastThread  = pThread;
    }
    else
    {
        m_pLastThread->m_pNextThread = pThread;
        pThread->m_pPrevThread = pThread; 
        m_pLastThread= pThread;
    }

    return pThread;
} // Executive::AddThread


//////////////////////////////////////////////////////////////////////
//
// Start executive
//
void
Executive::Start(
    size_t  cbAlloc )
{
    Memory::Start(cbAlloc);
} // Executive::Start


bool
Value_::IsHeap() const
{
    return Memory::IsHeap(const_cast<Value_*>(this));
} // Value::IsHeap

} // Kernel
