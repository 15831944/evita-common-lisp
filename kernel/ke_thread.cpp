#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - thread
// kernel/ke_thread.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_thread.cpp#8 $
//
#include "./ke_thread.h"

#include "./ke_executive.h"
#include "./ke_host.h"
#include "./ke_memory.h"

#include "../mini/mini_lisp.h"

namespace Kernel
{

#if USE_DECLSPEC_THREAD
    __declspec(thread) Thread* Thread::sm_pTlsThread;
#else // USE_DECLSPEC_THREAD
    DWORD Thread::sm_dwTlsThread = TLS_OUT_OF_INDEXES ;
#endif // USE_DECLSPEC_THREAD


//////////////////////////////////////////////////////////////////////
//
// Thread constructor
//
Thread::Thread(HANDLE hThread) :
        m_name(nil),
        m_hThread(hThread),
        m_waiting(nil),
        m_next_waiter(nil),
        m_pNextThread(NULL),
        m_pPrevThread(NULL),
        m_pObStackArea(&m_oEmptyObStackArea)
{
    m_classd = CLASSD_thread;

    if (NULL == hThread)
    {
        BOOL fSucceeded = ::DuplicateHandle(
            ::GetCurrentProcess(),
            ::GetCurrentThread(),
            ::GetCurrentProcess(),
            &m_hThread,
            0,
            FALSE,
            DUPLICATE_SAME_ACCESS );
        if (! fSucceeded)
        {
            //DWORD dwError = ::GetLastError();
            //REPORT_WIN32_ERROR("DuplicateHandle", dwError);
            //return HRESULT_FROM_WIN32(dwError);
            return;
        }

        #if ! USE_DECLSPEC_THREAD
            sm_dwTlsThread = ::TlsAlloc();

            if (TLS_OUT_OF_INDEXES == sm_dwTlsThread)
            {
                return;
            }
        #endif // ! USE_DECLSPEC_THREAD
    } // if

    init_tls();

    // Initialize TLV
    for (int i = 0; i < lengthof(mv_tlv); i++)
    {
        mv_tlv[i] = QQunbound_marker;
    } // for i
} // Thread::Thread


//////////////////////////////////////////////////////////////////////
//
// Thread initialization
//
void
Thread::Init(size_t)
{
    Executive::Get()->AddThread(this);

    ResetAlloc();

    #if ! defined(EVCL_BOOT)
    {
        Int n = Fixnum::Decode_(svref(VAR(Atlv_vectorA), Fixnum::Encode(0)));
        for (Int i = 1; i <= n; i++)
        {
            Val tlvrec = svref(VAR(Atlv_vectorA), i);
            set_tlv(
                tlv_record_index(tlvrec), 
                tlv_record_value(tlvrec) ); 
        } // for i
    }
    #endif // ! defined(EVCL_BOOT)
} // Thread::Init


//////////////////////////////////////////////////////////////////////
//
// Thread::ResetAlloc
//
void
Thread::ResetAlloc()
{
    m_pConsArea     = Area::GetEmpty();
    m_pBinObjArea   = Area::GetEmpty();
    m_pFunObjArea   = Area::GetEmpty();
    m_pRecordArea   = Area::GetEmpty();
} // Thread::ResetAlloc


//////////////////////////////////////////////////////////////////////
//
// SVC_C_stack_alloc
//
void* __fastcall
SVC_C_stack_alloc(Thread* pThread, size_t cbObject)
{
    cbObject = ROUNDUP(cbObject, Arch::ObStack_Align);

    ObStackArea* pArea = pThread->m_pObStackArea;
    for (;;)
    {
        void* pv = pArea->Alloc(cbObject);
        if (NULL != pv)
        {
            return pv;
        }

        if (NULL != pArea->m_pNext)
        {
            pArea = pArea->m_pNext->StaticCast<ObStackArea>();
            pArea->m_ofsFree = sizeof(Area);
        }
        else
        {
            ObStackArea* pNext = Memory::AllocObStackArea(cbObject);
            pArea->m_pNext = pNext;
            pNext->m_pPrev = pArea;
            pArea = pNext;
        } // if

        ASSERT(sizeof(Area) == pArea->m_ofsFree);
        pThread->m_pObStackArea = pArea;
    } // for
} // SVC_C_stack_alloc


#define define_alloc_area(mp_name, mp_Kind, mp_Name) \
    Area* __fastcall \
    SVC_C_alloc_##mp_name##_area(Thread* p, size_t cbObject) \
    { \
        ASSERT(0 == cbObject % Arch::mp_Name##_Align); \
        return p->m_p##mp_Name##Area = Memory::Alloc##mp_Kind##Area( \
            p, \
            Area::ScanType_##mp_Name, \
            cbObject ); \
    } // define_alloc_area

#define ScanType_FunObj ScanType_Function

define_alloc_area(bino, Data, BinObj)
define_alloc_area(code, Code, FunObj)
define_alloc_area(cons, Data, Cons)
define_alloc_area(reco, Data, Record)

} // Kernel
