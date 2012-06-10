#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - platform - windows - thread
// platform/win/kernel/ke_thread.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/kernel/win_ke_thread.cpp#2 $
//
#include "../../../kernel/ke_thread.h"

#include "../../../kernel/ke_memory.h"

#include "../../../arch/x86x64/kernel/x86x64_ke_layout.h"

#include "../../../big/big_lisp.h"

namespace Kernel
{

using namespace MiniLisp;

Val map_ra_to_fn(UInt);

static const int k_cbPageSize = 4096;

namespace
{

// locate_function
static Val locate_function(UInt nIp)
{
    Area* pArea = Memory::MapToArea(reinterpret_cast<void*>(nIp));
    if (NULL == pArea)
    {
        // Exception is occured in foreign function. Stack must have
        // "To Foreign" frame.
        return nil;
    }

    if (pArea->GetType() != Area::ScanType_Function)
    {
        // This can't be happend.
        return nil;
    }

    return map_ra_to_fn(nIp);
} // locate_function


// filter_exception
static int filter_exception(EXCEPTION_POINTERS* pInfo)
{
    EXCEPTION_RECORD* pRecord = pInfo->ExceptionRecord;

    Thread* p = Thread::Get();

    UInt nSp;
    #if MACH == MACH_x86
        nSp = pInfo->ContextRecord->Esp;
    #endif // MACH == MACH_x86
    #if MACH == MACH_x64
        nSp = pInfo->ContextRecord->Rsp;
    #endif // MACH == MACH_x64

    UInt const nIp = reinterpret_cast<UInt>(pRecord->ExceptionAddress);

    Val const fn = locate_function(nIp);

    if (EXCEPTION_STACK_OVERFLOW == pRecord->ExceptionCode)
    {
        p->mv_value[1] = reinterpret_cast<Val>(nSp & ~(k_cbPageSize-1));
        p->mv_value[0] = fn;
        p->m_n = Fixnum::Encode(1);
        return EXCEPTION_EXECUTE_HANDLER;
    }

    p->mv_value[1] = Kfunction;
    p->mv_value[2] = fn;
    p->mv_value[3] = Kip;

    if (nil == fn)
    {
        p->mv_value[4] = make_uint(nIp);
    }
    else
    {
        // Put exception ip on stack to simulate we call function error
        // at this ip.
        nSp -= sizeof(Val);
        #if MACH == MACH_x64
        {
            // Align stack to 16.
            nSp -= sizeof(Val);
        }
        #endif // MACH == MACH_x64

        reinterpret_cast<UInt*>(nSp)[0] = nIp;
        UInt const nIpOfs =
            nIp - (fn->ToInt() + sizeof(FunObj) - Funcallable::Tag);
        p->mv_value[4] = make_uint(nIpOfs);
    }

    switch (pRecord->ExceptionCode)
    {
    case EXCEPTION_ACCESS_VIOLATION:
        p->mv_value[0] = Qaccess_violation;
        p->mv_value[5] = Kaddress;
        p->mv_value[6] = make_uint(pRecord->ExceptionInformation[1]);
        p->m_n = Fixnum::Encode(7);
        break;

    default:
        p->mv_value[0] = Qplatform_exception;
        p->mv_value[5] = Kcode;
        p->mv_value[6] = make_uint(pRecord->ExceptionCode);
        p->m_n = Fixnum::Encode(7);
        break;
    } // switch code

    p->m_fn = Qerror->Decode<Symbol>()->m_function;

    #if MACH == MACH_x86
    {
        pInfo->ContextRecord->Esp = static_cast<DWORD>(nSp);

        pInfo->ContextRecord->Ecx =
            static_cast<DWORD>(reinterpret_cast<UInt>(p));

        pInfo->ContextRecord->Eip =
            static_cast<DWORD>(reinterpret_cast<UInt>(GoToLisp));
    }
    #endif // MACH == MACH_x86

    #if MACH == MACH_x64
    {
        pInfo->ContextRecord->Rsp = nSp;
        pInfo->ContextRecord->Rcx = reinterpret_cast<UInt>(p);

        // FIXME 2009-04-05 yosi@msn.com We should call lisp funciton error
        // instead of GoToLisp.
        pInfo->ContextRecord->Rip = reinterpret_cast<UInt>(GoToLisp);
    }
    #endif // MACH == MACH_x64

    return EXCEPTION_CONTINUE_EXECUTION;
} // filter_exception

} // namespace


// Thread::Start
void __declspec(noreturn) Thread::Start()
{
    MachInit();

    for (;;)
    {
        __try
        {
            CallLisp(this);
        }
        __except (filter_exception(GetExceptionInformation()))
        {
            // Stack overflow

            DWORD dwProtect;
            BOOL fSucceeded = ::VirtualProtect(
                mv_value[1],
                k_cbPageSize,
                PAGE_READWRITE | PAGE_GUARD,
                &dwProtect );
            if (! fSucceeded)
            {
                DEBUG_PRINTF(L"VirtualProtect PAGE_GUARD: %x",
                    ::GetLastError() );
            }

            m_fp = Fixnum::Encode(0);
            m_pObStackArea = &m_oEmptyObStackArea;
            m_fn = GetTlv(TLV_Astack_overflow_handlerA);
        }
    } // for
} // Thread::Start

} // Kernel
