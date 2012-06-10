#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - platform - win - big - console
// platform/win/big/win_big_21_console.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/big/win_big_21_console.cpp#4 $
//
// Description:
//  TBD
//
#include "../../../big/big_lisp.h"
#include "../../build.h"

#include "../mini/win_mini_21_stream.h"

#include <intrin.h> // _InterlockedCompareExhcange

namespace MiniLisp
{

// handlerRoutine
static BOOL WINAPI handlerRoutine(DWORD dwCtrlType)
{
    static long s_fInterrupt;

    if (0 != ::_InterlockedCompareExchange(&s_fInterrupt, 1, 0))
    {
        // Recursive interrupt.
        return TRUE;
    }

    switch (dwCtrlType)
    {
    case CTRL_C_EVENT:
        // invoke function keyboard-interrupt
        break;

    case CTRL_BREAK_EVENT:
        // NYI: complete restart?
    case CTRL_CLOSE_EVENT:
        // NYI: close file streams? invoke finalizations?
    case CTRL_LOGOFF_EVENT:
        // NYI: same as CLOSE_EVENT?
    case CTRL_SHUTDOWN_EVENT:
        // Note: This event is sent to service application instead of
        // interactive application.
        // NYI: same as LOGOFF_EVENT?
    default:
        return FALSE;
    } // swtich dwCtrlType

    ConsoleStreamImpl::sm_pThread->Interrupt(Qkeyboard_interrupt);

    while (1 != ::_InterlockedCompareExchange(&s_fInterrupt, 0, 1))
    {
        // nothing to do
    }

    return TRUE;
} // handlerRoutine

Thread* ConsoleStreamImpl::sm_pThread;

// ConsoleStreamImpl::init
void ConsoleStreamImpl::init()
{
    // BUGBUG: NYI: If we are invoked as batch mode, we don't set
    // title and ctrl handler.
    ::SetConsoleTitle(L"Evita Common Lisp " MY_FileVersionW);
    sm_pThread = MiniThread::Get();
    if (! ::SetConsoleCtrlHandler(handlerRoutine, TRUE))
    {
        Debugger::Fail(L"SetConsoleCtrlHandler");
    }
} // ConsoleStreamImpl::init

} // MiniLisp
