#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 25 Environment
// genesis/gs_25_env.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_25_env.cpp#4 $
//
// Description:
//  This file contains C implementation of lisp functions.
//      expand_time
//      Ptime
//
#include "./gs_lisp.h"

#include "../kernel/ke_memory.h"

namespace Genesis
{

// expand_time
Val expand_time(Val form, Val)
{
    CHECK_SYNTAX(form, 2, 2, "(time form)");

    return list(Q("TIME-IT"),
        list(Qfunction, list(Qlambda, nil, second(form))) );
} // expand_time


inline LONGLONG
FileTimeDiff(
    const FILETIME* pftEnd,
    const FILETIME* pftStart )
{
    ULARGE_INTEGER oX;
        oX.LowPart  = pftEnd->dwLowDateTime;
        oX.HighPart = pftEnd->dwHighDateTime;

    ULARGE_INTEGER oY;
        oY.LowPart  = pftStart->dwLowDateTime;
        oY.HighPart = pftStart->dwHighDateTime;

    return (oX.QuadPart - oY.QuadPart) / 10000;
} // FileTimeDiff

// time_it
//  FIXME 2007-05-09 We should move this function to platform/win.
Val time_it(Val fn)
{
    class Time
    {
        public: FILETIME m_ftElapsed;
        public: FILETIME m_ftCreate;
        public: FILETIME m_ftExit;
        public: FILETIME m_ftKernel;
        public: FILETIME m_ftUser;

        public: Time()
        {
            ::GetSystemTimeAsFileTime(&m_ftElapsed);

            ::GetThreadTimes(
                ::GetCurrentThread(),
                &m_ftCreate,
                &m_ftExit,
                &m_ftKernel,
                &m_ftUser );
        } // Time
    };

    MiniThread* p = MiniThread::Get();

    p->m_fn = fn;
    p->m_n  = Fixnum::Encode(0);

    Time oStart;
        CallLisp(p);
    Time oEnd;

    Val result = nil;
    Int i = Fixnum::Decode_(p->m_n);
    while (i >= 1)
    {
        i -= 1;
        result = cons(p->mv_value[i], result);
    } // while

    GcRoot oResult(result);

    LONGLONG llElapsed =
        FileTimeDiff(&oEnd.m_ftElapsed, &oStart.m_ftElapsed);

    LONGLONG llKernel =
        FileTimeDiff(&oEnd.m_ftKernel, &oStart.m_ftKernel);

    LONGLONG llUser =
        FileTimeDiff(&oEnd.m_ftUser, &oStart.m_ftUser);

    LONGLONG llGC = 0;

    format(t, L"; CPU-time (non-gc) ~D msec user, ~D msec system~%",
        Fixnum::Encode(static_cast<Int>(llUser)),
        Fixnum::Encode(static_cast<Int>(llKernel)) );
    format(t, L"; CPU-time (gc)     ~D msec user, ~D msec system~%",
        Fixnum::Encode(static_cast<Int>(llUser)),
        Fixnum::Encode(static_cast<Int>(llKernel)) );
    format(t, L"; Elapsed Time      ~D msec real, ~D msec gc~%",
        Fixnum::Encode(static_cast<Int>(llElapsed)),
        Fixnum::Encode(static_cast<Int>(llGC)) );
    format(t, L";~%");

    result = oResult.Get();
    return values_list(result);
} // time_it

} // Genesis
