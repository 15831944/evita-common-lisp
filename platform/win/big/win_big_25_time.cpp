#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 25 Environment - Time
// platform/win/big/big_25_time
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/big/win_big_25_time.cpp#4 $
//
// Description:
//  This file contains C implementation of lisp functions.
//      save_image
//
#include "../../../big/big_lisp.h"

namespace CommonLisp
{

// get_decoded_time
Val get_decoded_time()
{
    TIME_ZONE_INFORMATION oInfo;
    DWORD nInfo = ::GetTimeZoneInformation(&oInfo);

    // BUGBUG: bias can be ratio.
    Val bias = div(Fixnum::Encode(oInfo.Bias), 60);

    SYSTEMTIME stNow;
        ::GetLocalTime(&stNow);

    Thread* p = MiniThread::Get();

    p->mv_value[0] = Fixnum::Encode(stNow.wSecond);
    p->mv_value[1] = Fixnum::Encode(stNow.wMinute);
    p->mv_value[2] = Fixnum::Encode(stNow.wHour);
    p->mv_value[3] = Fixnum::Encode(stNow.wDay);
    p->mv_value[4] = Fixnum::Encode(stNow.wMonth);
    p->mv_value[5] = Fixnum::Encode(stNow.wYear);
    p->mv_value[6] = Fixnum::Encode((stNow.wDayOfWeek + 6) % 7);
    p->mv_value[7] = TIME_ZONE_ID_DAYLIGHT == nInfo ? t : nil;
    p->mv_value[8] = bias;

    p->m_n = Fixnum::Encode(9);
    return p->mv_value[0];
} // get_decoded_time

} // CommonLisp
