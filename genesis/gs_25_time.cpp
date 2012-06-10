#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 25 Environment - Time
// genesis/gs_25_inspect.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_25_time.cpp#4 $
//
// Description:
//  This file contains C implementation of lisp functions.
//      encode_universal_time
//      get_decoded_time
//      get_universal_time
//
#include "./gs_lisp.h"

namespace CommonLisp
{

static const Val kv_days_before_month[14] =
{
    nil,                    //  0
    Fixnum::Encode(0),      //  1 Jan
    Fixnum::Encode(31),     //  2 Feb
    Fixnum::Encode(59),     //  3 Mar
    Fixnum::Encode(90),     //  4 Apr
    Fixnum::Encode(120),    //  5 May
    Fixnum::Encode(151),    //  6 Jun
    Fixnum::Encode(181),    //  7 Jul
    Fixnum::Encode(212),    //  8 Aug
    Fixnum::Encode(243),    //  9 Sep
    Fixnum::Encode(273),    // 10 Oct
    Fixnum::Encode(304),    // 11 Nov
    Fixnum::Encode(334),    // 12 Dec
    Fixnum::Encode(365),    // 13 Jan
}; // kv_days_before_month

#if 0
#define add add_xx
#define cmp cmp_xx
#define mul mul_xx
#define sub sub_xx
#define truncate    truncate_xx
#endif

// Description:
//  Returns number of leap years between 1900 and specified year.
Val leap_years_before(Val year)
{
    Val nyears = sub(year, 1901);
    return
        add(
            sub(truncate(nyears, 4), truncate(nyears, 100)),
            truncate(add(nyears, 300), 400) );
} // leap_years_before


// encode_universal_time
Val encode_universal_time(Val s, Val n, Val h, Val d, Val m, Val y, Val z)
{
    Thread* p = MiniThread::Get();

    if (cmp(y, 100) < 0)
    {
        get_decoded_time();
        Val year = p->mv_value[5];
        y = add(y, sub(year, Fixnum::Decode_(year) % 100));

        if (cmp(sub(y, year), -50) < 0)
        {
            y = add(y, 100);

        }
        if (cmp(sub(y, year), 50) >= 0)
        {
            y = sub(y, 100);
        }
    } // if

    if (nil == z)
    {
        get_decoded_time();
        if (nil != p->mv_value[7])  // daylight-p
        {
            if (plusp_xx(z))
            {
                z = sub(z, 1);
            }
            else
            {
                z = add(z, 1);
            }
        }
    } // if

    Val ndays = add(sub(d, 1), kv_days_before_month[Fixnum::Decode_(m)]);
        if (cmp(m, 2) > 0)
        {
            ndays = add(ndays, leap_years_before(add(y, 1)));
        }
        else
        {
            ndays = add(ndays, leap_years_before(y));
        }

        ndays = add(ndays, mul(sub(y, 1900), 365));

    Val nhours = add(h, mul(ndays, 24));
    return add(s, mul(add(n, mul(add(nhours, z), 60)), 60));
} // encode_universal_time


// get_universal_time
Val get_universal_time()
{
    get_decoded_time();

    Thread* p = MiniThread::Get();

    Val sec  = p->mv_value[0];
    Val min  = p->mv_value[1];
    Val hour = p->mv_value[2];

    Val day  = p->mv_value[3];
    Val mon  = p->mv_value[4];
    Val year = p->mv_value[5];

    //Val dow  = p->mv_value[6];

    //Val dlp  = p->mv_value[7];

    Val bias = p->mv_value[8];

    return encode_universal_time(sec, min, hour, day, mon, year, bias);
} // get_universal_time

} // CommonLisp
