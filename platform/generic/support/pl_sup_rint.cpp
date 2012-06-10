#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - platform - generic - support - rint
// platform/generic/missing/pl_sup_rint.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/generic/support/pl_sup_rint.cpp#3 $
//
#undef t

#if FLOAT_WORD_ORDER == BIG_ENDIAN

    union Union64
    {
        float64 f;
        struct { int32 h; int32 l; } hl;
    }; // Union64

#elif FLOAT_WORD_ORDER == LITTLE_ENDIAN

    union Union64
    {
        float64 f;
        struct { int32 l; int32 h; } w;
    }; // Union64

#else // FLOAT_WORD_ORDER == LITTLE_ENDIAN
    #error Unsupported FLOAT_WORD_ORDER
#endif // FLOAT_WORD_ORDER

#define EXTRACT_WORDS(mp_h, mp_l, mp_f) \
    { Union64 u; u.f= mp_f; mp_h = u.w.h; mp_l = u.w.l; }

#define INSERT_WORDS(m_f, mp_h, mp_l) \
    { Union64 u; u.w.h = mp_h; u.w.l = mp_l; m_f = u.f; }

#define GET_HIGH_WORD(mp_h, mp_f) \
    { Union64 u; u.f= mp_f; mp_h = u.w.h; }

#define GET_LOW_WORD(mp_l, mp_f) \
    { Union64 u; u.f= mp_f; mp_l = u.w.l; }

#define SET_HIGH_WORD(mp_f, mp_h) \
    { Union64 u; u.f = mp_f; u.w.h = mp_h; }

#define SET_LOW_WORD(mp_f, mp_l) \
    { Union64 u; u.f = mp_f; u.w.l = mp_l; }

static const double TWO52[2]={
  4.50359962737049600000e+15, /* 0x43300000, 0x00000000 */
 -4.50359962737049600000e+15, /* 0xC3300000, 0x00000000 */
};

extern "C" double __cdecl rint(double x)
{
    int32 i0,j0,sx;
    int32 i,i1;
    double w,t;
    EXTRACT_WORDS(i0,i1,x);
    sx = (i0>>31)&1;
    j0 = ((i0>>20)&0x7ff)-0x3ff;
    if(j0<20) {
        if(j0<0) {
        if(((i0&0x7fffffff)|i1)==0) return x;
        i1 |= (i0&0x0fffff);
        i0 &= 0xfffe0000;
        i0 |= ((i1|-i1)>>12)&0x80000;
        SET_HIGH_WORD(x,i0);
            w = TWO52[sx]+x;
            t =  w-TWO52[sx];
        GET_HIGH_WORD(i0,t);
        SET_HIGH_WORD(t,(i0&0x7fffffff)|(sx<<31));
            return t;
        } else {
        i = (0x000fffff)>>j0;
        if(((i0&i)|i1)==0) return x; /* x is integral */
        i>>=1;
        if(((i0&i)|i1)!=0) {
            if(j0==19) i1 = 0x40000000; else
            i0 = (i0&(~i))|((0x20000)>>j0);
        }
        }
    } else if (j0>51) {
        if(j0==0x400) return x+x;    /* inf or NaN */
        else return x;        /* x is integral */
    } else {
        i = ((uint32)(0xffffffff))>>(j0-20);
        if((i1&i)==0) return x;    /* x is integral */
        i>>=1;
        if((i1&i)!=0) i1 = (i1&(~i))|((0x40000000)>>(j0-20));
    }
    INSERT_WORDS(x,i0,i1);
    w = TWO52[sx]+x;
    return w-TWO52[sx];
} // rint
