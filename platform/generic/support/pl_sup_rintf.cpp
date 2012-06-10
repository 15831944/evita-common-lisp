#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - platform - generic - support - rintf
// platform/generic/missing/pl_sup_rintf.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/generic/support/pl_sup_rintf.cpp#3 $
//

#undef t

union Union32
{
    float32 f;
    uint32  i;
}; // Union32

#define GET_FLOAT_WORD(mp_i, mp_f) \
    { Union32 u; u.f= mp_f; mp_i = u.i; }

#define SET_FLOAT_WORD(mp_f, mp_i) \
    { Union32 u; u.i = mp_i; mp_f = u.f; }

static const float TWO23[2]={
  8.3886080000e+06, /* 0x4b000000 */
 -8.3886080000e+06, /* 0xcb000000 */
}; // TWO23

extern "C" float __cdecl rintf(float x)
{
    int32 i0,j0,sx;
    uint32 i,i1;
    float w,t;
    GET_FLOAT_WORD(i0,x);
    sx = (i0>>31)&1;
    j0 = ((i0>>23)&0xff)-0x7f;
    if(j0<23) {
        if(j0<0) {     
        if((i0&0x7fffffff)==0) return x;
        i1 = (i0&0x07fffff);
        i0 &= 0xfff00000;
        i0 |= ((i1|-(int32) i1)>>9)&0x400000;
        SET_FLOAT_WORD(x,i0);
            w = TWO23[sx]+x;
            t =  w-TWO23[sx];
        GET_FLOAT_WORD(i0,t);
        SET_FLOAT_WORD(t,(i0&0x7fffffff)|(sx<<31));
            return t;
        } else {
        i = (0x007fffff)>>j0;
        if((i0&i)==0) return x; /* x is integral */
        i>>=1;
        if((i0&i)!=0) i0 = (i0&(~i))|((0x100000)>>j0);
        }
    } else {
        if(j0==0x80) return x+x;    /* inf or NaN */
        else return x;        /* x is integral */
    }
    SET_FLOAT_WORD(x,i0);
    w = TWO23[sx]+x;
    return w-TWO23[sx];
} // rintf
