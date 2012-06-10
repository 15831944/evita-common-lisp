#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 12 Numbers
// big/big_12_number.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_12_ratio.cpp#4 $
//
#include "./big_lisp.h"

#include "./big_12_ratio.h"


namespace MiniLisp
{

//////////////////////////////////////////////////////////////////////
//
// RatioImpl::Add
//
// n1   n2   n1d2 + n2d1
// -- + -- = ----------
// d1   d2      d1d2
//
Val RatioImpl::Add(Val x, Val y)
{
    Val n1 = x->Decode<Ratio>()->m_num;
    Val d1 = x->Decode<Ratio>()->m_den;

    Val n2 = y->Decode<Ratio>()->m_num;
    Val d2 = y->Decode<Ratio>()->m_den;

    return Make(
        add(mul(n1, d2), mul(n2, d1)),
        mul(d1, d2) );
} // RatioImpl::Add


//////////////////////////////////////////////////////////////////////
//
// Ratio::Cmp
//
Int RatioImpl::Cmp(Val x, Val y)
{
    Val n1 = x->Decode<Ratio>()->m_num;
    Val d1 = x->Decode<Ratio>()->m_den;

    Val n2 = y->Decode<Ratio>()->m_num;
    Val d2 = y->Decode<Ratio>()->m_den;

    return cmp(mul(n1, d2), mul(n2, d1));
} // RatioImpl::Cmp


//////////////////////////////////////////////////////////////////////
//
// RatioImpl::Div
//
// n1   n2   n1d2
// -- / -- = -----
// d1   d2   d1n2
//
Val RatioImpl::Div(Val x, Val y)
{
    Val n1 = x->Decode<Ratio>()->m_num;
    Val d1 = x->Decode<Ratio>()->m_den;

    Val n2 = y->Decode<Ratio>()->m_num;
    Val d2 = y->Decode<Ratio>()->m_den;

    return Make(mul(n1, d2), mul(d1, n2));
} // RatioImpl::Div


//////////////////////////////////////////////////////////////////////
//
// RatioImpl::Make
//
Val RatioImpl::Make(Val num, Val den)
{
    if (Fixnum::Encode(0) == num)
    {
        return num;
    }

    Val k = gcd(num, den);

    // num and den can be stack bignum.
    num = truncate(num, k);
    den = truncate(den, k);

    if (Fixnum::Encode(1) == den)
    {
        return num;
    }

    if (Fixnum::Encode(-1) == den)
    {
        return sub(0, num);
    }

    if (minusp(den))
    {
        num = sub(0, num);
        den = sub(0, den);
    }

    Val x = MiniThread::Get()->AllocRecord(CLASSD_ratio);
        x->Decode<Ratio>()->m_num = num;
        x->Decode<Ratio>()->m_den = den;
    // BUGBUG: NYI: GCD
    return x;
} // Make


//////////////////////////////////////////////////////////////////////
//
// RatioImpl::Mul
//
// n1   n2   n1n2
// -- x -- = ----
// d1   d2   d1d2
//
Val RatioImpl::Mul(Val x, Val y)
{
    Val n1 = x->Decode<Ratio>()->m_num;
    Val d1 = x->Decode<Ratio>()->m_den;

    Val n2 = y->Decode<Ratio>()->m_num;
    Val d2 = y->Decode<Ratio>()->m_den;

    return Make(mul(n1, n2), mul(d1, d2));
} // Mul


//////////////////////////////////////////////////////////////////////
//
// Sub
//
// n1   n2   n1d2 - n2d1
// -- - -- = ----------
// d1   d2      d1d2
//
Val RatioImpl::Sub(Val x, Val y)
{
    Val n1 = x->Decode<Ratio>()->m_num;
    Val d1 = x->Decode<Ratio>()->m_den;

    Val n2 = y->Decode<Ratio>()->m_num;
    Val d2 = y->Decode<Ratio>()->m_den;

    Val n = sub(mul(n1, d2), mul(n2, d1));
    Val d = mul(d1, d2);
    return Make(n, d);
} // Sub


//////////////////////////////////////////////////////////////////////
//
// RatioImpl::ToFloat64
//
double RatioImpl::ToFloat64(Val x)
{
    double dblNum = convert_to_float64(x->Decode<Ratio>()->m_num);
    double dblDen = convert_to_float64(x->Decode<Ratio>()->m_den);
    return dblNum / dblDen;
} // ToFloat64


//////////////////////////////////////////////////////////////////////
//
// RatioImpl::ToFloat
//
float RatioImpl::ToFloat32(Val x)
{
    float fltNum = convert_to_float32(x->Decode<Ratio>()->m_num);
    float fltDen = convert_to_float32(x->Decode<Ratio>()->m_den);
    return fltNum / fltDen;
} // ToFloat


//////////////////////////////////////////////////////////////////////
//
// RatioImpl::Truncate
//
void RatioImpl::Truncate(Val x, Val y, Val* out_q, Val* out_r)
{
    Val n1 = x->Decode<Ratio>()->m_num;
    Val d1 = x->Decode<Ratio>()->m_den;

    Val n2 = y->Decode<Ratio>()->m_num;
    Val d2 = y->Decode<Ratio>()->m_den;

    truncate(mul(n1, d2), mul(n2, d1), out_q, out_r);
} // RatioImpl::Truncate

} // MiniLisp
