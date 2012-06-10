#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - Interval
// compiler/util/interval.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/util/interval.cpp#1 $
//
#include "./interval.h"

namespace Utility
{

Interval Interval::Nil(FixInt::NaN, FixInt::NaN);

//////////////////////////////////////////////////////////////////////
//
// FixInt operator+
//
FixInt operator+(const FixInt& x, const FixInt& y)
{
    switch (FixInt::Classify(x, y))
    {
    case FixInt::NormalNormal:   return FixInt(x.m_v + y.m_v);
    case FixInt::NormalNaN:      goto nan;
    case FixInt::NormalMinusInf: goto minf;
    case FixInt::NormalPlusInf:  goto pinf;

    case FixInt::NaNNormal:     goto nan;
    case FixInt::NaNNaN:        goto nan;
    case FixInt::NaNMinusInf:   goto nan;
    case FixInt::NaNPlusInf:    goto nan;

    case FixInt::MinusInfNormal:     goto minf;
    case FixInt::MinusInfNaN:        goto nan;
    case FixInt::MinusInfMinusInf:   goto minf;
    case FixInt::MinusInfPlusInf:    goto nan;

    case FixInt::PlusInfNormal:     goto pinf;
    case FixInt::PlusInfNaN:        goto nan;
    case FixInt::PlusInfMinusInf:   goto nan;
    case FixInt::PlusInfPlusInf:    goto pinf;

    nan:  return FixInt(FixInt::NaN);
    pinf: return FixInt(FixInt::PlusInf);
    minf: return FixInt(FixInt::MinusInf);
    } // switch class
    CAN_NOT_HAPPEN();
} // operator+


//////////////////////////////////////////////////////////////////////
//
// FixInt operator-
//
FixInt operator-(const FixInt& x, const FixInt& y)
{
    switch (FixInt::Classify(x, y))
    {
    case FixInt::NormalNormal:   return FixInt(x.m_v - y.m_v);
    case FixInt::NormalNaN:      goto nan;
    case FixInt::NormalMinusInf: goto pinf;
    case FixInt::NormalPlusInf:  goto minf;

    case FixInt::NaNNormal:     goto nan;
    case FixInt::NaNNaN:        goto nan;
    case FixInt::NaNMinusInf:   goto nan;
    case FixInt::NaNPlusInf:    goto nan;

    case FixInt::MinusInfNormal:     goto minf;
    case FixInt::MinusInfNaN:        goto nan;
    case FixInt::MinusInfMinusInf:   goto nan;
    case FixInt::MinusInfPlusInf:    goto minf;

    case FixInt::PlusInfNormal:     goto pinf;
    case FixInt::PlusInfNaN:        goto nan;
    case FixInt::PlusInfMinusInf:   goto pinf;
    case FixInt::PlusInfPlusInf:    goto minf;

    nan:  return FixInt(FixInt::NaN);
    pinf: return FixInt(FixInt::PlusInf);
    minf: return FixInt(FixInt::MinusInf);
    } // switch class
    CAN_NOT_HAPPEN();
} // operator-


//////////////////////////////////////////////////////////////////////
//
// FixInt operator&
//
FixInt operator&(const FixInt& x, const FixInt& y)
{
    switch (FixInt::Classify(x, y))
    {
    case FixInt::NormalNormal:   return FixInt(x.m_v & y.m_v);
    case FixInt::NormalNaN:      goto nan;
    case FixInt::NormalMinusInf: goto pinf;
    case FixInt::NormalPlusInf:  goto minf;

    case FixInt::NaNNormal:     goto nan;
    case FixInt::NaNNaN:        goto nan;
    case FixInt::NaNMinusInf:   goto nan;
    case FixInt::NaNPlusInf:    goto nan;

    case FixInt::MinusInfNormal:     goto minf;
    case FixInt::MinusInfNaN:        goto minf;
    case FixInt::MinusInfMinusInf:   goto minf;
    case FixInt::MinusInfPlusInf:    goto minf;

    case FixInt::PlusInfNormal:     goto pinf;
    case FixInt::PlusInfNaN:        goto nan;
    case FixInt::PlusInfMinusInf:   goto pinf;
    case FixInt::PlusInfPlusInf:    goto pinf;

    nan:  return FixInt(FixInt::NaN);
    pinf: return FixInt(FixInt::PlusInf);
    minf: return FixInt(FixInt::MinusInf);
    } // switch class
    CAN_NOT_HAPPEN();
} // operator&


//////////////////////////////////////////////////////////////////////
//
// FixInt operator|
//
FixInt operator|(const FixInt& x, const FixInt& y)
{
    switch (FixInt::Classify(x, y))
    {
    case FixInt::NormalNormal:   return FixInt(x.m_v | y.m_v);
    case FixInt::NormalNaN:      goto nan;
    case FixInt::NormalMinusInf: goto pinf;
    case FixInt::NormalPlusInf:  goto minf;

    case FixInt::NaNNormal:     goto nan;
    case FixInt::NaNNaN:        goto nan;
    case FixInt::NaNMinusInf:   goto nan;
    case FixInt::NaNPlusInf:    goto nan;

    case FixInt::MinusInfNormal:     goto minf;
    case FixInt::MinusInfNaN:        goto minf;
    case FixInt::MinusInfMinusInf:   goto minf;
    case FixInt::MinusInfPlusInf:    goto minf;

    case FixInt::PlusInfNormal:     goto pinf;
    case FixInt::PlusInfNaN:        goto nan;
    case FixInt::PlusInfMinusInf:   goto pinf;
    case FixInt::PlusInfPlusInf:    goto pinf;

    nan:  return FixInt(FixInt::NaN);
    pinf: return FixInt(FixInt::PlusInf);
    minf: return FixInt(FixInt::MinusInf);
    } // switch class
    CAN_NOT_HAPPEN();
} // operator|


//////////////////////////////////////////////////////////////////////
//
// FixInt operator^
//
FixInt operator^(const FixInt& x, const FixInt& y)
{
    switch (FixInt::Classify(x, y))
    {
    case FixInt::NormalNormal:   return FixInt(x.m_v ^ y.m_v);
    case FixInt::NormalNaN:      goto nan;
    case FixInt::NormalMinusInf: goto pinf;
    case FixInt::NormalPlusInf:  goto minf;

    case FixInt::NaNNormal:     goto nan;
    case FixInt::NaNNaN:        goto nan;
    case FixInt::NaNMinusInf:   goto nan;
    case FixInt::NaNPlusInf:    goto nan;

    case FixInt::MinusInfNormal:     goto minf;
    case FixInt::MinusInfNaN:        goto minf;
    case FixInt::MinusInfMinusInf:   goto minf;
    case FixInt::MinusInfPlusInf:    goto minf;

    case FixInt::PlusInfNormal:     goto pinf;
    case FixInt::PlusInfNaN:        goto nan;
    case FixInt::PlusInfMinusInf:   goto pinf;
    case FixInt::PlusInfPlusInf:    goto pinf;

    nan:  return FixInt(FixInt::NaN);
    pinf: return FixInt(FixInt::PlusInf);
    minf: return FixInt(FixInt::MinusInf);
    } // switch class
    CAN_NOT_HAPPEN();
} // operator^


//////////////////////////////////////////////////////////////////////
//
// FixInt operator~
//
FixInt operator~(const FixInt& x)
{
    switch (x.Classify())
    {
    case FixInt::Normal:    return FixInt(~x.m_v);
    case FixInt::NaN:       return FixInt(FixInt::NaN);
    case FixInt::MinusInf:  return FixInt(FixInt::PlusInf);
    case FixInt::PlusInf:   return FixInt(FixInt::MinusInf);
    } // switch class
    CAN_NOT_HAPPEN();
} // operator~


//////////////////////////////////////////////////////////////////////
//
// FixInt operator<
//
bool operator<(const FixInt& x, const FixInt& y)
{
    switch (FixInt::Classify(x, y))
    {
    case FixInt::NormalNormal:   return x.m_v < y.m_v;
    case FixInt::NormalNaN:      goto nan;
    case FixInt::NormalMinusInf: goto pinf;
    case FixInt::NormalPlusInf:  goto minf;

    case FixInt::NaNNormal:     goto nan;
    case FixInt::NaNNaN:        goto nan;
    case FixInt::NaNMinusInf:   goto nan;
    case FixInt::NaNPlusInf:    goto nan;

    case FixInt::MinusInfNormal:     goto minf;
    case FixInt::MinusInfNaN:        goto nan;
    case FixInt::MinusInfMinusInf:   goto nan;
    case FixInt::MinusInfPlusInf:    goto minf;

    case FixInt::PlusInfNormal:     goto pinf;
    case FixInt::PlusInfNaN:        goto nan;
    case FixInt::PlusInfMinusInf:   goto pinf;
    case FixInt::PlusInfPlusInf:    goto minf;

    nan:  return false;
    pinf: return false;
    minf: return true;
    } // switch class
    CAN_NOT_HAPPEN();
} // operator<


//////////////////////////////////////////////////////////////////////
//
// FixInt operator<
//
bool operator<=(const FixInt& x, const FixInt& y)
{
    switch (FixInt::Classify(x, y))
    {
    case FixInt::NormalNormal:   return x.m_v <= y.m_v;
    case FixInt::NormalNaN:      goto nan;
    case FixInt::NormalMinusInf: goto pinf;
    case FixInt::NormalPlusInf:  goto minf;

    case FixInt::NaNNormal:     goto nan;
    case FixInt::NaNNaN:        goto nan;
    case FixInt::NaNMinusInf:   goto nan;
    case FixInt::NaNPlusInf:    goto nan;

    case FixInt::MinusInfNormal:     goto minf;
    case FixInt::MinusInfNaN:        goto nan;
    case FixInt::MinusInfMinusInf:   goto nan;
    case FixInt::MinusInfPlusInf:    goto minf;

    case FixInt::PlusInfNormal:     goto pinf;
    case FixInt::PlusInfNaN:        goto nan;
    case FixInt::PlusInfMinusInf:   goto pinf;
    case FixInt::PlusInfPlusInf:    goto minf;

    nan:  return false;
    pinf: return false;
    minf: return true;
    } // switch class
    CAN_NOT_HAPPEN();
} // operator<=


// Interval +
Interval operator +(const Interval& x, const Interval& y)
{
    return Interval(x.m_lower + y.m_lower, x.m_upper + y.m_upper);
} // operator +

// Interval operator -
Interval operator -(const Interval& x, const Interval& y)
{
    return Interval(x.m_lower - y.m_upper, x.m_upper - y.m_lower);
} // operator -


static uint intlen(Int x)
{
} // intlen


// Interval operator &
Interval operator &(const Interval& x, const Interval& y)
{
    switch (Interval::Classify(x, y))
    {
    case Interval::ClClClCl: goto close_close;
    case Interval::ClClClOp: goto close_open;
    case Interval::ClClOpCl: goto open_close;
    case Interval::ClClOpOp: goto open_open;
    case Interval::ClOpClCl: goto close_open;
    case Interval::ClOpClOp: goto close_open;
    case Interval::ClOpOpCl: goto open_open;
    case Interval::ClOpOpOp: goto open_open;

    case Interval::OpClClCl: goto open_close;
    case Interval::OpClClOp: goto open_open;
    case Interval::OpClOpCl: goto open_close;
    case Interval::OpClOpOp: goto open_open;
    case Interval::OpOpClCl: goto open_open;
    case Interval::OpOpClOp: goto open_open;
    case Interval::OpOpOpCl: goto open_open;
    case Interval::OpOpOpOp: goto open_open;

    case Interval::EmXx: return return Interval::Nil;

    default: CAN_NOT_HAPPEN();

    close_close:
        return Interval(
            (1 << min(intlen(x.m_lower), intlen(y.m_lower))) - 1,
            (1 << max(intlen(x.m_upper), intlen(y.m_upper))) - 1 );

    close_open:
        return Interval(
            (1 << min(intlen(x.m_lower), intlen(y.m_lower))) - 1,
            FixInt::PlusInf );

    open_close:
        return Interval(
            FixInt::MinusInf,
            (1 << max(intlen(x.m_upper), intlen(y.m_upper))) - 1 );

    open_opeN:
        return Interval(FixInt::MinusInf, FixInt::PlusInf);
    } // switch class_class
} // operator &

#if 0
// Interval operator |
//  From [Warren03] P.63
Interval operator |(const Interval& x, const Interval& y)
{
    switch (Interval::ClassifySigns(x, y))
    {
    case Interval::MsMsMsMs:
        return Interval(minOr(x, y) maxOr(x, y));

    case Interval::MsMsMsPs:
        return Interval(x.m_lower, -1);

    case Interval::MsMsPsPs:
        return Interval(minOr(x, y), maxOr(x, y));

    case Interval::MsPsMsMs:
        return Interval(y.m_lower, -1);

    case Interval::MsPsMsPs:
        return Interval(
            min(x.m_lower, y.m_lower),
            maxOr(0, x.m_upper, 0, y.m_upper) );

    case Interval::MsPsPsPs:
        return Interval(
            minOr(x.m_lower, FixInt::MostPositive, y.m_lower, y.m_upper),
            maxOr(0, x.m_upper, y.m_lower, y.m_upper) );

    case Interval::PsPsMsMs:
        return Interval(minOr(x, y), maxOr(x, y));

    case Interval::PsPsMsPs:
        return Interval(
            minOr(x.m_lower, x.m_upper, y.m_lower, FixInt::MostPositive),
            maxOr(x.m_lower, x.m_upper, 0, y.m_upper) );

    case Interval::PsPsPsPs:
        return Interval(minOr(x, y), maxOr(x, y));
    } // switch signs

    CAN_NOT_HAPPEN();
} // operator |
#endif

} // Utility
