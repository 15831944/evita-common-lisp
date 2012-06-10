#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - Interval
// compiler/util/interval.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/util/interval.cpp#7 $
//
//
//  References:
//      [Warren03] P.58
//
#include "./interval.h"

#include "../../big/big_12_bignum.h"


namespace Utility
{

Interval Interval::Nil;


namespace
{

enum ClassClass
{
    ClClClCl    = 0,    // 0000
    ClClClOp    = 1,    // 0001
    ClClOpCl    = 2,    // 0010
    ClClOpOp    = 3,    // 0011

    ClOpClCl    = 4,    // 0100
    ClOpClOp    = 5,    // 0101
    ClOpOpCl    = 6,    // 0110
    ClOpOpOp    = 7,    // 0111

    OpClClCl    = 8,    // 1000
    OpClClOp    = 9,    // 1001
    OpClOpCl    = 10,   // 1010
    OpClOpOp    = 11,   // 1011

    OpOpClCl    = 12,   // 1100
    OpOpClOp    = 13,   // 1101
    OpOpOpCl    = 14,   // 1110
    OpOpOpOp    = 15,   // 1111

    EmXx        = 16,
}; // ClassClass


static ClassClass classify(const Interval& x, const Interval& y)
{
    if (x.IsEmpty()) return EmXx;
    if (y.IsEmpty()) return EmXx;

    return static_cast<ClassClass>((x.Classify() << 2) | y.Classify());
} // classify


enum SignSign
{
    PosPos  = 0,
    PosNeg  = 1,
    NegPos  = 2,
    NegNeg  = 3,
}; // SignSign


static SignSign classifySignSign(const IntvEdge& x, const IntvEdge& y)
{
    uint e = 0;
    if (x.IsMinus()) e |= NegPos;
    if (y.IsMinus()) e |= PosNeg;
    return static_cast<SignSign>(e);
} // classifySignSign


enum SignSignSignSign
{
    PosPos_PosPos   = 0,
    PosPos_PosNeg   = 1,      // n/a
    PosPos_NegPos   = 2,
    PosPos_NegNeg   = 3,

    PosNeg_PosPos   = 4,      // n/a
    PosNeg_PosNeg   = 5,      // n/a
    PosNeg_NegPos   = 6,      // n/a
    PosNeg_NegNeg   = 7,      // n/a

    NegPos_PosPos   = 8,
    NegPos_PosNeg   = 9,      // n/a
    NegPos_NegPos   = 10,
    NegPos_NegNeg   = 11,

    NegNeg_PosPos   = 12,
    NegNeg_PosNeg   = 13,      // n/a
    NegNeg_NegPos   = 14,
    NegNeg_NegNeg   = 15,
}; // SignSignSingSign

static SignSignSignSign classifySignSign(const Interval& x, const Interval& y)
{
    uint e = classifySignSign(x.m_lower, x.m_upper);
        e <<= 2;
        e |= classifySignSign(y.m_lower, y.m_upper);
    return static_cast<SignSignSignSign>(e);
} // classifySignSign


// len
static int len(const Interval x)
{
    int iLowerLen = x.m_lower.IntegerLength();
    if (iLowerLen == -1) return -1;

    int iUpperLen = x.m_upper.IntegerLength();
    if (iUpperLen == -1) return -1;

    return max(iLowerLen, iUpperLen);
} // len

// len
static int len(Val val)
{
    if (symbolp(val)) return -1;

    if (fixnump(val))
    {
        Int iVal = Fixnum::Decode_(val);
        int cBits = 0;
        if (iVal >= 0)
        {
            while (0 != iVal)
            {
                iVal >>= 1;
                cBits += 1;
            } // while
        }
        else
        {
            while (-1 != iVal)
            {
                iVal >>= 1;
                cBits += 1;
            }
        }
        return cBits;
    } // fixnum

    return val->Decode<BignumImpl>()->IntegerLength();
} // len

} // namespace


//////////////////////////////////////////////////////////////////////
//
// IntvEdge ctor
//
IntvEdge::IntvEdge(Val val) :
    m_val(val)
{
    if (val == IntvEdge__MinusInf) return;
    if (val == IntvEdge__PlusInf) return;

    if (! integerp(val))
    {
        error(make_type_error(
            val, list(Qor, Qinteger, list(Qmember, Q_, QP)) ) );
    }

    if (len(val) <= 64) return;
    m_val = minusp(val) ? IntvEdge__MinusInf : IntvEdge__PlusInf;
} // IntvEdge::IntvEdge


//////////////////////////////////////////////////////////////////////
//
// len - length of integer
//
int IntvEdge::IntegerLength() const
{
    return len(m_val);
} // IntvEdge::IntegerLength


//////////////////////////////////////////////////////////////////////
//
// Interval operator +
//      [lower1 + lower2, upper1 + upper2]
//
Interval operator +(const Interval& x, const Interval& y)
{
    switch (classify(x, y))
    {
    case ClClClCl: goto close_close;
    case ClClClOp: goto close_open;
    case ClClOpCl: goto open_close;
    case ClClOpOp: goto open_open;
    case ClOpClCl: goto close_open;
    case ClOpClOp: goto close_open;
    case ClOpOpCl: goto open_open;
    case ClOpOpOp: goto open_open;

    case OpClClCl: goto open_close;
    case OpClClOp: goto open_open;
    case OpClOpCl: goto open_close;
    case OpClOpOp: goto open_open;
    case OpOpClCl: goto open_open;
    case OpOpClOp: goto open_open;
    case OpOpOpCl: goto open_open;
    case OpOpOpOp: goto open_open;

    case EmXx: return Interval::Nil;

    default: CAN_NOT_HAPPEN();

    close_close:
        return Interval(x.m_lower + y.m_lower, x.m_upper + y.m_upper);

    close_open:
        return Interval(x.m_lower + y.m_lower, Interval__OpenUpper);

    open_close:
        return Interval(Interval__OpenLower, x.m_upper + y.m_upper);

    open_open:
        return Interval(Interval__OpenLower, Interval__OpenUpper);
    } // switch class_class
} // operator +


//////////////////////////////////////////////////////////////////////
//
// Interval operator -
//      [lower1 - upper2, upper1 - lower2]
//
Interval operator -(const Interval& x, const Interval& y)
{
    switch (classify(x, y))
    {
    case ClClClCl: goto close_close;
    case ClClClOp: goto open_close;
    case ClClOpCl: goto close_open;
    case ClClOpOp: goto open_open;

    case ClOpClCl: goto close_open;
    case ClOpClOp: goto open_open;
    case ClOpOpCl: goto open_open;
    case ClOpOpOp: goto open_open;

    case OpClClCl: goto open_close;
    case OpClClOp: goto open_close;
    case OpClOpCl: goto open_open;
    case OpClOpOp: goto open_open;

    case OpOpClCl: goto open_open;
    case OpOpClOp: goto open_open;
    case OpOpOpCl: goto open_open;
    case OpOpOpOp: goto open_open;

    case EmXx: return Interval::Nil;

    default: CAN_NOT_HAPPEN();

    close_close:
        return Interval(x.m_lower - y.m_upper, x.m_upper - y.m_lower);

    close_open:
        return Interval(x.m_lower - y.m_upper, Interval__OpenUpper);

    open_close:
        return Interval(Interval__OpenLower, x.m_upper - y.m_lower);

    open_open:
        return Interval(Interval__OpenLower, Interval__OpenUpper);
    } // switch class_class
} // operator -


//////////////////////////////////////////////////////////////////////
//
// Interval operator &
//
//  Unsigned:
//      0 <= (x & y) <= min(b, d)
//
//  Signed:
//      p p p p    (unsigned-byte (max x y))
//      p p n p    (signed-byte (1+ (max x y)))
//      p p n n    (signed-byte (1+ (max x y)))
//
//      n p p p    (unsigned-byte (max x y))
//      n p n p    (unsigned-byte (max x y))
//      n p n n    (unsigned-byte (max x y))
//
//      n n p p    (unsigned-byte (max x y))
//      n n n p    (unsigned-byte (max x y))
//      n n n n    (unsigned-byte (max x y))
//
Interval operator &(const Interval& x, const Interval& y)
{
    switch (classifySignSign(x, y))
    {
    case PosPos_PosPos: goto pos_either;
    case PosPos_NegPos: goto pos_either;
    case PosPos_NegNeg: goto pos_either;

    case NegPos_PosPos: goto pos_either;
    case NegPos_NegPos: goto neg_either;
    case NegPos_NegNeg: goto neg_either;

    case NegNeg_PosPos: goto pos_either;
    case NegNeg_NegPos: goto neg_either;
    case NegNeg_NegNeg: goto neg_neg;

    default: CAN_NOT_HAPPEN();

    pos_either:
        if (x.IsClose() && y.IsClose())
            { return Interval::UnsignedByte(min(len(x), len(y))); }
        else if (x.IsClose())
            { return Interval::UnsignedByte(len(x)); }
        else if (y.IsClose())
            { return Interval::UnsignedByte(len(y)); }
        else
            { return Interval(0, Interval__OpenUpper); }

    neg_either:
        if (x.IsClose() && y.IsClose())
            { return Interval::SignedByte(min(len(x),len(y))+1); }
        else if (x.IsClose())
            { return Interval::SignedByte(len(x)+1); }
        else if (y.IsClose())
            { return Interval::SignedByte(len(y)+1); }
        else
            { return Interval::Unbound(); }

    neg_neg:
        if (x.IsClose() && y.IsClose())
            { return Interval(add(ash(-1, min(len(x),len(y))), 1), 0); }
        else if (x.IsClose())
            { return Interval(add(ash(-1, len(x)), 1), 0); }
        else if (y.IsClose())
            { return Interval(add(ash(-1, len(y)), 1), 0); }
        else
            { return Interval(Interval__OpenLower, 0); }

    } // switch sign x sign
} // operator &


//////////////////////////////////////////////////////////////////////
//
// Interval operator |
//
//  Unsigned:
//      max(a, c) <= (x | y) <= b + d
//
//  Signed:
//      p p p p     (minIor a b c d)        (maxIor a b c d)
//      p p n p     (minIor a b c #xf..f)   (maxIor a b 0 d)
//      p p n n     (minIor a b c d)        (maxIor a b c d)
//
//      n p p p     (minIor a #xf..f c d)   (maxIor 0 b c d)
//      n p n p     (min a c)               (maxIor 0 b 0 d)
//      n p n n     c                       -1
//
//      n n p p     (minIor a b c d)        (maxIor a b c d)
//      n n n p     a                       -1
//      n n n n     (minIor a b c d)        (maxIor a b c d)
//
Interval operator |(const Interval& x, const Interval& y)
{
    SignSignSignSign e = classifySignSign(x, y);
    switch (e)
    {
    case PosPos_PosPos: goto pos_pos;
    case PosPos_NegPos: goto pos_either;
    case PosPos_NegNeg: goto pos_neg;

    case NegPos_PosPos: goto either_pos;
    case NegPos_NegPos: goto either_either;
    case NegPos_NegNeg: goto either_neg;

    case NegNeg_PosPos: goto neg_pos;
    case NegNeg_NegPos: goto neg_either;
    case NegNeg_NegNeg: goto neg_neg;

    default:
        #if 0
        error(L"Interval operator| ~D for [~D, ~D] and [~D, ~D].",
            Fixnum::Encode(e),
            x.m_lower.Get(), x.m_upper.Get(),
            y.m_lower.Get(), y.m_upper.Get() );
        #endif
        CAN_NOT_HAPPEN();

    pos_pos:
        if (x.IsClose() && y.IsClose())
            { return Interval::UnsignedByte(max(len(x), len(y))); }
        else
            { return Interval::UnsignedByte(-1); }

    neg_neg:
        if (x.IsClose() && y.IsClose())
            { return Interval(ash(-1, min(len(x),len(y))), -1); }
        else
            { return Interval(Interval__OpenLower, -1); }

    neg_either:
    neg_pos:
        return Interval(x.m_lower, -1);

    either_neg:
    pos_neg:
        return Interval(y.m_lower, -1);

    either_pos:
    either_either:
    pos_either:
        if (x.IsClose() && y.IsClose())
            { return Interval::SignedByte(max(len(x),len(y))+1); }
        else
            { return Interval::Unbound(); }
    } // switch SignSign
} // operator |


//////////////////////////////////////////////////////////////////////
//
// Interval operator ^
//      x ^ y == (x | y) - (x & y)
//  Unsinged:
//      0 <= (x ^ y) <= b + d
//
//  Signed:
//      p p p p positve
//      p p n p either
//      p p n n negative
//
//      n p p p either
//      n p n p either
//      n p n n either
//
//      n n p p negative
//      n n n p either
//      n n n n positive;
//
//      either:   (signed-byte (1+ (max xlen ylen)))
//      negative: (integer (Nn (max xlen ylen)) -1)
//      positive: (integer 0  (Pn (max xlen ylen)))
//
Interval operator ^(const Interval& x, const Interval& y)
{
    switch (classifySignSign(x, y))
    {
    case PosPos_PosPos: goto positive;
    case PosPos_NegPos: goto either;
    case PosPos_NegNeg: goto negative;

    case NegPos_PosPos: goto either;
    case NegPos_NegPos: goto either;
    case NegPos_NegNeg: goto either;

    case NegNeg_PosPos: goto negative;
    case NegNeg_NegPos: goto either;
    case NegNeg_NegNeg: goto positive;

    default: CAN_NOT_HAPPEN();

    either:
        return Interval::SignedByte(max(len(x), len(y))+1);

    negative:
        if (x.IsClose() && y.IsClose())
        {
            return Interval(-1 << max(len(x), len(y)), -1);
        }
        return Interval(Interval__OpenLower, -1);

    positive:
        return Interval::UnsignedByte(max(len(x), len(y)));
    } // switch SignSignSignSign
} // operator ^


//////////////////////////////////////////////////////////////////////
//
// operator ~
//
//  Unsigned:
//      ~b <= ~x <= ~a
//
//  Signed
//      p p     ~b  ~a
//      n p     ~b  ~a
//      n n     ~b  ~b
//
Interval operator ~(const Interval& x)
{
    switch (x.Classify())
    {
    case Interval::CloseClose:
        return Interval(~x.m_upper, ~x.m_lower);

    case Interval::CloseOpen:
        return Interval(Interval__OpenLower, ~x.m_lower);

    case Interval::OpenClose:
        return Interval(~x.m_upper, Interval__OpenUpper);

    case Interval::OpenOpen:
        return Interval::Unbound();

    case Interval::Empty:
        return Interval::Nil;

    default:
        CAN_NOT_HAPPEN();
    } // switch class
} // operator ~


} // Utility
