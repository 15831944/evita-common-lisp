#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - ty - 12 Numbers - Types
// compiler/ty/ty_12_number.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ty/ty_12_number.cpp#14 $
//
#include "./ty_defs.h"

namespace Compiler
{

TyInteger TyInteger::Fixnum(
    Fixnum::Encode(Fixnum::MostNegative), 
    Fixnum::Encode(Fixnum::MostPositive) );

TyInteger TyInteger::Nil;

// See TyInteger ctor for initialization of TyInteger::SignedByte32.
TyInteger TyInteger::SignedByte32(
    Fixnum::Encode(0),
    Fixnum::Encode(0) );

// get_second
static Val get_second(Ty x)
{
    if (! consp(x)) return IntvEdge__MinusInf;
    if (! consp(cdr(x))) return IntvEdge__MinusInf;
    Val lower = second(x);
    return integerp(lower) ? lower : IntvEdge__MinusInf;
} // get_second

// get_third
static Val get_third(Ty x)
{
    if (! consp(x)) return IntvEdge__PlusInf;
    if (! consp(cdr(x))) return IntvEdge__PlusInf;
    if (! consp(cddr(x))) return IntvEdge__PlusInf;
    Val upper = third(x);
    return integerp(upper) ? upper : IntvEdge__PlusInf;
} // get_third


//////////////////////////////////////////////////////////////////////
//
// TyInteger ctor
//
TyInteger::TyInteger(Val l, Val u) :
    Interval(l, u)
{
    // Note: Since, image isn't loaded when initializing TyInteger::SignedByte32,
    // IntvEdge ctor can't use Bignum.
    if (this == &SignedByte32)
    {
        SignedByte32.m_lower.Set(val_i32_min);
        SignedByte32.m_upper.Set(val_i32_max);
    }
} // TyInteger::TyInteger


//////////////////////////////////////////////////////////////////////
//
// TyInteger::Parse
//
TyInteger TyInteger::Parse(Ty ty)
{
    if (Qbit == ty) return TyInteger(Fixnum::Encode(0), Fixnum::Encode(1));

    if (Qfixnum == ty || CLASS_fixnum == ty) return TyInteger::Fixnum;

    if (Qinteger == ty || CLASS_integer == ty || Qsigned_byte == ty)
        { return TyInteger(Interval__OpenLower, Interval__OpenUpper); }

    if (Qunsigned_byte == ty)
        { return TyInteger(0, Interval__OpenUpper); }

    if (! consp(ty)) return TyInteger::Nil;

    Val op = first(ty);

    if (Qeql == op)
    {
        Val obj = second(ty);
        if (! integerp(obj)) return TyInteger::Nil;
        return TyInteger(obj, obj);
    } // eql

    if (Qinteger == op)
        { return TyInteger(get_second(ty), get_third(ty)); }

    if (Qmod == op)
        { return TyInteger(Fixnum::Encode(0), sub(second(ty), 1)); }

    if (Qsigned_byte == op)
    {
        Val n = get_second(ty);
        if (! fixnump(n)) return Parse(Qsigned_byte);

        int iN = static_cast<int>(Fixnum::Decode_(n));
        return TyInteger(Interval::SignedByte(iN));
    } // signed_byte

    if (Qunsigned_byte == op)
    {
        Val n = get_second(ty);
        if (! fixnump(n)) return Parse(Qunsigned_byte);

        int iN = static_cast<int>(Fixnum::Decode_(n));
        return TyInteger(Interval::UnsignedByte(iN));
    } // unsigned_byte

    return TyInteger::Nil;
} // Parse


//////////////////////////////////////////////////////////////////////
//
// TyInteger::Unparse
//
Ty TyInteger::Unparse() const
{
    switch (Classify())
    {
    case Interval::CloseClose:
        return list(Qinteger, m_lower.Get(), m_upper.Get());

    case Interval::CloseOpen:
        return list(Qinteger, m_lower.Get(), QA);

    case Interval::OpenClose:
        return list(Qinteger, QA, m_upper.Get());

    case Interval::OpenOpen:
        return Qinteger;

    case Interval::Empty:
        return nil;
    } // switch

    CAN_NOT_HAPPEN();
} // Unparse


//////////////////////////////////////////////////////////////////////
//
// TyInteger operator &&
//  Disjoint:   l1..u1 l2..u2   => nil
//  Disjoint:   l2..u2 l1..u1   => nil
//  Inclusion:  l1..l2..u2..u1  => l2..u2
//  Inclusion:  l2..l1..u1..u2  => l1..u1
//  Overlap:    l1..l2..u1..u2  => l2..u1
//  Overlap:    l2..l1..u2..u1  => l1..u2
//
TyInteger operator &&(const TyInteger& x, const TyInteger& y)
{
    if (x.IsEmpty() || y.IsEmpty()) return TyInteger::Nil;
    if (x.m_upper < y.m_lower) return TyInteger::Nil;
    if (y.m_upper < x.m_lower) return TyInteger::Nil;

    return TyInteger(
        x.m_lower <= y.m_lower ? y.m_lower : x.m_lower,
        x.m_upper <= y.m_upper ? x.m_upper : y.m_upper );
} // operator &&


//////////////////////////////////////////////////////////////////////
//
// TyInteger operator ||
// Type UnionOr. We ignore hole.
TyInteger operator ||(const TyInteger& x, const TyInteger& y)
{
    if (x.IsEmpty() || y.IsEmpty()) return TyInteger::Nil;

    return TyInteger(
        x.m_lower <= y.m_lower ? x.m_lower : y.m_lower,
        x.m_upper <= y.m_upper ? y.m_upper : x.m_upper );
} // operator ||


//////////////////////////////////////////////////////////////////////
//
// IsSubtype
//
bool IsSubtype(const TyInteger& x, const TyInteger& y)
{
    return y.m_lower <= x.m_lower && x.m_upper <= y.m_upper;
} // IsSubtype


//////////////////////////////////////////////////////////////////////
//
// IsSubtype
//
bool IsSubtype(const TyInteger& x, Ty ty2)
{
    TyInteger y = TyInteger::Parse(ty2);
        if (y.IsValid()) return IsSubtype(x, y);

    return Qt        == ty2 ||
           Qnumber   == ty2 ||
           Qreal     == ty2 ||
           Qrational == ty2;
} // IsSubtype


//////////////////////////////////////////////////////////////////////
//
// IsSubtype
//
bool IsSubtype(Ty ty1, const TyInteger& y)
{
    if (! consp(ty1)) return false;

    Val op1 = first(ty1);

    if (Qeql == op1) return IsTypeOf(second(ty1), y);

    if (Qmember == op1)
    {
        foreach (EnumList, oEnum, rest(ty1))
            { if (! IsTypeOf(oEnum.Get(), y)) return false; }
        return true;
    }

    return false;
} // IsSubtypep


//////////////////////////////////////////////////////////////////////
//
// IsTypeOf
//
bool IsTypeOf(Val obj, const TyInteger& y)
{
    if (! integerp(obj)) return false;
    TyInteger x(obj, obj); if (x.IsEmpty()) return false;
    return y.m_lower <= x.m_lower && x.m_lower <= y.m_upper;
} // IsTypeOf


//////////////////////////////////////////////////////////////////////
//
// GreaterThan
//         x: o-----------o             o----o
//         y:    o-----o        o-----o
//    result:          o--o             o----o
//
// Description:
//  Returns interval x <y. This is used for limiting range.
//
// Example:
//  (dotimes (i 10) ...) -> (integer 0 *) < (eql 10) -> (integer 0 9)
//
TyInteger GreaterThan(const TyInteger& x, const TyInteger& y)
{
    if (x.IsEmpty() || y.IsEmpty()) return TyInteger::Nil;
    if (x.m_upper <= y.m_lower) return TyInteger::Nil;   // xxx yyy

    return TyInteger(
        x.m_lower > y.m_lower ? x.m_lower : y.m_lower + 1,
        x.m_upper );
} // GreaterThan


//////////////////////////////////////////////////////////////////////
//
// GreaterThanOrEqual
//         x: o-----------o             o----o
//         y:    o-----o        o-----o
//    result:          o--o             o-----o
//
// Description:
//  Returns interval x <y. This is used for limiting range.
//
// Example:
//  (dotimes (i 10) ...) -> (integer 0 *) < (eql 10) -> (integer 0 9)
//
TyInteger GreaterThanOrEqual(const TyInteger& x, const TyInteger& y)
{
    if (x.IsEmpty() || y.IsEmpty()) return TyInteger::Nil;
    if (x.m_upper < y.m_lower) return TyInteger::Nil;   // xxx yyy

    return TyInteger(
        x.m_lower > y.m_lower ? x.m_lower : y.m_lower,
        x.m_upper );
} // GreaterThanOrEqual


//////////////////////////////////////////////////////////////////////
//
// LessThan
//         x: o-----------o             o----o
//         y:    o-----o        o-----o
//    result: o--------o           nil
//
// Description:
//  Returns interval x <y. This is used for limiting range.
//
// Example:
//  (dotimes (i 10) ...) -> (integer 0 *) < (eql 10) -> (integer 0 9)
//
TyInteger LessThan(const TyInteger& x, const TyInteger& y)
{
    if (x.IsEmpty() || y.IsEmpty()) return TyInteger::Nil;
    if (y.m_upper <= x.m_lower) return TyInteger::Nil;   // yyy xxx

    return TyInteger(
        x.m_lower,
        x.m_upper < y.m_upper ? x.m_upper : y.m_upper - 1 );
} // LessThan


//////////////////////////////////////////////////////////////////////
//
// LessThanOrEqual
//         x: o-----------o             o----o
//         y:    o-----o        o-----o
//    result: o--------o           nil
//
// Description:
//  Returns interval x <y. This is used for limiting range.
//
// Example:
//  (dotimes (i 10) ...) -> (integer 0 *) < (eql 10) -> (integer 0 9)
//
TyInteger LessThanOrEqual(const TyInteger& x, const TyInteger& y)
{
    if (x.IsEmpty() || y.IsEmpty()) return TyInteger::Nil;
    if (y.m_upper < x.m_lower) return TyInteger::Nil;   // yyy xxx

    return TyInteger(
        x.m_lower,
        x.m_upper <= y.m_upper ? x.m_upper : y.m_upper );
} // LessThanOrEqual

} // Compiler
