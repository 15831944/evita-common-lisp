//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - Types - 12 Numbers
// compiler/ty/ty_12_number.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/util/interval.h#1 $
//
#if !defined(INCLUDE_compiler_util_interval_h)
#define INCLUDE_compiler_util_interval_h

#include "../../big/big_lisp.h"

namespace Utility
{

typedef Int IntInt;

const IntInt IntInt_MostNegative = Fixnum::MostNegative;
const IntInt IntInt_MostPositive = Fixnum::MostPositive;


//////////////////////////////////////////////////////////////////////
//
// FixInt
//
class FixInt
{
    public: enum Flag
    {
        Normal      = 0,    // 000
        NaN         = 1,    // 001
        PlusInf     = 3,    // 011
        MinusInf    = 7,    // 111
    }; // Flag

    public: enum FlagFlag
    {
        NormalNormal    = Normal,
        NormalNaN       = NaN,
        NormalMinusInf  = MinusInf,
        NormalPlusInf   = PlusInf,

        NaNNormal       = (NaN << 3) | Normal,
        NaNNaN          = (NaN << 3) | NaN,
        NaNMinusInf     = (NaN << 3) | MinusInf,
        NaNPlusInf      = (NaN << 3) | PlusInf,

        MinusInfNormal      = (MinusInf << 3) | Normal,
        MinusInfNaN         = (MinusInf << 3) | NaN,
        MinusInfMinusInf    = (MinusInf << 3) | MinusInf,
        MinusInfPlusInf     = (MinusInf << 3) | PlusInf,

        PlusInfNormal      = (PlusInf << 3) | Normal,
        PlusInfNaN         = (PlusInf << 3) | NaN,
        PlusInfMinusInf    = (PlusInf << 3) | MinusInf,
        PlusInfPlusInf     = (PlusInf << 3) | PlusInf,
    }; // enum

    Int m_v;
    public: Flag m_f;

    public: FixInt() : m_v(0), m_f(Normal) {}

    public: FixInt(Int v) :
        m_v(v)
    {
        if (v < IntInt_MostNegative) m_f = MinusInf;
        else if (v > IntInt_MostPositive) m_f = PlusInf;
        else m_f = Normal;
    } // FixInt

    public: FixInt(Flag e) : m_v(0), m_f(e) {}

    public: Flag Classify() const { return m_f; }

    public: static FixInt Parse(Val v)
    {
        if (fixnump(v)) return FixInt(Fixnum::Decode_(v));
        if (bignump(v)) return FixInt(plusp(v) ? PlusInf : MinusInf);
        return FixInt(NaN);
    } // FixInt

    public: bool IsInfinity() const
        { return MinusInf == m_f || PlusInf == m_f; }

    public: bool IsNaN() const
        { return NaN == m_f; }

    public: bool IsNormal() const
        { return Normal == m_f; }

    public: static uint Classify(const FixInt& x, const FixInt& y)
        { return (x.m_f << 3) | y.m_f; }

    public: Val Unparse() const
    {
        if (Normal == m_f) return Fixnum::Encode(m_v);
        return QA;
    } // Unparse
}; // FixInt

FixInt operator+(const FixInt&, const FixInt&);
FixInt operator-(const FixInt&, const FixInt&);
FixInt operator&(const FixInt&, const FixInt&);
FixInt operator|(const FixInt&, const FixInt&);
FixInt operator^(const FixInt&, const FixInt&);

FixInt operator~(const FixInt&);

bool operator<(const FixInt&, const FixInt&);
bool operator<=(const FixInt&, const FixInt&);


//////////////////////////////////////////////////////////////////////
//
// Interval
//
class Interval
{
    public: static Interval Nil;

    public: FixInt m_lower, m_upper;

    public: Interval() {}

    public: Interval(FixInt lower, FixInt upper) :
        m_lower(lower), m_upper(upper) {}

    protected: enum Class
    {
        CloseClose  = 0,
        CloseOpen   = 1,
        OpenClose   = 2,
        OpenOpen    = 3,
        Empty       = 4,
    }; // ClassClass

    protected: enum ClassClass
    {
        ClClClCl    = 0,    // 0000
        ClClClOp    = 1,    // 0001
        ClClOpCl    = 2,    // 0010
        ClClOpOp    = 3,    // 0011

        ClOpClOp    = 4,    // 0100
        ClOpClOp    = 5,    // 0101
        ClOpOpCl    = 6,    // 0110
        ClOpOpOp    = 7,    // 0111

        OpClClOp    = 8,    // 1000
        OpClClOp    = 9,    // 1001
        OpClOpCl    = 10,   // 1010
        OpClOpOp    = 11,   // 1011

        OpOpClOp    = 12,   // 1100
        OpOpClOp    = 13,   // 1101
        OpOpOpCl    = 14,   // 1110
        OpOpOpOp    = 15,   // 1111

        EmXx        = 16,
    }; // ClassClass

    public: bool IsValid() const
    {
        if (m_lower.IsNaN()) return false;
        if (m_upper.IsNaN()) return false;
        return m_lower <= m_upper;
    } // IsValid

    public: Class Classify() const
    {
        if (! IsValid()) return Empty;

        uint lu = 0;
        if (m_lower.IsInfinity()) lu |= 1;
        if (m_upper.IsInfinity()) lu |= 2;
        return static_cast<Class>(lu);
    } // Classify

    public: static ClassClass Classify(const Interval& x, const Interval& y)
    {
        Class c1 = x.Classify(); if (c1 == Empty) return EmXx;
        Class c2 = y.Classify(); if (c2 == Empty) return EmXx;

        return static_cast<ClassClass>((c1 << 4) | c2);
    } // Classify

    public: enum SignSign
    {
        MsMsMsMs    = 0,    // 0000
        MsMsMsPs    = 1,    // 0001
        MsMsPsPs    = 3,    // 0011

        MsPsMsMs    = 3,    // 0100
        MsPsMsPs    = 5,    // 0101
        MsPsPsPs    = 5,    // 0111

        PsPsMsMs    = 12,   // 1100
        PsPsMsPs    = 13,   // 1110
        PsPsPsPs    = 15,   // 1111
    }; // Signs

    public: static SignSign ClassifySigns(const FixInt& x, const FixInt& y)
    {
        uint s = 0;

        if (! x.m_lower.IsMinus()) s |= PsPsMsMs;
        else if (! x.m_upper.IsMinus()) s |= MsPsMsMs;

        if (! y.m_lower.IsMinus()) s |= MsMsPsPs;
        else if (! y.m_upper.IsMinus()) s |= MsMsMsPs;

        return s;
    } // ClassifySigns

}; // Interval

Interval operator +(const Interval& x, const Interval& y);
Interval operator -(const Interval& x, const Interval& y);

Interval operator &(const Interval& x, const Interval& y);
Interval operator |(const Interval& x, const Interval& y);
Interval operator ^(const Interval& x, const Interval& y);

} // Utility

#endif // !defined(INCLUDE_compiler_util_interval_h)
