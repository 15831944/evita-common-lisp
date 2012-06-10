//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - Types - 12 Numbers
// compiler/ty/ty_12_number.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/util/interval.h#5 $
//
#if !defined(INCLUDE_compiler_util_interval_h)
#define INCLUDE_compiler_util_interval_h

#include "../../big/big_lisp.h"

namespace Utility
{

#define IntvEdge__MinusInf Q_
#define IntvEdge__PlusInf  QP

#define Interval__OpenLower IntvEdge__MinusInf
#define Interval__OpenUpper IntvEdge__PlusInf



class IntvEdge
{
    Val m_val;

    public: IntvEdge() : m_val(Fixnum::Encode(0)) {}

    public: IntvEdge(Int iVal) :
        m_val(make_int(iVal)) {}

    public: IntvEdge(Val);

    public: Val Get() const  { return m_val; }
    public: Val Set(Val val) { return m_val = val; }

    public: int IntegerLength() const;

    public: bool IsMinus() const
    {
        if (m_val == IntvEdge__MinusInf) return true;
        if (m_val == IntvEdge__PlusInf)  return false;
        return minusp(m_val);
    } // IsMinus

    public: bool operator ==(const IntvEdge& y) const
    {
        if (m_val == y.m_val) return true;
        if (symbolp(m_val)) return false;
        if (symbolp(y.m_val)) return false;
        return cmp(m_val, y.m_val) == 0;
    } // operator ==

    public: bool operator ==(Val y) const
    {
        if (symbolp(m_val)) return false;
        if (symbolp(y)) return false;
        return cmp(m_val, y) == 0;
    } // operator ==

    public: bool operator !=(const IntvEdge& y) const
        { return ! operator ==(y); }

    public: bool operator <(const IntvEdge& y) const
    {
        if (m_val   == IntvEdge__MinusInf) return true;
        if (y.m_val == IntvEdge__MinusInf) return false;

        if (y.m_val == IntvEdge__PlusInf) return true;
        if (m_val   == IntvEdge__PlusInf) return false;

        return cmp(m_val, y.m_val) < 0;
    } // operator <=

    public: bool operator <=(const IntvEdge& y) const
    {
        if (m_val   == IntvEdge__MinusInf) return true;
        if (y.m_val == IntvEdge__MinusInf) return false;

        if (y.m_val == IntvEdge__PlusInf) return true;
        if (m_val   == IntvEdge__PlusInf) return false;

        return cmp(m_val, y.m_val) <= 0;
    } // operator <=

    public: bool operator >(const IntvEdge& y) const
    {
        if (m_val   == IntvEdge__PlusInf) return true;
        if (y.m_val == IntvEdge__MinusInf) return false;

        if (y.m_val == IntvEdge__PlusInf) return false;
        if (m_val   == IntvEdge__MinusInf) return false;

        return cmp(m_val, y.m_val) > 0;
    } // operator <=

    public: IntvEdge operator +(Int i) const
    {
        if (symbolp(m_val)) return IntvEdge(m_val);
        return IntvEdge(add(m_val, Fixnum::Encode(i)));
    } // operator +

    public: IntvEdge operator +(const IntvEdge& y) const
    {
        if (symbolp(m_val)) return m_val;
        if (symbolp(y.m_val)) return y.m_val;
        return add(m_val, y.m_val);
    } // operator +

    public: IntvEdge operator -(Int i) const
    {
        if (symbolp(m_val)) return IntvEdge(m_val);
        return IntvEdge(sub(m_val, Fixnum::Encode(i)));
    } // operator -

    public: IntvEdge operator -(const IntvEdge& y) const
    {
        if (symbolp(m_val)) return m_val;
        if (symbolp(y.m_val)) return y.m_val;
        return sub(m_val, y.m_val);
    } // operator -

    public: IntvEdge operator ~() const
    {
        if (m_val == IntvEdge__MinusInf) return IntvEdge(IntvEdge__PlusInf);
        if (m_val == IntvEdge__PlusInf)  return IntvEdge(IntvEdge__MinusInf);
        return lognot(m_val);
    } // operator ~
}; // IntvEdge

//////////////////////////////////////////////////////////////////////
//
// Interval
//
class Interval
{
    public: static Interval Nil;

    public: enum Class
    {
        CloseClose  = 0,
        CloseOpen   = 1,
        OpenClose   = 2,
        OpenOpen    = 3,
        Empty       = 4,
    }; // Class

    public: IntvEdge m_lower, m_upper;

    public: Interval() : m_lower(-1), m_upper(-2) {}

    public: Interval(Int lower, Int upper) :
        m_lower(lower), m_upper(upper) {}

    public: Interval(Val lower, Val upper) :
        m_lower(lower), m_upper(upper) {}

    public: Interval(IntvEdge lower, IntvEdge upper) :
        m_lower(lower), m_upper(upper) {}

    public: Interval(IntvEdge lower, Val upper) :
        m_lower(lower), m_upper(upper) {}

    public: Interval(Val lower, IntvEdge upper) :
        m_lower(lower), m_upper(upper) {}

    public: static Interval SignedByte(uint n)
        { n -= 1; return Interval(add(ash(-1, n), 1), sub(ash(1, n), 1)); }

    public: static Interval Unbound()
        { return Interval(Interval__OpenLower, Interval__OpenUpper); }

    public: static Interval UnsignedByte(int n)
    {
        if (n < 0)
            { return Interval(0, Interval__OpenUpper); }
        else
            { return Interval(0, sub(ash(1, n), 1)); }
    } // UnsignedByte

    public: bool IsClose()  const { return Classify() == CloseClose; }
    public: bool IsEmpty()  const { return m_lower > m_upper; }
    public: bool IsSingle() const { return m_lower == m_upper; }

    public: Class Classify() const
    {
        if (m_lower > m_upper) return Empty;

        uint e = CloseClose;
        if (m_lower.Get() == IntvEdge__MinusInf) e |= OpenClose;
        if (m_upper.Get() == IntvEdge__PlusInf)  e |= CloseOpen;
        return static_cast<Class>(e);
    } // Classify
}; // Interval

Interval operator +(const Interval& x, const Interval& y);
Interval operator -(const Interval& x, const Interval& y);

Interval operator &(const Interval& x, const Interval& y);
Interval operator |(const Interval& x, const Interval& y);
Interval operator ^(const Interval& x, const Interval& y);

Interval operator ~(const Interval& x);

} // Utility

#endif // !defined(INCLUDE_compiler_util_interval_h)
