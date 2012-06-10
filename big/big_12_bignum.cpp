#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 12 Numbers - Bignum
// big/big_12_bignum.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_12_bignum.cpp#20 $
//
// Reference:
//  [Warrn03] Henry S. Warren, Jr. "Hacker's Delight". Addison Wesley. 2003
//  [White86] Jon L White, Reconfigurable, Retargetable Bignums: A Case Study
//  in Efficient, Portable Lisp System Building, Proceedings of the ACM
//  conference on Lisp & FP, 1986.
//
#include "./big_lisp.h"

#include "./big_12_bignum.h"
#include "./big_12_float.h"
#include "./big_12_ratio.h"

#include <float.h>  //  _scalb

#include "../arch/generic/mini/gen_mini_12_bignum.h"

namespace MiniLisp
{

//namespace
//{

enum Signs
{
    Signs_Pos_Pos   = 0,
    Signs_Neg_Pos   = 1,
    Signs_Pos_Neg   = 2,
    Signs_Neg_Neg   = 3,
}; // Signs

enum
{
    BigitBits = BignumImpl::Bits,
}; // enum


BignumImpl* const FillZero   = reinterpret_cast<BignumImpl*>(1);
BignumImpl* const FillMinus1 = reinterpret_cast<BignumImpl*>(-1);


//////////////////////////////////////////////////////////////////////
//
// big_count
//   Returns minimum number of bigits to represent specified Bignum.
//
//   This function is used for normalization.
//
static uint big_count(uint n, const Bigit* p)
{
    if (static_cast<SignedBigit>(p[n-1]) >= 0)
    {
        while (n >= 2)
        {
            if (0 != p[n-1]) break;
            if (static_cast<SignedBigit>(p[n-2]) < 0) break;
            n -= 1;
        } // while
    }
    else
    {
        while (n >= 2)
        {
            if (static_cast<Bigit>(-1) != p[n-1]) break;
            if (static_cast<SignedBigit>(p[n-2]) >= 0) break;
            n -= 1;
        } // while
    }

    return n;
} // big_count


//////////////////////////////////////////////////////////////////////
//
// Big
//
class Big
{
    protected: Bigit*  m_p;
    protected: Int     m_n;

    protected: Big() {}

    protected: Big(const BignumImpl* p) :
        m_p(const_cast<Bigit*>(p->m_rgBigit)),
        m_n(p->GetLength()) {}

    protected: Big(BignumImpl* p, const BignumImpl* q)
        { init(p, q); }

    // Bigits
    public: Bigit* Bigits()
        { return m_p; }

    // Count
    public: Int GetLength() const
        { return m_n; }

    // init
    void init(BignumImpl* p, const BignumImpl* q)
    {
        m_p = p->m_rgBigit;
        m_n = p->GetLength();

        if (FillZero == q)
        {
            ::memset(m_p, 0x00, sizeof(Bigit) * m_n);
        }
        else if (FillMinus1 == q)
        {
            ::memset(m_p, 0xff, sizeof(Bigit) * m_n);
        }
        else
        {
            Int m = q->GetLength();
            if (m_n <= m)
            {
                ::memcpy(m_p, q->m_rgBigit, sizeof(Bigit) * m_n);
            }
            else
            {
                ::memcpy(m_p, q->m_rgBigit, sizeof(Bigit) * m);
                Bigit sign = q->GetSign();
                for (Int i = m; i < m_n; i++)
                {
                    m_p[i] = sign;
                } // for i
            }
        } // if
    } // init

    // Enum
    class Enum
    {
        protected: Bigit* m_pRunner;
        protected: Bigit* m_pEnd;

        protected: Enum(Bigit* pStart, Bigit* pEnd) :
            m_pRunner(pStart),
            m_pEnd(pEnd) {}

        public: bool AtEnd() const
            { return m_pRunner == m_pEnd; }

        public: Bigit Get() const
            { ASSERT(! AtEnd()); return *m_pRunner; }

        public: Bigit Set(Bigit d)
            { ASSERT(! AtEnd()); return *m_pRunner = d; }
    }; // Enum

    // EnumLtoM -- Least Significant Bigit to Most Significant Bigit.
    public: class EnumLtoM : public Enum
    {
        public: EnumLtoM(Big* p, Int nSkip = 0) :
            Enum(p->m_p + nSkip, p->m_p + p->m_n) {}

        public: void Next()
            { ASSERT(! AtEnd()); m_pRunner++; }
    }; // EnumLtoM

    // EnumMtoL -- Most Significant Bigit to Least Significant Bigit.
    public: class EnumMtoL : public Enum
    {
        public: EnumMtoL(Big* p) :
            Enum(p->m_p + p->m_n - 1, p->m_p - 1) {}

        public: void Next()
            { ASSERT(! AtEnd()); --m_pRunner; }

    }; // EnumMtoL
}; // Big


//////////////////////////////////////////////////////////////////////
//
// BigPin
//
class BigPin : public Big
{
    protected: BigPin() {}

    public: BigPin(Val x)
    {
        BignumImpl* p = x->Decode<BignumImpl>();
        m_p = p->m_rgBigit;
        m_n = p->GetLength();
    } // BigPin

    // Pin
    public: Val Pin()
    {
        normalize();

        if (1 == m_n)
        {
            Int iVal = static_cast<SignedBigit>(m_p[0]);

            if (iVal >= Fixnum::MostNegative &&
                iVal <= Fixnum::MostPositive )
            {
                return Fixnum::Encode(iVal);
            }
        }

        Val x = MiniThread::Get()->AllocBinVec(
            CLASSD_bignum, 
            Fixnum::Encode(m_n) );

        ::memcpy(
            x->Decode<BignumImpl>()->m_rgBigit,
            m_p,
            sizeof(Bigit) * m_n );

        return x;
    } // Pin

    // normalize
    void normalize()
    {
        m_n = big_count(static_cast<uint>(m_n), m_p);
    } // normalize
}; // BigPin


//////////////////////////////////////////////////////////////////////
//
// StackBignum
//  Bignum in object stack.
//
class StackBignum : public BigPin
{
    Val m_x;

    // StackBignum
    public: StackBignum(Int cBigits)
    {
        m_x = MiniThread::Get()->StackAllocBinVec(CLASSD_bignum, cBigits);
        m_p = m_x->Decode<BignumImpl>()->m_rgBigit;
        m_n = m_x->Decode<BignumImpl>()->GetLength();
    } // StackBignum

    // StackBignum
    public: StackBignum(Int cBigits, BignumImpl* q)
    {
        m_x = MiniThread::Get()->StackAllocBinVec(CLASSD_bignum, cBigits);
        init(m_x->Decode<BignumImpl>(), q);
    } // StackBignum

    // PinNeg
    public: Val PinNeg()
    {
        Bigit nCarry = 1;
        foreach (EnumLtoM, oEnum, this)
        {
            DblBigit oAcc(~oEnum.Get(), 0);
            oAcc += nCarry;
            oEnum.Set(oAcc.low);
            nCarry = oAcc.high;
        } // for each bigit
        return Pin();
    } // PinNeg

    public: operator Val() { return m_x; }
}; // StackBignum


//////////////////////////////////////////////////////////////////////
//
// StackBignumNeg
//  Allocate bignum in object stack.
//
class StackBignumNeg
{
    public: Val m_x;

    public: StackBignumNeg(const BignumImpl* pA)
    {
        ASSERT(pA->IsMinus());

        if (init(pA, 0)) init(pA, 1);
    } // StackBignumNeg

    // init
    bool init(const BignumImpl* pA, uint nExtra)
    {
        m_x = MiniThread::Get()->StackAllocBinVec(
            CLASSD_bignum, 
            pA->GetLength() + nExtra);

        BignumImpl* pB = m_x->Decode<BignumImpl>();

        BignumImpl::EnumLtoM oEnumB(pB);

        Bigit nCarry = 1;
        foreach (BignumImpl::EnumLtoM, oEnumA, pA)
        {
            DblBigit oAcc(~oEnumA.Get(), 0);
            oAcc += nCarry;
            oEnumB.Set(oAcc.low);
            nCarry = oAcc.high;
            oEnumB.Next();
        } // for A

        if (nExtra)
        {
            oEnumB.Set(nCarry);
        }

        return pB->GetMSB() < 0;
    } // StackBignumNeg

    public: operator Val() { return m_x; }
}; // StackBignumNeg

//} // namespace


//////////////////////////////////////////////////////////////////////
//
// SmallBignumBase::Pin
//  Returns bignum in heap.
//
Val SmallBignumBase::Pin()
{
    BigPin oA(Encode());
    return oA.Pin();
} // SmallBignumBase::Pin


//////////////////////////////////////////////////////////////////////
//
// Add
//  Adds two bignums and returns result, bignum or fixnum.
//
Val BignumImpl::Add(Val a, Val b)
{
    BignumImpl* pA = a->Decode<BignumImpl>();
    BignumImpl* pB = b->Decode<BignumImpl>();

    if (pA->IsZero()) return BignumImpl::Pin(b);
    if (pB->IsZero()) return BignumImpl::Pin(a);

    ObStackScope oObStack;

    StackBignum oC(max(pA->GetLength(), pB->GetLength()) + 1, pA);

    Big::EnumLtoM oEnumC(&oC);

    Bigit nCarry = 0;
    foreach (BignumImpl::EnumLtoM, oEnumB, pB)
    {
        DblBigit oAcc(nCarry, 0);
            oAcc += oEnumC.Get();
            oAcc += oEnumB.Get();

        oEnumC.Set(oAcc.low);
        oEnumC.Next();

        nCarry = oAcc.high;
    } // for B

    Bigit nB = pB->GetSign();
    while (! oEnumC.AtEnd())
    {
        DblBigit oAcc(nCarry, 0);
            oAcc += oEnumC.Get();
            oAcc += nB;

        oEnumC.Set(oAcc.low);
        oEnumC.Next();

        nCarry = oAcc.high;
    } // while

    return oC.Pin();
} // BignumImpl::Add


//////////////////////////////////////////////////////////////////////
//
// BignumImpl::cmp
//
Int BignumImpl::cmp(const BignumImpl* pA, const BignumImpl* pB)
{
    if (pA->IsPlus())
    {
        if (! pB->IsPlus()) return 1;

        Int nA = pA->GetLength();
        Int nB = pB->GetLength();

        if (nA != nB)
        {
            return nA - nB;
        }

        BignumImpl::EnumMtoL oEnumB(pB);
        foreach (BignumImpl::EnumMtoL, oEnumA, pA)
        {
            if (oEnumA.Get() > oEnumB.Get()) return 1;
            if (oEnumA.Get() < oEnumB.Get()) return -1;
            oEnumB.Next();
        } // for a

        return 0;
    }
    else if (pA->IsMinus())
    {
        if (! pB->IsMinus()) return -1;

        Int nA = pA->GetLength();
        Int nB = pB->GetLength();

        if (nA != nB)
        {
            return nB - nA;
        }

        BignumImpl::EnumMtoL oEnumB(pB);
        foreach (BignumImpl::EnumMtoL, oEnumA, pA)
        {
            if (oEnumA.Get() > oEnumB.Get()) return 1;
            if (oEnumA.Get() < oEnumB.Get()) return -1;
            oEnumB.Next();
        } // for a

        return 0;
    }
    else
    {
        if (pB->IsPlus())  return -1;
        if (pB->IsMinus()) return 1;
        return 0;
    }
} // BignumImp::Cmp


//////////////////////////////////////////////////////////////////////
//
// BignumImpl::Div
//
Val BignumImpl::Div(Val x, Val y)
{
    if(y->Decode<BignumImpl>()->IsOne()) return BignumImpl::Pin(x);
    return RatioImpl::Make(x, y);
} // BignumImpl::Div


namespace
{
template<class Op_>
class LogArith
{
    public: static Val Run(Val a, Val b)
    {
        BignumImpl* pA = a->Decode<BignumImpl>();
        BignumImpl* pB = b->Decode<BignumImpl>();

        ObStackScope oObStack;

        // +1 for sign
        StackBignum oC(max(pA->GetLength(), pB->GetLength()) + 1, pA);

        Big::EnumLtoM oEnumC(&oC);
        foreach (BignumImpl::EnumLtoM, oEnumB, pB)
        {
            oEnumC.Set(Op_::Compute(oEnumC.Get(), oEnumB.Get()));
            oEnumC.Next();
        } // for b

        Bigit nSignB = pB->GetSign();
        while (! oEnumC.AtEnd())
        {
            oEnumC.Set(Op_::Compute(oEnumC.Get(), nSignB));
            oEnumC.Next();
        } // for a

        return oC.Pin();
    } // Run
}; // LogArith

} // namespace


#define defclass_logarith(mp_Name, mp_op) \
    class Op##mp_Name \
    {  \
        public: static Bigit Compute(Bigit a, Bigit b) \
            { return a mp_op b; } \
    };

#define define_logarith(mp_Name, mp_op) \
    namespace { defclass_logarith(mp_Name, mp_op) } \
    Val BignumImpl::Log##mp_Name(Val x, Val y) \
        { return LogArith<Op##mp_Name>::Run(x, y); }

define_logarith(And, &)
define_logarith(Ior, |)
define_logarith(Xor, ^)


//////////////////////////////////////////////////////////////////////
//
// BignumImpl::Logbitp
//
// Note:
//  To compute bit position, we assume bignum is infinite bit string
//  in two's complement. For negative bignum, bit position greater
//  than places still points bit which value is one.
bool BignumImpl::Logbitp(Int iK, Val n)
{
    ASSERT(iK >= 0);

    BignumImpl* p = n->Decode<BignumImpl>();

    int iEltPos = static_cast<int>(iK / Bits);
    int iBitPos = static_cast<int>(iK % Bits);

    if (iEltPos >= p->GetLength()) return p->IsMinus();

    Bigit nMask = static_cast<Bigit>(1) << iBitPos;
    return 0 != (p->m_rgBigit[iEltPos] & nMask);
} // BignumImpl::Logbitp


//////////////////////////////////////////////////////////////////////
//
// BignumImpl::Mul
//
// Signe multiplication.
//
// See [Warrn03] p.130
//
// Test Cases:
//   (* -268435456 2) = -536870912
//   (* 4294967295 251658240) = 1080863910317260800
//
Val BignumImpl::Mul(Val x, Val y)
{
    BignumImpl* pA = x->Decode<BignumImpl>();
    BignumImpl* pB = y->Decode<BignumImpl>();

    if (pA->IsZero()) return Fixnum::Encode(0);
    if (pB->IsZero()) return Fixnum::Encode(0);
    if (pA->IsOne())  return BignumImpl::Pin(y);
    if (pB->IsOne())  return BignumImpl::Pin(x);

    Int nA = pA->GetLength();
    Int nB = pB->GetLength();

    // Make inner loop longer than outer loop.
    if (nA < nB)
    {
        swap(pA, pB);
        swap(nA, nB);
    }

    ObStackScope oObStack;

    // Allocate result bigits.
    // [White86], m x n requires m+n-1 or m+n bigits.
    // We need m+n bigits only if a=-2^n and b=-2^m.
    StackBignum oC(nA + nB, FillZero);

    if (1 == nB)
    {
        // m bigit x 1 bigit
        Bigit  nB = pB->m_rgBigit[0];
        Bigit* cij = oC.Bigits();
        Bigit nCarry = 0;
        foreach (BignumImpl::EnumLtoM, oEnumA, pA)
        {
            DblBigit oAcc = DblBigit::Mul(oEnumA.Get(), nB);
                oAcc += *cij;
                oAcc += nCarry;
            nCarry = oAcc.high;
            *cij++ = oAcc.low;
        } // for a
        *cij = nCarry;
    }
    else
    {
        // m bigit x n bigit
        Bigit* cj = oC.Bigits();
        foreach (BignumImpl::EnumLtoM, oEnumB, pB)
        {
            Bigit* cij = cj;
            Bigit nCarry = 0;
            foreach (BignumImpl::EnumLtoM, oEnumA, pA)
            {
                DblBigit oAcc = DblBigit::Mul(oEnumA.Get(), oEnumB.Get());
                    oAcc += *cij;
                    oAcc += nCarry;
                nCarry = oAcc.high;
                *cij++ = oAcc.low;
            } // for a
            *cij = nCarry;
            cj++;
        } // for b
    } // if

    // Now pC has the unsigned product. Correct by
    //  subtracting v x 2^(32+m) if u < 0, and
    //  subtracting u x 2^(32+n) if v < 0
    if (pA->IsMinus())
    {
        Bigit nBorrow = 0;
        Bigit* cjm = oC.Bigits() + nA;
        foreach (BignumImpl::EnumLtoM, oEnumB, pB)
        {
            DblBigit oAcc(*cjm, 0);
                oAcc -= oEnumB.Get();
                oAcc -= nBorrow;
            nBorrow = -static_cast<SignedBigit>(oAcc.high);
            *cjm++  = oAcc.low;;
        } // for b
        *cjm = nBorrow ? static_cast<Bigit>(-1) : 0;
    } // if a < 0

    if (pB->IsMinus())
    {
        Bigit nBorrow = 0;
        Bigit* cjn = oC.Bigits() + nB;
        foreach (BignumImpl::EnumLtoM, oEnumA, pA)
        {
            DblBigit oAcc(*cjn, 0);
                oAcc -= oEnumA.Get();
                oAcc -= nBorrow;
            nBorrow = -static_cast<SignedBigit>(oAcc.high);
            *cjn++  = oAcc.low;
        } // for a
        *cjn = nBorrow ? static_cast<Bigit>(-1) : 0;
    } // if b < 0

    return oC.Pin();
} // BignumImpl::Mul


//////////////////////////////////////////////////////////////////////
//
// BignumImpl::Pin
//  Returns bignum in heap.
//
Val BignumImpl::Pin(Val x)
{
    if (heap_object_p(x)) return x;

    BigPin oA(x);
    return oA.Pin();
} // BignumImpl::Pin


//////////////////////////////////////////////////////////////////////
//
// BignumImpl::ShiftLeft
//
Val BignumImpl::ShiftLeft(Val x, Int nCount)
{
    ASSERT(nCount >= 1);

    BignumImpl* pA = x->Decode<BignumImpl>();

    Int nA = pA->GetLength();
    Int cBits = nA * BigitBits + nCount;
    const Int cBigits = CEILING(cBits, BigitBits);

    ObStackScope oObStack;
    StackBignum oB(cBigits, FillZero);

    const uint nMod = static_cast<uint>(nCount % BigitBits);
    const uint nPos = static_cast<uint>(nCount / BigitBits);

    ::memcpy(
        oB.Bigits() + nPos,
        pA->m_rgBigit,
        sizeof(Bigit) * nA );

    if (pA->IsMinus() && 0 != nMod) 
    {
        oB.Bigits()[cBigits - 1] = static_cast<Bigit>(-1);
    }

    if (0 != nMod)
    {
        Big::EnumLtoM oEnumB(&oB, nPos);
        Bigit nCarry = 0;
        do
        {
            DblBigit oAcc = DblBigit::Shl(oEnumB.Get(), nMod);
                oAcc.low |= nCarry;

            oEnumB.Set(oAcc.low);

            nCarry = oAcc.high;

            oEnumB.Next();
        } while (! oEnumB.AtEnd());
    } // if

    return oB.Pin();
} // BignumImpl::ShiftLeft


//////////////////////////////////////////////////////////////////////
//
// BignumImpl::ShiftRight
//
Val BignumImpl::ShiftRight(Val a, Int nCount)
{
    ASSERT(nCount >= 1);

    BignumImpl* pA = a->Decode<BignumImpl>();

    Int nA = pA->GetLength();
    Int cBits = nA * BigitBits - nCount;

    if (cBits <= 0)
    {
        return Fixnum::Encode(0);
    }

    const Int nMod = nCount % BigitBits;

    Bigit nCarry = pA->GetSign() << (BigitBits - nMod);

    if (cBits <= Fixnum::Bits)
    {
        return Fixnum::Encode(
            nCarry | (pA->m_rgBigit[nA-1] >> nMod) );
    }
    else
    {
        ObStackScope oObStack;
        StackBignum oB(
            CEILING(cBits, BigitBits),
            pA->IsMinus() ? FillMinus1 : FillZero );

        BignumImpl::EnumMtoL oEnumA(pA);

        foreach (Big::EnumMtoL, oEnumB, &oB)
        {
            oEnumB.Set(nCarry | (oEnumA.Get() >> nMod));
            nCarry = oEnumA.Get() << (BigitBits - nMod);
            oEnumA.Next();
        } // for B

        return oB.Pin();
    } // if
} // BignumImpl::ShiftRight


//////////////////////////////////////////////////////////////////////
//
// BignumImpl::Sub
//
Val BignumImpl::Sub(Val a, Val b)
{
    BignumImpl* pA = a->Decode<BignumImpl>();
    BignumImpl* pB = b->Decode<BignumImpl>();

    if (pB->IsZero()) return BignumImpl::Pin(a);

    Int nA = pA->GetLength();
    Int nB = pB->GetLength();

    ObStackScope oObStack;
    StackBignum oC(max(nA, nB) + 1, pA);

    Big::EnumLtoM oEnumC(&oC);

    Bigit nBorrow = 0;
    foreach (BignumImpl::EnumLtoM, oEnumB, pB)
    {
        DblBigit oAcc(oEnumC.Get(), 0);
            oAcc -= oEnumB.Get();
            oAcc -= nBorrow;

        oEnumC.Set(oAcc.low);
        oEnumC.Next();

        nBorrow = -static_cast<SignedBigit>(oAcc.high);
    } // for A

    Bigit nSignB = pB->GetSign();
    if (nSignB != nBorrow)
    {
        while (! oEnumC.AtEnd())
        {
            DblBigit oAcc(oEnumC.Get(), 0);
                oAcc -= nSignB;
                oAcc -= nBorrow;

            oEnumC.Set(oAcc.low);
            oEnumC.Next();

            nBorrow = -static_cast<SignedBigit>(oAcc.high);
        } // for C
    } // if
    return oC.Pin();
} // BignumImpl::Sub


// BignumImpl::IntegerLength
uint BignumImpl::IntegerLength() const
{
    if (IsPlus())
    {
        uint cBigits = static_cast<uint>(GetLength());
        if (0 == m_rgBigit[cBigits - 1]) cBigits -= 1;
        uint cBits = (cBigits - 1) * sizeof(Bigit) * 8;
        Bigit k = m_rgBigit[cBigits - 1];
        while (0 != k)
        {
            k >>= 1;
            cBits += 1;
        } // while
        return cBits;
    }
    else
    {
        uint cBigits = static_cast<uint>(GetLength());
        uint cBits = (cBigits - 1) * sizeof(Bigit) * 8;
        SignedBigit k = m_rgBigit[cBigits - 1];
        while (-1 != k)
        {
            k >>= 1;
            cBits += 1;
        } // while
        return cBits;
    } // if
} // integer_length


//////////////////////////////////////////////////////////////////////
//
// toFloat64
//
static float64 toFloat64(Val a, uint nSign)
{
    const BignumImpl* pA = a->Decode<BignumImpl>();

    float64 flt = 0;
    DoubleFloat::Layout* p = reinterpret_cast<DoubleFloat::Layout*>(&flt);
    p->m_nSign = nSign;

    if (BignumImpl::Cmp(a, val_most_positive_float64_bignum) > 0)
    {
        p->m_nExponent = DoubleFloat::ExponentMax;
        return flt;
    }

    uint k = pA->IntegerLength();

    int m = k - DoubleFloat::NormalPrecision;

    Val b;

    if (m > 0)
    {
        b = BignumImpl::ShiftRight(a, m);
    }
    else if (m < 0)
    {
        b = BignumImpl::ShiftLeft(a, -m);
    }
    else
    {
        b = a;
    }

    #if SIZEOF_VAL == 4
    {
        const BignumImpl* pB = b->Decode<BignumImpl>();

        p->m_nSignificandL = pB->m_rgBigit[0];
        p->m_nSignificandH = pB->m_rgBigit[1];
    }
    #elif SIZEOF_VAL == 8
    {
        p->m_nSignificandL = Fixnum::Decode_(b);
        p->m_nSignificandH = Fixnum::Decode_(b) >> 32;
    }
    #endif // SIZEOF_VAL == 8

    uint e = k - 1 + DoubleFloat::ExponentBias;
        ASSERT(e < DoubleFloat::ExponentMax);

    p->m_nExponent = e;
    return flt;
} // toFloat64


//////////////////////////////////////////////////////////////////////
//
// BignumImpl::ToFloat64
//
//  Convert bignum to float64 if x is not
//    (<= (ash (1+ (ash -1 53)) 971) x (ash x (1- (ash 1 53)) 971))
//      971 = ExponentMax - ExponentBias - NormalPrecision
//          = 2047 - 1023 - 53
//  returns positive or negative infinity.
//
//  o Extract 53bit from MSB.
//
float64 BignumImpl::ToFloat64(Val a)
{
    BignumImpl* pA = a->Decode<BignumImpl>();

    if (pA->IsPlus())
    {
        return toFloat64(a, 0);
    }
    else
    {
        ObStackScope oObStack;
        StackBignumNeg oA(pA);
        return toFloat64(oA, 1);
    }
} // BignumImpl::ToFloat64


//////////////////////////////////////////////////////////////////////
//
// toFloat32
//
static float32 toFloat32(Val a, uint nSign)
{
    const BignumImpl* pA = a->Decode<BignumImpl>();

    float32 flt = 0;
    SingleFloat::Layout* p = reinterpret_cast<SingleFloat::Layout*>(&flt);
    p->m_nSign = nSign;

    if (BignumImpl::Cmp(a, val_most_positive_float32_bignum) > 0)
    {
        p->m_nExponent = SingleFloat::ExponentMax;
        return flt;
    }

    uint k = pA->IntegerLength();

    int m = k - SingleFloat::NormalPrecision;

    Val b;

    if (m > 0)
    {
        b = BignumImpl::ShiftRight(a, m);
    }
    else if (m < 0)
    {
        b = BignumImpl::ShiftLeft(a, -m);
    }
    else
    {
        CAN_NOT_HAPPEN();
    }

    p->m_nSignificand = Fixnum::Decode_(b);

    uint e = k - 1 + SingleFloat::ExponentBias;
        ASSERT(e < SingleFloat::ExponentMax);

    p->m_nExponent    = e;
    return flt;
} // toFloat32


//////////////////////////////////////////////////////////////////////
//
// BignumImpl::ToFloat32
//
//  Convert bignum to float32 if x is not
//    (<= (ash #x-FFFFFF 104) x (ash x #xFFFFFF 104)),
//      104 = ExponentMax - ExponentBias - NormalPrecision
//          = 255 - 127 - 24
//  returns positive or negative infinity.
//
//  o Extract 24bit from MSB.
//
float32 BignumImpl::ToFloat32(Val a)
{
    BignumImpl* pA = a->Decode<BignumImpl>();

    if (pA->IsPlus())
    {
        return toFloat32(a, 0);
    }
    else
    {
        ObStackScope oObStack;
        StackBignumNeg oA(pA);
        return toFloat32(oA, 1);
    }
} // BignumImpl::ToFloat32


//////////////////////////////////////////////////////////////////////
//
// BignumImpl::Truncate
//

static void truncate_aux(uint, Val, Val, Val*, Val*);
static void truncate_one(Big*, Bigit, Big*, Big* = NULL);
static void truncate_big(Big*, Big*, Big*, Big* = NULL);

void BignumImpl::Truncate(Val x, Val y, Val* out_q, Val* out_r)
{
    BignumImpl* pA = x->Decode<BignumImpl>();
    BignumImpl* pB = y->Decode<BignumImpl>();

    if (pB->IsZero())
    {
        error(Qdivision_by_zero,
            Koperation, Qtruncate,
            Koperands,  list(BignumImpl::Pin(x), BignumImpl::Pin(y)) );
    }

    int rgfSign = 0;
        if (pA->IsMinus()) rgfSign |= Signs_Neg_Pos;
        if (pB->IsMinus()) rgfSign |= Signs_Pos_Neg;

    switch (rgfSign)
    {
    case Signs_Pos_Pos:
    {
        truncate_aux(rgfSign, x, y, out_q, out_r);
        break;
    } // Signs_Pos_Pos

    case Signs_Neg_Pos:
    {
        ObStackScope oObStack;
        StackBignumNeg oA(pA);
        truncate_aux(rgfSign, oA, y, out_q, out_r);
        break;
    } // Signs_Neg_Pos

    case Signs_Pos_Neg:
    {
        ObStackScope oObStack;
        StackBignumNeg oB(pB);
        truncate_aux(rgfSign, x, oB, out_q, out_r);
        break;
    } // Signs_Pos_Neg

    case Signs_Neg_Neg:
    {
        ObStackScope oObStack;
        StackBignumNeg oA(pA);
        StackBignumNeg oB(pB);
        truncate_aux(rgfSign, oA, oB, out_q, out_r);
        break;
    } // Signs_Neg_Neg

    default:
        CAN_NOT_HAPPEN();
    } // switch sign
} // BignumImpl::Truncate


//////////////////////////////////////////////////////////////////////
//
// BignumImpl::truncate_aux
//
static void
truncate_aux(uint rgfSign, Val u, Val v, Val* out_q, Val* out_r)
{
    if (BignumImpl::Cmp(u, v) < 0)
    {
        if (NULL != out_r)
        {
            *out_r = BignumImpl::Pin(u);
        }

        if (NULL != out_q)
        {
            *out_q = Fixnum::Encode(0);
        }
        return;
    } // if u < v

    ObStackScope oObStack;

    BignumImpl* pU = u->Decode<BignumImpl>();
    StackBignum oU(pU->GetLength() + 1, pU);

    BignumImpl* pV = v->Decode<BignumImpl>();
    StackBignum oV(pV->GetLength(), pV);

    uint const nVLen = pV->GetLength() >= 2 && 0 == pV->GetMSB()
        ? pV->GetLength() - 1
        : pV->GetLength();

    // +1 for m+1
    // +1 for normalize (sign bigit)
    StackBignum oQ(oU.GetLength() - nVLen + 2, FillZero);
        // oQ[m-n+0]=0, oQ[m-n+1]=0

    if (NULL == out_r)
    {
        ASSERT(NULL != out_q);

        if (1 == nVLen)
        {
            truncate_one(&oU, oV.Bigits()[0], &oQ);
        }
        else
        {
            truncate_big(&oU, &oV, &oQ);
        }

        switch (rgfSign)
        {
        case Signs_Pos_Pos: // P / P => P, P
            *out_q = oQ.Pin();
            break;

        case Signs_Neg_Pos: // N / P => N, N
            *out_q = oQ.PinNeg();
            break;

        case Signs_Pos_Neg: // P / N => N, P
            *out_q = oQ.PinNeg();
            break;

        case Signs_Neg_Neg: // N / N => P, N
            *out_q = oQ.Pin();
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch rgfSign
    }
    else
    {
        // +f for normalize
        StackBignum oR(nVLen + 1);

        if (1 == nVLen)
        {
            truncate_one(&oU, oV.Bigits()[0], &oQ, &oR);
        }
        else
        {
            truncate_big(&oU, &oV, &oQ, &oR);
        }

        switch (rgfSign)
        {
        case Signs_Pos_Pos: // P / P => P, P
            if (NULL != out_q) *out_q = oQ.Pin();
            *out_r = oR.Pin();
            break;

        case Signs_Neg_Pos: // N / P => N, N
            if (NULL != out_q) *out_q = oQ.PinNeg();
            *out_r = oR.PinNeg();
            break;

        case Signs_Pos_Neg: // P / N => N, P
            if (NULL != out_q) *out_q = oQ.PinNeg();
            *out_r = oR.Pin();
            break;

        case Signs_Neg_Neg: // N / N => P, N
            if (NULL != out_q) *out_q = oQ.Pin();
            *out_r = oR.PinNeg();
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch rgfSign
    }
} // BignumImpl::truncate_aux


//////////////////////////////////////////////////////////////////////
//
// BignumImpl::truncate_big
//
// Note: For D6 testing (B=32), we can use following from 4500/501 where B=10:
//  (setq u #x7FFFFFFF800000000000000000000000)
//          ; 170141183420855150474555134919112130560
//  (setq v #x800000000000000000000001)
//          ; 39614081257132168796771975169
//  (/ u v)
//      56713727806951716824851711639704043520/13204693752377389598923991723
//      #x2AAAAAAA800000000000000000000000/2AAAAAAAAAAAAAAAAAAAAAAB
//
static void truncate_big(Big* pU, Big* pV, Big* pQ, Big* pR)
{
    Int m = pU->GetLength() - 1;
    Int n = pV->GetLength();

    Bigit* un = pU->Bigits();
    Bigit* vn = pV->Bigits();

    // D1: Normalize
    uint nShift = 0;
    {
        {
            Bigit dv = vn[n-1];
                if (0 == dv)
                {
                    n -= 1;
                    dv = vn[n-1];
                }
                ASSERT(0 != dv);
                ASSERT(n >= 2);
            while (dv < BignumImpl::BaseDiv2)
            {
                dv <<= 1;
                nShift += 1;
            }
        } // nShift

        if (0 == nShift)
        {
            un[m] = 0;
        }
        else
        {
            {
                Bigit nCarry = 0;
                foreach (Big::EnumLtoM, oEnum, pU)
                {
                    Bigit nHigh = oEnum.Get() >> (BigitBits - nShift);
                    oEnum.Set((oEnum.Get() << nShift) | nCarry);
                    nCarry = nHigh;
                } // for u
            }

            {
                Bigit nCarry = 0;
                foreach (Big::EnumLtoM, oEnum, pV)
                {
                    Bigit nHigh = oEnum.Get() >> (BigitBits - nShift);
                    oEnum.Set((oEnum.Get() << nShift) | nCarry);
                    nCarry = nHigh;
                } // for v
            }
        }
    } // D1

    {
        Bigit* q = pQ->Bigits();

        // D2: Q[m-n-1...0] := U[m-1...n] / V[n-1...0]
        for (Int j = m - n; j >= 0; j -= 1)
        {
            // D3: Calculate qhat
            Bigit qhat;
            {
                Bigit rhat;

                if (un[j+n] >= vn[n-1])
                {
                    qhat = BignumImpl::BaseMinus1;
                    rhat = un[j+n-1];
                    goto overflow;
                }
                else
                {
                    DblBigit::Div oDiv =
                        DblBigit::Truncate(un[j+n-1], un[j+n], vn[n-1]);

                    qhat = oDiv.q;
                    rhat = oDiv.r;
                }

                loop:
                    {
                        DblBigit aa = DblBigit::Mul(qhat, vn[n-2]);
                        DblBigit bb = DblBigit(un[j+n-2], rhat);

                        if (aa <= bb) goto done;
                    }

                    qhat -= 1;

                overflow:
                    {
                        bool fCarry;
                        rhat = DblBigit::Add(rhat, vn[n-1], &fCarry);
                        if (! fCarry) goto loop;
                    }
                done: ;
            } // qhat

            q[j] = qhat;

            // D4: mul and sub
            //  U[n+j...j] := U[n+j..j] - qhat * V[n-1...0]
            bool fBorrow;
            {
                Bigit nBorrow = 0;
                for (Int i = 0; i < n; i++)
                {
                    DblBigit ullM = DblBigit::Mul(qhat, vn[i]);
                    DblBigit ullS(un[i+j], 0);
                        ullS -= nBorrow;
                        ullS -= ullM.low;
                    un[i+j]  = ullS.low;
                    nBorrow = ullM.high - ullS.high;
                } // for i

                Bigit ujn = un[j+n] - nBorrow;
                un[j+n] = ujn;

                fBorrow = static_cast<int>(ujn) < 0;
            } // D4

            if (fBorrow)
            {
                q[j] -= 1;

                // D6: Add Back
                //  U[j+n...j] := U[j+n...j] + V[n-1...0]
                {
                    Bigit nCarry = 0;
                    for (Int i = 0; i < n; i++)
                    {
                        DblBigit ullS(nCarry, 0);
                            ullS += un[i+j];
                            ullS += vn[i];
                        un[i+j] = ullS.low;
                        nCarry  = ullS.high;
                    } // for v
                    un[j+n] += nCarry;
                } // D6
            } // if fBorrow
        } // for j
    }

    // D8: Shift Right
    if (NULL != pR)
    {
        ::memcpy(
            pR->Bigits(),
            pU->Bigits(),
            sizeof(Bigit) * pR->GetLength() );

        if (0 != nShift)
        {
            Bigit  nCarry = 0;
            foreach (Big::EnumMtoL, oEnum, pR)
            {
                Bigit nLowBits = oEnum.Get() << (BigitBits - nShift);
                oEnum.Set((oEnum.Get() >> nShift) | nCarry);
                nCarry = nLowBits;
            } // for
        } // if
    } // if
} // truncate_big


//////////////////////////////////////////////////////////////////////
//
// BignumImpl::truncate_one
//
static void truncate_one(Big* pU, Bigit nV, Big* pQ, Big* pR)
{
    Bigit nRem = 0;

    if (NULL != pQ)
    {
        Big::EnumMtoL oEnumQ(pQ);
            oEnumQ.Set(0);
            oEnumQ.Next();
        foreach (Big::EnumMtoL, oEnumU, pU)
        {
            DblBigit::Div oDiv = DblBigit::Truncate(oEnumU.Get(), nRem, nV);
            oEnumQ.Set(oDiv.q);
            nRem = oDiv.r;
            oEnumQ.Next();
        } // for U
    }
    else
    {
        foreach (Big::EnumMtoL, oEnumU, pU)
        {
            DblBigit::Div oDiv = DblBigit::Truncate(oEnumU.Get(), nRem, nV);
            nRem = oDiv.r;
        } // for U
    } // if

    if (NULL != pR)
    {
        pR->Bigits()[0] = nRem;
        pR->Bigits()[1] = 0;
    }
} // BignumImpl::truncate_one


// print_fixnum_aux
Val print_fixnum_aux(Val x, Val stream, Val baze)
{
    //(assert (<= 2 base 36))
    Val r;
    Val q = truncate(x, baze, &r);
    if (plusp(q)) print_fixnum_aux(q, stream, baze);
    write_char(digit_char(r, baze), stream);
    return x;
} // print_fixnum_aux


// print_bignum_aux
void print_bignum_aux(Val x, Val stream, Val baze)
{
    static Val s_divisor[37];
    static int s_rgnDigits[37];

    if (0 == s_rgnDigits[2])
    {
        for (int iBase = 2; iBase <= 36; iBase++)
        {
            int iPower1 = -1;
            int64 iNewDiv = iBase;
            int iDiv = 1;
            while (iNewDiv < ((1<<30) - 1))
            {
                s_divisor[iBase] = Fixnum::Encode(iDiv);
                s_rgnDigits[iBase] = iPower1 + 1;

                iPower1 += 1;
                iDiv = static_cast<int>(iNewDiv);
                iNewDiv = iNewDiv * iBase;
            } // while
        } // for base
    } // if

    class Printer
    {
        Val m_base;
        Val m_stream;
        Int m_iBase;

        public: void Run(Val x, Val stream, Val baze)
        {
            m_stream = stream;
            m_base = baze;
            m_iBase = Fixnum::Decode_(m_base);
            print_bignum_aux(x);
        } // Run

        void print_bignum_aux(Val x)
        {
            Val q, r;
                truncate(x, s_divisor[m_iBase], &q, &r);
            if (fixnump(q))
            {
                print_fixnum_aux(q, m_stream, m_base);
            }
            else
            {
                print_bignum_aux(q);
            }

            int cZeros = s_rgnDigits[m_iBase] - 1;
            Val base_power = m_base;
            while (MiniLisp::cmp(base_power, r) <= 0)
            {
                cZeros -= 1;
                base_power = MiniLisp::mul(base_power, m_base);
            }

            while (cZeros > 0)
            {
                write_char('0', m_stream);
                cZeros -= 1;
            }

            print_fixnum_aux(r, m_stream, m_base);
        } // print_aux
    }; // Printer

    Printer oPrinter;
    oPrinter.Run(x, stream, baze);
} // print_bignum_aux


// print_integer
Val print_integer(Val x, Val stream)
{
    Val baze = TLV(Aprint_baseA);
    if (! fixnump(baze) || cmp(baze, 2) < 0 || cmp(baze, 36) > 0)
    {
        baze = Fixnum::Encode(10);
    }

    if (minusp(x))
    {
        write_char('-', stream);
        x = sub(0, x);
    }

    if (fixnump(x))
    {
        print_fixnum_aux(x, stream, baze);
    }
    else
    {
        print_bignum_aux(x, stream, baze);
    }

    if (Fixnum::Encode(10) == baze && nil != TLV(Aprint_radixA))
    {
        write_char('.', stream);
    }

    return x;
} // print_integer


// coerce_bignum_float64
Val coerce_bignum_float64(Val x)
{
    check_type(x, bignum);
    return DoubleFloatImpl::Make(BignumImpl::ToFloat64(x));
} // coerce_bignum_float64


// coerce_bignum_float32
Val coerce_bignum_float32(Val x)
{
    check_type(x, bignum);
    return SingleFloatImpl::Make(BignumImpl::ToFloat32(x));
} // coerce_bignum_float32


// truncate_2
Val truncate_2(Val x, Val y)
{
    Thread* p = MiniThread::Get();

    switch (get_type_2(x, y, CLASSD_bignum, Qinteger))
    {
    case CLASSD2_(fixnum, fixnum):
    {
        BignumInt oX(Fixnum::Decode_(x));
        BignumInt oY(Fixnum::Decode_(y));
        BignumImpl::Truncate(oX, oY, &p->mv_value[0], &p->mv_value[1]);
        break;
    } // fixnum fixnum

    case CLASSD2_(fixnum, bignum):
    {
        BignumInt oX(Fixnum::Decode_(x));
        BignumImpl::Truncate(oX, y, &p->mv_value[0], &p->mv_value[1]);
        break;
    } // fixnum bignum

    case CLASSD2_(bignum, fixnum):
    {
        BignumInt oY(Fixnum::Decode_(y));
        BignumImpl::Truncate(x, oY, &p->mv_value[0], &p->mv_value[1]);
        break;
    } // fixnum fixnum

    case CLASSD2_(bignum, bignum):
    {
        BignumImpl::Truncate(x, y, &p->mv_value[0], &p->mv_value[1]);
        break;
    } // bignum fixnum

    default:
        CAN_NOT_HAPPEN();
    } // switch type

    return p->mv_value[0];
} // truncate_2

} // MiniLisp
