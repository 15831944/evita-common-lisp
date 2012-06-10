//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 12 Numbers - Bignum Primitives
// arch/generic/mini/gen_mini_12_bignum.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/generic/mini/gen_mini_12_bignum.h#2 $
//
#if !defined(INCLUDE_arch_generic_mini_gen_mini_12_bignum_h)
#define INCLUDE_arch_generic_mini_gen_mini_12_bignum_h

namespace MiniLisp
{

//////////////////////////////////////////////////////////////////////
//
// DblBigit
//  Primitivie operations for bignum arithmetic.
//
//  Primitive operations are:
//      operator +=     Accumrator
//      operator -=     Accumrator
//      operator <=     Compare two DblBigits
//      Add             N + N -> N+1 addition
//      Mul             N x N -> 2N
//      Shl             Shift left
//      Truncate        Quotient and remainder of 2N / N.
//
class DblBigit
{
    static const int N = sizeof(Bigit) * 8;

    public: Bigit   low;
    public: Bigit   high;

    public: DblBigit(Bigit l, Bigit h) :
        low(l), high(h) {}

    public: DblBigit& operator +=(Bigit n)
    {
        Bigit a = low;
        low += n;
        if (low < a) high += 1;
        return *this;
    } // operator +=

    public: DblBigit& operator -=(Bigit n)
    {
        Bigit a = low;
        low -= n;
        if (low > a) high -= 1;
        return *this;
    } // operator -=

    public: bool operator <=(const DblBigit& b) const
    {
        if (high == b.high) return low <= b.low;
        return high < b.high;
    } // operator <=

    // Add -- with carry
    public: static Bigit Add(Bigit a, Bigit b, bool* out_fCarry)
    {
        Bigit c = a + b;
        *out_fCarry = c < a;
        return c;
    } // Add

    // Mul -- a x b => double bigit
    public: static DblBigit Mul(Bigit a, Bigit b)
    {
        return DblBigit(a * b, umulh(a, b));
    } // Mul

    // Shl -- arithmetic shift left
    //  Note: k should be less than N.
    public: static DblBigit Shl(Bigit a, uint k)
    {
        Bigit sl = a << k;
        Bigit sh = a >> (N-k);
        return DblBigit(sl, sh);
    } // Shl

    // Div -- result of Truncate
    public: struct Div
    {
        Bigit q; Bigit r;
        Div(Bigit qq, Bigit rr) : q(qq), r(rr) {}
    }; // Div

    // Div -- (x||y) / z ... returns quotient and remainder
    // Note: On overflow, x >= z, this function returns undefined values.
    public: static Div Truncate(Bigit y, Bigit x, Bigit z)
    {
        for (int i = 1; i <= N; i++)
        {
            // All 1's if MSB=1
            Bigit w = static_cast<SignedBigit>(x) >> (N-1);

            // (x||y) <<= 1
            x = (x << 1) | (y >> (N-1));
            y <<= 1;

            // 33bit comparision
            if ((x | w) >= z)
            {
                x -= z;
                y += 1;
            }
        } // for i
        return Div(y, x);
    } // Truncate

    // umulh -- compute upper 64bit of multiplication
    static Bigit umulh(Bigit u, Bigit v)
    {
        const Bigit MASK = (static_cast<Bigit>(1) << N/2) - 1;
        Bigit u0 = u & MASK;
        Bigit u1 = u >> N/2;
        Bigit v0 = v & MASK;
        Bigit v1 = v >> N/2;
        Bigit w0 = u0 * v0;
        Bigit tt = u1*v0 + (w0 >> N/2);
        Bigit w1 = tt & MASK;
        Bigit w2 = tt >> N/2;
              w1 = u0*v1 + w1;
        return u1*v1 + w2 + (w1 >> N/2);
    } // umulh
}; // DblBigit

} // MiniLisp

#endif // !defined(INCLUDE_arch_generic_mini_gen_mini_12_bignum_h)
