//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 12 Numbers - Bignum
// arch/generic/mini/x64_mini_12_bignum.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_12_bignum.h#3 $
//
#if !defined(INCLUDE_arch_x64_mini_x64_mini_12_bignum64_h)
#define INCLUDE_arch_x64_mini_x64_mini_12_bignum64_h

#if _WIN32

#include <intrin.h>
#pragma intrinsic(_umul128)
#pragma intrinsic(__shiftleft128)

namespace MiniLisp
{

class DblBigit
{
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

    // Complement
    public: static DblBigit Complement(Bigit a)
    {
        return DblBigit(~a, 0);
    } // Complement

    // Mul -- a x b => double bigit
    public: static DblBigit Mul(Bigit a, Bigit b)
    {
        uint64 ml, mh;
        ml = _umul128(a, b, &mh);
        return DblBigit(ml, mh);
    } // Mul

    // Shl -- arithmetic shift left
    //  Note: k should be less than 64.
    public: static DblBigit Shl(Bigit a, uint k)
    {
        uint64 sl = a << k;
        uint64 sh = __shiftleft128(a, 0, static_cast<uint8>(k));
        return DblBigit(sl, sh);
    } // Shl

    public: struct Div
    {
        Bigit q; Bigit r;
        Div(Bigit qq, Bigit rr) : q(qq), r(rr) {}
    }; // Div

    // Rem -- remiander
    public: static Bigit Rem(Bigit y, Bigit x, Bigit z)
    {
        Div oDiv = Truncate(x, y, z);
        return oDiv.r;
    } // Rem

    // Div -- (x||y) / z ... returns quotient and remainder
    // Note: On overflow, x >= z, this function returns undefined values.
    public: static Div Truncate(Bigit y, Bigit x, Bigit z)
    {
        const int N = sizeof(Bigit) * 8;
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
}; // DblBigit

#endif // _WIN32

} // MiniLisp

#endif // !defined(INCLUDE_arch_x64_mini_x64_mini_12_bignum64_h)
