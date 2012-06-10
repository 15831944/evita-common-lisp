//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 12 Numbers - Bignum
// big/big_12_bignum.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_12_bignum.h#7 $
//
#if !defined(INCLUDE_big_12_bignum_h)
#define INCLUDE_big_12_bignum_h

namespace MiniLisp
{

class BignumImpl;


//////////////////////////////////////////////////////////////////////
//
// BignumImpl
//
// Description:
//  Represents a multiple word integer.
//
//  Arbitrary  integer is stored as vector of "bigit" in two's complement.
//  A "bigit" is 32-bit unsigned integer.
//
//  m_p contains bigits in least significant bigit first (little endian):
//      m_p[0]      = least significant bigit
//      ...
//      m_p[n-1]    = most significant bigit
//
//  A most signficant bigit is 32-bit integer instead of unsigned integer.
//
//  A m_p[n-1] is 0 or -1 when
//       0      if m_p[n-2] >= 0x8000_0000
//      -1      if m_p[n-2] <  0x8000_0000
//
class BignumImpl : public Kernel::Bignum
{
    public: static Val Add(Val, Val);
    public: static Int Cmp(Val x, Val y);
    public: static Val Div(Val, Val);
    public: static Val Mul(Val, Val);
    public: static Val Sub(Val, Val);
    public: static float64 ToFloat64(Val);
    public: static float32 ToFloat32(Val);

    public: static void Ceiling(Val, Val, Val*, Val*);
    public: static void Floor(Val, Val, Val*, Val*);
    public: static void Round(Val, Val, Val*, Val*);
    public: static void Truncate(Val, Val, Val*, Val*);

    public: static Val ShiftLeft(Val, Int);
    public: static Val ShiftRight(Val, Int);

    public: static Val LogAnd(Val, Val);
    public: static Val LogIor(Val, Val);
    public: static Val LogXor(Val, Val);
    public: static bool Logbitp(Int, Val);

    ////////////////////////////////////////////////////////////
    //
    // Internal
    //
    public: static const int   Bits = sizeof(Bigit) * 8;
    public: static const Bigit BaseDiv2 = static_cast<Bigit>(1) << (Bits-1);
    public: static const Bigit BaseMinus1 = static_cast<Bigit>(-1);

    public: uint IntegerLength() const;

    // GetLength
    public: Int GetLength() const
        { return Fixnum::Decode_(m_length); }

    // GetMSB
    public: SignedBigit GetMSB() const
        { return static_cast<SignedBigit>(m_rgBigit[GetLength() - 1]); }

    // GetSign
    public: Bigit GetSign() const
        { return IsMinus() ? static_cast<Bigit>(-1) : 0; }

    // IsPlus
    public: bool IsPlus() const
        { return ! IsZero() && GetMSB() >= 0; }

    // IsMinus
    public: bool IsMinus() const
        { return GetMSB() < 0; }

    // IsOne
    public: bool IsOne() const
        { return 1 == GetLength() && 1 == m_rgBigit[0]; }

    // IsZero
    public: bool IsZero() const
        { return 1 == GetLength() && 0 == m_rgBigit[0]; }

    // Pin
    public: static Val Pin(Val);

    // cmp
    static Int cmp(const BignumImpl*, const BignumImpl*);

    // Enum
    class Enum
    {
        protected: Bigit* m_pRunner;
        protected: Bigit* m_pEnd;

        protected: Enum(Bigit* pStart, Bigit* pEnd) :
            m_pRunner(pStart),
            m_pEnd(pEnd) {}

        protected: Enum(const Bigit* pStart, const Bigit* pEnd) :
            m_pRunner(const_cast<Bigit*>(pStart)),
            m_pEnd(const_cast<Bigit*>(pEnd)) {}

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
        public: EnumLtoM(BignumImpl* p, Int nSkip = 0) :
            Enum(p->m_rgBigit + nSkip, p->m_rgBigit + p->GetLength()) {}

        public: EnumLtoM(const BignumImpl* p, Int nSkip = 0) :
            Enum(p->m_rgBigit + nSkip, p->m_rgBigit + p->GetLength()) {}

        public: void Next()
            { ASSERT(! AtEnd()); m_pRunner++; }
    }; // EnumLtoM

    // EnumMtoL -- Most Significant Bigit to Least Significant Bigit.
    public: class EnumMtoL : public Enum
    {
        public: EnumMtoL(BignumImpl* p) :
            Enum(p->m_rgBigit + p->GetLength() - 1, p->m_rgBigit - 1) {}

        public: EnumMtoL(const BignumImpl* p) :
            Enum(p->m_rgBigit + p->GetLength() - 1, p->m_rgBigit - 1) {}

        public: void Next()
            { ASSERT(! AtEnd()); --m_pRunner; }

    }; // EnumMtoL
}; // BignumImpl

inline Int BignumImpl::Cmp(Val x, Val y)
    { return cmp(x->Decode<BignumImpl>(), y->Decode<BignumImpl>()); }

} // MiniLisp

#endif //!defined(INCLUDE_big_12_bignum_h)
