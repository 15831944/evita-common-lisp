//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - Bit Vector
// cm_bitvec.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_bitvec.h#2 $
//
#if !defined(INCLUDE_compiler_cm_bitvec_h)
#define INCLUDE_compiler_cm_bitvec_h

#include "./cm_base.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// BitVec
//
class BitVec
{
    private: static void* operator new(size_t, void* p) { return p; }

    protected: uint m_cBits;
    protected: uint m_cAlloc;

    public: BitVec(uint cBits, uint cAlloc) :
        m_cBits(cBits),
        m_cAlloc(cAlloc) {}

    public: static BitVec* Null();

    public: static BitVec* Make(Mm*, uint);

    public: BitVec* Adjust(Mm* p, uint n)
    {
        if (n > m_cAlloc) return Make(p, n);
        m_cBits = n;
        return this;
    } // Adjust

    public: uint    CountOne() const;
    public: uint    GetLength() const { return m_cBits; }
    public: BitVec* Copy(const BitVec*);
    public: bool    Equal(const BitVec*) const;
    public: BitVec* FillOne();
    public: BitVec* FillZero();
    public: uint    Hash() const;
    public: uint    IsOne(uint) const;
    public: uint    IsZero(uint i) const { return ! IsOne(i); }
    public: uint    PositionOne() const;
    public: uint    PositionOneFromEnd() const;

    public: void SetOne(uint);
    public: void SetZero(uint);

    public: void And(const BitVec*);
    public: void AndC2(const BitVec*);
    public: void Ior(const BitVec*);
    public: void Xor(const BitVec*);

    protected: uint32* getBits()
        { return reinterpret_cast<uint32*>(this + 1); }

    protected: const uint32* getBits() const
        { return const_cast<BitVec*>(this)->getBits(); }

    protected: size_t getSize() const
        { return CEILING(GetLength(), 32) * 4; }
}; // BitVec

} // Compiler

#endif //!defined(INCLUDE_compiler_cm_bitvec_h)
