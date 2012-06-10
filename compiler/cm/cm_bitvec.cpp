#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - main
// gs_main.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_bitvec.cpp#3 $
//
#include "./cm_bitvec.h"

#include "./cm_mm.h"

namespace Compiler
{

BitVec s_oBitVecNull(0, 0);

BitVec* BitVec::Null() { return &s_oBitVecNull; }


namespace
{

//////////////////////////////////////////////////////////////////////
//
// Mask For Element Access
//
uint32 const
k_rgnBitMask[32] =
{
    1u <<  0,  1u <<  1, 1u <<  2, 1u <<  3,
    1u <<  4,  1u <<  5, 1u <<  6, 1u <<  7,
    1u <<  8,  1u <<  9, 1u << 10, 1u << 11,
    1u << 12,  1u << 13, 1u << 14, 1u << 15,
    1u << 16,  1u << 17, 1u << 18, 1u << 19,
    1u << 20,  1u << 21, 1u << 22, 1u << 23,
    1u << 24,  1u << 25, 1u << 26, 1u << 27,
    1u << 28,  1u << 29, 1u << 30, 1u << 31,
}; // k_rgnBitMask

//////////////////////////////////////////////////////////////////////
//
// Mask For Copy
//
uint32 const
k_rgnCopyMask[32] =
{
    0,               //  0: 0000 0000 0000 0000 0000 0000 0000 0000 (not used)
    (1u <<  1) - 1,  //  1: 0000 0000 0000 0000 0000 0000 0000 0001
    (1u <<  2) - 1,  //  2: 0000 0000 0000 0000 0000 0000 0000 0011
    (1u <<  3) - 1,  //  3: 0000 0000 0000 0000 0000 0000 0000 0111

    (1u <<  4) - 1,  //  4: 0000 0000 0000 0000 0000 0000 0000 1111
    (1u <<  5) - 1,  //  5: 0000 0000 0000 0000 0000 0000 0001 1111
    (1u <<  6) - 1,  //  6: 0000 0000 0000 0000 0000 0000 0011 1111
    (1u <<  7) - 1,  //  7: 0000 0000 0000 0000 0000 0000 0111 1111

    (1u <<  8) - 1,  //  8: 0000 0000 0000 0000 0000 0000 1111 1111
    (1u <<  9) - 1,  //  9: 0000 0000 0000 0000 0000 0001 1111 1111
    (1u << 10) - 1,  // 10: 0000 0000 0000 0000 0000 0011 1111 1111
    (1u << 11) - 1,  // 11: 0000 0000 0000 0000 0000 0111 1111 1111

    (1u << 12) - 1,  // 12: 0000 0000 0000 0000 0000 1111 1111 1111
    (1u << 13) - 1,  // 13: 0000 0000 0000 0000 0001 1111 1111 1111
    (1u << 14) - 1,  // 14: 0000 0000 0000 0000 0011 1111 1111 1111
    (1u << 15) - 1,  // 15: 0000 0000 0000 0000 0111 1111 1111 1111

    (1u << 16) - 1,  // 16: 0000 0000 0000 0000 1111 1111 1111 1111
    (1u << 17) - 1,  // 17: 0000 0000 0000 0001 1111 1111 1111 1111
    (1u << 18) - 1,  // 18: 0000 0000 0000 0011 1111 1111 1111 1111
    (1u << 19) - 1,  // 19: 0000 0000 0000 0111 1111 1111 1111 1111

    (1u << 20) - 1,  // 20: 0000 0000 0000 1111 1111 1111 1111 1111
    (1u << 21) - 1,  // 21: 0000 0000 0001 1111 1111 1111 1111 1111
    (1u << 22) - 1,  // 22: 0000 0000 0011 1111 1111 1111 1111 1111
    (1u << 23) - 1,  // 23: 0000 0000 0111 1111 1111 1111 1111 1111

    (1u << 24) - 1,  // 24: 0000 0000 1111 1111 1111 1111 1111 1111
    (1u << 25) - 1,  // 25: 0000 0001 1111 1111 1111 1111 1111 1111
    (1u << 26) - 1,  // 26: 0000 0011 1111 1111 1111 1111 1111 1111
    (1u << 27) - 1,  // 27: 0000 0111 1111 1111 1111 1111 1111 1111

    (1u << 28) - 1,  // 28: 0000 1111 1111 1111 1111 1111 1111 1111
    (1u << 29) - 1,  // 29: 0001 1111 1111 1111 1111 1111 1111 1111
    (1u << 30) - 1,  // 30: 0011 1111 1111 1111 1111 1111 1111 1111
    (1u << 31) - 1,  // 31: 0111 1111 1111 1111 1111 1111 1111 1111
}; // k_rgnCopyMask

} // namespace

//////////////////////////////////////////////////////////////////////
//
// And
//
void
BitVec::And(const BitVec* pBitVec)
{
    ASSERT(NULL != pBitVec);
    ASSERT(GetLength() == pBitVec->GetLength());

    uint32* pStart = getBits();
    uint32* pEnd   = pStart + CEILING(GetLength(), 32);
    const uint32* pSrc = pBitVec->getBits();

    for (uint32* pRunner = pStart; pRunner < pEnd; pRunner++)
    {
        *pRunner &= *pSrc;
        pSrc++;
    } // for each word
} // BitVec::And


//////////////////////////////////////////////////////////////////////
//
// AndC2
//
void
BitVec::AndC2(const BitVec* pBitVec)
{
    ASSERT(NULL != pBitVec);
    ASSERT(GetLength() == pBitVec->GetLength());

    uint32* pStart = getBits();
    uint32* pEnd   = pStart + CEILING(GetLength(), 32);
    const uint32* pSrc = pBitVec->getBits();

    for (uint32* pRunner = pStart; pRunner < pEnd; pRunner++)
    {
        *pRunner &= ~*pSrc;
        pSrc++;
    } // for each word
} // BitVec::AndC2


//////////////////////////////////////////////////////////////////////
//
// Count One In uint32
//
// See [Warren02], Chapter 5.
//
// [Warren02] Henry S. Warren, Jr.: Hacker's Delight. Addison Wesley. 2002
//
#if 1
static uint
count_one_in_uint32(uint32 x)
{
    uint32 n;

    n = (x >> 1) & 0x77777777;
    x -= n;

    n = (n >> 1) & 0x77777777;
    x -= n;

    n = (n >> 1) & 0x77777777;
    x -= n;

    x = (x + (x >> 4)) & 0x0F0F0F0F;
    x *= 0x01010101;
    x >>= 24;

    return x;
} // count_one_in_uint32
#else
static uint
count_one_in_uint32(uint32 x)
{
    x -= (x >> 1) & 0x55555555;
    x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
    x = (x + (x >> 4)) & 0x0F0F0F0F;
    x += x >> 8;
    x += x >> 16;
    return x & 0x3F;
} // count_one_in_uint32
#endif


//////////////////////////////////////////////////////////////////////
//
// Count One
//
uint
BitVec::CountOne() const
{
    const uint32* pStart = getBits();
    const uint32* pEnd   = pStart + GetLength() / 32;

    uint nCount = 0;
    for (const uint32* pRunner = pStart; pRunner < pEnd; pRunner++)
    {
        nCount += count_one_in_uint32(*pRunner);
    } // for each uint32

    uint nRest = GetLength() % 32;
    if (0 != nRest)
    {
        nCount += count_one_in_uint32(*pEnd & k_rgnCopyMask[nRest]);
    }

    return nCount;
} // BitVec::CountOne


//////////////////////////////////////////////////////////////////////
//
// Copy
//
BitVec*
BitVec::Copy(const BitVec* pBitVec)
{
    ASSERT(NULL != pBitVec);
    ASSERT(GetLength() == pBitVec->GetLength());
    ::CopyMemory(getBits(), pBitVec->getBits(), getSize());
    return this;
} // BitVec::Copy


//////////////////////////////////////////////////////////////////////
//
// Equal
//
bool
BitVec::Equal(const BitVec* pBitVec) const
{
    ASSERT(NULL != pBitVec);
    ASSERT(GetLength() == pBitVec->GetLength());

    const uint32* pStart = getBits();
    const uint32* pEnd   = pStart + GetLength() / 32;

    const uint32* pX = pStart;
    const uint32* pY = pBitVec->getBits();
    while (pX < pEnd)
    {
        if (*pX != *pY) return false;
        pX++;
        pY++;
    } // while

    uint nRest = GetLength() % 32;

    if (0 == nRest)
    {
        return true;
    }

    uint32 nX = *pX & k_rgnCopyMask[nRest];
    uint32 nY = *pY & k_rgnCopyMask[nRest];

    return nX == nY;
} // BitVec::Copy


//////////////////////////////////////////////////////////////////////
//
// Fill One
//
BitVec*
BitVec::FillOne()
{
    ::FillMemory(getBits(), getSize(), 0xFF);
    return this;
} // BitVec::FillOne


//////////////////////////////////////////////////////////////////////
//
// Fill Zero
//
BitVec*
BitVec::FillZero()
{
    ::ZeroMemory(getBits(), getSize());
    return this;
} // BitVec::FillZero


//////////////////////////////////////////////////////////////////////
//
// IsOne
//
uint
BitVec::IsOne(uint nIndex) const
{
    uint nWordPos = nIndex / 32;
    uint nBitPos  = nIndex % 32;
    const uint32* pBits = getBits();
    return pBits[nWordPos] & k_rgnBitMask[nBitPos];
} // BitVec::IsOne


//////////////////////////////////////////////////////////////////////
//
// Hash
//
uint
BitVec::Hash() const
{
    const uint32* pStart = getBits();
    const uint32* pEnd   = pStart + GetLength() / 32;

    uint nHashCode = 0;
    for (const uint* pRunner = pStart; pRunner < pEnd; pRunner++)
    {
        nHashCode ^= *pRunner;
        nHashCode <<= 1;
    } // for each element

    if (0 != GetLength() % 32)
    {
        nHashCode ^= *pEnd & k_rgnCopyMask[GetLength() % 32];
        nHashCode ^= GetLength();
    }

    return nHashCode;
} // BitVec::Hash


//////////////////////////////////////////////////////////////////////
//
// Ior
//
void
BitVec::Ior(const BitVec* pBitVec)
{
    ASSERT(NULL != pBitVec);
    ASSERT(GetLength() == pBitVec->GetLength());

    uint32* pStart = getBits();
    uint32* pEnd   = pStart + CEILING(GetLength(), 32);
    const uint32* pSrc   = pBitVec->getBits();

    for (uint32* pRunner = pStart; pRunner < pEnd; pRunner++)
    {
        *pRunner |= *pSrc;
        pSrc++;
    } // for each word
} // BitVec::Ior


//////////////////////////////////////////////////////////////////////
//
// Make
//
BitVec*
BitVec::Make(Mm* pMm, uint cBits)
{
    ASSERT(cBits >= 1);
    ASSERT(cBits <= 9999);

    uint cWords   = static_cast<uint>(CEILING(cBits, 32));
    uint cbBitVec = sizeof(BitVec) + cWords * 4;
    BitVec* pBitVec = new(pMm->Alloc(cbBitVec)) BitVec(cBits, cWords * 32);
    return pBitVec;
} // BitVec::Make


//////////////////////////////////////////////////////////////////////
//
// Position One
//
uint
BitVec::PositionOne() const
{
    const uint32* pStart = getBits();
    const uint32* pEnd   = pStart + GetLength() / 32;

    uint nBase = 0;
    for (const uint32* pRunner = pStart; pRunner < pEnd; pRunner++)
    {
        uint32 x = *pRunner;
        uint nPos = nBase;
        while (x != 0)
        {
            if (x & 1)
            {
                return nPos;
            }

            nPos += 1;

            x >>= 1;
        } // while

        nBase += 32;
    } // for each uint32

    uint nRest = GetLength() % 32;
    if (0 != nRest)
    {
        uint32 x = *pEnd & k_rgnCopyMask[nRest];
        uint nPos = nBase;
        while (x != 0)
        {
            if (x & 1)
            {
                return nPos;
            }

            nPos += 1;

            x >>= 1;
        } // while
    } // if

    return static_cast<uint>(-1);
} // BitVec::PositionOne


//////////////////////////////////////////////////////////////////////
//
// Position One From End
//
uint
BitVec::PositionOneFromEnd() const
{
    const uint32* pStart = getBits();
    const uint32* pEnd   = pStart + GetLength() / 32;

    uint nRest = GetLength() % 32;
    if (0 != nRest)
    {
        uint32 x = *pEnd & k_rgnCopyMask[nRest];
        uint nPos = GetLength();
        uint nMask = k_rgnBitMask[nRest];
        while (0 != nMask)
        {
            nMask >>= 1;
            nPos -= 1;
            if (x & nMask)
            {
                return nPos;
            }
        } // while
    } // if

    uint nBase = GetLength() - nRest;
    const uint32* pRunner = pEnd;
    while (pRunner > pStart)
    {
        --pRunner;

        uint32 x = *pRunner;
        uint nPos = nBase;
        while (x != 0)
        {
            nPos -= 1;
            if (static_cast<int>(x) < 0)
            {
                return nPos;
            }

            x <<= 1;
        } // while

        nBase -= 32;
    } // for each uint32

    return static_cast<uint>(-1);
} // BitVec::PositionOneFromEnd


//////////////////////////////////////////////////////////////////////
//
// Set One
//
void
BitVec::SetOne(uint nIndex)
{
    ASSERT(nIndex < m_cBits);

    uint nWordPos = nIndex / 32;
    uint nBitPos  = nIndex % 32;
    uint32* pBits = getBits();
    pBits[nWordPos] |= k_rgnBitMask[nBitPos];
} // BitVec::SetOne


//////////////////////////////////////////////////////////////////////
//
// SetZero
//
void
BitVec::SetZero(uint nIndex)
{
    ASSERT(nIndex < m_cBits);

    uint nWordPos = nIndex / 32;
    uint nBitPos  = nIndex % 32;
    uint32* pBits = getBits();
    pBits[nWordPos] &= ~k_rgnBitMask[nBitPos];
} // BitVec::SetZero


//////////////////////////////////////////////////////////////////////
//
// Xor
//
void
BitVec::Xor(const BitVec* pBitVec)
{
    ASSERT(NULL != pBitVec);
    ASSERT(GetLength() == pBitVec->GetLength());

    uint32* pStart = getBits();
    uint32* pEnd   = pStart + CEILING(GetLength(), 32);
    const uint32* pSrc   = pBitVec->getBits();

    for (uint32* pRunner = pStart; pRunner < pEnd; pRunner++)
    {
        *pRunner ^= *pSrc;
        pSrc++;
    } // for each word
} // BitVec::Xor

} // Compiler
