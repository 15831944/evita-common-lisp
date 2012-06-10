#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - big - 15 Arrays
// big/big_15_array.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/generic/mini/gen_mini_15_bitvec.cpp#5 $
//
#include "../../big/big_lisp.h"

namespace MiniLisp
{

void bit_replace(Val v1, Val v2, uint nS1, uint nS2, uint nN);


namespace
{

typedef Arch::BitEltT BitEltT;
const int k_BITS = sizeof(BitEltT) * 8;

// BitReaderBackward
class BitReaderBackward
{
    const BitEltT*  m_p;
    const BitEltT*  m_s;
    int   m_k;
    int   m_n;

    public: BitReaderBackward(const BitEltT* p, uint s, uint e)
    {
        uint sw = s / k_BITS;
        //uint sb = s % k_BITS;

        e -= 1;

        uint ew = e / k_BITS;
        uint eb = e % k_BITS;

        m_p = p + ew;
        m_s = p + sw;

        m_k = eb + 1;
        m_n = e - s + 1;
    } // BitReaderBackward

    public: bool AtEnd() const { return m_n <= 0; }

    //       V------eb = 2
    //  abcdefgh    bits  = m_p[0]
    //  fgh00000    bis <<= k_BITS - m_k;
    //  IJKLMNOP    m_p[-1]
    //  fghIJKLM    bits |= m_p[-1] >> m_k;
    public: BitEltT Read()
    {
        ASSERT(m_n > 0);

        BitEltT bits = *m_p--;

        if (k_BITS != m_k)
        {
            bits <<= k_BITS - m_k;

            if (m_n > m_k)
            {
                bits |= *m_p >> m_k;
            }
        }

        DEBUG_PRINTF(L" %3d %p %p bits=%p\r\n", m_n, m_p, m_p[1], bits);

        m_n -= k_BITS;
        return bits;
    } // Read
}; // BitReaderBackward


// BitReaderForward
class BitReaderForward
{
    const BitEltT*  m_p;
    const BitEltT*  m_e;
    int   m_k;
    int   m_n;

    public: BitReaderForward(const BitEltT* p, uint s, uint e)
    {
        uint sw = s / k_BITS;
        uint sb = s % k_BITS;

        uint ew = e / k_BITS;
        //uint eb = e % k_BITS;

        m_p = p + sw;
        m_e = p + ew;

        m_k = sb;
        m_n = e - s;
    } // BitReaderForward

    public: bool AtEnd() const { return m_n <= 0; }

    //       V------sb = 2
    //  abcdefgh    bits   = m_p[0]
    //  ---abcde    bits >>= m_k
    //  IJKLMNOP    m_p[1]
    //  NOPabcde    bits  |= m_p[1] << (k_BITS - m_k)
    public: BitEltT Read()
    {
        ASSERT(m_n > 0);

        BitEltT bits = *m_p++;

        if (0 != m_k)
        {
            bits >>= m_k;

            if (m_n > k_BITS - m_k)
            {
                bits |= *m_p << (k_BITS - m_k);
            }
        }

        DEBUG_PRINTF(L" %3d %p\r\n", m_n, m_p - 1, m_p[-1]);

        m_n -= k_BITS;
        return bits;
    } // Read
}; // BitReaderForward


// BitWriterBackward
class BitWriterBackward
{
    BitEltT m_acc;

    BitEltT*    m_p;
    BitEltT*    m_s;
    BitEltT     m_last;

    uint        m_sb;
    int         m_k;
    uint        m_c;
    int         m_n;

    public: BitWriterBackward(BitEltT* p, uint s, uint e)
    {
        uint sw = s / k_BITS;
        uint sb = s % k_BITS;

        e -= 1;

        uint ew = e / k_BITS;
        uint eb = e % k_BITS;

        m_p = p + ew;
        m_s = p + sw;

        m_sb = sb;

        // Reset LSB's
        //      V------ m_k = 2
        // abcdefgh     p[ew]
        // abcde000     p[ew] & ~( (1 << (eb+1)) - 1 )
        m_acc  = p[ew] & ~( (static_cast<BitEltT>(1) << (eb+1)) - 1 );
        m_last = p[sw];

        m_c = eb;
        m_k = eb;
        m_n = e - s + 1;
    } // BitWriteBackward

    public: ~BitWriterBackward()
        { Close(); }

    // Close -- bit write stream
    //      V------ m_sb = 3
    //  abcdefgh    *m_p
    //  ABCDEFGH    m_last
    //  abcdeFGH
    public: void Close()
    {
        DEBUG_PRINTF(L"%3d %p *m_p=%p last=%p m_acc=%p\r\n",
            m_n, m_p, *m_p, m_last, m_acc );

        if (m_n > 0)
        {
            *m_p = m_acc;
        }

        if (0 != m_sb)
        {
            // Restore LSB's of the last word
            *m_p |= m_last & ( (static_cast<BitEltT>(1) << m_sb)-1 );
        }
    } // Close

    // Write -- put bits into stream
    //       V------m_k = 2
    //  abcdefgh    *m_p
    //  abcde000    m_acc
    //  ABCDEFGH    bits
    //  00000ABC    bits >> (k_BITS - m_k)
    //  abcdeABC    *m_p = bits >> (k_BITS - m_k - 1)
    //  DEFGH000    m_acc = bits << (m_k + 1)
    public: void Write(BitEltT bits)
    {
        ASSERT(m_n > 0);

        DEBUG_PRINTF(L"%3d %p %p bits=%p m_acc=%p\r\n",
            m_n, m_p, *m_p, bits, m_acc);

        if (0 == m_k)
        {
            *m_p = bits;
            m_n -= k_BITS;
        }
        else
        {
            m_n -= m_c;
            m_c = k_BITS;

            *m_p = m_acc;
            *m_p |= bits >> (k_BITS - m_k - 1);
            m_acc = bits << (m_k + 1);
        }

        DEBUG_PRINTF(L"*m_p=%p m_acc=%p\r\n", *m_p, bits, m_acc);

        if (m_n > 0) --m_p;
    } // Write
}; // BitWriteBackward


// BitWriterForward
class BitWriterForward
{
    BitEltT m_acc;

    BitEltT*    m_p;
    BitEltT*    m_e;
    BitEltT     m_last;
    uint        m_sb;
    uint        m_eb, m_ew;
    uint        m_c;
    int         m_n;

    public: BitWriterForward(BitEltT* p, uint s, uint e)
    {
        uint sw = s / k_BITS;
        uint sb = s % k_BITS;

        uint ew = e / k_BITS;
        uint eb = e % k_BITS;

        m_p = p + sw;
        m_e = p + ew;

        m_sb = sb;
        m_eb = eb;

        // extract LSB's
        m_acc  = p[0] & ((static_cast<BitEltT>(1) << sb) - 1);
        m_last = p[ew];

        m_n = e - s;
        m_c = k_BITS - sb;
    } // BitWriteForward

    public: ~BitWriterForward()
        { Close(); }

    // Close -- bit write stream
    //      V------ m_eb = 3
    //  abcdefgh    m_last
    //  000abcde    m_last >> m_eb
    //  abcde000    (m_last >> m_eb) << m_eb
    public: void Close()
    {
        if (m_n > 0)
        {
            *m_p = m_acc;
        }

        if (0 != m_eb)
        {
            // Restore MSB's of the last word
            *m_p &= (static_cast<BitEltT>(1) << m_eb) - 1;
            *m_p |= (m_last >> m_eb) << m_eb;
        }
    } // Close

    // Write -- put bits into stream
    //      V-------m_sb = 3
    //  abcdefgh    *m_p
    //  00000fgh    m_acc
    //  ABCDEFGH    bits
    //  DEFGH000    *m_p = bits << m_sb
    //  00000ABC    m_acc = bits >> (k_BITS - m_sb)
    public: void Write(BitEltT bits)
    {
        ASSERT(m_p <= m_e);

        if (0 == m_sb)
        {
            *m_p = bits;
            m_n -= k_BITS;
        }
        else
        {
            *m_p = m_acc;
            *m_p |= bits << m_sb;
            m_acc = bits >> (k_BITS - m_sb);

            m_n -= m_c;
            m_c = k_BITS;
        }

        DEBUG_PRINTF(L"%3d %p %p acc=%p\r\n", m_n, m_p, bits, m_acc);

        if (m_n > 0) m_p++;
    } // Write
}; // BitWriteForward


// is_overlap
inline bool is_overlap(uint s1, uint e1, uint e2)
{
    return e2 > s1 && e2 < e1;
} // is_overlap


// v1 <= v2 op v3
template<class Op>
Val bit_op(Val v1, Val v2, Val v3, Val s1, Val s2, Val s3, Val n)
{
          BitEltT* p1 = v1->Decode<SimpleBitVector>()->GetElements();
    const BitEltT* p2 = v2->Decode<SimpleBitVector>()->GetElements();
    const BitEltT* p3 = v3->Decode<SimpleBitVector>()->GetElements();

    uint nS1 = static_cast<uint>(Fixnum::Decode_(s1));
    uint nS2 = static_cast<uint>(Fixnum::Decode_(s2));
    uint nS3 = static_cast<uint>(Fixnum::Decode_(s3));
    uint nN  = static_cast<uint>(Fixnum::Decode_(n));

    uint nE1 = nS1 + nN;
    uint nE2 = nS2 + nN;
    uint nE3 = nS3 + nN;

    uint nOverlap = 0;

    if (v1 == v2 && is_overlap(nS1, nE1, nE3)) nOverlap |= 1;
    if (v1 == v2 && is_overlap(nS1, nE1, nE2)) nOverlap |= 2;

    Val v11 = v1;

    if (3 == nOverlap && (nE2 > nE1 || nE3 > nE1))
    {
        //  1   o----------o
        //  2       o----------o
        //  3 o----------o
        nOverlap = 0;
        v11 = MiniThread::Get()->AllocBinVec(CLASSD_simple_bit_vector, n);
        nS3 = 0; nE3 = nN;
    } // if

    if (0 == nOverlap)
    {
        BitWriterForward o1(p1, nS1, nE1);
        BitReaderForward o2(p2, nS2, nE2);
        BitReaderForward o3(p3, nS3, nE3);

        while (! o2.AtEnd())
            { o1.Write(Op::Compute(o2.Read(), o3.Read())); }
    }
    else
    {
        BitWriterBackward o1(p1, nS1, nE1);
        BitReaderBackward o2(p2, nS2, nE2);
        BitReaderBackward o3(p3, nS3, nE3);

        while (! o2.AtEnd())
            { o1.Write(Op::Compute(o2.Read(), o3.Read())); }
    } // if

    if (v11 != v1) bit_replace(v11, v1, 0, nS1, nN);

    return v11;
} // bit_op

} // namespace


#define define_bit_3op(mp_name, mp_expr) \
    namespace { class Op_##mp_name \
    { \
        public: static BitEltT Compute(BitEltT x, BitEltT y) \
            { return mp_expr; } \
    }; } \
    Val bit_##mp_name(Val v1, Val v2, Val v3, Val s1, Val s2, Val s3, Val n) \
        { return bit_op<Op_##mp_name>(v1, v2, v3, s1, s2, s3, n); }

define_bit_3op(and,      ( x &  y))
define_bit_3op(andc1,    (~x &  y))
define_bit_3op(andc2,    ( x & ~y))
define_bit_3op(eqv,     ~( x ^  y))
define_bit_3op(ior,      ( x |  y))
define_bit_3op(nand,    ~( x &  y))
define_bit_3op(nor,     ~( x |  y))
define_bit_3op(orc1,     (~x |  y))
define_bit_3op(orc2,     ( x | ~y))
define_bit_3op(xor,      ( x ^  y))

// bit_not
Val bit_not(Val v1, Val v2, Val s1, Val s2, Val n)
{
          BitEltT* p1 = v1->Decode<SimpleBitVector>()->GetElements();
    const BitEltT* p2 = v2->Decode<SimpleBitVector>()->GetElements();

    uint nS1 = static_cast<uint>(Fixnum::Decode_(s1));
    uint nS2 = static_cast<uint>(Fixnum::Decode_(s2));
    uint nN  = static_cast<uint>(Fixnum::Decode_(n));

    uint nE1 = nS1 + nN;
    uint nE2 = nS2 + nN;

    if (v1 == v2 && is_overlap(nS1, nE1, nE2))
    {
        BitWriterBackward o1(p1, nS1, nE1);
        BitReaderBackward o2(p2, nS2, nE2);

        while (! o2.AtEnd()) o1.Write(~o2.Read());
    }
    else
    {
        BitWriterForward o1(p1, nS1, nE1);
        BitReaderForward o2(p2, nS2, nE2);

        while (! o2.AtEnd()) o1.Write(~o2.Read());
    }

    return v1;
} // bit_not

// bit_replace
//  Replaces v1[s1..s1+n] with v2[s2..s2+n]
void bit_replace(Val v1, Val v2, uint nS1, uint nS2, uint nN)
{
          BitEltT* p1 = v1->Decode<SimpleBitVector>()->GetElements();
    const BitEltT* p2 = v2->Decode<SimpleBitVector>()->GetElements();

    uint nE1 = nS1 + nN;
    uint nE2 = nS2 + nN;

    if (v1 == v2 && (nE2 > nS1 && nE2 < nE1))
    {
        //  p1  -----o==========o
        //  p2  o==========o----
        BitWriterBackward o1(p1, nS1, nE1);
        BitReaderBackward o2(p2, nS2, nE2);
        while (! o2.AtEnd()) { o1.Write(o2.Read()); }
    }
    else
    {
        // nS1 <= nS2
        //  p1  o==========o--------------
        //  p2  ---------------o==========o
        //
        // nS1 >= nE2
        //  p1  ---------------o==========o
        //  p2  o==========o--------------
        BitWriterForward o1(p1, nS1, nE1);
        BitReaderForward o2(p2, nS2, nE2);
        while (! o2.AtEnd()) { o1.Write(o2.Read()); }
    }
} // bit_replace


} // MiniLisp
