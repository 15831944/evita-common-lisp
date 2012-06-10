#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 15 Arrays
// mini/mini_15_array.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_15_array.cpp#6 $
//
#include "./mini_lisp.h"

namespace MiniLisp
{

// make_vector
//  for string-output-stream
Val make_vector(Val classd, Val data)
{
    Val str = MiniThread::Get()->AllocRecord(classd);
    String* p = str->Decode<String>();
        p->m_fill_pointer = Fixnum::Encode(0);
        p->m_flags        = Fixnum::Encode(Array::Attr_FillPointer);
        p->m_displaced_to = data;
        p->m_offset       = Fixnum::Encode(0);
        p->m_total_size   = length(data);
    return str;
} // make_vector

// make_vector
Val make_vector(Val length)
{
    if (Fixnum::Encode(0) == length) return QQnull_vector;
    Val vec = MiniThread::Get()->AllocVector(CLASSD_simple_vector, length);
    return vec;
} // make_vector


} // MiniLisp


namespace CommonLisp
{

// arrayp
bool arrayp(Val x)
{
    if (! x->Is<Record>())
    {
        return false;
    }

    Int iClassD = CLASSD_INDEX_OF(x->Decode<Record>()->m_classd);

    return iClassD >= CLASSD_(array_min) && iClassD <= CLASSD_(array_max);
} // arrayp

// vectorp
bool vectorp(Val x)
{
    if (! x->Is<Record>())
    {
        return false;
    }

    Int iClassD = CLASSD_INDEX_OF(x->Decode<Record>()->m_classd);

    return iClassD >= CLASSD_(vector_min) && iClassD <= CLASSD_(vector_max);
} // vectorp

// bit
Val bit(Val bitvec, Val index)
{
    if (simple_bit_vector_p(bitvec))
    {
        return sbit(bitvec, index);
    }
    else if (bit_vector_p(bitvec))
    {
        const Vector* pRunner =
            bitvec->Decode<Vector>();

        if (cmp_xx(index, pRunner->m_total_size) >= 0)
        {
            error(Qvector_index_error,
                Kvector, bitvec,
                Kdatum, index );
        }

        for (;;)
        {
            index = add_xx(index, pRunner->m_offset);
            Val datavec = pRunner->m_displaced_to;
            if (simple_bit_vector_p(datavec))
            {
                return sbit(datavec, index);
            }

            if (! bit_vector_p(datavec))
            {
                error(make_type_error(bitvec, Qbit_vector));
            }

            pRunner = datavec->Decode<BitVector>();
        } // for
    } // if

    error(make_type_error(bitvec, Qbit_vector));
} // bit


// sbit
Val sbit(Val bitvec, Val index)
{
    check_type(bitvec, simple_bit_vector);

    const SimpleBitVector* pBitVec = bitvec->Decode<SimpleBitVector>();

    if (cmp_xx(index, pBitVec->m_length) >= 0)
    {
        error(Qvector_index_error, Kvector, bitvec, Kdatum, index);
    } // if

    const uint k_BITS = sizeof(Arch::BitEltT) * 8;
    Int iIndex    = Fixnum::Decode_(index);
    Int iWordPos  = iIndex / k_BITS;
    Int iBitPos   = iIndex % k_BITS;
    if (pBitVec->GetElements()[iWordPos] & Kernel::Arch::k_rgfBit[iBitPos])
    {
        return Fixnum::Encode(1);
    }
    else
    {
        return Fixnum::Encode(0);
    } // if
} // sbit

} // CommonLisp
