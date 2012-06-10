#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 12 Numbers
// big/big_12_number.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_18_hash_table.cpp#4 $
//
#include "./big_lisp.h"

namespace MiniLisp
{

namespace
{

static const Int MaxHashCode   = HashTable::MaxHashCode;

// sxhash_ float64
static Int sxhash_(float64 f)
{
    Float64_LayoutHL* p = reinterpret_cast<Float64_LayoutHL*>(&f);
    return (p->m_nHigh ^ p->m_nLow) % MaxHashCode;
} // sxhash_

// sxhash_ float32
static Int sxhash_(float32 f)
{
    uint32* p = reinterpret_cast<uint32*>(&f);
    return *p % MaxHashCode;
} // sxhash_

} // namespace

Val sxhash_eql(Val obj)
{
    if (obj->Is<Record>())
    {
        switch (CLASSD_INDEX_OF(obj->Decode<Record>()->m_classd))
        {
        case CLASSD_(bignum):
            return Fixnum::Encode(logxor(
                obj->Decode<Bignum>()->m_length,
                Fixnum::Encode(obj->Decode<Bignum>()->m_rgBigit[0] %
                MaxHashCode ) ) );

        case CLASSD_(double_float):
            return Fixnum::Encode(
                sxhash_(obj->Decode<DoubleFloat>()->m_dbl) );

        case CLASSD_(ratio):
            return logxor(
                sxhash_eql(obj->Decode<Ratio>()->m_num),
                sxhash_eql(obj->Decode<Ratio>()->m_den) );

        case CLASSD_(single_float):
            return Fixnum::Encode(
                sxhash_(obj->Decode<SingleFloat>()->m_flt) );

        case CLASSD_(double_float_complex):
            return Fixnum::Encode(
                sxhash_(obj->Decode<DoubleFloatComplex>()->m_dblReal) ^
                sxhash_(obj->Decode<DoubleFloatComplex>()->m_dblImag) );

        case CLASSD_(rational_complex):
            return logxor(
                sxhash_eql(obj->Decode<RationalComplex>()->m_real),
                sxhash_eql(obj->Decode<RationalComplex>()->m_imag) );

        case CLASSD_(single_float_complex):
            return Fixnum::Encode(
                sxhash_(obj->Decode<SingleFloatComplex>()->m_fltReal) ^
                sxhash_(obj->Decode<SingleFloatComplex>()->m_fltImag) );
        } // switch classd
    } // if

    return sxhash_eq(obj);
} // sxhash_eql

} // MiniLisp
