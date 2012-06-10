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
// @(#)$Id: //proj/evcl3/mainline/big/big_15_array.cpp#7 $
//
#include "./big_lisp.h"

namespace MiniLisp
{

void bit_replace(Val v1, Val v2, uint nS1, uint nS2, uint nN);

namespace
{

// data_vectorp - for check_type
bool data_vectorp(Val x)
{
    if (! x->Is<Record>()) return false;
    Val classd = x->Decode<ClassD>()->m_classd;
    return classd >= CLASSD_data_vector_min &&
           classd <= CLASSD_data_vector_max;
} // si_data_vectorp

} // namespace

// Syntax:
//  .replace-vector vector-1 vector-2 &optional start-1 end-1 start-2 end-2
//      => vector-1
//
// Arguments and Values:
//  vector-1    specialized vector or simple-vector.
//  vector-2    vector of same type of vector-1
//
// For:
//  bit-and, bit-andc1, ... bit-xor
//  replace
//
Val replace_vector(Val v1, Val v2, Val s1, Val e1, Val s2, Val e2)
{
    check_type(v1, data_vector);

    Val classd = v1->Decode<Record>()->m_classd;

    if (v2->Is<Record>() && v2->Decode<Record>()->m_classd != classd)
    {
        error(make_type_error(v2, type_of(v1)));
    }

    if (! fixnump(s1) || minusp_xx(s1) || cmp_xx(s1, length(v1)) > 0)
    {
        error(Qbounding_index_error, Kdatum, s1, Ksequence, v1);
    }

    if (nil == e1) e1 = length(v1);
    if (! fixnump(e1) || cmp_xx(s1, e1) > 0 && cmp_xx(e1, length(v1)) > 0)
    {
        error(Qbounding_index_error,
            Ksequence, v1,
            Kstart,    s1,
            Kdatum,    e1 );
    }

    if (! fixnump(s2) || minusp_xx(s2) || cmp_xx(s2, length(v2)) > 0)
    {
        error(Qbounding_index_error, Kdatum, s2, Ksequence, v2);
    }

    if (nil == e2) e2 = length(v1);
    if (! fixnump(e2) || cmp_xx(s2, e2) > 0 && cmp_xx(e2, length(v2)) > 0)
    {
        error(Qbounding_index_error,
            Ksequence, v2,
            Kstart,    s2,
            Kdatum,    e2 );
    }

    Int iS1 = Fixnum::Decode_(s1);
    Int iS2 = Fixnum::Decode_(s2);

    Int n1 = Fixnum::Decode_(e1) - iS1;
    Int n2 = Fixnum::Decode_(e2) - iS2;
    Int n = min(n1, n2);
    if (0 == n) return v1;

    void* pv1 = reinterpret_cast<void*>(v1->Decode<DataVector>() + 1);
    void* pv2 = reinterpret_cast<void*>(v2->Decode<DataVector>() + 1);

    switch (classd->Decode<ClassD>()->m_format_param->ToInt())
    {
    case Fixnum::One * 1:
        bit_replace(v1, v2, 
            static_cast<uint>(iS1), static_cast<uint>(iS2),
            static_cast<uint>(n) );
        break;

    case Fixnum::One * 8:
        evcl_memmove(
            reinterpret_cast<uint8*>(pv1) + iS1,
            reinterpret_cast<uint8*>(pv2) + iS2,
            n );
        break;

    case Fixnum::One * 16:
        evcl_memmove(
            reinterpret_cast<uint16*>(pv1) + iS1,
            reinterpret_cast<uint16*>(pv2) + iS2,
            n * 2 );
        break;

    case Fixnum::One * 32:
        evcl_memmove(
            reinterpret_cast<uint32*>(pv1) + iS1,
            reinterpret_cast<uint32*>(pv2) + iS2,
            n * 4 );
        break;

    case Fixnum::One * 64:
        evcl_memmove(
            reinterpret_cast<uint64*>(pv1) + iS1,
            reinterpret_cast<uint64*>(pv2) + iS2,
            n * 8 );
        break;

    case Fixnum::One * 128:
        evcl_memmove(
            reinterpret_cast<uint64*>(pv1) + iS1 * 2,
            reinterpret_cast<uint64*>(pv2) + iS2 * 2,
            n * 16 );
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch classd

    return v1;
} // replace_vector

} // MiniLisp
