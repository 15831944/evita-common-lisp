#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 12 Numbers - Complex
// big/big_12_complex.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_12_complex.cpp#6 $
//
#include "./big_lisp.h"

#include "./big_12_complex.h"
#include "./big_12_float.h"

namespace MiniLisp
{

Val print_double_float(Val, Val);
Val print_single_float(Val, Val);


// print_double_float_complex
Val print_double_float_complex(Val obj, Val stream)
{
    write_string(L"#c(", stream);
    {
        StackDoubleFloat oX(obj->Decode<DoubleFloatComplex>()->m_dblReal);
        print_double_float(oX, stream);
    }
    write_char(' ', stream);
    {
        StackDoubleFloat oX(obj->Decode<DoubleFloatComplex>()->m_dblImag);
        print_double_float(oX, stream);
    }
    write_string(L")", stream);
    return obj;
} // print_double_float_complex


// print_single_float_complex
Val print_single_float_complex(Val obj, Val stream)
{
    write_string(L"#c(", stream);
    {
        StackSingleFloat oX(obj->Decode<SingleFloatComplex>()->m_fltReal);
        print_single_float(oX, stream);
    }
    write_char(' ', stream);
    {
        StackSingleFloat oX(obj->Decode<SingleFloatComplex>()->m_fltImag);
        print_single_float(oX, stream);
    }
    write_string(L")", stream);
    return obj;
} // print_single_float_complex


//////////////////////////////////////////////////////////////////////
//
// RationalComplex::Eq
//
bool RationalComplexImpl::Eq(Val x, Val y)
{
    ASSERT(x->Is<RationalComplex>());

    switch (get_type_1(y, CLASSD_number_max, Qnumber))
    {
    case CLASSD_(fixnum):                   // [1]
    case CLASSD_(bignum):                   // [2]
    case CLASSD_(ratio):                    // [3]
    case CLASSD_(double_float):             // [4]
    case CLASSD_(single_float):             // [5]
        return false;

    case CLASSD_(double_float_complex):  // [6]
        return
            0 == cmp(x->Decode<RationalComplex>()->m_real,
                     rational(
                        y->Decode<DoubleFloatComplex>()->m_dblReal )) &&
            0 == cmp(x->Decode<RationalComplex>()->m_imag,
                     rational(
                        y->Decode<DoubleFloatComplex>()->m_dblImag ));

    case CLASSD_(rational_complex):      // [7]
        return
            0 == cmp(x->Decode<RationalComplex>()->m_real,
                     y->Decode<RationalComplex>()->m_real ) &&
            0 == cmp(x->Decode<RationalComplex>()->m_imag,
                     y->Decode<RationalComplex>()->m_imag );

    case CLASSD_(single_float_complex):  // [8]
        return
            0 == cmp(x->Decode<RationalComplex>()->m_real,
                     rational(
                        y->Decode<SingleFloatComplex>()->m_fltReal )) &&
            0 == cmp(x->Decode<RationalComplex>()->m_imag,
                     rational(
                        y->Decode<SingleFloatComplex>()->m_fltImag ));

    default:
        CAN_NOT_HAPPEN();
    } // switch type
} // RationalComplexImpl::Eq

} // MiniLisp
