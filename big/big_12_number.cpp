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
// @(#)$Id: //proj/evcl3/mainline/big/big_12_number.cpp#7 $
//
#include "./big_lisp.h"

#include "./big_12_bignum.h"
#include "./big_12_complex.h"
#include "./big_12_float.h"
#include "./big_12_ratio.h"

namespace MiniLisp
{

namespace
{

#define dcomplex    double_float_complex
#define rcomplex    rational_complex
#define scomplex    single_float_complex

#define case_complex(mp_1) \
    case CLASSD2_(mp_1, dcomplex): \
        { StackDoubleFloatComplex oX(x); \
            return T::ComputeDoubleFloatComplex(oX, y); } \
    case CLASSD2_(mp_1, rcomplex): \
        { StackRationalComplex oX(x); \
            return T::ComputeRationalComplex(oX, y); } \
    case CLASSD2_(mp_1, scomplex): \
        { StackSingleFloatComplex oX(x); \
            return T::ComputeSingleFloatComplex(oX, y); }

#define case_float(mp_1) \
    case CLASSD2_(mp_1, double_float): \
        { StackDoubleFloat oX(x); return T::ComputeDoubleFloat(oX, y); } \
    case CLASSD2_(mp_1, single_float): \
        { StackSingleFloat oX(x); return T::ComputeSingleFloat(oX, y); }


template<class T>
class Arith
{
    public: static Val Run(Val x, Val y)
    {
        Int nTypes = get_type_2(x, y, CLASSD_number_max, Qnumber);

        switch (nTypes)
        {
        ////////////////////////////////////////////////////////////
        //
        // [1] fixnum
        //
        case CLASSD2_(fixnum, fixnum):  // [1][1]   1
        {
            BignumInt oX(Fixnum::Decode_(x));
            BignumInt oY(Fixnum::Decode_(y));
            return T::ComputeBignum(oX, oY);
        } // fixnum fixnum

        case CLASSD2_(fixnum, bignum):  // [1][2]   2
        {
            BignumInt oX(Fixnum::Decode_(x));
            return T::ComputeBignum(oX, y);
        } // fixnum bignum

        case_float(fixnum)
        case_complex(fixnum)

        case CLASSD2_(fixnum, ratio):   // [1][3]   3
        {
            StackRatio oX(x);
            return T::ComputeRatio(oX, y);
        } // fixnum RatioImpl

        ////////////////////////////////////////////////////////////
        //
        // [2] bignum
        //
        case CLASSD2_(bignum, fixnum):
        {
            BignumInt oY(Fixnum::Decode_(y));
            return T::ComputeBignum(x, oY);
        } // bignum bignum

        case CLASSD2_(bignum, bignum):
            return T::ComputeBignum(x, y);

        case_float(bignum)
        case_complex(bignum)

        case CLASSD2_(bignum, ratio):
        {
            StackRatio oX(x);
            return T::ComputeRatio(oX, y);
        } // bignum RatioImpl

        ////////////////////////////////////////////////////////////
        //
        // [3] RatioImpl
        //
        case CLASSD2_(ratio, fixnum):
        case CLASSD2_(ratio, bignum):
        {
            StackRatio oY(y);
            return T::ComputeRatio(x, oY);
        } // RatioImpl RatioImpl

        case_float(ratio)
        case_complex(ratio)

        case CLASSD2_(ratio, ratio):
            return T::ComputeRatio(x, y);

        ////////////////////////////////////////////////////////////
        //
        // [4] double float
        //
        case CLASSD2_(double_float, fixnum):        // [1] fixnum
        case CLASSD2_(double_float, bignum):        // [2] bignum
        case CLASSD2_(double_float, ratio):         // [3] RatioImpl
        case CLASSD2_(double_float, single_float):  // [5] single-float
        {
            StackDoubleFloat oY(y);
            return T::ComputeDoubleFloat(x, oY);
        } // double_float real

        case CLASSD2_(double_float, double_float):  // [4] double-float
            return T::ComputeDoubleFloat(x, y);

        case CLASSD2_(double_float, dcomplex):  // [6]
        {
            StackDoubleFloatComplex oX(x);
            return T::ComputeDoubleFloatComplex(oX, y);
        } // complex double-float

        case CLASSD2_(double_float, rcomplex):  // [7]
        {
            StackDoubleFloatComplex oX(x);
            StackDoubleFloatComplex oY(
                y->Decode<RationalComplex>()->m_real,
                y->Decode<RationalComplex>()->m_imag );
            return T::ComputeDoubleFloatComplex(oX, oY);
        } // complex double-float

        case CLASSD2_(double_float, scomplex):  // [8]
        {
            StackDoubleFloatComplex oX(x);
            StackDoubleFloatComplex oY(
                y->Decode<SingleFloatComplex>()->m_fltReal,
                y->Decode<SingleFloatComplex>()->m_fltImag );
            return T::ComputeDoubleFloatComplex(oX, oY);
        } // complex double-float

        ////////////////////////////////////////////////////////////
        //
        // [5] single float
        //
        case CLASSD2_(single_float, fixnum):        // [1] fixnum
        case CLASSD2_(single_float, bignum):        // [2] bignum
        case CLASSD2_(single_float, ratio):         // [3] RatioImpl
        {
            StackSingleFloat oY(y);
            return T::ComputeSingleFloat(x, oY);
        } // single_float real

        case CLASSD2_(single_float, double_float):  // [4] double-float
        {
            StackDoubleFloat oX(x);
            return T::ComputeDoubleFloat(oX, y);
        } // double_float real

        case CLASSD2_(single_float, single_float):  // [5] single-float
            return T::ComputeSingleFloat(x, y);

        case CLASSD2_(single_float, dcomplex):      // [6]
        {
            StackDoubleFloatComplex oX(x);
            return T::ComputeDoubleFloatComplex(oX, y);
        } // complex double-float

        case CLASSD2_(single_float, rcomplex):      // [7]
        {
            StackSingleFloatComplex oX(x);
            StackSingleFloatComplex oY(
                y->Decode<RationalComplex>()->m_real,
                y->Decode<RationalComplex>()->m_imag );
            return T::ComputeSingleFloatComplex(oX, oY);
        } // complex single-float

        case CLASSD2_(single_float, scomplex):      // [8]
        {
            StackSingleFloatComplex oX(x);
            return T::ComputeSingleFloatComplex(oX, y);
        } // complex single-float

        ////////////////////////////////////////////////////////////
        //
        // [6] double float complex
        //
        case CLASSD2_(dcomplex, fixnum):            // [1]
        case CLASSD2_(dcomplex, bignum):            // [2]
        case CLASSD2_(dcomplex, ratio):             // [3]
        case CLASSD2_(dcomplex, double_float):      // [4]
        case CLASSD2_(dcomplex, single_float):      // [5]
        {
            StackDoubleFloatComplex oY(y);
            return T::ComputeDoubleFloatComplex(x, oY);
        } // dcomplex real

        case CLASSD2_(dcomplex, rcomplex):          // [7]
        {
            StackDoubleFloatComplex oY(
                y->Decode<RationalComplex>()->m_real,
                y->Decode<RationalComplex>()->m_imag );
            return T::ComputeDoubleFloatComplex(x, oY);
        } // dcomplex rcomplex

        case CLASSD2_(dcomplex, scomplex):          // [8]
        {
            StackDoubleFloatComplex oY(
                y->Decode<SingleFloatComplex>()->m_fltReal,
                y->Decode<SingleFloatComplex>()->m_fltImag );
            return T::ComputeDoubleFloatComplex(x, oY);
        } // dcomplex scomplex

        case CLASSD2_(dcomplex, dcomplex):
            return T::ComputeDoubleFloatComplex(x, y);

        ////////////////////////////////////////////////////////////
        //
        // [7] rational complex
        //
        case CLASSD2_(rcomplex, fixnum):    // [1]
        case CLASSD2_(rcomplex, bignum):    // [2]
        case CLASSD2_(rcomplex, ratio):     // [3]
        {
            StackRationalComplex oY(y);
            return T::ComputeRationalComplex(x, oY);
        } // complex fixnum

        case CLASSD2_(rcomplex, double_float):  // [4]
        {
            StackDoubleFloatComplex oX(
                x->Decode<RationalComplex>()->m_real,
                x->Decode<RationalComplex>()->m_imag );
            StackDoubleFloatComplex oY(y);
            return T::ComputeDoubleFloatComplex(oX, oY);
        } // complex double-float

        case CLASSD2_(rcomplex, single_float):  // [5]
        {
            StackSingleFloatComplex oX(
                x->Decode<RationalComplex>()->m_real,
                x->Decode<RationalComplex>()->m_imag );
            StackSingleFloatComplex oY(y);
            return T::ComputeSingleFloatComplex(oX, oY);
        } // complex single-float

        case CLASSD2_(rcomplex, dcomplex):  // [6]
        {
            StackDoubleFloatComplex oX(
                x->Decode<RationalComplex>()->m_real,
                x->Decode<RationalComplex>()->m_imag );
            return T::ComputeDoubleFloatComplex(oX, y);
        } // rcomplex dcomplex

        case CLASSD2_(rcomplex, rcomplex):  // [7]
            return T::ComputeRationalComplex(x, y);

        case CLASSD2_(rcomplex, scomplex):  // [6]
        {
            StackSingleFloatComplex oX(
                x->Decode<RationalComplex>()->m_real,
                x->Decode<RationalComplex>()->m_imag );
            return T::ComputeSingleFloatComplex(oX, y);
        } // rcomplex dcomplex

        ////////////////////////////////////////////////////////////
        //
        // [8] single float complex
        //
        case CLASSD2_(scomplex, fixnum):        // [8][1]
        case CLASSD2_(scomplex, bignum):        // [8][2]
        case CLASSD2_(scomplex, ratio):         // [8][3]
        {
            StackSingleFloatComplex oY(y);
            return T::ComputeSingleFloatComplex(x, oY);
        } // complex fixnum

        case CLASSD2_(scomplex, single_float):  // [8][5]
        {
            StackSingleFloatComplex oY(y);
            return T::ComputeSingleFloatComplex(x, oY);
        } // complex double-float

        case CLASSD2_(scomplex, double_float):  // [8][4]
        {
            StackDoubleFloatComplex oX(
                x->Decode<SingleFloatComplex>()->m_fltReal,
                x->Decode<SingleFloatComplex>()->m_fltImag );
            StackDoubleFloatComplex oY(y);
            return T::ComputeDoubleFloatComplex(oX, oY);
        } // complex double-float

        case CLASSD2_(scomplex, dcomplex):      // [8][6]
        {
            StackDoubleFloatComplex oX(
                x->Decode<SingleFloatComplex>()->m_fltReal,
                x->Decode<SingleFloatComplex>()->m_fltImag );
            return T::ComputeDoubleFloatComplex(oX, y);
        } // rcomplex dcomplex

        case CLASSD2_(scomplex, rcomplex):      // [8][7]
        {
            StackSingleFloatComplex oY(
                y->Decode<RationalComplex>()->m_real,
                y->Decode<RationalComplex>()->m_imag );
            return T::ComputeSingleFloatComplex(x, oY);
        } // rcomplex dcomplex

        case CLASSD2_(scomplex, scomplex):      // [8][8]
            return T::ComputeSingleFloatComplex(x, y);

        default:
            CAN_NOT_HAPPEN();
        } // switch nTypes
    } // Run
}; // Arith

} // namespace


//////////////////////////////////////////////////////////////////////
//
// OpXXX
//
#define defmethod_arith(mp_Type, mp_Name) \
    public: static Val Compute##mp_Type(Val x, Val y) \
        { return mp_Type##Impl::mp_Name(x, y); } \

#define defclass_arith(mp_Name) \
    class Op##mp_Name \
    { \
        defmethod_arith(Bignum, mp_Name) \
        defmethod_arith(Ratio, mp_Name) \
        defmethod_arith(DoubleFloat, mp_Name) \
        defmethod_arith(SingleFloat, mp_Name) \
        defmethod_arith(DoubleFloatComplex, mp_Name) \
        defmethod_arith(RationalComplex, mp_Name) \
        defmethod_arith(SingleFloatComplex, mp_Name) \
    }; // defclass_arith

#define defun_arith(mp_name, mp_Name) \
    namespace { defclass_arith(mp_Name) } \
    Val C_##mp_name(Val x, Val y) \
        { return Arith<Op##mp_Name>::Run(x, y); }

defun_arith(add, Add)
defun_arith(div, Div)
defun_arith(mul, Mul)
defun_arith(sub, Sub)


//////////////////////////////////////////////////////////////////////
//
// C_num_eq
//
bool C_num_eq(Val x, Val y)
{
    Int nType1 = get_type_1(x, CLASSD_number_max, Qnumber);
    Int nType2 = get_type_1(y, CLASSD_number_max, Qnumber);
    if (x == y) return true;

    switch (nType1)
    {
    case CLASSD_(double_float_complex):
        return DoubleFloatComplexImpl::Eq(x, y);

    case CLASSD_(rational_complex):
        return RationalComplexImpl::Eq(x, y);

    case CLASSD_(single_float_complex):
        return SingleFloatComplexImpl::Eq(x, y);
    } // switch type1

    switch (nType2)
    {
    case CLASSD_(double_float_complex):
        return DoubleFloatComplexImpl::Eq(y, x);

    case CLASSD_(rational_complex):
        return RationalComplexImpl::Eq(y, x);

    case CLASSD_(single_float_complex):
        return SingleFloatComplexImpl::Eq(y, x);
    } // switch type2

    return 0 == cmp(x, y);
} // C_num_eq


//////////////////////////////////////////////////////////////////////
//
// convert_to_float64
//
float64 convert_to_float64(Val x)
{
    switch (get_type_1(x, CLASSD_real_max, Qreal))
    {
    case CLASSD_(fixnum):
        return static_cast<double>(Fixnum::Decode_(x));

    case CLASSD_(bignum):
        return BignumImpl::ToFloat64(x);

    case CLASSD_(ratio):
        return RatioImpl::ToFloat64(x);

    case CLASSD_(double_float):
        return x->Decode<DoubleFloat>()->m_dbl;

    case CLASSD_(single_float):
        return x->Decode<SingleFloat>()->m_flt;

    default:
        CAN_NOT_HAPPEN();
    } // switch type
} // convert_to_float64


//////////////////////////////////////////////////////////////////////
//
// convert_to_float32
//
float32 convert_to_float32(Val x)
{
    switch (get_type_1(x, CLASSD_real_max, Qreal))
    {
    case CLASSD_(fixnum):
        return static_cast<float>(Fixnum::Decode_(x));

    case CLASSD_(bignum):
        return BignumImpl::ToFloat32(x);

    case CLASSD_(ratio):
        return RatioImpl::ToFloat32(x);

    case CLASSD_(double_float):
        return static_cast<float>(x->Decode<DoubleFloat>()->m_dbl);

    case CLASSD_(single_float):
        return x->Decode<SingleFloat>()->m_flt;

    default:
        CAN_NOT_HAPPEN();
    } // switch type
} // convert_to_float32


//////////////////////////////////////////////////////////////////////
//
// get_type_1
//  Returns CLASSD index of x or signals type error if x isn't
//  real/nummber specified by classd.
//
Int get_type_1(Val x, Val classd_max, Val expected_ty)
{
    if (fixnump(x))
    {
        return CLASSD_(fixnum);
    }
    else if (x->Is<Record>())
    {
        if (x->Decode<Record>()->m_classd >= CLASSD_bignum &&
            x->Decode<Record>()->m_classd <= classd_max)
        {
            return CLASSD_INDEX_OF(x->Decode<Record>()->m_classd);
        }
    }

    error(make_type_error(x, expected_ty));
} // get_type_1


//////////////////////////////////////////////////////////////////////
//
// get_type_2
//  Returns CLASSD indexs of x and y or signals type error if x or y isn't
//  real/nummber specified by classd.
//
Int get_type_2(Val x, Val y, Val classd_max, Val expected_ty)
{
    Int nTypes = get_type_1(x, classd_max, expected_ty) << 16;
    nTypes |= get_type_1(y, classd_max, expected_ty);
    return nTypes;
} // get_type_2


#define define_rtl(mp_name, mp_cname) \
    Val RTL_##mp_name(Val a, Val b) \
        { return MiniThread::Get()->GcFence(mp_cname(a, b)); }

define_rtl(add,     C_add)
define_rtl(ash,     C_ash)
define_rtl(div,     C_div)
define_rtl(gcd,     C_gcd)
define_rtl(mul,     C_mul)
define_rtl(sub,     C_sub)
define_rtl(logand,  logand_2)
define_rtl(logior,  logior_2)
define_rtl(logxor,  logxor_2)


Val C_neg(Val x)
{
    switch (get_type_1(x, CLASSD_number_max, Qnumber))
    {
    case CLASSD_(double_float):
    {
        Val y = DoubleFloatImpl::Make(x->Decode<DoubleFloatImpl>()->Get());
        DoubleFloat::Layout* p = const_cast<DoubleFloat::Layout*>(
            y->Decode<DoubleFloatImpl>()->GetLayout() );
        p->m_nSign = 1 - p->m_nSign;
        return y;
    } // double_float

    case CLASSD_(single_float):
    {
        Val y = SingleFloatImpl::Make(x->Decode<SingleFloatImpl>()->Get());
        SingleFloat::Layout* p = const_cast<SingleFloat::Layout*>(
            y->Decode<SingleFloatImpl>()->GetLayout() );
        p->m_nSign = 1 - p->m_nSign;
        return y;
    } // single_float

    default:
        return C_sub(Fixnum::Encode(0), x);
    } // switch type
} // C_neg

Val RTL_neg(Val a)
{ 
    return MiniThread::Get()->GcFence(C_neg(a));
} // RTL_neg

} // MiniLisp
