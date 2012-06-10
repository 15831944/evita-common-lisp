#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 12 Numbers - Real
// big/big_12_real.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_12_real.cpp#6 $
//
#include "./big_lisp.h"

#include "./big_12_bignum.h"
#include "./big_12_float.h"
#include "./big_12_ratio.h"


namespace MiniLisp
{

namespace
{

template<class T>
class Arith_
{
    public: static void Run(Val x, Val y, Val* out_q, Val* out_r)
    {
        Int nTypes = get_type_2(x, y, CLASSD_real_max, Qreal);

        switch (nTypes)
        {
        ////////////////////////////////////////////////////////////
        //
        // [1] fixnum
        //
        case CLASSD2_(fixnum, fixnum):              // [1][1]   1
            return T::ComputeFixnum(x, y, out_q, out_r);

        case CLASSD2_(fixnum, bignum):              // [1][2]   2
        {
            BignumInt oX(Fixnum::Decode_(x));
            return T::ComputeBignum(oX, y, out_q, out_r);
        } // fixnum bignum

        case CLASSD2_(fixnum, ratio):               // [1][3]   3
        case CLASSD2_(bignum, ratio):               // [2][3]   4
        {
            StackRatio oX(x);
            return T::ComputeRatio(oX, y, out_q, out_r);
        } // fixnum ratio

        case CLASSD2_(fixnum,       double_float):  // [1][4]   5
        case CLASSD2_(bignum,       double_float):  // [2][4]   6
        case CLASSD2_(ratio,        double_float):  // [3][4]   7
        case CLASSD2_(single_float, double_float):  // [5][4]   8
        {
            StackDoubleFloat oX(x);
            return T::ComputeDoubleFloat(oX, y, out_q, out_r);
        } // fixnum double-float

        case CLASSD2_(fixnum, single_float):        // [1][5]   9
        case CLASSD2_(bignum, single_float):        // [2][5]   10
        case CLASSD2_(ratio,  single_float):        // [3][5]   11
        {
            StackSingleFloat oX(x);
            return T::ComputeSingleFloat(oX, y, out_q, out_r);
        } // fixnum single-float

        ////////////////////////////////////////////////////////////
        //
        // [2] bignum
        //
        case CLASSD2_(bignum, fixnum):              // [1][2]   12
        {
            BignumInt oY(Fixnum::Decode_(y));
            return T::ComputeBignum(x, oY, out_q, out_r);
        } // bignum fixnum

        case CLASSD2_(bignum, bignum):              // [2][2]   13
            return T::ComputeBignum(x, y, out_q, out_r);

        ////////////////////////////////////////////////////////////
        //
        // [3] ratio
        //
        case CLASSD2_(ratio, fixnum):               // [3][1]   14
        case CLASSD2_(ratio, bignum):               // [3][2]   15
        {
            StackRatio oY(y);
            return T::ComputeRatio(x, oY, out_q, out_r);
        } // fixnum ratio

        case CLASSD2_(ratio, ratio):                // [3][3]   16
            return T::ComputeRatio(x, y, out_q, out_r);

        ////////////////////////////////////////////////////////////
        //
        // [4] double-float
        //
        case CLASSD2_(double_float, fixnum):        // [4][1]   17
        case CLASSD2_(double_float, bignum):        // [4][2]   18
        case CLASSD2_(double_float, ratio):         // [4][3]   19
        case CLASSD2_(double_float, single_float):  // [4][5]   20
        {
            StackDoubleFloat oY(y);
            return T::ComputeDoubleFloat(x, oY, out_q, out_r);
        } // double_float

        case CLASSD2_(double_float, double_float):  // [4][4]   21
            return T::ComputeDoubleFloat(x, y, out_q, out_r);

        ////////////////////////////////////////////////////////////
        //
        // [5] single-float
        //
        case CLASSD2_(single_float, fixnum):          // [5][1] 22
        case CLASSD2_(single_float, bignum):          // [5][2] 23
        case CLASSD2_(single_float, ratio):           // [5][3] 24
        {
            StackSingleFloat oY(y);
            return T::ComputeSingleFloat(x, oY, out_q, out_r);
        } // single_float

        case CLASSD2_(single_float, single_float):    // [5][5] 25
            return T::ComputeSingleFloat(x, y, out_q, out_r);

        default:
            CAN_NOT_HAPPEN();
        } // switch nTypes
    } // Run
}; // Arith_

} // namespace


//////////////////////////////////////////////////////////////////////
//
// C_cmp
//
Int C_cmp(Val x, Val y)
{
    Int nTypes = get_type_2(x, y, CLASSD_real_max, Qreal);

    if (x == y)
    {
        return 0;
    }

    switch (nTypes)
    {
    ////////////////////////////////////////////////////////////
    //
    // [1] fixnum
    //
    case CLASSD2_(fixnum, fixnum):          // [1][1]   1
        return Fixnum::Decode_(x) - Fixnum::Decode_(y);

    case CLASSD2_(fixnum, bignum):          // [1][2]   2
    {
        BignumInt oX(Fixnum::Decode_(x));
        return BignumImpl::Cmp(oX, y);
    } // fixnum bignum

    case CLASSD2_(fixnum, ratio):           // [1][3]   3
    case CLASSD2_(bignum, ratio):           // [2][3]   4
    {
        StackRatio oX(x);
        return RatioImpl::Cmp(oX, y);
    } // fixnum ratio

    case CLASSD2_(fixnum, double_float):    // [1][4]   5
    case CLASSD2_(bignum, double_float):    // [2][4]   6
    case CLASSD2_(ratio, double_float):     // [3][4]   7
        return cmp(x, DoubleFloatImpl::Rational(y));

    case CLASSD2_(fixnum, single_float):    // [1][5]   8
    case CLASSD2_(bignum, single_float):    // [2][5]   9
    case CLASSD2_(ratio,  single_float):    // [3][5]   10
        return cmp(x, SingleFloatImpl::Rational(y));

    ////////////////////////////////////////////////////////////
    //
    // [2] bignum
    //
    case CLASSD2_(bignum, fixnum):          // [2][1]   11
    {
        BignumInt oY(Fixnum::Decode_(y));
        return BignumImpl::Cmp(x, oY);
    } // bignum fixnum

    case CLASSD2_(bignum, bignum):          // [2][2]   12
        return BignumImpl::Cmp(x, y);

    ////////////////////////////////////////////////////////////
    //
    // [3] ratio
    //
    case CLASSD2_(ratio, fixnum):           // [3][1]   13
    case CLASSD2_(ratio, bignum):           // [3][2]   14
    {
        StackRatio oY(y);
        return RatioImpl::Cmp(x, oY);
    } // fixnum ratio

    case CLASSD2_(ratio, ratio):            // [3][3]   15
        return RatioImpl::Cmp(x, y);

    ////////////////////////////////////////////////////////////
    //
    // [4] double-float
    //
    case CLASSD2_(double_float, fixnum):        // [4][1]   16
    case CLASSD2_(double_float, bignum):        // [4][2]   17
    case CLASSD2_(double_float, ratio):         // [4][3]   18
        return cmp(DoubleFloatImpl::Rational(x), y);

    case CLASSD2_(double_float, double_float):  // [4][4]   19
    {
        double dblX = x->Decode<DoubleFloat>()->m_dbl;
        double dblY = y->Decode<DoubleFloat>()->m_dbl;
        if (dblX > dblY) return 1;
        if (dblX < dblY) return -1;
        return 0;
    } // double_float double_float

    case CLASSD2_(double_float, single_float):  // [4][5]   20
    {
        double dblX = x->Decode<DoubleFloat>()->m_dbl;
        double dblY = y->Decode<SingleFloat>()->m_flt;
        if (dblX > dblY) return 1;
        if (dblX < dblY) return -1;
        return 0;
    } // double_float single_float

    ////////////////////////////////////////////////////////////
    //
    // [5] single-float
    //
    case CLASSD2_(single_float, fixnum):        // [5][1]   21
    case CLASSD2_(single_float, bignum):        // [5][2]   22
    case CLASSD2_(single_float, ratio):         // [5][3]   23
        return cmp(SingleFloatImpl::Rational(x), y);

    case CLASSD2_(single_float, double_float):  // [5][4]   24
    {
        double dblX = x->Decode<SingleFloat>()->m_flt;
        double dblY = y->Decode<DoubleFloat>()->m_dbl;
        if (dblX > dblY) return 1;
        if (dblX < dblY) return -1;
        return 0;
    } // double_float double_float

    case CLASSD2_(single_float, single_float):  // [5][5]   25
    {
        float fltX = x->Decode<SingleFloat>()->m_flt;
        float fltY = y->Decode<SingleFloat>()->m_flt;
        if (fltX > fltY) return 1;
        if (fltX < fltY) return -1;
        return 0;
    } // single_float single_float

    default:
        CAN_NOT_HAPPEN();
    } // switch nTypes
} // cmp


#define defmethod_div(mp_Type, mp_Name) \
    public: static void Compute##mp_Type( \
        Val x, Val y, Val* out_q, Val* out_r ) \
    { \
        mp_Type##Impl::mp_Name(x, y, out_q, out_r); \
    }

#define defclass_div(mp_Name) \
    class Op##mp_Name \
    { \
        public: static void ComputeFixnum( \
            Val x, Val y, Val* out_q, Val* out_r ) \
        { \
            BignumInt oX(Fixnum::Decode_(x)); \
            BignumInt oY(Fixnum::Decode_(y)); \
            BignumImpl::mp_Name(oX, oY, out_q, out_r); \
        }  \
        defmethod_div(Bignum, mp_Name) \
        defmethod_div(Ratio, mp_Name) \
        defmethod_div(DoubleFloat, mp_Name) \
        defmethod_div(SingleFloat, mp_Name) \
    }; // defclass_div

#define defun_div(mp_name, mp_Name) \
    namespace { defclass_div(mp_Name) } \
    void mp_name(Val x, Val y, Val* out_q, Val* out_r) \
        { Arith_<Op##mp_Name>::Run(x, y, out_q, out_r); }

} // MiniLisp

namespace CommonLisp
{

defun_div(truncate, Truncate)

// rem - for gcd
Val rem(Val x, Val y)
{
    Val r;
    truncate(x, y, NULL, &r);
    return r;
} // rem

} // CommonLisp
