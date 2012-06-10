#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 5 Data and Control Flow
// gs_05_ctrl.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_12_number.cpp#6 $
//
//      1+
//      1-      -- for tak
//      >=      -- for tak
//
#include "./gs_lisp.h"

#include "../big/big_12_complex.h"
#include "../big/big_12_float.h"

namespace Genesis
{

// expand_decf
Val expand_decf(Val form, Val)
{
    CHECK_SYNTAX(form, 2, 3, "(decf place [delta])");
    Val delta = third(form);
    if (nil == delta) delta = Fixnum::Encode(1);
    Val place = second(form);
    return list(Qsetq, place, list(Q_, place, delta));
} // expand_decf


// expand_incf
Val expand_incf(Val form, Val)
{
    CHECK_SYNTAX(form, 2, 3, "(incf place [delta])");
    Val delta = third(form);
    if (nil == delta) delta = Fixnum::Encode(1);
    Val place = second(form);
    return list(Qsetq, place, list(QP, place, delta));
} // expand_incf


// op_1P = 1+
Val op_1P(Val x)
{
    return add(x, 1);
} // op_1P


// op_1_1 = 1-
Val op_1_(Val x)
{
    return sub(x, 1);
} // op_1_

// c_op_GE
Val C_op_GE(MiniThread* p)
{
    Int n = Fixnum::Decode_(p->m_n);
    ASSERT(n >= 1);
    Val x = p->mv_value[0];
    for (Int i = 1; i < n; i++)
    {
        if (cmp(x, p->mv_value[i]) < 0)
        {
            return nil;
        }
        x = p->mv_value[i];
    } // for i
    return t;
} // c_op_GE

} // Genesis



namespace CommonLisp
{

// complex
Val complex(Val x, Val y)
{
    Int nTypes = get_type_2(x, y, CLASSD_real_max, Qreal);
    switch (nTypes)
    {
    case CLASSD2_(fixnum, fixnum):              // [1][1]   1
    case CLASSD2_(fixnum, bignum):              // [1][2]   2
    case CLASSD2_(fixnum, ratio):               // [1][3]   3
    case CLASSD2_(bignum, fixnum):              // [2][1]   4
    case CLASSD2_(bignum, bignum):              // [2][2]   5
    case CLASSD2_(bignum, ratio):               // [2][3]   6
    case CLASSD2_(ratio,  fixnum):              // [3][1]   7
    case CLASSD2_(ratio,  bignum):              // [3][2]   8
    case CLASSD2_(ratio,  ratio):               // [3][3]   9
        return RationalComplexImpl::Make(x, y);

    case CLASSD2_(fixnum,       double_float):  // [1][4]   10
    case CLASSD2_(bignum,       double_float):  // [2][4]   11
    case CLASSD2_(ratio,        double_float):  // [3][4]   12
    case CLASSD2_(single_float, double_float):  // [5][4]   13
    case CLASSD2_(double_float, double_float):  // [4][4]   14
    case CLASSD2_(double_float, fixnum):        // [4][1]   15
    case CLASSD2_(double_float, bignum):        // [4][2]   16
    case CLASSD2_(double_float, ratio):         // [4][3]   17
    case CLASSD2_(double_float, single_float):  // [4][5]   18
        return DoubleFloatComplexImpl::Make(x, y);

    case CLASSD2_(fixnum,       single_float):  // [1][5]   19
    case CLASSD2_(bignum,       single_float):  // [2][5]   20
    case CLASSD2_(ratio,        single_float):  // [3][5]   21
    case CLASSD2_(single_float, single_float):  // [5][5]   22
    case CLASSD2_(single_float, fixnum):        // [4][1]   23
    case CLASSD2_(single_float, bignum):        // [4][2]   24
    case CLASSD2_(single_float, ratio):         // [4][3]   25
        return SingleFloatComplexImpl::Make(x, y);

    default:
        CAN_NOT_HAPPEN();
    } // switch nTypes
} // complex

} // CommonLisp
