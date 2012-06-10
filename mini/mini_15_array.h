//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 15 Arrays
// mini/mini_15_array.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_15_array.h#2 $
//
#if !defined(INCLUDE_mini_15_array_h)
#define INCLUDE_mini_15_array_h

namespace CommonLisp
{
    bool arrayp(Val);
    bool bit_vector_p(Val);
    bool simple_bit_vector_p(Val);
    bool simple_vector_p(Val);
    bool vectorp(Val);


    Val sbit(Val, Val);
    Val bit(Val, Val);

    Val svref(Val, Val);
    Val setf_svref(Val val, Val, Val);

    Val svref(Val, Int);
    Val setf_svref(Val val, Val, Int);
} // CommonLisp

namespace MiniLisp
{
    Val make_vector(Val);
    Val make_vector(Int);
    Val make_vector(Val, Val);
} // MiniLisp

#endif //!defined(INCLUDE_mini_15_array_h)
