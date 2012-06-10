//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - pre-compiled header
// mini_12_number.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_12_number.h#2 $
//
#if !defined(INCLUDE_mini_12_number_h)
#define INCLUDE_mini_12_number_h

namespace CommonLisp
{
    bool complexp(Val);
    bool floatp(Val);
    bool integerp(Val);
    bool rationalp(Val);
    bool realp(Val);
    bool numberp(Val);
} // CommonLisp

namespace MiniLisp
{
    bool fixnump(Val);

    Int cmp_xx(Val, Val);

    Val add_xx(Val, Val);
    Val mul_xx(Val, Val);
    Val sub_xx(Val, Val);
    Val truncate_xx(Val, Val);

    Int cmp_xx(Val, Int);
    Val add_xx(Val, Int);
    Val mul_xx(Val, Int);
    Val sub_xx(Val, Int);
    Val truncate_xx(Val, Int);
} // MiniLisp

#endif //!defined(INCLUDE_mini_12_number_h)
