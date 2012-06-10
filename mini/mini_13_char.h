//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 13 Characters
// mini/mini_13_char.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_13_char.h#2 $
//
#if !defined(INCLUDE_mini_13_char_h)
#define INCLUDE_mini_13_char_h

namespace CommonLisp
{
    Val char_upcase(Val);
    Val char_downcase(Val);
    Val digit_char(Val, Val);
    Val digit_char_p(Val, Val);

    inline bool characterp(Val x)
        { return x->Is<Character>(); }
} // CommonLisp

#endif //!defined(INCLUDE_mini_13_char_h)
