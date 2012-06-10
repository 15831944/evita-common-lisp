//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - pre-compiled header
// big_lisp.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_lisp.h#5 $
//
#if !defined(INCLUDE_big_lisp_h)
#define INCLUDE_big_lisp_h

#include "../mini/mini_lisp.h"

#include "./big_12_number.h"
#include "./big_21_stream.h"

namespace MiniLisp
{
    // 15 Arrays
    Val replace_vector(Val, Val, Val, Val, Val, Val);

    #define keyword_argument(mp_name, mp_init) \
        { &mp_name, K##mp_name, mp_init }

    struct KeyArg { Val* m_pval; Val m_key; Val m_init; };

    void parse_keys(Int iStart, KeyArg* prgoKey, uint cKeys);
} // MiniLisp

namespace CommonLisp
{
    // 13 Characters2
    Val name_char(Val);

    inline Val digit_char_p(Val ch)
        { return digit_char_p(ch, TLV(Aread_baseA)); }

    inline Val digit_char_p(Val ch, Int iBase)
        { return digit_char_p(ch, Fixnum::Encode(iBase)); }

    inline Val digit_char_p(char16 wch, Int iBase)
        { return digit_char_p(Character::Encode(wch), Fixnum::Encode(iBase)); }
} // CommonLisp

#endif //!defined(INCLUDE_big_lisp_h)
