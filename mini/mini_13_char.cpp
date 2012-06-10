#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 13 Characters
// mini/mini_13_char.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_13_char.cpp#3 $
//
#include "./mini_lisp.h"

namespace CommonLisp
{
// char_downcase
Val char_downcase(Val ch)
{
    WCHAR wchChar = Character::ToCode(ch);

    wchChar = reinterpret_cast<WCHAR>(
        ::CharLower(reinterpret_cast<LPWSTR>(wchChar)) );

    return Character::Encode(wchChar);
} // char_downcase


// char_upcase
Val char_upcase(Val ch)
{
    WCHAR wchChar = Character::ToCode(ch);

    wchChar = reinterpret_cast<WCHAR>(
        ::CharUpper(reinterpret_cast<LPWSTR>(wchChar)) );

    return Character::Encode(wchChar);
} // char_upcase

} // CommonLisp
