#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - big - 13 Characters
// big/big_13_char.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_13_char.cpp#3 $
//
#include "./big_lisp.h"

namespace CommonLisp
{

// digit_char
Val digit_char(Val n, Val baze)
{
    if (! fixnump(n) || minusp_xx(n))
    {
        error(make_type_error(
            n,
            list(Qinteger, Fixnum::Encode(0), QA) ));
    }

    if (! fixnump(baze) || cmp_xx(baze, 2) < 0 || cmp_xx(baze, 36) > 0)
    {
        error(make_type_error(
            baze,
            list(Qinteger, Fixnum::Encode(2), Fixnum::Encode(36)) ));
    }

    if (cmp_xx(n, baze) >= 0)
    {
        return nil;
    }

    if (cmp_xx(n, 10) < 0)
    {
        return Character::Encode(static_cast<char16>(
            Fixnum::Decode_(n) + '0' ));
    }
    else
    {
        return Character::Encode(static_cast<char16>(
            Fixnum::Decode_(n) + 'A' - 10 ));
    }
} // digit_char

// digit_char_p
Val digit_char_p(Val ch, Val baze)
{
    Int iDigit = Character::ToCode(ch);
    Int iBase = Fixnum::Decode_(baze);
    if (iBase <= 10)
    {
        if (iDigit >= '0' && iDigit < '0' + iBase)
        {
            return Fixnum::Encode(iDigit - '0');
        }
    }
    else
    {
        if (iDigit >= '0' && iDigit <= '9')
        {
            return Fixnum::Encode(iDigit - '0');
        }

        if (iDigit >= 'A' && iDigit < 'A' + iBase - 10)
        {
            return Fixnum::Encode(iDigit - 'A' + 10);
        }

        if (iDigit >= 'a' && iDigit < 'a' + iBase - 10)
        {
            return Fixnum::Encode(iDigit - 'a' + 10);
        }
    }
    return nil;
} // digit_char_p


// name_char
Val name_char(Val name)
{
    check_type(name, string);

    if (Fixnum::Encode(1) == length(name))
    {
        return Character::Encode(
            name->Decode<SimpleString>()->m_rgwchElement[0] );
    }

    Val ch = gethash(name, VAR(Aname_char_tableA));
    if (nil == ch) error(L"No such character: ~S", name);
    return ch;
} // name_char

} // CommonLisp
