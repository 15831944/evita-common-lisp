#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 12 Numbers
// mini/mini_lisp.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_12_number.cpp#3 $
//
#include "./mini_lisp.h"

namespace MiniLisp
{

//////////////////////////////////////////////////////////////////////////////
//
// 12 Numbers
//

Int cmp_xx(Val x, Val y)
{
    check_type(x, fixnum);
    check_type(y, fixnum);
    return x->ToInt() - y->ToInt();
} // cmp


Val add_xx(Val x, Val y)
{
    check_type(x, fixnum);
    check_type(y, fixnum);
    return FromInt<Val_>(x->ToInt() + y->ToInt());
} // add


Val mul_xx(Val x, Val y)
{
    check_type(x, fixnum);
    check_type(y, fixnum);
    return FromInt<Val_>(x->ToInt() * Fixnum::Decode_(y));
} // mul


Val sub_xx(Val x, Val y)
{
    check_type(x, fixnum);
    check_type(y, fixnum);
    return FromInt<Val_>(x->ToInt() - y->ToInt());
} // sub

Val truncate_xx(Val x, Val y)
{
    check_type(x, fixnum);
    check_type(y, fixnum);
    return Fixnum::Encode(x->ToInt() / y->ToInt());
} // truncate

} // MiniLisp
