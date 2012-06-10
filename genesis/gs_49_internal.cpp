#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 49 Internals
// genesis/gs_49_internal.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_49_internal.cpp#4 $
//
// Description:
//  This file contains C implementation of lisp functions.
//      expand_ext_ref
//
#include "./gs_lisp.h"

namespace Genesis
{
// expand_ref
//  (ref class slot obj) = (%ref 'class 'slot obj)
Val expand_ref(Val form, Val)
{
    CHECK_SYNTAX(form, 4, 4, "(ref class slot obj)");
    Val klass = second(form);
    Val slot  = third(form);
    Val obj   = fourth(form);
    return list(QZref, list(Qquote, klass), list(Qquote, slot), obj);
} // expand_ext_ref

Val expand_without_garbage_collection(Val form, Val)
{
    return cons(Qlocally, cdr(form));
} // expand_without_garbage_collection

} // Genesis
