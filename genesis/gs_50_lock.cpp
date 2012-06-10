#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 49 Internals
// genesis/gs_49_internal.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_50_lock.cpp#3 $
//
// Description:
//  This file contains C implementation of lisp functions.
//      expand_ext_with_latch
//
#include "./gs_lisp.h"

namespace Genesis
{

// expand_with_latch
Val expand_with_latch(Val form, Val)
{
    CHECK_SYNTAX(form, 2, MaxFormLength,
        "(with-latch (latch [mode]) decl* form*)" );

    Val spec  = second(form);
        CHECK_SYNTAX(spec, 1, 2, "(latch [mode])");

    Val mode  = nil == cdr(spec) ? Kexclusive : second(spec);
    Val forms = cddr(form);

    Val var = make_symbol(L"latch");
    return
        list(Qlet, list(list(var, first(spec))),
            list(Qlock_latch, var, mode),
            list(Qunwind_protect,
                cons(Qlocally, forms),
                list(Qunlock_latch, var) ) );
} // expand_ext_with_latch

} // Genesis
