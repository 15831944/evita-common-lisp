#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 14 Conses
// gs_14_cons.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_14_cons.cpp#4 $
//
//
//      expand_pop
//      expand_push
//
#include "./gs_lisp.h"

namespace Genesis
{

// expand_pop
Val expand_pop(Val form, Val)
{
    CHECK_SYNTAX(form, 2, 2, "(pop place)");

    Val place = second(form);

    return list(Qprog1, list(Qcar, place),
        list(Qsetq, place, list(Qcdr, place)) );
} // expand_pop

// expand_push
Val expand_push(Val form, Val)
{
    CHECK_SYNTAX(form, 3, 3, "(push value place)");

    Val val = second(form);
    Val place = third(form);

    return list(Qsetq, place, list(Qcons, val, place));
} // expand_push

// C_list
Val C_list(MiniThread* p)
{
    Int n = Fixnum::Decode_(p->m_n);
    Val list =nil;
    while (n >= 1)
    {
        n -= 1;
        list = cons(p->mv_value[n], list);
    } // while

    return p->GcFence(list);
} // C_list

// C_listA
Val C_listA(MiniThread* p)
{
    Int n = Fixnum::Decode_(p->m_n);

    if (1 == n)
    {
        return p->mv_value[0];
    }

    n -= 1;
    Val list = p->mv_value[n];
    while (n >= 1)
    {
        n -= 1;
        list = cons(p->mv_value[n], list);
    } // while

    return p->GcFence(list);
} // C_listA

// C_nconcA
Val C_nconc(MiniThread* p)
{
    Int nN = Fixnum::Decode_(p->m_n);
    if (0 == nN)
    {
        return nil;
    }

    if (1 == nN)
    {
        return p->mv_value[0];
    }

    Int nI = 0;

    Val list1 = p->mv_value[0];

    // Skip leading nil's
    while (nil == list1)
    {
        nI += 1;
        if (nI == nN)
        {
            return nil;
        }
        list1 = p->mv_value[nI];
    } // while

    Val tail = last(list1);

    for (;;)
    {
        nI += 1;

        if (nI == nN)
        {
            return list1;
        }

        Val list = p->mv_value[nI];

        setf_cdr(list, tail);

        if (nil != list)
        {
            tail = last(list);
        }
    } // for
} // C_nconc

} // Genesis

namespace CommonLisp
{

// last
Val last(Val list, Val n)
{
    Val i = Fixnum::Encode(0);
    Val result = list;
    for (Val runner = list; consp(runner); runner = cdr(runner))
    {
        if (cmp_xx(i, n) >= 0)
        {
            result = cdr(result);
        }

        i = add_xx(i, 1);
    } // for

    return result;
} // last

} // CommonLisp
