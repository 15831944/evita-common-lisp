#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 14 Conses
// mini/mini_14_cons.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_14_cons.cpp#3 $
//
//
#include "./mini_lisp.h"

namespace CommonLisp
{

Val append(Val x, Val y)
{
    if (nil == x)
    {
        return y;
    }

    Val result = list(car(x));
    Val last = result;
    for (;;)
    {
        x = cdr(x);
        if (endp(x))
        {
            setf_cdr(y, last);
            return result;
        }

        last = setf_cdr(list(car(x)), last);
    } // for
} // append

// cons
Val cons(Val car, Val cdr)
{
    Val kons = MiniThread::Get()->AllocCons();
        setf_car(car, kons);
        setf_cdr(cdr, kons);
    return kons;
} // cons

// copy_list
Val copy_list(Val x)
{
    Val y = nil;
    while (! endp(x))
    {
        y = cons(car(x), y);
        x = cdr(x);
    } // while
    return nreverse_list(y);
} // copy_list


// endp
bool endp(Val object)
{
    if (consp(object))
    {
        return false;
    }

    if (nil == object)
    {
        return true;
    }

    error(make_type_error(object, Qlist));
} // endp

// nconc
Val nconc(Val list1, Val list2)
{
    if (endp(list1))
    {
        return list2;
    }

    Val scan1 = list1;
    for (;;)
    {
        Val next1 = cdr(scan1);
        if (! consp(next1))
        {
            setf_cdr(list2, scan1);
            return list1;
        }
        scan1 = next1;
    } // for
} // nconc
} // CommonLisp

namespace MiniLisp
{
// assq
Val assq(Val key, Val alist)
{
    for (Val runner = alist; ! endp(runner); runner = cdr(runner))
    {
        Val kons = car(runner);
        if (consp(kons) && car(kons) == key)
        {
            return kons;
        }
    } // for
    return nil;
} // assq

// memq
Val memq(Val key, Val list)
{
    for (Val runner = list; ! endp(runner); runner = cdr(runner))
    {
        if (car(runner) == key)
        {
            return runner;
        }
    } // for
    return nil;
} // memq

// nreverse_list
Val nreverse_list(Val list)
{
    Val runner = nil;
    while (! endp(list))
    {
        // (rotatef (cdr list) runner list)
        Val temp = cdr(list);
        setf_cdr(runner, list);
        runner = list;
        list = temp;
    } // while
    return runner;
} // nreverse_list

} // MiniLisp
