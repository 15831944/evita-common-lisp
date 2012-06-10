//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - pre-compiled header
// mini_14_cons.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_14_cons.h#2 $
//
#if !defined(INCLUDE_mini_14_cons_h)
#define INCLUDE_mini_14_cons_h

namespace CommonLisp
{
    bool consp(Val x);
    bool listp(Val x);
    bool endp(Val);

    Val append(Val, Val);

    Val cons(Val, Val);
    Val copy_list(Val);
    Val nconc(Val, Val);
    Val nconc(Val, Val, Val);
    Val nconc(Val, Val, Val, Val);
    Val nconc(Val, Val, Val, Val, Val);
    Val nconc(Val, Val, Val, Val, Val, Val);

    Val car(Val x);
    Val cdr(Val x);
    Val setf_car(Val val, Val kcons);
    Val setf_cdr(Val val, Val kcons);

    Val list(Val);  // 1
    Val list(Val, Val); // 2
    Val list(Val, Val, Val); // 3
    Val list(Val, Val, Val, Val); // 4
    Val list(Val, Val, Val, Val, Val); // 5
    Val list(Val, Val, Val, Val, Val, Val); // 6
    Val list(Val, Val, Val, Val, Val, Val, Val); // 7
    Val list(Val, Val, Val, Val, Val, Val, Val, Val); // 8
    Val list(Val, Val, Val, Val, Val, Val, Val, Val, Val); // 9
    Val list(Val, Val, Val, Val, Val, Val, Val, Val, Val, Val); // 10
    Val list(Val, Val, Val, Val, Val, Val, Val, Val, Val, Val, Val); // 11
    Val list(Val, Val, Val, Val, Val, Val, Val, Val, Val, Val, Val, Val); // 12

    Val listA(Val);  // 1
    Val listA(Val, Val); // 2
    Val listA(Val, Val, Val); // 3
    Val listA(Val, Val, Val, Val); // 4
    Val listA(Val, Val, Val, Val, Val); // 5
    Val listA(Val, Val, Val, Val, Val, Val); // 6
    Val listA(Val, Val, Val, Val, Val, Val, Val); // 7
    Val listA(Val, Val, Val, Val, Val, Val, Val, Val); // 8

    Val caar(Val);
    Val cadr(Val);
    Val cdar(Val);
    Val cddr(Val);

    Val caaar(Val);
    Val caadr(Val);
    Val cadar(Val);
    Val caddr(Val);
    Val cdaar(Val);
    Val cdadr(Val);
    Val cddar(Val);
    Val cdddr(Val);

    Val caaaar(Val);
    Val caaadr(Val);
    Val caadar(Val);
    Val caaddr(Val);
    Val cadaar(Val);
    Val cadadr(Val);
    Val caddar(Val);
    Val cadddr(Val);

    Val cdaaar(Val);
    Val cdaadr(Val);
    Val cdadar(Val);
    Val cdaddr(Val);
    Val cddaar(Val);
    Val cddadr(Val);
    Val cdddar(Val);
    Val cddddr(Val);

    Val rest(Val);
    Val first(Val);
    Val second(Val);
    Val third(Val);
    Val fourth(Val);
    Val fifth(Val);
    Val sixth(Val);
    Val seventh(Val);
    Val eithth(Val);
    Val ninth(Val);
    Val tenth(Val);
} // CommonLisp


namespace MiniLisp
{
    Val assq(Val, Val);
    Val memq(Val, Val);
    Val nreverse_list(Val);
} // MiniLisp

#endif //!defined(INCLUDE_mini_14_cons_h)
