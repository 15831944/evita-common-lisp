//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - pre-compiled header
// mini_18_hash_table.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_18_hash_table.h#2 $
//
#if !defined(INCLUDE_mini_18_hash_table_h)
#define INCLUDE_mini_18_hash_table_h

namespace MiniLisp
{
    Val sxhash_eq(Val);
    Val sxhash_eql(Val);
    Val sxhash_equal(Val);
    Val sxhash_equalp(Val);
} // MiniLisp

namespace CommonLisp
{
    bool hash_table_p(Val x);

    Val clrhash(Val);
    Val gethash(Val, Val, Val, Val*);
    Val gethash(Val key, Val htb);
    Val gethash(Val key, Val htb, Val def);
    Val hash_table_count(Val);
    Val hash_table_test(Val htb);
    Val make_hash_table(Val = Qeq);
    Val remhash(Val, Val);

    inline Val sxhash(Val x) 
        { return MiniLisp::sxhash_equal(x); }

    Val setf_gethash(Val, Val, Val);
} // CommonLisp

#endif //!defined(INCLUDE_mini_18_hash_table_h)
