//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 50 Extensions - Weak Objects
// mini/mini_50_weak.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_50_weak.h#2 $
//
#if !defined(INCLUDE_mini_50_weak_h)
#define INCLUDE_mini_50_weak_h

namespace MiniLisp
{
    Val allocate_weakobj(Val);

    Val make_caller_set(Val);
    Val make_weak_vector(Val, Val);

    inline Val make_caller_set(Int n)
        { return make_caller_set(Fixnum::Encode(n)); }
} // MiniLisp

#endif //!defined(INCLUDE_mini_50_weak_h)
