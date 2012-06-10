#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - lisp
// genesis/geneis_lisp.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_lisp.cpp#3 $
//
#include "./mini_lisp.h"
#include "../kernel/ke_memory.h" // for Memory::IsHeap

namespace MiniLisp
{

//////////////////////////////////////////////////////////////////////
//
// 0 Misc
//

bool heap_object_p(Val x)
{
    return Kernel::Memory::IsHeap(x);
} // heap_object_p


//////////////////////////////////////////////////////////////////////
//
// safe_list_length
//  Returns number of list in specified parameter is proper list.
//  Otherwise:
//      atom            => -1
//      dotted-list     => number of const plus one in negative
//      cyclcic-list    => number of cons times two in negative
//
//      (safe-list-length nil)              =>  0
//      (safe-list-length '(1 2))           =>  2
//      (safe-list-length 1)                => -1
//      (safe-list-length '(1 . 2))         => -2
//      (safe-list-length '#1=(1 2 . #1#))  => -5
Val
safe_list_length(Val slow)
{
    int n = 0;
    Val fast = slow;

    while (consp(fast))
    {
        fast = cdr(fast);
        n += 1;

        if (! consp(fast)) break;
        fast = cdr(fast);
        n += 1;

        slow = cdr(slow);
        if (slow == fast) break;
    } // while

    if (nil != fast)
    {
        n = -1 - n;
    }

    return Fixnum::Encode(n);
} // safe_list_length

} // MiniLisp
