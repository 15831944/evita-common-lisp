#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 17 Sequences
// mini/mini_17_sequence.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_17_sequence.cpp#3 $
//
#include "./mini_lisp.h"

namespace CommonLisp
{

// length
Val length(Val x)
{
    if (listp(x))
    {
        Val n = 0;
        while (! endp(x))
        {
            n = add_xx(n, 1);
            x = cdr(x);
        } // while
        return n;
    }
    else if (x->Is<DataVector>())
    {
        return x->Decode<DataVector>()->m_length;
    }

    error(make_type_error(x, Qsequence));
} // length


//////////////////////////////////////////////////////////////////////
//
// Fill
//
Val fill(Val vector, Val datum)
{
    SimpleVector* pVector = vector->Decode<SimpleVector>();
    Val* pStart = &pVector->mv_element[0];
    Val* pEnd   = &pVector->mv_element[Fixnum::Decode_(pVector->m_length)];
    for (Val* pRunner = pStart; pRunner < pEnd; pRunner++)
    {
        *pRunner = datum;
    } // for
    return vector;
} // fill

} // CommonLisp
