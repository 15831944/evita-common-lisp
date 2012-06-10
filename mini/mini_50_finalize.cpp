#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Finalization
// kernel/ke_finalize.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_50_finalize.cpp#3 $
//
#include "../mini/mini_lisp.h"

#include "../kernel/ke_finalize.h"


namespace MiniLisp
{

// schedule_finalization
Val schedule_finalization(Val obj, Val fun)
{
    if (! symbolp(fun) && ! functionp(fun))
        { error(make_type_error(fun, ty_function_designator)); }

    Val finrec = allocate_weakobj(CLASSD_finalization);

    Finalization* p = finrec->Decode<Finalization>();
        p->m_object   = obj;
        p->m_function = fun;
        p->m_state    = Kscheduled;

    Val kons = list(finrec);

    {
        with_exclusive_latch(VAR(Afinalizations_latchA));

        setf_cdr(VAR(AfinalizationsA), kons);
        VAR(AfinalizationsA) = kons;
    }

    return finrec;
} // schedule_finalization

} // MiniLisp
