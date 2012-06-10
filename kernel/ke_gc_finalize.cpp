#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Finalization
// kernel/ke_gc_finalize.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_gc_finalize.cpp#4 $
//
#include "./ke_gc.h"

#include "./ke_finalize.h"

#include "../mini/mini_lisp.h"

namespace Kernel
{

// finalizationp
bool finalizationp(Val x)
    { return x->Is<Finalization>(); }


// Gc::phaseInvokeFinalization
//  o Disable GC during executing scheduled finalization.
//      BUGBUG: We should enable GC during executing finalizations.
//  o Save thread.m_fn, m_n, mv_value.
void Gc::phaseInvokeFinalization()
{
    DisableGc oDisableGC;

    Thread::SaveContext oSaveContext;

    Val* pp = &VAR(AfinalizationsA);

    for (;;)
    {
        Val cons = *pp;

        if (! consp(cons)) break;

        Val finrec = car(cons);

        if (! finalizationp(finrec))
        {
            pp = &cons->Decode<Cons>()->m_cdr;
            continue;
        }

        Finalization* p = finrec->Decode<Finalization>();

        if (Kscheduled == p->m_state)
        {
            pp = &cons->Decode<Cons>()->m_cdr;
        }
        else
        {
            *pp = cdr(cons);

            if (nil == p->m_state)
            {
                DEBUG_PRINTF(L"finrec=%p obj=%p\r\n", p, p->m_object);

                p->m_state = Kerror;

                funcall(p->m_function, p->m_object);

                p->m_state = Kcompleted;
            }
        }
    } // for
} // Gc::phaseInvokeFinalization

} // Kernel
