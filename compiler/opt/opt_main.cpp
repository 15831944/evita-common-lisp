#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - optimization - dispatcher
// opt_main.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/opt/opt_main.cpp#10 $
//
#include "./opt_defs.h"

#include "../cm/cm_session.h"


namespace Compiler
{

void optimize_analyze_type();
void optimize_call();
void optimize_clean();
void optimize_closure();
void optimize_inline();
void optimize_cfg2ssa();
void optimize_gvn_gcm();
void optimize_ssa2ssi();
void optimize_strength_reduction();

typedef void (*PassFn)();

static const PassFn k_rgpPass[] =
{
    optimize_clean,
    optimize_inline,
    optimize_call,
    optimize_closure,
    optimize_cfg2ssa,
    optimize_ssa2ssi,
    optimize_analyze_type,
    optimize_clean,
    optimize_strength_reduction,
    //optimize_gvn_gcm,
}; // k_rgpPass


// optimize
void optimize()
{
    Session* pSession = Session::Get();

    for (
        const PassFn* pRunner = &k_rgpPass[0];
        pRunner < &k_rgpPass[lengthof(k_rgpPass)];
        pRunner++ )
    {
        (*pRunner)();
        if (! pSession->CanContinue()) break;
    } // for each pass
} // Optimizer

} // Compiler
