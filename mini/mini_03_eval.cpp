#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 3 Evaluation and Compilation
// mini/mini_03_eval.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_03_eval.cpp#3 $
//
#include "./mini_lisp.h"

#include "../kernel/ke_arch.h"
#include "../kernel/ke_frame.h"

namespace MiniLisp
{

//////////////////////////////////////////////////////////////////////
//
// make_environment
//
// Description:
//  Makes toplevel enviroment and returns it.
//
Val make_environment(Val name, Val toplevel_p)
{
    Val env = MiniThread::Get()->AllocRecord(CLASSD_environment);

    Environment* pEnv = env->Decode<Environment>();
        pEnv->m_latch      = make_latch(name);
        pEnv->m_variables  = make_hash_table();
        pEnv->m_functions  = make_hash_table();
        pEnv->m_outer      = nil;

        if (nil != toplevel_p)
        {
            pEnv->m_types      = make_hash_table();
            pEnv->m_classes    = make_hash_table();
            pEnv->m_others     = make_hash_table();
        }
        else
        {
            pEnv->m_types       = nil;
            pEnv->m_classes     = nil;
            pEnv->m_others      = nil;
        }

    return env;
} // make_environment


// var_info
Val var_info(Val name)
{
    check_type(name, symbol);

    Val env = TLV(AenvironmentA);
    Environment* pEnv = env->Decode<Environment>();
    Val htb = pEnv->m_variables;

    with_exclusive_latch(pEnv->m_latch);
    return gethash(name, htb);
} // var_info

} // MiniLisp

namespace CommonLisp
{

// constantp
bool constantp(Val form, Val)
{
    if (symbolp(form))
    {
        Val kind_alist = var_info(form);
        return car(kind_alist) == Kconstant;
    }
    else if (! consp(form))
    {
        return true;
    }
    else if (Qquote == car(form) && consp(cdr(form)) && nil == cddr(form))
    {
        return true;
    }
    return false;
} // constantp


//////////////////////////////////////////////////////////////////////
//
// special_operator_p
//
bool special_operator_p(Val name)
{
    Environment* pEnv = VAR(Aruntime_environmentA)->Decode<Environment>();

    with_shared_latch(pEnv->m_latch);

    return Kspecial_operator == car(gethash(name, pEnv->m_functions));
} // special_operator_p

} // CommonLisp
