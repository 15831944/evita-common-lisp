#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - main
// gs_main.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_03_env.cpp#3 $
//
#include "./gs_lisp.h"

namespace Genesis
{

// set_info
static Val set_info(Val latch, Val htb, Val name, Val kind, Val key, Val val)
{
    with_exclusive_latch(latch);

    Val entry = gethash(name, htb);

    if (nil == entry)
    {
        if (nil != val)
        {
            entry = cons(kind, list(cons(kind, val)));
            setf_gethash(entry, name, htb);
        }
    }
    else if (nil != val || Kconstant == kind)
    {
        if (nil != kind) setf_car(kind, entry);
        Val pair = assq(key, cdr(entry));
        if (nil == pair)
        {
            setf_cdr(cons(cons(key, val), cdr(entry)), entry);
        }
        else
        {
            setf_cdr(val, pair);
        }
    }
    else
    {
        if (key == kind) setf_car(nil, entry);
        Val pair = assq(key, cdr(entry));
        if (nil != pair)
        {
            setf_cdr(nil, pair);
        }
    } // if

    return val;
} // set_info


// function_information
Val function_information(Val name, Val key, Val* out_present_p, Val env)
{
    ASSERT(NULL != out_present_p);

    if (nil == env) env = TLV(AenvironmentA);

    *out_present_p = nil;

    do
    {
        check_type(env, environment);

        Environment* pEnv = env->Decode<Environment>();

        with_shared_latch(pEnv->m_latch);

        Val frob = gethash(name, pEnv->m_functions);
        if (nil != frob)
        {
            *out_present_p = t;
            return cdr(assq(key, cdr(frob)));
        } // if

        env = pEnv->m_outer;
    } while (nil != env);
    return nil;
} // function_information


// variable_information
Val variable_information(Val name, Val key, Val* out_present_p, Val env)
{
    ASSERT(NULL != out_present_p);

    if (nil == env) env = TLV(AenvironmentA);

    *out_present_p = nil;

    do
    {
        check_type(env, environment);

        Environment* pEnv = env->Decode<Environment>();

        with_shared_latch(pEnv->m_latch);

        Val frob = gethash(name, pEnv->m_variables);
        if (nil != frob)
        {
            *out_present_p = t;
            return cdr(assq(key, cdr(frob)));
        } // if

        env = pEnv->m_outer;
    } while (nil != env);

    return nil;
} // variable_information


// setf_function_information
Val setf_function_information(Val val, Val name, Val key, Val env)
{
    check_type(env, environment);
        ASSERT(toplevel_environment_p(env));

    if (symbolp(name)) ;
    else if (setf_cell_p(name)) ;
    else if (function_name_p(name)) name = intern_setf_cell(second(name));
    else error(make_type_error(name, Qfunction_name));

    Val kind;
    {
        switch (key - nil)
        {
        case Kmacro - nil:            kind = Kmacro; break;
        case Kspecial_operator - nil: kind = Kspecial_operator; break;
        case Kcompiler_macro - nil:   kind = nil; break;
        default:                         kind = Kfunction; break;
        } // key
    } // kind

    set_info(
        env->Decode<Environment>()->m_latch,
        env->Decode<Environment>()->m_functions,
        name, kind,
        key, val );

    return val;
} // setf_function_information

// setf_variable_information
Val setf_variable_information(Val val, Val name, Val key, Val env)
{
    check_type(env, environment);
        ASSERT(toplevel_environment_p(env));

    check_type(name, symbol);

    Val kind;
    {
        switch (key - nil)
        {
        case Kspecial - nil:      kind = Kspecial; break;
        case Kconstant - nil:     kind = Kconstant; break;
        case Ksymbol_macro - nil: kind = Ksymbol_macro; break;
        default:                     kind = nil; break;
        } // key
    } // kind

    return set_info(
        env->Decode<Environment>()->m_latch,
        env->Decode<Environment>()->m_variables,
        name, kind,
        key, val );
} // setf_variable_information


// toplevel_envionment
Val toplevel_environment(Val env)
{
    if (nil == env)
    {
        env = TLV(AenvironmentA);
    }

    for (;;)
    {
        check_type(env, environment);
        if (toplevel_environment_p(env)) return env;
        env = env->Decode<Environment>()->m_outer;
    } // for
} // toplevel_environment

} // Genesis
