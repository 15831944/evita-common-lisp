#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - main
// gs_main.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_03_eval.cpp#6 $
//
#include "./gs_lisp.h"

namespace Compiler
{
    extern Val compile_form(Val);
} // Compiler

namespace
{

// eval_list
Val eval_list(Val forms)
{
    Val anchor = list(nil);
    Val last = anchor;
    for (Val runner = forms; ! endp(runner); runner = cdr(runner))
    {
        last = setf_cdr(list(eval(car(runner))), last);
    } // for

    return cdr(anchor);
} // eval_list

// need_eval eval_when
static bool need_eval(Val situations)
{
    if (nil != memq(Kexecute, situations) ||
        nil != memq(Qeval, situations) )
    {
        return true;
    }

    if (TLV(c6_AsituationA) == Qload)
    {
        return nil != memq(Kload_toplevel, situations) ||
               nil != memq(Qload, situations);
    }

    if (TLV(c6_Aprocessing_modeA) == Kcompile_time_too)
    {
        return nil != memq(Kcompile_toplevel, situations) ||
               nil != memq(Qcompile, situations);
    }

    return false;
} // need_eval

} // namespace

namespace Genesis
{

// lambda
Val
expand_lambda(Val form, Val)
{
    CHECK_SYNTAX(form, 2, MaxFormLength,
        "(lambda lambda-list decl* form*)" );

    return list(Qfunction, form);
} // expand_lambda

} // Genesis


namespace CommonLisp
{

using namespace MiniLisp;
using namespace Genesis;


// compiler_macro_function
Val compiler_macro_function(Val name, Val env)
{
    if (symbolp(name))
    {
    }
    else if (function_name_p(name))
    {
        name = intern_setf_cell(second(name));
    }
    else
    {
        error(make_type_error(name, Qfunction_name));
    }

    return function_information(
        name,
        Kcompiler_macro,
        toplevel_environment(env) );
} // compiler_macro_function


// compile_form
static Val compile_form(Val form)
{
    return Compiler::compile_form(form);
} // compiler_form


//////////////////////////////////////////////////////////////////////
//
// eval
//
Val eval(Val form)
{
    Thread::Get()->m_n = Fixnum::Encode(1);

  try_again:
    if (nil == form)
    {
        return nil;
    }
    else if (symbolp(form))
    {
        // 3.1.2.1.1 Symbols as Forms
        return symbol_value(form);
    }
    else if (! consp(form))
    {
        // 3.1.2.1.3 Self-Evaluating Form
        return form;
    }
    else if (consp(car(form)) && Qlambda == caar(form))
    {
        // 3.1.2.1.24 Lambda Forms
        Val tpl = compile_form(car(form));
        if (! functionp(tpl))
        {
            goto compilation_error;
        }

        Val fun = apply(tpl, nil);
        Val vals = eval_list(cdr(form));
        return apply(fun, vals);
    }
    else if (symbolp(car(form)))
    {
        Val name = car(form);

        if (Qfunction == name)
        {
            Val fname = cadr(form);

            if (consp(fname) && Qlambda == car(fname))
            {
                Val tpl = compile_form(form);
                if (! functionp(tpl))
                {
                    goto compilation_error;
                }
                return apply(tpl, nil);
            }

            return fdefinition(fname);
        }

        if (Qeval_when == name)
        {
            if (! need_eval(cadr(form)))
            {
                return nil;
            }
        }

        if (Qfuncall == name)
        {
            Val args = eval_list(cdr(form));
            return apply(car(args), cdr(args));
        }

        if (Qif == name)
        {
            Val val;
            {
                GcRoot oRoot(form);
                val = eval(cadr(form));
                form = oRoot.Get();
            } // val

            if (nil != val)
            {
                return eval(caddr(form));
            }
            else
            {
                return eval(car(cdddr(form)));
            }
        }

        if (Qprogn == name)
        {
            Val runner = cdr(form);
            while (! endp(runner))
            {
                Val val;
                {
                    GcRoot oRoot(runner);
                    val = eval(car(runner));
                    runner = oRoot.Get();
                }

                runner = cdr(runner);
                if (nil == runner)
                {
                    return val;
                }
            } // while

            return nil;
        }

        if (Qquote == name)
        {
            return cadr(form);
        }

        if (Qsetq == name)
        {
            Val runner = cdr(form);
            Val val = nil;
            while (! endp(runner))
            {
                Val sym = car(runner);
                    runner = cdr(runner);
                if (endp(runner))
                {
                    error(L"Missing value form for ~S", sym);
                }

                val = car(runner);
                    runner = cdr(runner);

                if (! symbolp(sym))
                {
                    error(make_type_error(sym, Qsymbol));
                }

                {
                    GcRoot oRoot(runner);
                    val = eval(val);
                    runner = oRoot.Get();
                }

                val = setf_symbol_value(val, sym);
            } // while
            return val;
        }

        // macro?
        {
            Val expander = macro_function(name, nil);
            if (functionp(expander))
            {
                form = funcall(expander, form, nil);
                goto try_again;
            }
        }

        if (special_operator_p(name))
        {
            Val fun = Compiler::compile_form(form);
            if (! functionp(fun))
            {
                goto compilation_error;
            }
            return apply(fun, nil);
        }

        return apply(name, eval_list(cdr(form)));
    }
    else
    {
        error(L"Invalid function form: ~S", form);
    }

 compilation_error:
    error(L"Compilation error");
} // eval


// macroexpand_1
Val macroexpand_1(Val form, Val env)
{
    if (! consp(form) || !symbolp(car(form)))
    {
        return values(form, nil);
    }

    Val expander = macro_function(car(form));
    if (nil == expander)
    {
        return values(form, nil);
    }

    Val expansion = funcall(expander, form, env);
    return values(expansion, expansion != form ? t : nil);
} // macroexpand_1


// macro_function
Val macro_function(Val name, Val env)
{
    check_type(name, symbol);
    env = toplevel_environment(env);
    return function_information(name, Kmacro, env);
} // macro_function


// setf_macro_function
Val setf_macro_function(Val fn, Val name, Val env)
{
    if (! functionp(fn) && nil != fn)
    {
        error(make_type_error(fn, list(Qor, Qfunction, Qnull)));
    }

    check_type(name, symbol);

    env = toplevel_environment(env);

    setf_function_information(
        fn,
        name,
        Kmacro,
        env );

    if (runtime_environment_p(env))
    {
        // runtime environment
        setf_symbol_function(
            make_not_function_function(name),
            name );
    }

    return fn;
} // setf_macro_function

} // CommonLisp
