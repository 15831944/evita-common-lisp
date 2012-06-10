#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 5 Data and Control Flow
// gs_05_ctrl.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_05_ctrl.cpp#6 $
//
// Description:
//  This file contains following functions:
//
//      expand_and
//      expand_cond
//      expand_defconstant
//      expand_defun
//      expand_defvar
//      expand_multiple_value_bind
//      expand_multiple_value_list
//      expand_multiple_value_setq
//      expand_nth_value
//      expand_or
//      expand_return
//      expand_setq
//      expand_unless
//      expand_when
//      funcall
//      c_funcall
//
//      Pdefconstant            -- si::%defconstant
//      Pdefun                  -- si::%defun
//      Pdefvar                 -- si::%devar
//
//      apply
//      eql
//      equal
//      values_list
//
#include "./gs_lisp.h"

using namespace Genesis;

namespace MiniLisp
{

Val call_lisp(Thread* p)
{
    ASSERT(NULL == p->GetFP() || p->GetFP()->GetType() != Frame::Type_FromForeign);
    return CallLisp(p);
} // call_lisp

// ensure_funcallable
Val ensure_funcallable(Val fn)
{
    if (functionp(fn))
    {
        return fn;
    }

    if (symbolp(fn))
    {
        return symbol_function(fn);
    }

    error(make_type_error(fn, list(Qor, Qfunction, Qsymbol)));
} // ensure_funcallable

} // MiniLisp

namespace Genesis
{

// expand_and
//      (and)           => t
//      (and x)         => x
//      (and x ...)   => (if x (and ...))
//
Val expand_and(Val form, Val)
{
    CHECK_SYNTAX(form, 1, MaxFormLength, "(and form*)");
    Val runner = cdr(form);

    if (nil == runner)
    {
        return t;
    }
    else if (nil == cdr(runner))
    {
        return car(runner);
    }
    else
    {
        return list(Qif, car(runner), cons(Qand, cdr(runner)));
    }
} // expand_and

// expand_cond
//      (cond)                => nil
//      (cond (test ...))     => (if test (progn ...))
//      (cond (test ...) ...) => (if test (progn ...) (cond ...))
Val expand_cond(Val form, Val)
{
    CHECK_SYNTAX(form, 1, MaxFormLength, "(cond clause*");
    Val clauses = cdr(form);

    if (nil == clauses)
    {
        Val tmp = make_symbol(L"cond");
        return list(Qlet, list(tmp), tmp);
    }

    Val clause = car(clauses);
    CHECK_SYNTAX(clause, 1, MaxFormLength, "(test form*)");

    clauses = cdr(clauses);

    Val test  = car(clause);
    Val then_form = cdr(clause);

    if (nil == then_form)
    {
    }
    else if (nil == cdr(then_form))
    {
        then_form = car(then_form);
    }
    else
    {
        then_form = cons(Qprogn, then_form);
    }

    if (t == test && nil == clauses)
    {
        return then_form;
    }

    // expansion
    if (nil != cdr(clause))
    {
        if (nil == clauses)
        {
            return list(Qif, test, then_form);
        }
        else
        {
            return list(Qif, test, then_form, cons(Qcond, clauses));
        }
    }
    else
    {
        Val temp = make_symbol(L"cond");

        if (nil == clauses)
        {
            return list(Qlet, list(list(temp, car(clause))),
                list(Qif, temp, temp) );
        }
        else
        {
            return list(Qlet, list(list(temp, car(clause))),
                list(Qif, temp, temp, cons(Qcond, clauses)) );
        }
    }
} // expand_cond


// expand_defconstant
Val expand_defconstant(Val form, Val)
{
    CHECK_SYNTAX(form, 2, 3,
        "(defconstant name value [doc-string])" );

    Val name   = list(Qquote, second(form));
    Val init   = third(form);
    Val docstr = fourth(form);

    return list(Q("SI:%DEFCONSTANT"), name, init, docstr);
} // expand_defconstant


// expand_defun
Val expand_defun(Val form, Val)
{
    CHECK_SYNTAX(form, 3, MaxFormLength,
        "(defun fname lambda-list decl* form*)" );

    Val fname = second(form);
    Val lambda_list = third(form);
    Val decls_forms = cdddr(form);

    Val expansion = list(
        Q("SI:%DEFUN"),
        list(Qquote, fname),
        list(Qquote, lambda_list), // for compatible to evcl2.
        list(Qlabels,
                list(listA(fname, lambda_list, decls_forms)),
                list(Qfunction, fname) ) );

    return expansion;
} // expand_defun


// expand_defvar
Val expand_defvar(Val form, Val)
{
    CHECK_SYNTAX(form, 2, 3,
        "(defvar name value [doc-string])" );

    Val name   = list(Qquote, second(form));
    Val init   = third(form);
    Val docstr = fourth(form);

    return list(Qif, list(Qboundp, name),
        list(Q("SI:%DEFVAR"), name, nil,  docstr, nil),
        list(Q("SI:%DEFVAR"), name, init, docstr, t) );
} // expand_defvar


// expand_multiple_value_bind
//  (multiple-value-bind (var*) values decl* form*)
//    => (multiple-value-call 
//          (lambda (&optional var* &rest #:rest) decl* form*)
//           values )
Val expand_multiple_value_bind(Val form, Val)
{
    CHECK_SYNTAX(form, 2, MaxFormLength,
        "(multiple-value-bind (var*) values decl* form*)" );

    Val vars = second(form);
    Val rest = make_symbol(L"rest");
    Val values_form = third(form);
    Val decls_forms = cdddr(form);

    return list(Qmultiple_value_call,
        listA(Qlambda, append(cons(QAoptional, vars), list(QArest, rest)),
                list(Qdeclare, list(Qignore, rest)),
                decls_forms ),
        values_form );
} // expand_multiple_value_bind


// expand_multiple_value_list
//  (multiple-value-list form) = (mutlipe-value-call #'list form)
Val expand_multiple_value_list(Val form, Val)
{
    CHECK_SYNTAX(form, 2, 2, "(multiple-value-list form)");
    return list(Qmultiple_value_call, list(Qfunction, Qlist), second(form));
} // expand_multiple_value_list

#if 0
// expand_multiple_value_setq
//  (multiple-value-setq (var ...) form) => (setf (values var ...) form)
//
//  This expander doesn't use setf values for simplicity.
//  (multiple-value-bind (tmp ...) form (setq var tmp ...) tmp)
//
Val expand_multiple_value_setq(Val form, Val)
{
    CHECK_SYNTAX(form, 3, 3, "(multiple-value-setq (var...) form)");

    Val bind_head = list(nil);
    Val bind_tail = bind_head;
    Val setq = list(Qsetq);
    Val setq_tail = setq;
    Val vars = second(form);
    foreach (EnumList, oEnum, vars)
    {
        Val var = oEnum.Get();
        Val tmp = make_symbol(symbol_name(var));
        bind_tail = setf_cdr(list(tmp), bind_tail);
        setq_tail = setf_cdr(list(var), setq_tail);
        setq_tail = setf_cdr(list(tmp), setq_tail);
    } // for each elt

    Val bind = cdr(bind_head);
    return list(Qmultiple_value_bind, bind, third(form), setq, first(bind));
} // expand_multiple_value_setq
#endif


// expand_nth_value
//  (nth-value nth form) = (nth nth (multiple-value-list form))
Val expand_nth_value(Val form, Val)
{
    CHECK_SYNTAX(form, 3, 3, "(nth-value nth form)");
    return list(Qnth, second(form), list(Qmultiple_value_list, third(form)));
} // expand_nth_value


//////////////////////////////////////////////////////////////////////
//
// expor_or
//      (or)        => t
//      (or x)      => x
//      (or x ...)  => (let ((#:or x)) (if #:or #:or (or ...)))
Val expand_or(Val form, Val)
{
    CHECK_SYNTAX(form, 1, MaxFormLength, "(or form*)");
    Val runner = cdr(form);

    if (nil == runner)
    {
        return nil;
    }
    else if (nil == cdr(runner))
    {
        return car(runner);
    }
    else
    {
        Val temp = make_symbol(make_string(L"or"));
        return
            list(Qlet, list(list(temp, car(runner))),
              list(Qif, temp, temp, cons(Qor, cdr(runner))) );
    }
} // expor_or


// expor_prog1 -- `(let ((,temp ,form1)) ,@forms ,temp)
Val expand_prog1(Val form, Val)
{
    CHECK_SYNTAX(form, 2, MaxFormLength, "(prog1 form1 form*)");

    Val form1 = second(form);
    Val forms = cddr(form);

    Val temp = make_symbol(make_string(L"v"));
    return list(Qlet, list(list(temp, form1)), cons(Qprogn, forms), temp);
} // expand_prog1


// expand_psetq -- for do
//  (let ((#:temp1 form1) ...) (setq var-1 #:temp1 ...) nil)
Val expand_psetq(Val form, Val)
{
    CHECK_SYNTAX(form, 1, MaxFormLength, "(psetq {var form}*)");

    Val bindings  = nil;
    Val var_temps = nil;

    Val runner = cdr(form);
    while (! endp(runner))
    {
        Val name = car(runner);
            runner = cdr(runner);
        Val form = car(runner);
            runner = cdr(runner);

        Val temp = make_symbol(L"temp");

        bindings  = cons(list(temp, form), bindings);
        var_temps = listA(temp, name, var_temps);
    } // while

    bindings  = nreverse_list(bindings);
    var_temps = nreverse_list(var_temps);

    Val vnil = make_symbol(L"nil");

    return list(Qlet, bindings, cons(Qsetq, var_temps),
        list(Qlet, list(vnil), vnil) );
} // expand_psetq


// expand_return
Val expand_return(Val form, Val)
{
    CHECK_SYNTAX(form, 1, 2, "(return [form])");
    return listA(Qreturn_from, nil, cdr(form));
} // expand_return


// expand_setf
Val expand_setf(Val form, Val)
{
    Val runner = cdr(form);
    Val forms = nil;
    Collector oForms(&forms);
    while (! endp(runner))
    {
        Val place = pop(runner);
        Val val   = pop(runner);
        if (symbolp(place))
        {
            oForms.Add(list(Qsetq, place, val));
        }
        else
        {
            oForms.Add(listA(Qfuncall,
                list(Qfunction, list(Qsetf, car(place))),
                val,
                cdr(place) ) );
        } // if
    } // while

    if (nil == cdr(forms)) return car(forms);
    return cons(Qprogn, forms);
} // expand_setf

// unless
Val expand_unless(Val form, Val)
{
    CHECK_SYNTAX(form, 2, MaxFormLength, "(unless test form*)");
    return list(Qif, list(Qnot, second(form)), cons(Qprogn, cddr(form)));
} // expand_unless


// when
Val expand_when(Val form, Val)
{
    CHECK_SYNTAX(form, 2, MaxFormLength, "(when test form*)");
    return list(Qif, second(form), cons(Qprogn, cddr(form)));
} // expand_when

// c_funcall
Val C_funcall(MiniThread* p)
{
    p->m_fn = p->mv_value[0];

    if (functionp(p->m_fn))
    {
        // nothing to do
    }
    else if (symbolp(p->m_fn))
    {
        Val name = p->m_fn;
        p->m_fn = symbol_function(name);
        if (! functionp(p->m_fn))
        {
            error(make_undefined_function(name));
        }
    }
    else
    {
        error(make_type_error(p->m_fn, list(Qor, Qfunction, Qsymbol)));
    }

    p->m_n = sub_xx(p->m_n, 1);

    ::CopyMemory(
        p->mv_value,
        p->mv_value + 1,
        Fixnum::Decode_(p->m_n) * sizeof(Val) );

    return call_lisp(p);
} // c_funcall


// %defconstant - (%defconstant name init docstr)
Val Pdefconstant(Val name, Val init, Val)
{
    check_type(name, symbol);

    Val env = toplevel_environment(nil);

    setf_variable_information(init, name, Kconstant, env);

    if (runtime_environment_p(env))
    {
        setf_symbol_value(init, name);
    }

    return name;
} // Pdefconstant

// %defun - (%defun fname funobj) => fname
Val Pdefun(Val fname, Val, Val funobj)
{
    setf_fdefinition(funobj, fname);
    return fname;
} // Pdefun

// %defvar - (%defvar name init docstr initp)
Val Pdefvar(Val name, Val init, Val, Val initp)
{
    check_type(name, symbol);

    Val env = toplevel_environment(nil);

    setf_variable_information(t, name, Kspecial, env);

    if (runtime_environment_p(env) && nil != initp)
    {
        setf_symbol_value(init, name);
    }

    return name;
} // Pdefvar

} // Genesis

namespace CommonLisp
{

// apply
Val apply(Val fn, Val args)
{
    if (symbolp(fn))
    {
        if (! fboundp(fn))
        {
            error(Qundefined_function, Kname, fn);
        }

        fn = symbol_function(fn);
    }

    if (! functionp(fn))
    {
        error(make_type_error(fn, Qfunction));
    }

    values_list(args);

    MiniThread* p = MiniThread::Get();
    p->m_fn = fn;

    #if NDEBUG
        return ::call_lisp(p);
    #else // NDEBUG
    {

        // For testing, restify with alloc-cons-area.
        //p->m_pConsArea = Area::GetEmpty();

        Val fp = p->m_fp;
        Val fn = p->m_fn;
        Val x = ::call_lisp(p);

        if (! fixnump(p->m_n))
        {
            DEBUG_PRINTF(L"Broken m_n after calling %ls.\r\n",
                fn->Decode<NativeCodeFunction>()->m_name->
                    Decode<Symbol>()->m_name->
                    Decode<SimpleString>()->m_rgwchElement );
            CAN_NOT_HAPPEN();
        }

        if (fp != p->m_fp)
        {
            DEBUG_PRINTF(L"Broken m_fp after calling %ls.\r\n",
                fn->Decode<NativeCodeFunction>()->m_name->
                    Decode<Symbol>()->m_name->
                    Decode<SimpleString>()->m_rgwchElement );
            CAN_NOT_HAPPEN();
        }

        return x;
    }
    #endif // _NDEBUG
} // apply


Val funcall(Val fn)
{
    MiniThread::Get()->m_fn = ensure_funcallable(fn);
    values();
    return call_lisp(MiniThread::Get());
} // funcall


Val funcall(Val fn, Val a)
{
    MiniThread::Get()->m_fn = ensure_funcallable(fn);
    values(a);
    return call_lisp(MiniThread::Get());
} // funcall


Val funcall(Val fn, Val a, Val b)
{
    MiniThread::Get()->m_fn = ensure_funcallable(fn);
    values(a, b);
    return call_lisp(MiniThread::Get());
} // funcall

Val funcall(Val fn, Val a, Val b, Val c)
{
    MiniThread::Get()->m_fn = ensure_funcallable(fn);
    values(a, b, c);
    return call_lisp(MiniThread::Get());
} // funcall

// values_list
Val values_list(Val runner)
{
    MiniThread* p = MiniThread::Get();

    int n = 0;
    while (! endp(runner))
    {
        p->mv_value[n] = car(runner);
        n += 1;
        runner = cdr(runner);
    } // while

    if (0 == n)
    {
        p->mv_value[0] = nil;
    }

    p->m_n = Fixnum::Encode(n);
    return p->mv_value[0];
} // values_list

} // CommonLisp
