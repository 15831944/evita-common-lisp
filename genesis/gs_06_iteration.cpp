#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 6 Iteration
// gs_06_iteration.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_06_iteration.cpp#3 $
//
//      expand_do
//      expand_doA
//      expand_dolist
//      expand_dotimes
//      expand_loop
//
#include "./gs_lisp.h"

using namespace Genesis;

namespace
{
//  (do ((var initform stepform)*) (end-test result*) decl* form*)
//  =>
//  (block nil
//    (let ((var initform) ...)
//      decl*
//      (tagbody #:loop
//        (when end-test (return result*))
//        form*
//        (psetq var stepform ...)
//        (go #:loop) ) ))
//
static Val expand_do_family(Val form, Val let, Val setq)
{
    Val bindings = nil;
    Val steps = nil;
    {
        for (Val runner = second(form); ! endp(runner); runner = cdr(runner))
        {
            Val spec = car(runner);
            Val binding = nil;

            if (symbolp(spec))
            {
                binding = list(spec);
            }
            else if (consp(spec))
            {
                CHECK_SYNTAX(spec, 1, 3, "(var [init [step]])");

                if (nil == cdr(spec))
                {
                    binding = list(spec);
                }
                else if (nil == cddr(spec))
                {
                    binding = list(first(spec), second(spec));
                }
                else if (nil == cdddr(spec))
                {
                    binding = list(first(spec), second(spec));
                    steps = listA(third(spec), first(spec), steps);
                }
                else
                {
                    CAN_NOT_HAPPEN();
                }
            } // if

            bindings = cons(binding, bindings);
        } // for each spec

        bindings = nreverse_list(bindings);
        steps    = nreverse_list(steps);
    } // bindings, steps

    Val test;
    Val ret;
    {
        Val test_ret = third(form);
        CHECK_SYNTAX(test_ret, 1, MaxFormLength, "(test result*)");

        test = car(test_ret);

        if (nil == cdr(test_ret))
        {
            ret = list(Qreturn);
        }
        else if (nil == cddr(test_ret))
        {
            ret = list(Qreturn, second(test_ret));
        }
        else
        {
            ret = list(Qreturn, cons(Qprogn, cdr(test_ret)));
        }
    } // test, ret

    Val decls = nil;
    Val forms;
    {
        Val runner = cdddr(form);
        while (! endp(runner))
        {
            Val decl = car(runner);
            if (! consp(decl) || Qdeclare != car(decl))
            {
                break;
            }

            decls = cons(decl, decls);
            runner = cdr(runner);
        } // while

        decls = nreverse_list(decls);
        forms = runner;
    } // body

    Val loop = make_symbol(L"loop");

    return list(Qblock, nil,
        nconc(list(let), list(bindings), decls,
            list(nconc(list(Qtagbody), list(loop),
                       list(list(Qwhen, test, ret)),
                       forms,
                       list(cons(setq, steps)),
                       list(list(Qgo, loop)) ))) );
} // expand_do_family

} // namespace

namespace Genesis
{

// expand_do
Val expand_do(Val form, Val)
{
    CHECK_SYNTAX(form, 2, MaxFormLength,
        "(do ((var [init [step]])*)"
        L"  (end-test result*)"
        L"  decl* {tag|statement}*)" );

    return expand_do_family(form, Qlet, Qpsetq);
} // expand_do


// expand_doA
Val expand_doA(Val form, Val)
{
    CHECK_SYNTAX(form, 2, MaxFormLength,
        "(do* ((var [init [step]])*)"
        L"  (end-test result*)"
        L"  decl* {tag|statement}*)" );

    return expand_do_family(form, QletA, Qsetq);
} // expand_do


// expand_dolist
//  (dolist (var init result) decl* form*)
//  ==>
//  (do ((#:runner init (cdr #:runner)))
//      ((endp #:runner) (let ((var nil) result)))
//    (let ((var (car #:runner))) (declare (ignorable var)) decl* form*) )
//
Val expand_dolist(Val form, Val)
{
    CHECK_SYNTAX(form, 2 , MaxFormLength,
        "(dolist (var init [result]) decl* form*)" );

    Val spec = second(form);
    CHECK_SYNTAX(spec, 2, 3, "(var init [result])");

    Val name   = car(spec);
    Val init   = cadr(spec);
    Val result = cddr(spec);
    Val var    = make_symbol(L"runner");

    return list(
        QdoA, list(list(var, init, list(Qcdr, var))),
          listA(list(Qendp, var), result),
          listA(Qlet, list(list(name, list(Qcar, var))),
            list(Qdeclare, list(Qignorable, name)),
            cddr(form)) );
} // expand_dolist


// expand_dotimes
//  (dotimes (var limit result) decl* form*)
//  ==>
//  (do ((var init (1+ var))
//       (lim limit) )
//      ((>= var lim) result)
//    decl* form* )
//
Val expand_dotimes(Val form, Val)
{
    CHECK_SYNTAX(form, 2 , MaxFormLength,
        "(dotimes (var init [result]) decl* form*)" );

    Val spec = second(form);
    CHECK_SYNTAX(spec, 2, 3, "(var init [result])");

    Val var    = car(spec);
    Val limit  = cadr(spec);
    Val result = cddr(spec);
    Val lim    = make_symbol(L"lim");

    return list(
        QdoA, list(list(var, 0, list(Q1P, var)),
                   list(lim, limit) ),
          listA(list(QGE, var, lim), result),
          list(Qdeclare, list(Qtype, Qinteger, var)),
          cons(Qprogn, cddr(form)) );
} // expand_dotimes

// expand_loop
//  (block nil (tagbody #:loop form* (go #:loop)))
Val expand_loop(Val form, Val)
{
    CHECK_SYNTAX(form, 1 , MaxFormLength, "(loop form*)");

    Val loop = make_symbol(L"loop");

    return list(Qblock, nil,
        nconc(list(Qtagbody, loop),
              copy_list(cdr(form)),
              list(list(Qgo, loop)) ));
} // expand_loop

} // Genesis
