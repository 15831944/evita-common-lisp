#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - initialization
// platform/win/vanilla/vanilla_stab.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/vanilla/vanilla_lisp.cpp#1 $
//
// Description:
//  Installs following C implemented lisp function for genesis.
//
#include "../../big/big_lisp.h"

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


namespace CommonLisp
{

Val values_list(Val);

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

Val funcall(Val fn, Val a, Val b, Val c, Val d)
{
    MiniThread::Get()->m_fn = ensure_funcallable(fn);
    values(a, b, c, d);
    return call_lisp(MiniThread::Get());
} // funcall

Val funcall(Val fn, Val a, Val b, Val c, Val d, Val e)
{
    MiniThread::Get()->m_fn = ensure_funcallable(fn);
    values(a, b, c, d, e);
    return call_lisp(MiniThread::Get());
} // funcall

Val funcall(Val fn, Val a, Val b, Val c, Val d, Val e, Val f)
{
    MiniThread::Get()->m_fn = ensure_funcallable(fn);
    values(a, b, c, d, e, f);
    return call_lisp(MiniThread::Get());
} // funcall

Val funcall(Val fn, Val a, Val b, Val c, Val d, Val e, Val f, Val g)
{
    MiniThread::Get()->m_fn = ensure_funcallable(fn);
    values(a, b, c, d, e, f, g);
    return call_lisp(MiniThread::Get());
} // funcall

Val funcall(Val fn, Val a, Val b, Val c, Val d, Val e, Val f, Val g, Val h)
{
    MiniThread::Get()->m_fn = ensure_funcallable(fn);
    values(a, b, c, d, e, f, g, h);
    return call_lisp(MiniThread::Get());
} // funcall

Val funcall(Val fn, Val a, Val b, Val c, Val d, Val e, Val f, Val g, Val h,
    Val i )
{
    MiniThread::Get()->m_fn = ensure_funcallable(fn);
    values(a, b, c, d, e, f, g, h, i);
    return call_lisp(MiniThread::Get());
} // funcall

Val funcall(Val fn, Val a, Val b, Val c, Val d, Val e, Val f, Val g, Val h,
    Val i, Val j )
{
    MiniThread::Get()->m_fn = ensure_funcallable(fn);
    values(a, b, c, d, e, f, g, h, i, j);
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


void __declspec(noreturn)
error(Val cond)
{
    funcall(Qerror, cond);
} // error

// error
void __declspec(noreturn)
error(Val cond, Val a, Val b)
    { funcall(Qerror, cond, a, b); }

// error
void __declspec(noreturn)
error(Val cond, Val a, Val b, Val c, Val d)
    { funcall(Qerror, cond, a, b, c, d); }

void __declspec(noreturn)
error(Val cond, Val a, Val b, Val c, Val d, Val e, Val f)
    { funcall(Qerror, cond, a, b, c, d, e, f); }

} // CommonLisp
