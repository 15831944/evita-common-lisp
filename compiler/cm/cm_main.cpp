#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - main
// cm_main.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_main.cpp#8 $
//

#include "./cm_fns.h"
#include "./cm_session.h"
#include "./cm_target.h"

namespace Compiler
{

extern void cg_init();


//////////////////////////////////////////////////////////////////////
//
// Compiler::Run
//  Main function of compiler. This function takes lisp form and returns
//  function object which takes no parameter.
//
Val compile_form(Val form)
{
    cg_init();

    Target* pTarget = Target::Get();
    if (NULL == pTarget)
    {
        warn(L"No such compilation target ~S.", TLV(c6_AtargetA));
        return nil;
    }

    DisableGc oDisableGc;

    BindFrameScope oLet(1);
        oLet.Bind(TLV_c6_AsessionA);

    Val ltvs = nil;
    Val fn = nil;

    {
        // Note: dtor of Session may change thread values.
        Session oSession(pTarget, form);
            oSession.m_littab = make_hash_table();

        parse();
        if (! oSession.CanContinue()) goto compile_error;

        optimize();
        if (! oSession.CanContinue()) goto compile_error;

        generate();
        if (! oSession.CanContinue()) goto compile_error;

        ltvs = oSession.GetLoadTimeValues();
        foreach (EnumList, oEnum, ltvs)
        {
            Val key_fun = oEnum.Get();
            Function* pFun = cdr(key_fun)->StaticCast<Function>();
            setf_cdr(pFun->GetFunObj(), key_fun);
        } // for each ltv

        Module::EnumFunction oEnum(oSession.GetModule());
        fn = oEnum.Get()->GetFunObj();
    }

  compile_error:
    {
        Thread::Get()->mv_value[1] = nil;
        Thread::Get()->mv_value[2] = nil;
        Thread::Get()->mv_value[3] = ltvs;
        return fn;
    }
} // compile_form

} // Compiler
