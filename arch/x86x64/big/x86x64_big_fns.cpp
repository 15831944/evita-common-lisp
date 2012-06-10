#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - big - x86
// arch/x86/big/x86_big_fns.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/big/x86x64_big_fns.cpp#4 $
//
// Description:
//  TBD
#include "../kernel/x86x64_ke_layout.h"
#include "../../../mini/mini_lisp.h"    // register_caller

namespace MiniLisp
{

// copy_code_annotation
//  Called by:
//      copy_function
//      make_trampoline
//      Dmake_closure
Val copy_code_annotation(
    Val             newfun,
    Val             templ,
    FunObj::Annon   oAnnon )
{
    const FunObj* pTempl = templ->Decode<FunObj>();
    FunObj*       pNewFun = newfun->Decode<FunObj>();

    switch (oAnnon.m_eType)
    {
    case FunObj::Annon::Type_NamedCallee:
    {
        Val callee = pTempl->FetchCallee(oAnnon.m_ofs);
        register_caller(callee, newfun);
        goto update_call_site;
    } // NamedCallee

    case FunObj::Annon::Type_LocalCallee:
        goto update_call_site;

    update_call_site:
    {
        Val callee = pTempl->FetchCallee(oAnnon.m_ofs);
        pNewFun->PatchCallee(oAnnon.m_ofs, callee);
        break;
    } // update_call_site

    case FunObj::Annon::Type_Label:
    {
        Int iAddr = pTempl->FetchUn(oAnnon.m_ofs);
            iAddr -= templ->ToInt();
            iAddr += newfun->ToInt();

        pNewFun->PatchUn(oAnnon.m_ofs, static_cast<UInt>(iAddr));
        break;
    } // Label
    } // switch annon

    return newfun;
} // copy_code_annotation


//////////////////////////////////////////////////////////////////////
//
//  make-closure
//  Note: stab: we'll replace this with lisp code.
//
Val Dmake_closure(MiniThread* p)
{
    Val templ = p->m_fn;
    FunObj* pTempl = templ->Decode<FunObj>();

    Val closure = p->AllocFunction(
        pTempl->m_classd,
        pTempl->m_cbFunction );

    FunObj* pClosure = closure->Decode<FunObj>();

    ::CopyMemory(
        pClosure + 1,
        pTempl + 1,
        pTempl->m_cbFunction - sizeof(*pTempl) );

    foreach (FunObj::EnumAnnon, oEnum, pTempl)
    {
        FunObj::Annon oAnnon = oEnum.Get();

        switch (oAnnon.m_eType)
        {
        case FunObj::Annon::Type_ClosedLit:
        {
            Int i = Fixnum::Decode_(pTempl->FetchVal(oAnnon.m_ofs));
            pClosure->PatchVal(oAnnon.m_ofs, p->mv_value[i]);
            break;
        } // ClosedLit

        case FunObj::Annon::Type_ClosedVar:
        {
            Int i = Fixnum::Decode_(pTempl->FetchVal(oAnnon.m_ofs));
            Int iPtr = p->mv_value[i]->Decode<ClosedCell>()->ToInt();
                iPtr += offsetof(ClosedCell, m_value);

            pClosure->PatchUn(
                oAnnon.m_ofs,
                static_cast<UInt>(iPtr) );
            break;
        } // ClosedVar

        default:
            copy_code_annotation(closure, templ, oAnnon);
            break;
        } // switch annon
    } // for each annon

    pClosure->m_name = pTempl->m_name;

    // Make closure GC aware.
    pClosure->m_nCookie = FunObj::Cookie;

    return closure;
} // Dmake_closure

} // MiniLisp
