#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - x86 - main
// arch/x86/genesis/x86_gs_main.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/genesis/x86x64_gs_fns.cpp#4 $
//
#include "../kernel/x86x64_ke_layout.h"
#include "../../../mini/mini_lisp.h"    // register_caller


namespace MiniLisp
{
    Val copy_code_annotation(Val, Val, FunObj::Annon);
} // Kernel


namespace Genesis
{
using namespace MiniLisp;

//////////////////////////////////////////////////////////////////////
//
//  copy_function
//
// For creating alias function, such as car and first.
//
Val copy_function(Val templ)
{
    FunObj* pTempl = templ->Decode<FunObj>();

    Val newfun = MiniThread::Get()->AllocFunction(
        pTempl->m_classd,
        pTempl->m_cbFunction );

    FunObj* pNewFun = newfun->Decode<FunObj>();

    ::CopyMemory(
        pNewFun + 1,
        pTempl + 1,
        pTempl->m_cbFunction - sizeof(*pTempl) );

    foreach (FunObj::EnumAnnon, oEnum, pTempl)
    {
        FunObj::Annon oAnnon = oEnum.Get();

        copy_code_annotation(newfun, templ, oAnnon);
    } // for each annon

    pNewFun->m_name = pTempl->m_name;

    // Make newfun GC aware.
    pNewFun->m_nCookie = FunObj::Cookie;

    return newfun;
} // copy_function

} // Genesis
