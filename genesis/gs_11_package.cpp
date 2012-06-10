#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 11 Packages
// genesis/gs_11_package.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_11_package.cpp#3 $
//
// Description:
//  This file contains C implementation of lisp functions.
//      inspect
//      time
//
#include "./gs_lisp.h"

namespace Genesis
{

//////////////////////////////////////////////////////////////////////
//
// ensure_package
//
Val ensure_package(Val design)
{
    Val package = find_package(design);
    if (nil == package)
    {
        error(L"No such package: ~S", design);
    }
    return package;
} // ensure_package


// expand_in_package
Val expand_in_package(Val form, Val)
{
    CHECK_SYNTAX(form, 2, 2, "(in-package name)");
    return list(Q("SI:%IN-PACKAGE"), list(Qquote, second(form)));
} // expand_in_package

Val Zin_package(Val name)
{
    Val package = find_package(name);
    if (nil == package)
    {
        error(L"No such package called ~S.", name);
    }
    return set_tlv(TLV_ApackageA, package);
} // Zin_package

} // Genesis
