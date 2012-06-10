#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - Compiler Base
// cm_object.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_object.cpp#4 $
//

#include "./cm_base.h"
#include "./cm_fns.h"

namespace Compiler
{

void
Object::HtmlPrint(Val stream, bool fDef) const
{
    HtmlA(stream, fDef);
    Print(stream);
    write_string(L"</a>", stream);
} // Object::HtmlPrint


void
Object::Print(Val stream) const
{
    cm_format(stream, L"(object #x~X~X)",
        Fixnum::Encode(reinterpret_cast<Int>(this) >> 4),
        Fixnum::Encode(reinterpret_cast<Int>(this) & 15) );
} // Object::Print

void
Object::HtmlA(Val stream, bool fDef) const
{
    if (fDef)
    {
        html_format(stream, L"<a class='~A' name='o~X'>",
            GetHtmlClass(),
            Fixnum::Encode(const_cast<Object*>(this)) );
    }
    else
    {
        html_format(stream, L"<a class='~A' href='#o~X'>",
            GetHtmlClass(),
            Fixnum::Encode(const_cast<Object*>(this)) );
    }
} // Object::HtmlA

} // Compiler
