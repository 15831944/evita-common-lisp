#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - Compiler Base
// cm_base.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_base.cpp#3 $
//

#include "./cm_base.h"
#include "./cm_session.h"

namespace Compiler
{

void*
Atom::operator new(size_t cb)
{
    Mm* pMm = Session::Get();
    return pMm->Alloc(cb);
} // Atom::operator new


void*
Atom::operator new[](size_t cb)
{
    Mm* pMm = Session::Get();
    return pMm->Alloc(cb);
} // Atom::operator new

} // Compiler
