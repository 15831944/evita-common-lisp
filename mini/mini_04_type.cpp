#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - minilisp - 04 Types
// mini/mini_04_type.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_04_type.cpp#3 $
//

#include "./mini_lisp.h"

namespace CommonLisp
{

// type_of
Val type_of(Val x)
{
    return class_of(x)->Decode<Class>()->m_name;
} // type_of

} // CommonLisp
