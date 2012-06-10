#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - lisp
// genesis/geneis_lisp.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_09_cond.cpp#3 $
//

#include "./mini_lisp.h"

namespace MiniLisp
{

// make_simple_error
Val make_simple_error(LPCWSTR control, Val args)
{
    Val error = MiniThread::Get()->AllocInstance(CLASSD_simple_error);
    SimpleError* pError = error->Decode<SimpleError>();
        pError->m_format_control   = make_string(control);
        pError->m_format_arguments = args;
    return error;
} // make_simple_error


// make_type_error
Val make_type_error(Val datum, Val expected_type)
{
    Val error = MiniThread::Get()->AllocInstance(CLASSD_type_error);
    TypeError* pError = error->Decode<TypeError>();
        pError->m_expected_type = expected_type;
        pError->m_datum = datum;
    return error;
} // make_type_error

} // MiniLisp
