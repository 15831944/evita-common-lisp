#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - main
// genesis/main.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_util.cpp#3 $
//

#include "./ke_util.h"

LPCWSTR
lstrchrW(LPCWSTR pwszString, WCHAR wchChar)
{
    while (0 != *pwszString)
    {
        if (*pwszString == wchChar)
        {
            return pwszString;
        }
        pwszString++;
    } // while
    return NULL;
} // lstrchrW
