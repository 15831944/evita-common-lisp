#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// PlIndnt - Perl Indent
// sysdep_win.cpp
//
// Copyright (C) 2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_debugger.h#4 $
//

void Assert(
    const char* pszFile,
    int         iLineNum,
    const char* pszFunc,
    const char* pszExpr,
    bool        fExpr )
{
    if (fExpr)
    {
        return;
    }

    char16 wsz[100];
    ::wsprintf(wsz,
        L"Assertion Failed: %hs\n"
        L"File:     %hs\n"
        L"Line:     %d\n"
        L"Function: %hs\n",
        pszExpr,
        pszFile,
        iLineNum,
        pszFunc );

    ::MessageBox(NULL, wsz, "Perl Indent", MB_ICONERROR);
    ::DebugBreak();
    ::ExitProcess(1);
} // Assert


void DebugPrintf