#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - image
// boot/bt_image.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/boot/bt_image.cpp 25 2006-10-04 11:01:16 yosi $
//
#include "../kernel/ke_memory.h"

#undef REPORT_HRESULT
#undef REPORT_WIN32_ERROR
#define REPORT_HRESULT      __noop
#define REPORT_WIN32_ERROR  (void)

namespace Kernel
{

//////////////////////////////////////////////////////////////////////
//
// Save Image
//
HRESULT
Memory::Save(HANDLE hOutput, size_t*)
{
    ASSERT(INVALID_HANDLE_VALUE != hOutput);

    // BUGBUG: NYI: Stop all threads
    // BUGBUG: NYI: Write image header
    // BUGBUG: NYI: Update RS from Write watch

    DWORD cbWritten;
    BOOL fSucceeded = WriteFile(
        hOutput,
        sm_pbStart,
        static_cast<DWORD>(sm_pbCommit - sm_pbStart),
        &cbWritten,
        NULL );

    if (! fSucceeded)
    {
        DWORD dwError = ::GetLastError();
        return HRESULT_FROM_WIN32(dwError);
    }

    return S_OK;
} // Memory::Save

} // Kernel
