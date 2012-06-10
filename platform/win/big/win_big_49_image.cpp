#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 25 Environment
// genesis/gs_25_env.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/big/win_big_49_image.cpp#5 $
//
// Description:
//  This file contains C implementation of lisp functions.
//      save_image
//
#if defined(_WIN32)

#include "../../../big/big_lisp.h"

#include "../kernel/win_ke_util.h"

namespace MiniLisp
{

//////////////////////////////////////////////////////////////////////
//
// Save Image
//
Val internal_save_image(Val filename)
{
    check_type(filename, simple_string);

    LPCWSTR pwszImage = filename->Decode<SimpleString>()->GetElements();

    FileHandle shFile = ::CreateFileW(
        pwszImage,
        GENERIC_WRITE,
        FILE_SHARE_READ | FILE_SHARE_DELETE,
        NULL,
        CREATE_ALWAYS,
        0,
        NULL );

    if (INVALID_HANDLE_VALUE == shFile.h)
    {
        error(L"Can't create ~A", filename);
    }

    size_t cbImage;
    Kernel::Memory::Save(shFile, &cbImage);

    return make_uint(cbImage);
} // internal_save_image

} // MiniLisp

#endif // defined(_WIN32)
