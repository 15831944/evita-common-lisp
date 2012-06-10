#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - Image Writer
// boot/bt_writer.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/boot/win_boot_stream.cpp#4 $
//
#include "../../../mini/mini_lisp.h"

#include "../mini/win_mini_21_stream.h"

namespace MiniLisp
{

void ConsoleStreamImpl::init()
 {}


// make_file_stream_char_out
Val make_file_stream_char_out(Val filename)
{
    check_type(filename, simple_string);

    HANDLE hFile = ::CreateFileW(
        filename->Decode<SimpleString>()->m_rgwchElement,
        GENERIC_WRITE,
        FILE_SHARE_READ | FILE_SHARE_DELETE,
        NULL,
        CREATE_ALWAYS,
        0,
        NULL );

    if (INVALID_HANDLE_VALUE == hFile)
    {
        error(L"Can't create ~S.", filename);
    }

    Val stream = make_file_stream(hFile, Koutput, Qcharacter);
        stream->Decode<FileStream>()->m_pathname = filename;

    return stream;
} // make_file_stream_char_out

} // MiniLisp


namespace Boot
{

// save_image
void save_image(const char16* pwszImage)
{
    HANDLE hFile = ::CreateFileW(
        pwszImage,
        GENERIC_WRITE,
        FILE_SHARE_READ | FILE_SHARE_DELETE,
        NULL,
        CREATE_ALWAYS,
        0,
        NULL );

    if (INVALID_HANDLE_VALUE == hFile)
    {
        error(L"Can't create ~A", make_string(pwszImage));
    }

    Kernel::Memory::Save(hFile);

    ::CloseHandle(hFile);
} // save_image

} // Boot
