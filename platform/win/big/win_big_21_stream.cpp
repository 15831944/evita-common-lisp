#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - lisp
// genesis/geneis_lisp.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/big/win_big_21_stream.cpp#8 $
//
// Description:
//  This file contains implementation of
//      realize-instance    console-stream
//      realize-instance    file-stream
//
#include "../../../big/big_lisp.h"

#include "../mini/win_mini_21_stream.h"

namespace MiniLisp
{

Val make_external_format(Val, Val, Val);
Val realize_file_stream(Val, HANDLE);
Val schedule_finalization(Val, Val);

inline bool charset_p(Val x)
    { return x->Is<Charset>(); }

inline bool external_format_p(Val x)
    { return x->Is<ExternalFormat>(); }


// parse_keys
void parse_keys(Int iStart, KeyArg* prgoKey, uint cKeys)
{
    for (
        KeyArg* pRunner = prgoKey;
        pRunner < &prgoKey[cKeys];
        pRunner++ )
    {
        *pRunner->m_pval = pRunner->m_init;
    } // for key

    Thread* p = Thread::Get();

    Int iEnd = Fixnum::Decode_(p->m_n);
    for (Int i = iStart; i < iEnd; i += 2)
    {
        Val key = p->mv_value[i];
        for (
            KeyArg* pRunner = prgoKey;
            pRunner < &prgoKey[cKeys];
            pRunner++ )
        {
            if (pRunner->m_key == key)
            {
                *pRunner->m_pval = p->mv_value[i+1];
                break;
            }
        } // for
    } // for i
} // parse_keys


//////////////////////////////////////////////////////////////////////
//
// realize_instance_console_stream
//
Val realize_instance_console_stream(MiniThread* p)
{
    Val stream = p->mv_value[0];

    stream->Decode<ConsoleStream>()->m_blob =
        Fixnum::Encode(new ConsoleStreamImpl());

    return stream;
} // realize_instance_console_stream


//////////////////////////////////////////////////////////////////////
//
// realize_instance_file_stream
//
Val realize_instance_file_stream(MiniThread* p)
{
    Val stream = p->mv_value[0];
    Val external_format;
    Val filename, if_exists, if_does_not_exist, direction;

    KeyArg keys[] =
    {
        keyword_argument(direction,         Kinput ),
        keyword_argument(external_format,   Kdefault),
        keyword_argument(filename,          nil ),
        keyword_argument(if_exists,         Kerror ),
        keyword_argument(if_does_not_exist, QQunbound_marker ),
    }; // keys

    parse_keys(1, keys, lengthof(keys));

    check_type(filename, simple_string);

    const char16* pwszFilename = filename->Decode<SimpleString>()->
        GetElements();

    if (external_format == Kdefault)
    {
        external_format = make_external_format(
            TLV(Adefault_charsetA),
            Kdefault,
            Fixnum::Encode(0) );
    }
    else if (symbolp(external_format) || fixnump(external_format))
    {
        Val name = external_format;
        Val charset = gethash(name, VAR(Acharset_tableA));
        if (nil == charset)
        {
            error(L"No such charset: ~S", name);
        }

        external_format = make_external_format(
            charset,
            Kdefault,
            Fixnum::Encode(0) );
    }
    else if (charset_p(external_format))
    {
        Val charset = external_format;
        external_format = make_external_format(
            charset,
            Kdefault,
            Fixnum::Encode(0) );
    }

    if (! external_format_p(external_format))
    {
        error(make_type_error(external_format, Qexternal_format));
    }

    DWORD dwAccess;
    DWORD dwShare;
    UINT_PTR nFlags;
    {
        if (Kinput == direction)
        {
            dwAccess = GENERIC_READ;
            dwShare  = FILE_SHARE_READ;
            nFlags   = Stream::Flag_Input;

            if (QQunbound_marker == if_does_not_exist)
            {
                if_does_not_exist = Kerror;
            }
        }
        else if (Koutput == direction)
        {
            dwAccess = GENERIC_WRITE;
            dwShare  = FILE_SHARE_READ;
            nFlags   = Stream::Flag_Output;

            if (QQunbound_marker == if_does_not_exist)
            {
                if (Koverwrite == if_exists ||
                    Kappend    == if_exists )
                {
                    if_does_not_exist = Kerror;
                }
                else
                {
                    if_does_not_exist = Kcreate;
                }
            }
        }
        else if (Kio == direction)
        {
            dwAccess = GENERIC_READ | GENERIC_WRITE;
            dwShare  = FILE_SHARE_READ;
            nFlags   = Stream::Flag_Both;

            if (QQunbound_marker == if_does_not_exist)
            {
                if (Koverwrite == if_exists ||
                    Kappend    == if_exists )
                {
                    if_does_not_exist = Kerror;
                }
                else
                {
                    if_does_not_exist = Kcreate;
                }
            }
        }
        else if (Kprobe == direction)
        {
            dwAccess = 0;
            dwShare  = FILE_SHARE_READ;
            nFlags   = Stream::Flag_Probe;

            if (QQunbound_marker == if_does_not_exist)
            {
                if_does_not_exist = nil;
            }
        }
        else
        {
            goto error_bad_direction;
        }
    } // dwAccess

    DWORD dwCreate;
    {
        if (0 == (dwAccess & GENERIC_WRITE))
        {
            if (Kerror == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else if (Kcreate == if_does_not_exist)
            {
                dwCreate = OPEN_ALWAYS;
            }
            else if (nil == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else
            {
                goto error_bad_if_does_not_exist;
            }
        }
        else if (Kerror == if_exists)
        {
            if (Kerror == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else if (Kcreate == if_does_not_exist)
            {
                dwCreate = CREATE_NEW;
            }
            else if (nil == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else
            {
                goto error_bad_if_does_not_exist;
            }
        }
        else if (Knew_version == if_exists)
        {
            if (Kerror == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else if (Kcreate == if_does_not_exist)
            {
                dwCreate = CREATE_NEW;
            }
            else if (nil == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else
            {
                goto error_bad_if_does_not_exist;
            }
        }
        else if (Koverwrite == if_exists ||
                 Ksupersede == if_exists )
        {
            if (Kerror == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else if (Kcreate == if_does_not_exist)
            {
                dwCreate = CREATE_ALWAYS;
            }
            else if (nil == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else
            {
                goto error_bad_if_does_not_exist;
            }
        }
        else if (Kappend == if_exists)
        {
            if (Kerror == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else if (Kcreate == if_does_not_exist)
            {
                dwCreate = OPEN_ALWAYS;
            }
            else if (nil == if_does_not_exist)
            {
                dwCreate = OPEN_EXISTING;
            }
            else
            {
                goto error_bad_if_does_not_exist;
            }
        }
        else if (Krename == if_exists || Krename_and_delete == if_exists)
        {
            // We've already renamed existing file.
            dwCreate = CREATE_NEW;
        }
        else
        {
            goto error_bad_if_exists;
        }
    } // dwCreate

    HANDLE hFile;
    {
        hFile = ::CreateFileW(
            pwszFilename,
            dwAccess,
            dwShare,
            NULL,
            dwCreate,
            0,
            NULL );
        if (INVALID_HANDLE_VALUE == hFile)
        {
            DWORD dwError = ::GetLastError();

            switch (dwError)
            {
            case ERROR_FILE_NOT_FOUND:
                if (nil == if_does_not_exist)
                {
                    return nil;
                }

                error(Qfile_not_found, Kpathname, stream);
                // NOTREACHED

            case ERROR_PATH_NOT_FOUND:
                error(Qpath_not_found, Kpathname, stream);
                // NOTREACHED
            } // switch dwError

            error(Qplatform_error,
                    Kcode, make_uint(dwError),
                    Koperation, Qopen,
                    Koperands, list(stream) );
            // NOTREACHED
        } // if
    } // hFile

    stream->Decode<FileStream>()->m_flags = FromInt<Val_>(nFlags);
    stream->Decode<FileStream>()->m_external_format = external_format;

    realize_file_stream(stream, hFile);
    schedule_finalization(stream, Qclose);

    if (Kappend == if_exists)
    {
        DWORD dwPos = ::SetFilePointer(
            hFile,
            0,
            NULL,
            FILE_END );
        if (INVALID_SET_FILE_POINTER == dwPos)
        {
            DWORD dwError = ::GetLastError();
            error(Qplatform_error,
                Kcode, make_uint(dwError),
                Koperation, list(Qsetf, Qfile_position),
                Koperands, list(stream) );
            // NOTREACHED
        }
    }

    return stream;

  error_bad_direction:
    error(make_type_error(
        direction,
        list(Qmember, Kinput, Kio, Koutput, Kprobe, nil) ) );
    // NOTREACHED

  error_bad_if_exists:
    error(make_type_error(
        if_exists,
        list(Qmember, Kappend, Kerror, Knew_version, Koverwrite,
            Krename, Krename_and_delete, Ksupersede, nil )) );
    // NOTREACHED

  error_bad_if_does_not_exist:
    error(make_type_error(
        if_does_not_exist,
        list(Qmember, Kerror, Kcreate, nil )) );
    // NOTREACHED
} // realize_instance_file_stream

} // MiniLisp
