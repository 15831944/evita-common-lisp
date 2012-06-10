#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - big - 20 Files
// platform/win/big/win_big_20_file.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/big/win_big_20_file.cpp#1 $
//
// Description:
//  This file contains C implementation of lisp functions.
//      .current-directory
//      (setf .current-directory)
//      .create-directory
//      .delete-file
//      .file-attributes
//      .find-close
//      .find-first-file
//      .find-next-file
//      .remove-direcotry
//
#include "../../../big/big_lisp.h"

extern "C" ULONG __fastcall
FileTimeToUnixTime(
    const FILETIME* pftX )
{
    ULARGE_INTEGER ullEpoch;
        ullEpoch.LowPart  = 0xD53E8000;
        ullEpoch.HighPart = 0x019DB1DE; 

    ULARGE_INTEGER ullX;
        ullX.LowPart  = pftX->dwLowDateTime;
        ullX.HighPart = pftX->dwHighDateTime;

    ullX.QuadPart -= ullEpoch.QuadPart;

    // 1 nanosecond = 10^-9. 100 nanosecond = 10^-7
    ullX.QuadPart /= 10000000;

    if (0 != ullX.HighPart)
    {
        return static_cast<ULONG>(-1);
    }

    return ullX.LowPart;
} // FileTimeToUnixTime


extern "C" BOOL __fastcall
UnixTimeToFileTime(
    ULONG       nUxTime,
    FILETIME*   out_ftX )
{
    if (NULL == out_ftX)
    {
        ::SetLastError(ERROR_INVALID_PARAMETER);
        return FALSE;
    }

    ULARGE_INTEGER ullEpoch;
        ullEpoch.LowPart  = 0xD53E8000;
        ullEpoch.HighPart = 0x019DB1DE; 

    ULARGE_INTEGER ullX;
        ullX.LowPart  = nUxTime;
        ullX.HighPart = 0;

    // 1 nanosecond = 10^-9. 100 nanosecond = 10^-7
    ullX.QuadPart *= 10000000;

    ullX.QuadPart += ullEpoch.QuadPart;

    out_ftX->dwLowDateTime  = ullX.LowPart;
    out_ftX->dwHighDateTime = ullX.HighPart;

    return TRUE;
} // UnixTimeToFileTime


namespace MiniLisp
{

class Handle : public Record_<Layout::C_handle> {};

namespace
{

// get_cstring
static char16* get_cstring(Val str)
{
    if (simple_string_p(str))
    {
        return str->Decode<SimpleString>()->m_rgwchElement;
    }

    if (str->Is<String>())
    {
        Val runner = str->Decode<String>()->m_displaced_to;
        Int ofs    = Fixnum::Decode_(str->Decode<String>()->m_offset);
        Int len    = Fixnum::Decode_(str->Decode<String>()->m_fill_pointer);
        for (;;)
        {
            if (simple_string_p(runner))
            {
                SimpleString* s = runner->Decode<SimpleString>();
                if (s->m_length == Fixnum::Encode(ofs + len))
                {
                    return s->m_rgwchElement + ofs;
                }

                Val q = make_string(s->m_rgwchElement + ofs, len);
                return q->Decode<SimpleString>()->m_rgwchElement;
            } // if simple_string

            String* p = runner->Decode<String>();
            ofs += Fixnum::Decode_(p->m_offset);
            runner = p->m_displaced_to;
        } // for
    } // if

    error(make_type_error(str, Qstring));
} // get_cstring

// get_handle
static HANDLE get_handle(Val handle)
{
    if (! handle->Is<Handle>())
    {
        error(make_type_error(handle, Qhandle));
    }

    return reinterpret_cast<HANDLE>(
        handle->Decode<Handle>()->m_value );
} // get_handle


// get_last_eror
static inline Val get_last_error()
    { return make_uint(::GetLastError()); }

// make_handle
static inline Val make_handle(HANDLE h)
{
    Val handle = MiniThread::Get()->AllocBinObj(CLASSD_handle);
    handle->Decode<Handle>()->m_value = Fixnum::Encode(h);
    return handle;
} // make_handle

// make_int
static inline Val make_int(int low, int high)
{
    LARGE_INTEGER llSize;
        llSize.LowPart  = low;
        llSize.HighPart = high;
    return make_int64(llSize.QuadPart);
} // make_int


// platform_last_error
static void __declspec(noreturn)
platform_last_error(const char16* pwsz, Val args)
{
    Val code = get_last_error();
    error(Qplatform_error,
        Kcode,      code,
        Koperands,  args,
        Koperation, make_string(pwsz) );
} // platform_last_error

} // namespace


#define define_file_op(mp_name, mp_Name) \
    Val mp_name(Val name) \
    { \
        const char16* pwsz = get_cstring(name); \
        if (! ::mp_Name##W(pwsz)) return get_last_error(); \
        return nil; \
    }

BOOL createDirectoryW(const char16* name)
    { return ::CreateDirectoryW(name, NULL); }

define_file_op(create_directory,    createDirectory)
define_file_op(delete_file,         DeleteFile)
define_file_op(remove_directory,    RemoveDirectory)


// curent_directory
Val current_directory(MiniThread* p)
{
    char16 wszFullname[MAX_PATH+1]; // +1 for trailing slash(/)
    uint cwchFullname;
    if (p->m_n == Fixnum::Encode(0))
    {
        cwchFullname = ::GetCurrentDirectoryW(
            lengthof(wszFullname),
            wszFullname );
        if (0 == cwchFullname)
        {
            platform_last_error(L"GetCurrentDirectory", nil);
        }
    }
    else
    {
        Val drive = p->mv_value[0];
        Int iDrive = fixnump(drive) ? Fixnum::Decode_(drive) : -1;
        unless (iDrive >= 0 && iDrive <= 25)
        {
            error(make_type_error(
                drive,
                list(Qinteger, Fixnum::Encode(0), Fixnum::Encode(25)) ) );
        }

        char16 wszDrive[3];
            wszDrive[0] = static_cast<char16>('A' + iDrive);
            wszDrive[1] = ':';
            wszDrive[2] = 0;

        char16* pwszName;
        cwchFullname = ::GetFullPathNameW(
            wszDrive,
            lengthof(wszFullname),
            wszFullname,
            &pwszName );
        if (0 == cwchFullname)
        {
            platform_last_error(L"GetFullPathName", list(drive));
        }
    } // if

    for (char16* pwsz = wszFullname; 0 != *pwsz; pwsz++)
    {
        if ('\\' == *pwsz) *pwsz = '/';
    } // for pwsz

    if ('/' != wszFullname[cwchFullname - 1])
    {
        wszFullname[cwchFullname] = '/';
        cwchFullname += 1;
        wszFullname[cwchFullname] = 0;
    }

    return make_string(wszFullname, cwchFullname);
} // current_directory


// setf_current_directory
Val setf_current_directory(MiniThread* p)
{
    Val name = p->mv_value[0];
    const char16* pwsz = get_cstring(name);
    if (! ::SetCurrentDirectoryW(pwsz)) return get_last_error();
    return name;
} // setf_current_directory


// file_attributes
Val file_attributes(Val filename)
{
    const char16* pwsz = get_cstring(filename);

    WIN32_FILE_ATTRIBUTE_DATA oData;

    if (! ::GetFileAttributesExW(pwsz, GetFileExInfoStandard, &oData))
    {
        return values(get_last_error(), nil, nil, nil, nil);
    } // if

    return values(
        make_uint(oData.dwFileAttributes),                      // 0
        make_int(FileTimeToUnixTime(&oData.ftLastWriteTime)),   // 1
        make_int(FileTimeToUnixTime(&oData.ftLastAccessTime)),  // 2
        make_int(FileTimeToUnixTime(&oData.ftCreationTime)),    // 3
        make_int(oData.nFileSizeLow, oData.nFileSizeHigh) );    // 4
} // file_attributes


// find_close
Val find_close(Val handle)
{
    ::FindClose(get_handle(handle));
    return nil;
} // find_close


// find_first_file
Val find_first_file(Val pattern)
{
    const char16* pwsz = get_cstring(pattern);

    WIN32_FIND_DATAW oData;
    HANDLE hFind = ::FindFirstFileW(pwsz, &oData);
    if (INVALID_HANDLE_VALUE == hFind)
    {
        platform_last_error(L"FindFirstFile", list(pattern));
    }
    return values(
        make_handle(hFind),                     // 0
        make_string(oData.cFileName),           // 1
        make_uint(oData.dwFileAttributes) );    // 2
} // find_first_file


// find_next_file
Val find_next_file(Val handle)
{
    WIN32_FIND_DATAW oData;
    if (! ::FindNextFileW(get_handle(handle), &oData))
    {
        uint32 dwError = ::GetLastError();
        if (ERROR_NO_MORE_FILES == dwError)
        {
            return values(nil, nil);
        }
    } // if

    return values(
        make_string(oData.cFileName),
        make_uint(oData.dwFileAttributes) );
} // find_next_file


// rename_file
Val rename_file(Val oldname, Val newname)
{
    const char16* pwszOld = get_cstring(oldname);
    const char16* pwszNew = get_cstring(newname);
    if (! ::MoveFileW(pwszOld, pwszNew)) return get_last_error();
    return nil;
} // rename_file


//////////////////////////////////////////////////////////////////////
//
// 49 Internals
//

Val get_command_line()
{
    return make_string(::GetCommandLineW());
} // get_command_line


Val exit_process(Val x)
{
    DWORD dwExitCode;

    if (fixnump(x))
    {
        if (minusp(x)) goto type_error;
        dwExitCode = static_cast<DWORD>(Fixnum::Decode_(x));
    }
    else if (bignump(x))
    {
        Bignum* p = x->Decode<Bignum>();
        if (Fixnum::Encode(1) == p->m_length)
        {
            dwExitCode = static_cast<DWORD>(x->Decode<Bignum>()->m_rgBigit[0]);
        }
        else if (Fixnum::Encode(2) == p->m_length &&
                 0 == p->m_rgBigit[1] )
        {
            dwExitCode = static_cast<DWORD>(x->Decode<Bignum>()->m_rgBigit[0]);
        }
        else
        {
            goto type_error;
        }
    }
    else
    {
        goto type_error;
    }

    if (::IsDebuggerPresent()) __debugbreak();

    ::ExitProcess(dwExitCode);
    // NOTREACHED

 type_error:
    error(make_type_error(
        x,
        list(Qunsigned_byte, Fixnum::Encode(32)) ) );
} // exit_process


} // MiniLisp
