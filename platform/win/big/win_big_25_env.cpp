#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 25 Environment - Time
// platform/win/big/big_25_time
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/big/win_big_25_env.cpp#7 $
//
// Description:
//  This file contains C implementation of lisp functions.
//      get_computer_name
//      software_version
//
#include "../../../big/big_lisp.h"

namespace Kernel
{
    bool collect_garbage(uint, size_t*, size_t*);
} // Kernel

namespace MiniLisp
{

namespace
{
class Time
{
    public: FILETIME m_ftElapsed;
    public: FILETIME m_ftCreate;
    public: FILETIME m_ftExit;
    public: FILETIME m_ftKernel;
    public: FILETIME m_ftUser;
}; // Time

class ProcessTime : public Time
{
    public: ProcessTime()
    {
        ::GetSystemTimeAsFileTime(&m_ftElapsed);

        ::GetProcessTimes(
            ::GetCurrentThread(),
            &m_ftCreate,
            &m_ftExit,
            &m_ftKernel,
            &m_ftUser );
    } // Time
}; // Time


class ThreadTime : public Time
{
    public: ThreadTime()
    {
        ::GetSystemTimeAsFileTime(&m_ftElapsed);

        ::GetThreadTimes(
            ::GetCurrentThread(),
            &m_ftCreate,
            &m_ftExit,
            &m_ftKernel,
            &m_ftUser );
    } // ThreadTime
}; // ThreadTime

inline LONGLONG
FileTimeDiff(
    const FILETIME* pftEnd,
    const FILETIME* pftStart )
{
    ULARGE_INTEGER oX;
        oX.LowPart  = pftEnd->dwLowDateTime;
        oX.HighPart = pftEnd->dwHighDateTime;

    ULARGE_INTEGER oY;
        oY.LowPart  = pftStart->dwLowDateTime;
        oY.HighPart = pftStart->dwHighDateTime;

    return (oX.QuadPart - oY.QuadPart) / 10000;
} // FileTimeDiff

// convert_time
Val convert_time(FILETIME ft)
{
    ULARGE_INTEGER ullTick;
        ullTick.HighPart = ft.dwHighDateTime;
        ullTick.LowPart  = ft.dwLowDateTime;

    // 100ns => 1ms
    ullTick.QuadPart /= 10000;

    return make_uint64(ullTick.QuadPart);
} // convert_time

} // namespace


// collect_garbage
Val big_collect_garbage(MiniThread* p)
{
    Val age;

    KeyArg keys [] =
    {
        keyword_argument(age,   Fixnum::Encode(0))
    }; // keys

    parse_keys(0, keys, lengthof(keys));

    check_type(age, fixnum);

    size_t cbBefore;
    size_t cbAfter;

    if (! collect_garbage(static_cast<uint>(Fixnum::Decode_(age)), &cbBefore, &cbAfter))
    {
        p->mv_value[1] = Fixnum::Encode(0);
        return p->mv_value[0] = Fixnum::Encode(0);
    }

    p->mv_value[1] = Fixnum::Encode(cbAfter);
    return p->mv_value[0] = Fixnum::Encode(cbBefore);
} // C_collect_garbage


// get_computer_name
Val get_computer_name(Val format)
{
    BOOL fSucceeded;
    WCHAR rgwchName[200];
    DWORD cwchName = lengthof(rgwchName);

    fSucceeded = ::GetComputerNameEx(
        static_cast<COMPUTER_NAME_FORMAT>(Fixnum::Decode_(format)),
        rgwchName,
        &cwchName );

    if (! fSucceeded)
    {
        //DWORD dwError = ::GetLastError();
        //REPORT_WIN32_ERROR("GetComputerNameEx", dwError);
        ::lstrcpyW(rgwchName, L"unknown");
    }

    return make_string(rgwchName);
} // get_computer_name


// get_process_times
// BUGBUG: We should be implement this in assembler.
Val get_process_times()
{
    MiniThread * p = MiniThread::Get();
    ProcessTime oTime;
    p->mv_value[2] = convert_time(oTime.m_ftUser);
    p->mv_value[1] = convert_time(oTime.m_ftKernel);
    return p->mv_value[0] = convert_time(oTime.m_ftElapsed);
} // get_process_times


// get_thread_times
// BUGBUG: We should be implement this in assembler.
Val get_thread_times()
{
    MiniThread* p = MiniThread::Get();
    ThreadTime oTime;
    p->mv_value[2] = convert_time(oTime.m_ftUser);
    p->mv_value[1] = convert_time(oTime.m_ftKernel);
    return p->mv_value[0] = convert_time(oTime.m_ftElapsed);
} // get_thread_times


// software_version
Val software_version()
{
    BOOL fSucceeded;

    WCHAR rgwchName[200];
        ::lstrcpyW(rgwchName, L"unknown");

    OSVERSIONINFOEX oInfo;
        oInfo.dwOSVersionInfoSize = sizeof(oInfo);

#pragma warning(suppress: 4996)
		fSucceeded = ::GetVersionEx(reinterpret_cast<OSVERSIONINFO*>(&oInfo));

    if (fSucceeded)
    {
        if (0 == oInfo.wServicePackMajor)
        {
            ::wsprintfW(
                rgwchName,
                L"%u.%u.%u",
                oInfo.dwMajorVersion,
                oInfo.dwMinorVersion,
                oInfo.dwBuildNumber );
        }
        else
        {
            ::wsprintfW(
                rgwchName,
                L"%u.%u.%u SP%u.%u",
                oInfo.dwMajorVersion,
                oInfo.dwMinorVersion,
                oInfo.dwBuildNumber,
                oInfo.wServicePackMajor,
                oInfo.wServicePackMinor );
        }
    }

    return make_string(rgwchName);
} // software_version

} // MiniLisp
