#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - console - main
// console.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/console/console.cpp 12 2006-01-26 01:14:00 yosi $
//
namespace
{

char16 const k_wszTitle[] = L"Evita Common Lisp";

//////////////////////////////////////////////////////////////////////
//
// EnumArg
//  Enumerates command line arguments.
//
class EnumArg
{
    enum { MAX_WORD_LEN = MAX_PATH };

    enum State
    {
        State_Start,
    }; // State

    LPCWSTR m_pwszRunner;
    char16 m_wsz[MAX_WORD_LEN];

    public: EnumArg(LPCWSTR pwsz) :
        m_pwszRunner(pwsz)
      { next(); }

    public: bool AtEnd() const
        { return 0 == *m_pwszRunner && 0 == *m_wsz; }

    public: LPCWSTR Get() const
        { ASSERT(! AtEnd()); return m_wsz; }

    public: void Next()
        { ASSERT(! AtEnd()); next(); }

    static bool isspace(char16 wch)
        { return ' ' == wch || '\t' == wch; }

    void next()
    {
        while (isspace(*m_pwszRunner)) m_pwszRunner++;
        char16* pwsz = m_wsz;
        if (0x22 != *m_pwszRunner)
        {
            while (0 != *m_pwszRunner)
            {
                if (isspace(*m_pwszRunner)) break;
                *pwsz++ = *m_pwszRunner++;
            } // while
        }
        else
        {
            m_pwszRunner++;
            while (0 != *m_pwszRunner)
            {
                if (0x22 == *m_pwszRunner)
                {
                    m_pwszRunner++;
                    break;
                }
                *pwsz++ = *m_pwszRunner++;
            } // while
        } // if
        *pwsz = 0;
    } // next
}; // EnumArg

} // namespace


//////////////////////////////////////////////////////////////////////
//
// Main Entry Point
//
int __cdecl
main(int, char*)
{
    HRESULT hr;

    char16 wszDll[MAX_PATH];
    wszDll[0] = 0;

    char16 wszImage[MAX_PATH];
    wszImage[0] = 0;

    foreach (EnumArg, oEnum, ::GetCommandLine())
    {
        if (::lstrcmpW(oEnum.Get(), L"-image") == 0)
        {
            oEnum.Next();
            if (! oEnum.AtEnd()) ::lstrcpyW(wszImage, oEnum.Get());
        }
        else if (::lstrcmpW(oEnum.Get(), L"-dll") == 0)
        {
            oEnum.Next();
            if (! oEnum.AtEnd()) ::lstrcpyW(wszDll, oEnum.Get());
        } // if
    } // for each arg

    if (*wszDll == 0)
    {
        ::GetModuleFileName(NULL, wszDll, lengthof(wszDll));
        char16* pwsz = wszDll + ::lstrlenW(wszDll);
        for (;;)
        {
            --pwsz;
            if ('.' == *pwsz) break;
            if (wszDll == pwsz) break;
        } // for

        ::lstrcpyW(pwsz + 1, L"dll");
    } // if

    HMODULE hDll = ::LoadLibrary(wszDll);

    if (hDll == NULL)
    {
        //if (::IsDebuggerPresent())
        {
            DWORD dwError = ::GetLastError();
            char16 wszMessage[256];
            ::wsprintf(wszMessage,
                L"Can't load lisp dll.\r\n"
                L"  File: %s\r\n"
                L"  Reason: %d",
                wszDll,
                dwError );
            ::MessageBoxW(NULL, wszMessage, k_wszTitle, MB_ICONWARNING);
        } // if
        return EXIT_FAILURE;
    } // if

    if (*wszImage == 0)
    {
        ::GetModuleFileName(hDll, wszImage, lengthof(wszImage));
        char16* pwsz = wszImage + ::lstrlenW(wszImage);
        for (;;)
        {
            --pwsz;
            if ('.' == *pwsz) break;
            if (wszImage == pwsz) break;
        } // for

        ::lstrcpyW(pwsz + 1, L"image");
    } // if

    GetEngineFn GetEngine = reinterpret_cast<GetEngineFn>(
            ::GetProcAddress(hDll, "GetEngine") );

    const size_t k_cbHeap = 1024 * 1024 * sizeof(void*) * 32;
    const size_t k_cbThread = 128 * 1024;

    ILispEngine* pIEngine = GetEngine();
    pIEngine->Init(k_cbHeap);

    hr = pIEngine->LoadImage(wszImage);
    if (FAILED(hr))
    {
        //if (::IsDebuggerPresent())
        {
            DWORD dwError = ::GetLastError();
            char16 wszMessage[256];
            ::wsprintf(wszMessage,
                L"Can't load lisp image.\r\n"
                L"  File: %s\r\n"
                L"  Reason: %d",
                wszImage,
                dwError );
            ::MessageBoxW(NULL, wszMessage, k_wszTitle, MB_ICONWARNING);
        } // if
        return EXIT_FAILURE;
    } // if

    pIEngine->Bless(k_cbThread, NULL);
    return pIEngine->Start(NULL);
} // main

extern "C"
{
int __cdecl mainCRTStartup()
{
    ::ExitProcess(main(0,NULL));
} // mainCRTStartup

} // extern "C"
