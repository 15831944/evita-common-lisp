#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - DLL Link
// kernel/ke_dll_link.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_dll_link.cpp#4 $
//
#include "./ke_dll_link.h"

#include "../mini/mini_lisp.h"

namespace Kernel
{

HMODULE g_hSelf;

//////////////////////////////////////////////////////////////////////
//
// DllResolve
//
void* __fastcall
DllResolve(Thread* pThread, DllEntry* pEntry)
{
    ASSERT(NULL != pThread);
    ASSERT(NULL != pEntry);

    DllProcInfo* pProcInfo = pEntry->m_proc_info->
        Decode<DllProcInfo>();

    DllFileInfo* pFileInfo = pProcInfo->m_file_info->
        Decode<DllFileInfo>();

    if (NULL == pFileInfo->m_handle)
    {
        HMODULE hModule = ::LoadLibraryW(
            pFileInfo->m_filename->Decode<SimpleString>()->m_rgwchElement );

        if (NULL == hModule)
        {
            goto error;
        } // if

        pFileInfo->m_handle = Fixnum::Encode(hModule);
    } // if not loaded

    // Get Procedure Address
    {
        CHAR szProcName[100];
        {
            LPCWSTR pwchStart = pProcInfo->m_proc_name->
                Decode<SimpleString>()->GetElements();

            LPCWSTR pwchEnd = Fixnum::Decode_(pProcInfo->m_proc_name->
                Decode<SimpleString>()->m_length ) + pwchStart;

            CHAR* pszRunner = szProcName;
            for (
                LPCWSTR pwchRunner = pwchStart;
                pwchRunner < pwchEnd;
                pwchRunner++ )
            {
                *pszRunner++ = static_cast<CHAR>(*pwchRunner);
            } // for

            *pszRunner = 0;
        } // szProcName

        pEntry->m_pfn = ::GetProcAddress(
            reinterpret_cast<HMODULE>(pFileInfo->m_handle),
            szProcName );

        if (NULL == pEntry->m_pfn)
        {
            goto error;
        }

        return pEntry->m_pfn;
    } // GetProcAddress

  error:
    {
        DWORD dwError = ::GetLastError();
        error(Qdll_link_error,
            Kfilename, pFileInfo->m_filename,
            Kname, pProcInfo->m_proc_name,
            Kcode, Fixnum::Encode(static_cast<uint32>(dwError)) );
    } // error
} // DllResolver


//////////////////////////////////////////////////////////////////////
//
// DllRestart
//
void
DllRestart()
{
    foreach (EnumHashTable, oEnum, VAR(Adll_file_tableA))
    {
        DllFileInfo* pFileInfo = oEnum.GetVal()->Decode<DllFileInfo>();
        pFileInfo->m_handle = Fixnum::Encode(0);
    } // for each key

    QQself_file_info->Decode<DllFileInfo>()->m_handle =
        Fixnum::Encode(g_hSelf);
} // DllRestart


//////////////////////////////////////////////////////////////////////
//
// DllLinkArea::Reinitialize
//
// Called by:
//  Memory::Load
//
void
DllLinkArea::Reinitialize()
{
    foreach (EnumEntry, oEnum, this)
    {
        DllEntry* pEntry = oEnum.Get();
        pEntry->m_pfn = DllLinkStab;
    } // for each entry
} // DllLinkAreaReinitialize

} // Kernel
