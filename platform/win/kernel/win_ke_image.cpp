#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Image loader/saver for Windows platform.
// platform/win/kernel/ke_image.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/kernel/win_ke_image.cpp#6 $
//
#include "../../../kernel/ke_memory.h"

#include "../../../kernel/ke_dll_link.h"
#include "../../../kernel/ke_sxhash.h"

#include "./win_ke_util.h"

#undef REPORT_HRESULT
#undef REPORT_WIN32_ERROR
#define REPORT_HRESULT      __noop
#define REPORT_WIN32_ERROR  (void)


namespace Kernel
{

extern void update_remembered_set();

//////////////////////////////////////////////////////////////////////
//
// Load Image
//
HRESULT
Memory::Load(LPCWSTR pwszImage)
{
    HRESULT hr;

    ASSERT(NULL != pwszImage);

    FileHandle shFile = CreateFileW(
            pwszImage,
            GENERIC_READ,
            0,
            NULL,
            OPEN_EXISTING,
            FILE_FLAG_SEQUENTIAL_SCAN,
            NULL );
        if (INVALID_HANDLE_VALUE == shFile.h)
        {
            DWORD dwError = ::GetLastError();
            REPORT_WIN32_ERROR("CreateFile", dwError);
            return HRESULT_FROM_WIN32(dwError);
        }

    DWORD cbImage = ::GetFileSize(shFile, NULL);

    if (INVALID_FILE_SIZE == cbImage)
    {
        DWORD dwError = ::GetLastError();
        REPORT_WIN32_ERROR("GetFileSize", dwError);
        return HRESULT_FROM_WIN32(dwError);
    }

    if (0 != cbImage % Param_AllocUnit)
    {
        hr = E_FAIL;
        REPORT_HRESULT("cbImage % Param_AllocUnit", hr);
        return hr;
    }

    BYTE* pbImage = reinterpret_cast<BYTE*>(::VirtualAlloc(
            sm_pbStart,
            Param_AllocUnit,
            MEM_COMMIT,
            PAGE_READWRITE ) );
        if (NULL == pbImage)
        {
            DWORD dwError = ::GetLastError();
            REPORT_WIN32_ERROR("VirtualAlloc MEM_COMMIT", dwError);
            Debugger::Fail(L"VirtualAlloc MEM_COMMIT");
            // NOTREACHED
        }

        sm_pbCommit = sm_pbStart + Param_AllocUnit;

    while (cbImage >= 1)
    {
        Area* pArea = reinterpret_cast<Area*>(pbImage);

        // Read the first unit of area
        {
            DWORD cbRest   = Param_AllocUnit;
            BYTE* pbRunner = pbImage;

            while (cbRest >= 1)
            {
                DWORD cbRead;
                BOOL fSucceeded = ::ReadFile(
                    shFile,
                    pbRunner,
                    Param_AllocUnit,
                    &cbRead,
                    NULL );
                if (! fSucceeded)
                {
                    DWORD dwError = ::GetLastError();
                    REPORT_WIN32_ERROR("ReadFile image", dwError);
                    Debugger::Fail(L"ReadFile image");
                    // NOTREACHED
                }

                pbRunner += cbRead;
                cbImage  -= cbRead;
                cbRest   -= cbRead;
            } // while
        } // read the first page

        DEBUG_PRINTF(
            L"Area: %p %06X: %8s alloc=%d/%d\r\n",
            pArea->m_pSelf,
            pArea->m_nFlags,
            pArea->GetTypeName(),
            pArea->m_ofsFree,
            pArea->m_cbArea );

        BYTE* pbArea = reinterpret_cast<BYTE*>(pArea->m_pSelf);

        {
            BYTE* pbCommit = pbArea + pArea->m_cbArea;

            if (cbImage + Param_AllocUnit > pArea->m_cbArea)
            {
                pbCommit += Param_AllocUnit;    // for next read
            }

            if (pbCommit > sm_pbCommit)
            {
                void* pvCommit =::VirtualAlloc(
                    sm_pbCommit,
                    pbCommit - sm_pbCommit,
                    MEM_COMMIT,
                    PAGE_READWRITE );
                if (NULL == pvCommit)
                {
                    DWORD dwError = ::GetLastError();
                    REPORT_WIN32_ERROR("VirtualAlloc MEM_COMMIT", dwError);
                    Debugger::Fail(L"VirtualAlloc MEM_COMMIT");
                    // NOTREACHED
                }

                sm_pbCommit = pbCommit;
            } // if
        }

        // Relocate area if needed
        if (pArea->m_pSelf != pArea)
        {
            ::CopyMemory(pbArea, pArea, Param_AllocUnit);

            {
                Area* pHole = pArea;
                    pHole->m_pSelf  = pHole;
                    pHole->m_cbArea = static_cast<UINT>(pbArea - pbImage);
                AddFreeArea(pHole);
            }

            pArea = reinterpret_cast<Area*>(pbArea);
        } // if

        pbImage = pbArea + Param_AllocUnit;

        // Read rest of pages of area
        {
            DWORD cbRest = static_cast<DWORD>(
                pArea->m_cbArea - Param_AllocUnit );

            while (cbRest >= 1)
            {
                DWORD cbRead;
                BOOL fSucceeded = ::ReadFile(
                    shFile,
                    pbImage,
                    cbRest,
                    &cbRead,
                    NULL );
                if (! fSucceeded)
                {
                    DWORD dwError = ::GetLastError();
                    REPORT_WIN32_ERROR("ReadFile image", dwError);
                    Debugger::Fail(L"ReadFile image");
                    // NOTREACHED
                }

                pbImage += cbRead;
                cbImage -= cbRead;
                cbRest  -= cbRead;
            } // while
        } // read rest

        pArea->m_pNext = NULL;
        pArea->m_pPrev = NULL;

        if (Area::ScanType_None == pArea->GetType())
        {
            Memory::AddFreeArea(pArea);
        }
        else
        {
            Memory::AddArea(pArea);
        }
    } // while

    foreach (Memory::EnumAreaAll, oEnum, Memory::GetStart())
    {
        Area* pArea = oEnum.Get();

        Area::ScanType eType = pArea->GetType();
        switch (eType)
        {
        case Area::ScanType_Function:
        {
            DWORD dwOldProtect;
            BOOL fSucceeded = ::VirtualProtect(
                pArea,
                pArea->m_cbArea,
                PAGE_EXECUTE_READ,
                &dwOldProtect );
            if (! fSucceeded)
            {
                DWORD dwError = ::GetLastError();
                REPORT_WIN32_ERROR("VirtualProtect", dwError);
                Debugger::Fail(L"VirtualProtect");
                // NOTREACHED
            }
            break;
        } // function

        case Area::ScanType_DllLink:
            pArea->StaticCast<DllLinkArea>()->Reinitialize();
            break;

        case Area::ScanType_HashTable:
            ObjTab::Init(pArea->StaticCast<ObjTab>());
            break;
        } // switch type
    } // for each area

    {
        DWORD dwOldProtect;
        BOOL fSucceeded = ::VirtualProtect(
            sm_pbStart,
            GetStart()->m_cbArea,
            PAGE_READONLY,
            &dwOldProtect );
        if (! fSucceeded)
        {
            DWORD dwError = ::GetLastError();
            REPORT_WIN32_ERROR("VirtualProtect", dwError);
            Debugger::Fail(L"VirtualProtect");
            // NOTREACHED
        }
    }

    Memory::ResetWriteWatch();

    DllRestart();

    return S_OK;
} // Memory::Load


//////////////////////////////////////////////////////////////////////
//
// Save Image
//
HRESULT
Memory::Save(HANDLE hOutput, size_t* out_cbImage)
{
    ASSERT(INVALID_HANDLE_VALUE != hOutput);

    // BUGBUG: NYI: Stop all threads
    // BUGBUG: NYI: Write image header

    // Update RS from Write watch
    update_remembered_set();

    size_t cbImage = 0;

    foreach (Memory::EnumAreaAll, oEnum, Memory::GetStart())
    {
        const Area* pArea = oEnum.Get();

        if (Area::ScanType_None == pArea->GetType()) continue;

        DWORD cbWritten;
        BOOL fSucceeded = WriteFile(
            hOutput,
            pArea,
            static_cast<DWORD>(pArea->m_cbArea),
            &cbWritten,
            NULL );

        if (! fSucceeded)
        {
            DWORD dwError = ::GetLastError();
            return HRESULT_FROM_WIN32(dwError);
        }

        cbImage += pArea->m_cbArea;
    } // for each area

    *out_cbImage = cbImage;

    return S_OK;
} // Memory::Save

} // Kernel
