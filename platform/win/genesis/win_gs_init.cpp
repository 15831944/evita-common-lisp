#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - initialization
// genesis/gs_init.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/genesis/win_gs_init.cpp#4 $
//
// Description:
//  Installs following C implemented lisp function for genesis.
//
#include "../../kernel/ke_executive.h"
#include "../../kernel/ke_dll_link.h"
#include "../../kernel/ke_memory.h"

#include "../../genesis/gs_init.h"
#include "../ILispEngine.h"

namespace Genesis
{

int Toplevel();


// Initializer::init_Platform
void Initializer::init_Platform()
{
    install_function(
        Q("GET-COMMAND-LINE"), 0, 0,
        L"get_command_line" );

    install_function(
        Q(".CREATE-DIRECTORY"), 1, 1,
        L"create_directory" );

    install_function(
        Q(".CURRENT-DIRECTORY"), 0, 1,
        L"current_directory" );

    install_function(
        list(Qsetf, Q(".CURRENT-DIRECTORY")), 1, 2,
        L"setf_current_directory" );

    install_function(
        Q(".DELETE-FILE"), 1, 1,
        L"delete_file" );

    install_function(
        Q(".FILE-ATTRIBUTES"), 1, 1,
        L"file_attributes", 5 );

    install_function(
        Q(".FIND-CLOSE"), 1, 1,
        L"find_close" );

    install_function(
        Q(".FIND-FIRST-FILE"), 1, 1,
        L"find_first_file", 3 );

    install_function(
        Q(".FIND-NEXT-FILE"), 1, 1,
        L"find_next_file", 3 );

    install_function(
        Q(".REMOVE-DIRECTORY"), 1, 1,
        L"remove_directory" );

    install_function(
        Q(".RENAME-FILE"), 2, 2,
        L"rename_file" );
} // Initializer::init_Platform

} // Genesis


//////////////////////////////////////////////////////////////////////
//
// Main Entry Point
//
extern "C" BOOL WINAPI
DllMain(
    HANDLE      hinstDll,
    DWORD       dwReason,
    void*       ) // pvReserved )
{
    switch (dwReason)
    {
    case DLL_PROCESS_ATTACH:
        ::DisableThreadLibraryCalls(reinterpret_cast<HMODULE>(hinstDll));
        Kernel::g_hSelf = reinterpret_cast<HMODULE>(hinstDll);
        break;
    } // switch dwReason
    return TRUE;
} // DllMain

class LispEngine : public ILispEngine
{
    virtual void* Bless(size_t, void*);
    virtual int Init(size_t);
    virtual int Interrupt(void*);
    virtual int LoadImage(const char16*);
    virtual int Start(void*);
}; // LispEngine

LispEngine oLispEngine;


//////////////////////////////////////////////////////////////////////
//
// LispEngine::Bless
//
void* LispEngine::Bless(size_t cbThread, void*)
{
    Kernel::Thread* pThread =
        new(Kernel::Memory::AllocThread(cbThread))
            Kernel::Thread();

    pThread->Init(cbThread);

    return pThread;
} // LispEngine::Bless


//////////////////////////////////////////////////////////////////////
//
// LispEngine::Init
//
int LispEngine::Init(size_t cbHeap)
{
    Kernel::Executive::Start(cbHeap);

    return 0;
} // LispEngine::Init


//////////////////////////////////////////////////////////////////////
//
// LispEngine::Interrupt
//
int LispEngine::Interrupt(void* pvThread)
{
    reinterpret_cast<Kernel::Thread*>(pvThread)->
        Interrupt(Qkeyboard_interrupt);

    return 0;
} // LispEngine::Interrupt


//////////////////////////////////////////////////////////////////////
//
// LispEngine::LoadImage
//
int LispEngine::LoadImage(const char16* pwszImage)
{
    HRESULT hr;

    hr = Kernel::Memory::Load(pwszImage);
    if (FAILED(hr)) return hr;

    // Make FunObj Area Writable

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
                PAGE_EXECUTE_READWRITE,
                &dwOldProtect );
            if (! fSucceeded)
            {
                DWORD dwError = ::GetLastError();
                //REPORT_WIN32_ERROR("VirtualProtect", dwError);
                Debugger::Fail(L"VirtualProtect", dwError);
                // NOTREACHED
            }
            break;
        } // function
        } // switch type
    } // for each area

    return 0;
} // LispEngine::LoadImage


//////////////////////////////////////////////////////////////////////
//
// LispEngine::Start
//
int LispEngine::Start(void*)
{
    return Genesis::Toplevel();
} // LispEngine::Start


ILispEngine* __fastcall GetEngine()
{
    return &oLispEngine;
} // GetEngine
