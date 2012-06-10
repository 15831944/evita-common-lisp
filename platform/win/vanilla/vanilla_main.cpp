#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - initialization
// platform/win/vanilla/vanilla_main.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/vanilla/vanilla_main.cpp#1 $
//
// Description:
//  Installs following C implemented lisp function for genesis.
//
#include "../../kernel/ke_executive.h"
#include "../../kernel/ke_dll_link.h"
#include "../../kernel/ke_memory.h"
#include "../../kernel/ke_thread.h"

#include "../ILispEngine.h"

namespace MiniLisp
{
    Val bind_standard_streams();
} // MiniLisp

using namespace MiniLisp;

void bind_listener_streams(void*);

HINSTANCE g_hInstance;
HINSTANCE g_hResource;


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
    virtual int   Init(size_t);
    virtual int   Interrupt(void*);
    virtual int   LoadImage(const char16*);
    virtual int   Start(void*);
}; // LispEngine

LispEngine oLispEngine;

//////////////////////////////////////////////////////////////////////
//
// LispEngine::Bless
//
void* LispEngine::Bless(size_t cbThread, void* pv)
{
    Kernel::Thread* pThread =
        new(Kernel::Memory::AllocThread(cbThread))
            Kernel::Thread();

    pThread->Init(cbThread);


    if (NULL == pv)
    {
        bind_standard_streams();
    }
    else
    {
        bind_listener_streams(pv);
    } // if

    return pThread;
} // LispEngine::Bless


//////////////////////////////////////////////////////////////////////
//
// LispEngine::Init
//
int LispEngine::Init(size_t cbHeap)
{
    Kernel::Executive::Start(cbHeap);

    g_hInstance = Kernel::g_hSelf;
    g_hResource = Kernel::g_hSelf;

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
    Val fn = Qstart_application->Decode<Symbol>()->m_function;
    if (fn == nil) return 1;

    Thread* p = Thread::Get();
    p->m_fn = fn;
    p->m_n = Fixnum::Encode(1);
    p->mv_value[0] = nil;
    p->Start();
} // LispEngine::Start


ILispEngine* __fastcall GetEngine()
{
    return &oLispEngine;
} // GetEngine
