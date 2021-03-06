#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - editor - IoManager
// listener/winapp/IoManager.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_IoManager.cpp#3 $
//
#include "./vi_IoManager.h"

#include "./ed_Mode.h"

#include "./vi_Application.h"
#include "./vi_Buffer.h"
#include "./vi_Frame.h"
#include "./vi_EditPane.h"
#include "./vi_Listener.h"
#include "./vi_Selection.h"

#include "./listener.h"

HANDLE g_hEvent;
bool   g_fMultiple;

namespace
{

// FinishLoadParam
struct FinishLoadParam
{
    NewlineMode     m_eNewline;
    FILETIME        m_ftLastWrite;
    uint            m_nError;
    uint            m_nFileAttrs;
    Buffer*         m_pBuffer;
    char16          m_wszFileName[MAX_PATH];

    static void Run(LPARAM lParam)
    {
        reinterpret_cast<FinishLoadParam*>(lParam)->run();
    } // Run

    void run()
    {
        if (ERROR_HANDLE_EOF == m_nError)
        {
            m_pBuffer->SetFile(m_wszFileName, &m_ftLastWrite);
            if (NewlineMode_Detect == m_pBuffer->GetNewline())
            {
                m_pBuffer->SetNewline(m_eNewline);
            }

            if (m_nFileAttrs & FILE_ATTRIBUTE_READONLY)
            {
                m_pBuffer->SetReadOnly(true);
            }

            // Make EOF not error
            m_nError = 0;
        } // if

        if (0 == m_nError)
        {
            Edit::ModeFactory* pModeFactory = Edit::ModeFactory::Get(m_pBuffer);

            Edit::Mode* pMode = m_pBuffer->GetMode();
            if (pMode->GetClass() != pModeFactory)
            {
                m_pBuffer->SetMode(pModeFactory->Create(m_pBuffer));
            }

            for (auto& window: m_pBuffer->windows())
                window.GetSelection()->RestoreForReload();

            m_pBuffer->GetMode()->DoColor(m_pBuffer->GetEnd());
        } // if

        m_pBuffer->FinishIo(m_nError);
    } // run
}; // FinishLoadParam


// FinishSaveParam
struct FinishSaveParam
{
    NewlineMode     m_eNewline;
    FILETIME        m_ftLastWrite;
    uint            m_nError;
    uint            m_nFileAttrs;
    Buffer*         m_pBuffer;
    char16          m_wszFileName[MAX_PATH];

    static void Run(LPARAM lParam)
    {
        reinterpret_cast<FinishSaveParam*>(lParam)->run();
    } // Run

    void run()
    {
        if (0 == m_nError)
        {
            m_pBuffer->SetFile(m_wszFileName, &m_ftLastWrite);
        } // if

        m_pBuffer->FinishIo(m_nError);
    } // run
}; // FinishSaveParam


//////////////////////////////////////////////////////////////////////
//
// InsertStringParam
//
struct InsertStringParam
{
    Count           m_cwch;
    Posn            m_lPosn;
    Edit::Buffer*   m_pBuffer;
    const char16*   m_pwch;

    static void Run(LPARAM lParam)
    {
        reinterpret_cast<InsertStringParam*>(lParam)->run();
    } // Run

    void run()
    {
        m_pBuffer->InternalInsert(
            m_lPosn,
            m_pwch,
            m_cwch );
    } // Message_InsertString
}; // InsertStringParam

enum Message
{
    Message_Start = LISTENER_WM_LIMIT,

    Message_FinishLoad,
    Message_FinishSave,
    Message_InsertString,
}; // Message

} // namespace


//////////////////////////////////////////////////////////////////////
//
// IoManager::FinishLoad
//
void
IoManager::FinishLoad(
    Buffer*             pBuffer,
    const char16*       pwszFileName,
    uint                nError,
    NewlineMode         eNewline,
    uint                nFileAttrs,
    const FILETIME*     pftLastWrite )
{
    FinishLoadParam oParam;
    oParam.m_eNewline     = eNewline;
    oParam.m_ftLastWrite  = *pftLastWrite;
    oParam.m_nError       = nError;
    oParam.m_nFileAttrs   = nFileAttrs;
    oParam.m_pBuffer      = pBuffer;

    ::lstrcpyW(oParam.m_wszFileName, pwszFileName);

    ::SendMessage(
        *Application::Get()->GetIoManager(),
        Message_FinishLoad,
        0,
        reinterpret_cast<LPARAM>(&oParam) );
} // IoManager::FinishLoad


//////////////////////////////////////////////////////////////////////
//
// IoManager::FinishSave
//
void
IoManager::FinishSave(
    Buffer*             pBuffer,
    const char16*       pwszFileName,
    uint                nError,
    NewlineMode         eNewline,
    uint                nFileAttrs,
    const FILETIME*     pftLastWrite )
{
    FinishSaveParam oParam;
    oParam.m_eNewline     = eNewline;
    oParam.m_ftLastWrite  = *pftLastWrite;
    oParam.m_nError       = nError;
    oParam.m_nFileAttrs   = nFileAttrs;
    oParam.m_pBuffer      = pBuffer;

    ::lstrcpyW(oParam.m_wszFileName, pwszFileName);

    ::SendMessage(
        *Application::Get()->GetIoManager(),
        Message_FinishSave,
        0,
        reinterpret_cast<LPARAM>(&oParam) );
} // IoManager::FinishSave


//////////////////////////////////////////////////////////////////////
//
// IoManager::InsertString
//
void
IoManager::InsertString(
    Buffer*         pBuffer,
    Posn            lPosn,
    const char16*   pwch,
    Count           cwch )
{
    InsertStringParam oParam;
    oParam.m_cwch    = cwch;
    oParam.m_lPosn   = lPosn;
    oParam.m_pBuffer = pBuffer;
    oParam.m_pwch    = pwch;

    ::SendMessage(
        *Application::Get()->GetIoManager(),
        Message_InsertString,
        0,
        reinterpret_cast<LPARAM>(&oParam) );
} // IoManager::InsertString


//////////////////////////////////////////////////////////////////////
//
// IoManager::onMessage
//
LRESULT
IoManager::onMessage(UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg)
    {
    case WM_CREATE:
    {
        when (g_fMultiple) break;

        HANDLE hMapping = ::CreateFileMapping(
            INVALID_HANDLE_VALUE,   // hFile
            NULL,                   // lpAttributes
            PAGE_READWRITE,         // flProtect
            0,                      //  dwMaximumSizeHigh
            k_cbFileMapping,        // dwMaximumSizeLow
            k_wszFileMapping );     // lpName
            
        ASSERT(NULL != hMapping);

        SharedArea* p = reinterpret_cast<SharedArea*>(::MapViewOfFile(
            hMapping,
            FILE_MAP_READ | FILE_MAP_WRITE,
            0,      // dwFileOffsetHigh
            0,      // dwFileOffsetLow
            k_cbFileMapping ) );

        if (NULL != p)
        {
            p->m_hwnd = *this;
        }

        ::SetEvent(g_hEvent);
        break;
    } // WM_CREATE

    case WM_COPYDATA:
    {
        COPYDATASTRUCT* p = reinterpret_cast<COPYDATASTRUCT*>(lParam);
        if (0 == p->dwData)
        {
            ListenerText* q = reinterpret_cast<ListenerText*>(p->lpData);
            Listener* pListener = reinterpret_cast<Listener*>(q->m_pvListener);
            pListener->OnReceiveText(
                reinterpret_cast<char16*>(q + 1),
                q->m_cwch );
        }
        else
        {
            visitFile(reinterpret_cast<char16*>(p->lpData));
        }
        return true;
    } // WM_COPYDATA

    case LISTENER_WM_QUERYCOLUMN:
    {
        Listener* pListener= reinterpret_cast<Listener*>(lParam);
        return pListener->QueryColumn();
    } // LISTENER_WM_QUERYCOLUMN

    case LISTENER_WN_READY:
    {
        Listener* pListener= reinterpret_cast<Listener*>(lParam);
        pListener->OnReady();
        return 0;
    } // LISTENER_WM_READY

    case LISTENER_WN_RECEIVETEXT:
    {
        Listener* pListener= reinterpret_cast<Listener*>(lParam);
        pListener->OnSentText(static_cast<Count>(wParam));
        return 0;
    } // LISTENER_WM_RECEIVETEXT

    case Message_FinishLoad:
        FinishLoadParam::Run(lParam);
        return 0;

    case Message_FinishSave:
        FinishSaveParam::Run(lParam);
        return 0;

    case Message_InsertString:
        InsertStringParam::Run(lParam);
        return 0;
    } // switch message

    return ::DefWindowProc(m_hwnd, uMsg, wParam, lParam);
} // IoManager::onMessage


//////////////////////////////////////////////////////////////////////
//
// IoManager::Realize
//
void IoManager::Realize()
{
    CreateWindowEx(0, NULL, 0, HWND_MESSAGE);
    ASSERT(NULL != *this);
} // IoManager::Realize


//////////////////////////////////////////////////////////////////////
//
// IoManager::visitFile
//
void IoManager::visitFile(const char16* pwsz)
{
    Buffer* pBuffer = Application::Get()->Load(pwsz);

    Pane* pPane = NULL;
    Frame* pFrame = Application::Get()->GetActiveFrame();
    for (auto& pane: pFrame->panes()) {
        auto const pPresent = pane.DynamicCast<EditPane>();
        if (NULL == pPresent)
        {
            continue;
        }

        if (pPresent->GetBuffer() == pBuffer)
        {
            pPane = pPresent;
            break;
        }
    } // for each pane

    if (NULL == pPane)
    {
        pPane = new EditPane(pBuffer);
        pFrame->AddPane(pPane);
    }

    pPane->Activate();
    ::SetForegroundWindow(*pPane->GetFrame());
} // IoManager::visitFile
