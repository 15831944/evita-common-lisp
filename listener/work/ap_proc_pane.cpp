#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - winapp - process window
// listener/winapp/winmain.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/console/console.cpp 12 2006-01-26 01:14:00 yosi $
//
#include "./ap_proc_pane.h"

#include "./ed_buffer.h"
#include "./ed_range.h"

#include "./vi_framewnd.h"
#include "./vi_selection.h"

enum MyWindowMessage
{
    MY_WM_OUTPUT = WM_USER, // 0x400
    MY_WN_ENDSUBPROC,       // 0x401
}; // MyWindowMessage

enum SenderMessage
{
    SENDER_WM_SEND  = WM_USER,
}; // SenderMessage


//////////////////////////////////////////////////////////////////////
//
// ProcessPane ctor
//
ProcessPane::ProcessPane(Edit::Buffer* pBuffer) :
    // Note: for cmd.exe, m_fEcho should be true.
    m_fEcho(false),
    m_hProcess(NULL),
    m_hSender(NULL),
    m_hStdInR(INVALID_HANDLE_VALUE),
    m_hStdInW(INVALID_HANDLE_VALUE),
    m_hStdOutR(INVALID_HANDLE_VALUE),
    m_hStdOutW(INVALID_HANDLE_VALUE),
    m_hReceiver(NULL),
    m_pOutput(pBuffer->CreateRange()),
    EditPane(pBuffer)
{
    // We don't accept user input until we receive output from
    // subprocess.
    pBuffer->SetReadOnly(true);
} // ProcessPane::ProcessPane


//////////////////////////////////////////////////////////////////////
//
// ProcessPane dtor
//
ProcessPane::~ProcessPane()
{
    if (NULL != m_hSender) ::CloseHandle(m_hSender);
    if (NULL != m_hProcess) ::CloseHandle(m_hProcess);
    if (INVALID_HANDLE_VALUE != m_hStdInR)  ::CloseHandle(m_hStdInR);
    if (INVALID_HANDLE_VALUE != m_hStdInW) ::CloseHandle(m_hStdInW);
    if (NULL != m_hReceiver) ::CloseHandle(m_hReceiver);
} // ProcessPane::~ProcessPane


//////////////////////////////////////////////////////////////////////
//
// ProcessPane::OnIdle
//
bool ProcessPane::OnIdle()
{
    return EditPane::OnIdle();
} // ProcessPane::OnIdle


//////////////////////////////////////////////////////////////////////
//
// ProcessPane::onMessage
//
LRESULT ProcessPane::onMessage(UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg)
    {
    case MY_WM_OUTPUT:
    {
        Edit::Buffer* pBuffer = GetBuffer();

        bool fReadOnly = pBuffer->SetReadOnly(false);

        {
            Edit::UndoBlock oUndo(pBuffer, L"Listener.Output");

            int cch = static_cast<int>(wParam);
            const char* pchStart = reinterpret_cast<char*>(lParam);
            const char* pchEnd = pchStart + cch;
            for (const char* pch = pchStart; pch < pchEnd; pch++)
            {
                char16 wch = *pch;
                if (wch == 0x0D) continue;

                m_pOutput->SetEnd(pBuffer->GetEnd());
                m_pOutput->Collapse(false);
                m_pOutput->SetText(&wch, 1);
            } // for

            m_pOutput->Collapse(false);
        }

        if (fReadOnly)
        {
            ::SendMessage(
                m_hwndFrame,
                FRAME_WM_SETSTATUS,
                0,
                reinterpret_cast<LPARAM>(L"Ready") );
        }

        Posn lPosn = m_pOutput->GetEnd();
        GetSelection()->SetStart(lPosn);
        GetSelection()->Collapse(false);

        // FIXME 2007-05-08 REVIEW How about do we call redraw instead of
        // OnIdle?
        if (! ::GetQueueStatus(QS_ALLEVENTS))
        {
            OnIdle();
        }
        return 0;
    } // MY_WM_OUTPUT

    case MY_WN_ENDSUBPROC:
        ::PostQuitMessage(0);
        return 0;

    case WM_CHAR:
    {
        char16 wch = static_cast<char16>(wParam);
        if (wch == 0x0D)
        {
            sendText();
            return 0;
        }
        else if (wch >= 0x20 && wch != 0x7F)
        {
            if (GetSelection()->GetStart() < m_pOutput->GetStart())
            {
                GetSelection()->SetRange(
                    GetBuffer()->GetEnd(),
                    GetBuffer()->GetEnd() );
            }
        } // if
        break;
    } // WM_CHAR

    case WM_KEYDOWN:
        DEBUG_PRINTF(L"WM_KEYUP: 0x%0x\n", wParam);
        switch (wParam & 0xFF)
        {
        case VK_DOWN:
            if (GetSelection()->GetStart() == m_pOutput->GetEnd() &&
                GetSelection()->GetEnd()   == m_pOutput->GetEnd() )
            {
                m_oHistory.Forward(GetSelection());
                return 0;
            } // if
            break;

        case VK_UP:
            if (GetSelection()->GetStart() == m_pOutput->GetEnd() &&
                GetSelection()->GetEnd()   == m_pOutput->GetEnd() )
            {
                m_oHistory.Backward(GetSelection());
                return 0;
            } // if
            break;
        } // switch
        break;

    case WM_KEYUP:
    {
        DEBUG_PRINTF(L"WM_KEYUP: 0x%0x\n", wParam);
        #if 0
            // recall history
            switch (wParam & 0xFF)
            {
            case VK_UP:
                if (::GetKeyState(VK_CONTROL) < 0)
                {
                }
                break;
            } // swtich key
        #endif
        break;
    } // WM_KEYUP
    } // switch uMsg
    return EditPane::onMessage(uMsg, wParam, lParam);
} // onMessage


//////////////////////////////////////////////////////////////////////
//
// ProcessPane::receiverProc
//
// Description:
//  This method receives output text from subprocess.
//
DWORD ProcessPane::receiverProc()
{
    for (;;)
    {
        DWORD cbRead;
        char rgch[4096];
        BOOL fSucceeded = ::ReadFile(
            m_hStdOutR,
            rgch,
            sizeof(rgch),
            &cbRead,
            NULL );
        if (! fSucceeded)
        {
            DWORD dwError = ::GetLastError();
            // ERROR_BROKEN_PIPE(109)
            if (dwError == ERROR_BROKEN_PIPE)
            {
                ::PostMessage(m_hwnd, MY_WN_ENDSUBPROC, 0, 0);
                return 0;
            }
            ASSERT(0 != dwError);
            DEBUG_PRINTF(L"ReadFile: %u\n", dwError);
        } // if

        DEBUG_PRINTF(L"cbRead=%u\n", cbRead);

        if (cbRead > 0)
        {
            ::SendMessage(
                m_hwnd,
                MY_WM_OUTPUT,
                cbRead,
                reinterpret_cast<LPARAM>(rgch) );
        } // if
    } // for
} // ProcessPane::receiverProc


//////////////////////////////////////////////////////////////////////
//
// ProcessPane::senderProc
//
// Description:
//  This method sends text input to subprocess.
//
DWORD ProcessPane::senderProc()
{
    for (;;)
    {
        MSG oMsg;
        int iGet = ::GetMessage(&oMsg, NULL, 0, 0);
        if (iGet <= 0) return 0;

        switch (oMsg.message)
        {
        case SENDER_WM_SEND:
        {
            Posn lStart = static_cast<Posn>(oMsg.wParam);
            Posn lEnd   = static_cast<Posn>(oMsg.lParam);

            Posn lPosn = lStart;
            while (lPosn < lEnd)
            {
                char rgch[1024];
                char* pch = rgch;
                while (lPosn < lEnd)
                {
                    if (pch == &rgch[lengthof(rgch)]) break;
                    *pch++ = static_cast<char>(GetBuffer()->GetCharAt(lPosn));
                    lPosn += 1;
                } // for

                m_pOutput->SetEnd(lPosn);
                m_pOutput->Collapse(false);

                GetSelection()->SetStart(lPosn);
                GetSelection()->Collapse(false);

                // This thread (sender) is blocked until subprocess consumes
                // pipe input.
                DWORD cbWritten;
                BOOL fSucceeded = ::WriteFile(
                    m_hStdInW,
                    rgch,
                    static_cast<DWORD>(pch - rgch),
                    &cbWritten,
                    NULL );
                if (! fSucceeded)
                {
                    DWORD dwError = ::GetLastError();
                    ASSERT(0 != dwError);
                    DEBUG_PRINTF(L"WriteFile stdin %u\n", dwError);
                } // if
            } // while
            break;
        } // SENDER_WM_SEND
        } // swtich message
    } // for
} // ProcessPane::senderProc


//////////////////////////////////////////////////////////////////////
//
// ProcessPane::sendText
//
// Description:
//  Sends characters between end of output to end of buffer to
//  subprocess.
//
void ProcessPane::sendText()
{
    // Listener doesn't accept user input until listener gets output
    // from subprocess.
    ::SendMessage(
        m_hwndFrame,
        FRAME_WM_SETSTATUS,
        0,
        reinterpret_cast<LPARAM>(L"Busy") );

    GetSelection()->SetRange(
        GetBuffer()->GetEnd(),
        GetBuffer()->GetEnd() );

    GetSelection()->SetText(L"\n", 1);

    Posn lStart = m_pOutput->GetEnd();
    Posn lEnd   = GetBuffer()->GetEnd();

    m_oHistory.Add(GetBuffer(), lStart, lEnd - 1);

    // We don't accept user input until we receive
    GetBuffer()->SetReadOnly(true);

    // Ask sender thread sending text to subprocess.
    ::PostThreadMessage(m_dwSender, SENDER_WM_SEND, lStart, lEnd);
} // ProcessPane::sendText


//////////////////////////////////////////////////////////////////////
//
// ProcessPane::Start
//
uint ProcessPane::Start(HWND hwndFrame, const char16* pwszCmdLine)
{
    BOOL fSucceeded;

    m_hwndFrame = hwndFrame;

    SECURITY_ATTRIBUTES oSA;
    oSA.nLength = sizeof(oSA);
    oSA.bInheritHandle = TRUE;
    oSA.lpSecurityDescriptor = NULL;

    fSucceeded = ::CreatePipe(&m_hStdInR, &m_hStdInW, &oSA, 0);
    if (! fSucceeded)
    {
        DWORD dwError = ::GetLastError();
        DEBUG_PRINTF(L"CreatePipe: %u\n", dwError);
        return dwError;
    } // if

    ::SetHandleInformation(m_hStdInW, HANDLE_FLAG_INHERIT, 0);

    fSucceeded = ::CreatePipe(&m_hStdOutR, &m_hStdOutW, &oSA, 0);
    if (! fSucceeded)
    {
        DWORD dwError = ::GetLastError();
        DEBUG_PRINTF(L"CreatePipe: %u\n", dwError);
        return dwError;
    } // if

    ::SetHandleInformation(m_hStdOutR, HANDLE_FLAG_INHERIT, 0);

    STARTUPINFO oStart;
    myZeroMemory(&oStart, sizeof(oStart));
    oStart.cb = sizeof(oStart);
    oStart.hStdInput  = m_hStdInR;
    oStart.hStdOutput = m_hStdOutW;
    oStart.dwFlags    = STARTF_USESTDHANDLES;

    oStart.hStdError = oStart.hStdOutput;

    char16 wszCmdLine[100];
    ::lstrcpyW(wszCmdLine, pwszCmdLine);

    PROCESS_INFORMATION oProcInfo;

    fSucceeded = ::CreateProcessW(
        NULL,
        wszCmdLine,
        NULL,   // lpProcessAttributes
        NULL,   // lpThreadAttributes,
        TRUE,   // fInheritHandles
        DETACHED_PROCESS,      // dwCreationFlags
        NULL,   // lpEnvironment
        NULL,   // lpCurrentDirectory
        &oStart,
        &oProcInfo );
    if (! fSucceeded)
    {
        DWORD dwError = ::GetLastError();
        DEBUG_PRINTF(L"CreateProcess: %u\n", dwError);
        return dwError;
    }

    ::CloseHandle(m_hStdInR);
    m_hStdInR = NULL;

    ::CloseHandle(m_hStdOutW);
    m_hStdOutW = NULL;

    ::CloseHandle(oProcInfo.hThread);
    ::CloseHandle(oProcInfo.hProcess);

    m_hSender = ::CreateThread(
        NULL,   // lpThreadAttributes
        0,      // dwStackSize
        senderProc_,
        this,   // lpParameter
        0,      // dwCreationFlags
        &m_dwSender );
    if (m_hSender == NULL)
    {
        DWORD dwError = ::GetLastError();
        DEBUG_PRINTF(L"CreateThread: %u\n", dwError);
        return dwError;
    }

    m_hReceiver = ::CreateThread(
        NULL,   // lpThreadAttributes
        0,      // dwStackSize
        receiverProc_,
        this,   // lpParameter
        0,      // dwCreationFlags
        NULL );
    if (m_hReceiver == NULL)
    {
        DWORD dwError = ::GetLastError();
        DEBUG_PRINTF(L"CreateThread: %u\n", dwError);
        return dwError;
    }

    return 0;;
} // ProcessPane::Start
