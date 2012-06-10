#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - winapp - listener pane
// listener/winapp/ap_listener_pane.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/console/console.cpp 12 2006-01-26 01:14:00 yosi $
//
#include "./ap_listener_pane.h"

#include "./ed_buffer.h"
#include "./ed_range.h"

#include "./vi_framewnd.h"
#include "./vi_selection.h"
#include "./vi_style.h"

#include "./listener.h"

#include "../../platform/win/ILispEngine.h"

static ILispEngine* sm_pIEngine;
static Style* g_pStyleInput;

//////////////////////////////////////////////////////////////////////
//
// ListenerPane ctor
//
ListenerPane::ListenerPane(Edit::Buffer* pBuffer) :
    m_dwLisp(0),
    m_hDll(NULL),
    m_hLisp(NULL),
    m_pOutput(pBuffer->CreateRange()),
    m_pvLisp(NULL),
    EditPane(pBuffer)
{
    // We don't accept user input until we receive output from
    // subprocess.
    pBuffer->SetReadOnly(true);

    if (g_pStyleInput == NULL)
    {
        FontSet* pFontSet = new FontSet;

        foreach (
            FontSet::EnumFont,
            oEnum,
            g_pDefaultStyle->GetFontSet() )
        {
            Font* pFont = oEnum.Get();
            LOGFONT oLogFont = *pFont->GetLogFont();
            oLogFont.lfItalic = TRUE;
            pFontSet->Add(Font::Create(&oLogFont));
        } // for each font


        g_pStyleInput = new Style(
            Color(0, 0, 0x99),
            g_pDefaultStyle->GetBackground() );
        g_pStyleInput->SetFontSet(pFontSet);
    } // if
} // ListenerPane::ListenerPane


//////////////////////////////////////////////////////////////////////
//
// ListenerPane dtor
//
ListenerPane::~ListenerPane()
{
    if (m_hLisp != NULL) ::CloseHandle(m_hLisp);
} // ListenerPane::~ListenerPane


//////////////////////////////////////////////////////////////////////
//
// ListenerPane::lispProc
//
DWORD ListenerPane::lispProc()
{
    const size_t k_cbThread = 128 * 1024;

    ListenerInfo oInfo;
    oInfo.m_hwndListener = m_hwnd;

    m_pvLisp = sm_pIEngine->Bless(k_cbThread, &oInfo);

    return sm_pIEngine->Start(NULL);
} // Listener::lispProc


//////////////////////////////////////////////////////////////////////
//
// ListenerPane::OnIdle
//
bool ListenerPane::OnIdle()
{
    return EditPane::OnIdle();
} // ListenerPane::OnIdle


//////////////////////////////////////////////////////////////////////
//
// ListenerPane::onMessage
//
LRESULT ListenerPane::onMessage(UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg)
    {
    case LISTENER_WM_QUERYCOLUMN:
    {
        return GetColumn(m_pOutput->GetEnd());
    } // LISTENER_WM_QUERYCOLUMN

    case LISTENER_WM_SENDTEXT:
    {
        // Text from lisp.
        const char16* pwch = reinterpret_cast<char16*>(lParam);
        uint cwch = static_cast<uint>(wParam);

        Edit::Buffer* pBuffer = GetBuffer();

        pBuffer->SetReadOnly(false);

        {
            Edit::UndoBlock oUndo(pBuffer, L"Listener.Output");

            m_pOutput->SetText(pwch, cwch);
            m_pOutput->Collapse(false);
        }

        Posn lPosn = m_pOutput->GetEnd();
        GetSelection()->SetStart(lPosn);
        GetSelection()->Collapse(false);

        ::ReplyMessage(0);
        //::Sleep(0);

        // FIXME 2007-05-08 REVIEW How about do we call redraw instead of
        // OnIdle?
        uint nFlags = QS_ALLEVENTS;
        nFlags |= QS_SENDMESSAGE;
        if (! ::GetQueueStatus(nFlags))
        {
            OnIdle();
        }
        return 0;
    } // LISTENER_WM_SENDTEXT

    case LISTENER_WN_READY:
        GetBuffer()->SetReadOnly(false);

        ::SendMessage(
            m_hwndFrame,
            FRAME_WM_SETSTATUS,
            0,
            reinterpret_cast<LPARAM>(L"Ready") );
        break;

    case LISTENER_WN_RECEIVETEXT:
    {
        Count cwch = static_cast<Count>(wParam);
        m_oSendInfo.m_lPosn += cwch;
        sendTextAux();
        return 0;
    } // LISTENER_WN_RECEIVETEXT

    case WM_CHAR:
    {
        char16 wch = static_cast<char16>(wParam);
        if (wch == 0x0D)
        {
            sendText();
            return 0;
        }
        else if (wch == 0x03)
        {
            if (GetSelection()->GetType() == Selection_None )
            {
                if (m_pvLisp != NULL)
                {
                    sm_pIEngine->Interrupt(m_pvLisp);
                    return 0;
                } // if
            } // if
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
        DEBUG_PRINTF(L"WM_KEYDOWN: 0x%0x\n", wParam);
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
// ListenerPane::sendText
//
// Description:
//  Sends characters between end of output to end of buffer to
//  subprocess.
//
void ListenerPane::sendText()
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

    // -1 for Newline.
    m_oHistory.Add(GetBuffer(), lStart, lEnd - 1);

    GetBuffer()->SetStyle(lStart, lEnd - 1, g_pStyleInput);

    // We don't accept user input until we receive
    GetBuffer()->SetReadOnly(true);

    m_pOutput->SetEnd(lEnd);
    m_pOutput->Collapse(false);

    GetSelection()->SetStart(lEnd);
    GetSelection()->Collapse(false);

    m_oSendInfo.m_lEnd  = lEnd;
    m_oSendInfo.m_lPosn = lStart;

    sendTextAux();
} // ListenerPane::sendText


// ListenerPane::sendTextAux
void ListenerPane::sendTextAux()
{
    const Count k = lengthof(m_rgwch);
    Posn lPosn = m_oSendInfo.m_lPosn;
    Posn lEnd  = min(m_oSendInfo.m_lEnd,  lPosn + k);

    if (lEnd == lPosn) return;

    GetBuffer()->GetText(m_rgwch, lPosn, lEnd);

    #if _DEBUG
    {
        m_rgwch[lEnd - lPosn] = 0;
        DEBUG_PRINTF(L"%s\n", m_rgwch);
    }
    #endif // _DEBUG

    // Ask sender thread sending text to subprocess.
    ::PostThreadMessage(
        m_dwLisp,
        LISTENER_WM_SENDTEXT,
        lEnd - lPosn,
        reinterpret_cast<LPARAM>(m_rgwch) );
} // ListenerPane::sendTextAux


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
// ListenerPane::Start
//
uint ListenerPane::Start(HWND hwndFrame, const char16*)
{
    m_hwndFrame = hwndFrame;

    if (sm_pIEngine == NULL)
    {
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

        m_hDll = ::LoadLibrary(wszDll);

        if (m_hDll == NULL)
        {
            DWORD dwError = ::GetLastError();
            char16 wsz[100];
            ::wsprintf(wsz, L"LoadLibrary %s %u", wszDll, dwError);
            ::MessageBox(NULL, wsz, L"Listener", 0);
            return 0;
        } // if

        GetEngineFn GetEngine = reinterpret_cast<GetEngineFn>(
            ::GetProcAddress(m_hDll, "GetEngine") );
        if (GetEngine == NULL)
        {
            DWORD dwError = ::GetLastError();
            char16 wsz[100];
            ::wsprintf(wsz, L"GetProcAddress GetEngine %u", dwError);
            ::MessageBox(NULL, wsz, L"Listener", 0);
            return 0;
        } // if

        sm_pIEngine = GetEngine();

        const size_t k_cbHeap = 1024 * 1024 * sizeof(void*) * 32;
        sm_pIEngine->Init(k_cbHeap);

        if (*wszImage == 0)
        {
            ::GetModuleFileName(m_hDll, wszImage, lengthof(wszImage));
            char16* pwsz = wszImage + ::lstrlenW(wszImage);
            for (;;)
            {
                --pwsz;
                if ('.' == *pwsz) break;
                if (wszImage == pwsz) break;
            } // for

            ::lstrcpyW(pwsz + 1, L"image");
        } // if

        HRESULT hr = sm_pIEngine->LoadImage(wszImage);
        if (FAILED(hr))
        {
            char16 wsz[100];
            ::wsprintf(wsz, L"LoadImage %s 0x%08X", wszImage, hr);
            ::MessageBox(NULL, wsz, L"Listener", 0);
            return 0;
        } // if
    } // if

    m_hLisp = ::CreateThread(
        NULL,   // lpThreadAttributes
        0,      // dwStackSize
        lispProc_,
        this,   // lpParameter
        0,      // dwCreationFlags
        &m_dwLisp );
    if (m_hLisp == NULL)
    {
        DWORD dwError = ::GetLastError();
        DEBUG_PRINTF(L"CreateThread: %u\n", dwError);
        return dwError;
    }

    return 0;
} // ListenerPane::Start
