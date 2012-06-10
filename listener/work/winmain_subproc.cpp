#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - winapp - winmain
// listener/winapp/winmain.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/console/console.cpp 12 2006-01-26 01:14:00 yosi $
//
#define DEBUG_IDLE 0
#include "./ed_buffer.h"
#include "./ap_proc_pane.h"
#include "./vi_framewnd.h"

#pragma comment(lib, "comctl32.lib")

//#define SUBPROCESS L"\\bin\\evcl3.com --interactive"
#define SUBPROCESS L"evcl3.com --interactive"
//#define SUBPROCESS L"cmd.exe"

#if defined(SUBPROCESS)
    typedef ProcessPane MyPane;
#else // defined(SUBPROCESS)
    typedef EditPane MyPane;
#endif // defined(SUBPROCESS)



HINSTANCE g_hInstance;
HINSTANCE g_hResource;

// MainWindow
class MainWindow : public BaseWindow
{
    MyPane*         m_pEditPane;
    Edit::Buffer*   m_pBuffer;
    HWND            m_hwndStatusBar;
    int             m_cyStatusBar;

    public: MainWindow() :
        m_pEditPane(NULL),
        m_pBuffer(NULL) {}

    public: bool OnIdle()
    {
        return m_pEditPane->OnIdle();
    } // OnIdle

    public: void Start()
    {
        m_pBuffer = new Edit::Buffer;

        m_pEditPane = new MyPane(m_pBuffer);

        const int cColumns = 100;
        const int cRows    = 50;

        Font* pFont = g_pDefaultStyle->GetFontSet()->FindFont('x');
        int cx = pFont->GetWidth() * cColumns;
        int cy = pFont->GetHeight() * cRows;

        DWORD dwExStyle =
            WS_EX_APPWINDOW |
            WS_EX_WINDOWEDGE;

        DWORD dwStyle =
            WS_OVERLAPPEDWINDOW |
            WS_VISIBLE;

        // See WM_GETMINMAXINFO
        CreateWindowEx(
            dwExStyle,
            L"This is Window Text.",
            dwStyle,
            NULL,
            cx, cy );

        ::SendMessage(
            m_hwndStatusBar,
            SB_SETTEXT,
            SBT_NOBORDERS,
            reinterpret_cast<LPARAM>(L"Initializing...") );

        m_pEditPane->Start(m_hwnd, SUBPROCESS);
    } // Start

    protected: virtual LRESULT onMessage(
        UINT    uMsg,
        WPARAM  wParam,
        LPARAM  lParam )
    {
        switch (uMsg)
        {
        case FRAME_WM_SETSTATUS:
            ::SendMessage(
                m_hwndStatusBar,
                SB_SETTEXT,
                SBT_NOBORDERS,
                lParam );
            break;

        case WM_CREATE:
        {
            m_hwndStatusBar = ::CreateWindowExW(
                0,
                STATUSCLASSNAMEW,
                NULL,
                WS_CHILD | WS_VISIBLE,
                0, 0, 0, 0,
                m_hwnd,
                reinterpret_cast<HMENU>(1),
                g_hInstance,
                NULL );
            {
                RECT rc;
                ::GetWindowRect(m_hwndStatusBar, &rc);
                m_cyStatusBar = rc.bottom - rc.top;
            }

            m_pEditPane->CreateWindowEx(
                0,
                NULL,
                WS_CHILD | WS_VISIBLE | WS_VSCROLL,
                m_hwnd );
            break;
        } // WM_CREATE

        case WM_DESTROY:
            ::PostQuitMessage(0);
            break;

        case WM_SETFOCUS:
            ::SetFocus(*m_pEditPane);
            break;

        case WM_WINDOWPOSCHANGED:
        {
            // DefWindowProc sents WM_SIZE and WM_MOVE, so handling
            // WM_WINDPOSCHANGED is faster than DefWindowProc.
            //
            // #define SWP_NOSIZE          0x0001
            // #define SWP_NOMOVE          0x0002
            // #define SWP_NOZORDER        0x0004
            // #define SWP_NOREDRAW        0x0008
            // #define SWP_NOACTIVATE      0x0010
            // #define SWP_FRAMECHANGED    0x0020  /* The frame changed: send WM_NCCALCSIZE */
            // #define SWP_SHOWWINDOW      0x0040
            // #define SWP_HIDEWINDOW      0x0080
            // #define SWP_NOCOPYBITS      0x0100
            // #define SWP_NOOWNERZORDER   0x0200  /* Don't do owner Z ordering */
            // #define SWP_NOSENDCHANGING  0x0400  /* Don't send WM_WINDOWPOSCHANGING */
            //
            // #define SWP_DRAWFRAME       SWP_FRAMECHANGED
            // #define SWP_NOREPOSITION    SWP_NOOWNERZORDER
            //
            //#if(WINVER >= 0x0400)
            // #define SWP_DEFERERASE      0x2000
            // #define SWP_ASYNCWINDOWPOS  0x4000
            //#endif /* WINVER >= 0x0400 */

            // Initial  0x10001843
            // Minimize 0x8130
            // Restore  0x8124
            // Move     0x0A15
            //if (wp->flags & SWP_NOSIZE) return 0;

            const WINDOWPOS* wp = reinterpret_cast<WINDOWPOS*>(lParam);

            if (wp->flags & SWP_HIDEWINDOW)
            {
                // We don't take care hidden window.
                return 0;
            }

            if (::IsIconic(m_hwnd))
            {
                // We don't take care miminize window.
                return 0;
            }

            RECT rc;
            ::GetClientRect(m_hwnd, &rc);

            #if 1
            {
                ASSERT(NULL != wp);
                DEBUG_PRINTF(L"WM_WINDOWPOSCHANGED:"
                    L" flags=0x%0X (%d,%d)-(%d,%d)\r\n", 
                    wp->flags,
                    rc.left, rc.top, rc.right, rc.bottom );
            }
            #endif

            {
                ::SetWindowPos(
                    m_hwndStatusBar,
                    NULL,
                    rc.left, rc.bottom - m_cyStatusBar,
                    rc.right - rc.left, rc.bottom - m_cyStatusBar,
                    SWP_NOZORDER );

                int rgiPart[1];
                rgiPart[0] = -1;
                ::SendMessage(
                    m_hwndStatusBar,
                    SB_SETPARTS,
                    lengthof(rgiPart),
                    reinterpret_cast<LPARAM>(rgiPart) );
            }

            ::SetWindowPos(
                *m_pEditPane,
                HWND_TOP,
                rc.left, rc.top,
                rc.right - rc.left, rc.bottom - rc.top - m_cyStatusBar,
                SWP_NOZORDER );

            return 0;
        } // WM_WINDOWPOSCHANGED
        } // switch uMsg
        return BaseWindow::onMessage(uMsg, wParam, lParam);
    } // onMessage
}; // MainWindow


// mainLoop
static int mainLoop()
{
    // Initialize Default Style
    // This initialize must be before creating edit buffers.
    {
        g_pDefaultStyle = new Style;
        //g_pDefaultStyle->SetBackground(Color(0xF0, 0xF0, 0xF0));
        g_pDefaultStyle->SetBackground(Color(247, 247, 239));
        g_pDefaultStyle->SetColor(Color(0x00, 0x00, 0x00));
        g_pDefaultStyle->SetMarker(Color(0x00, 0x66, 0x00));

        {
            FontSet* pFontSet = new FontSet;
            pFontSet->Add(Font::Create(L"Lucida Console", 9, ANSI_CHARSET));
            //pFontSet->Add(Font::Create(L"Courier New", 9, ANSI_CHARSET));
            pFontSet->Add(Font::Create(L"MS Gothic", 9, SHIFTJIS_CHARSET));
            g_pDefaultStyle->SetFontSet(pFontSet);
        }
    }

    MainWindow oMainWnd;

    oMainWnd.Start();

    int iIdle = 1;
    uint  nMessage = 0;

    for (;;)
    {
        MSG oMsg;
        if (! ::PeekMessage(&oMsg, NULL, 0, 0, PM_REMOVE))
        {
            if (iIdle != 0)
            {
                #if DEBUG_IDLE
                {
                    DEBUG_PRINTF(L"idle %d msg=0x%x qs=0x%0x\n", 
                        iIdle, nMessage, ::GetQueueStatus(QS_ALLEVENTS) );
                }
                #endif // DEBUG_IDLE
                oMainWnd.OnIdle();
            }

            iIdle = 0;
            ::GetMessage(&oMsg, NULL, 0, 0);
        }

        if (WM_QUIT == oMsg.message)
        {
            return static_cast<int>(oMsg.wParam);
        }

        ::TranslateMessage(&oMsg);
        ::DispatchMessage(&oMsg);

        nMessage = oMsg.message;
        switch (oMsg.message)
        {
        case WM_PAINT:
        case 0x118: // WM_SYSTIME for bliking caret
            break;

        default:
            iIdle = 1;
            break;
        } // switch messgae
    } // for
} // mainLoop

// WinMain
int WinMain(
    HINSTANCE hInstance,
    HINSTANCE,
    LPSTR,
    int )
{
    g_hInstance = hInstance;
    g_hResource = hInstance;

    {
        INITCOMMONCONTROLSEX oInit;
        oInit.dwSize = sizeof(oInit);
        oInit.dwICC  = ICC_BAR_CLASSES;
        if (! ::InitCommonControlsEx(&oInit))
        {
            ::MessageBox(
                NULL,
                L"InitCommonControlsEx",
                L"Evita Common Lisp Listner",
                MB_APPLMODAL | MB_ICONERROR );
            return 1;
        }
    }

    int iExit = BaseWindow::Init();
    if (0 != iExit) return iExit;
    return mainLoop();
} // WinMain


#if NDEBUG
void* __cdecl operator new(size_t cb)
    { return ::HeapAlloc(::GetProcessHeap(), 0, cb); }

void __cdecl operator delete(void* pv)
    { ::HeapFree(::GetProcessHeap(), 0, pv); }

extern "C"
{
int __cdecl _purecall(void) { return 0; }


typedef unsigned char uint8;

void* myCopyMemory(void* dst, const void* src, size_t count)
{
    const uint8* s = reinterpret_cast<const uint8*>(src);
    const uint8* e = s + count;
    uint8* d = reinterpret_cast<uint8*>(dst);
    while (s < e) *d++ = *s++;
    return dst;
} // memcpy

void* myMoveMemory(void* dst, const void* src, size_t count)
{
    const uint8* s = reinterpret_cast<const uint8*>(src);
    const uint8* e = s + count;
    uint8* d = reinterpret_cast<uint8*>(dst);
    if (s < d && d < e)
    {
        while (e > s) *d++ = *--s;
    }
    else
    {
        while (s < e) *d++ = *s++;
    }
    return dst;
} // myMoveMemory

void* myZeroMemory(void* dst, size_t count)
{
    uint8* d = reinterpret_cast<uint8*>(dst);
    uint8* e = d + count;
    while (d < e) *d++ = 0;
    return dst;
} // memcpy

#if defined (_M_IX86)
#pragma function(memcpy)

void* __cdecl memcpy(void* dst, const void* src, size_t count)
{
    const uint8* s = reinterpret_cast<const uint8*>(src);
    const uint8* e = s + count;
    uint8* d = reinterpret_cast<uint8*>(dst);
    while (s < e) *d++ = *s++;
    return dst;
} // memcpy
#endif // defined (_M_IX86)

// See crt/src/crtexe.c
void __cdecl WinMainCRTStartup()
{
    int iRet = WinMain(::GetModuleHandle(NULL), NULL, NULL, 0);
    ::ExitProcess(iRet);
} // WinMainCRTStartup

} // extern "C"
#endif // NDEBUG
