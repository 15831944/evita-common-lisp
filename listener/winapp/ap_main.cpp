#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - Editor - Application Main
// listener/winapp/ap_main.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/ap_main.cpp#3 $
//
// Example options:
//  -dll vanilla.dll -image evcl3.image -multiple
//
#define DEBUG_IDLE 0

#if USE_LISTENER
    #include "./ap_listener_buffer.h"
    #define SINGLE_INSTANCE_NAME    L"3C9C7EC2-0DE7-461e-8F09-14F3B830413F" 
#else // USE_LISTENER
    #include "./vi_buffer.h"
    #define SINGLE_INSTANCE_NAME    L"D47A7677-9F8E-467c-BABE-8ABDE8D58475" 
#endif // USE_LISTENER

#include "./ctrl_TabBand.h"

#include "./vi_Application.h"
#include "./vi_EditPane.h"
#include "./vi_IoManager.h"
#include "./vi_Style.h"

#pragma comment(lib, "comctl32.lib")
#pragma comment(lib, "gdiplus.lib")

#if _WIN64
#pragma comment(linker, "\"/manifestdependency:type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='amd64' publicKeyToken='6595b64144ccf1df' language='*'\"")
#else
#pragma comment(linker, "\"/manifestdependency:type='Win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='X86' publicKeyToken='6595b64144ccf1df' language='*'\"")
#endif

const char16* k_pwszTitle = L"Evita Common Lisp Listner";

void Application::InternalAddBuffer(Buffer* pBuffer)
{
    m_oBuffers.Append(pBuffer);
} // Application::InternalAddBuffer

namespace
{

//////////////////////////////////////////////////////////////////////
//
// EnumArg
//  Enumerates command line arguments.
//
// FIXME 2007-08-07 yosi@msn.com We should share EnumArg with console
// and listener.
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
    {
        // skip command name
        next();
        unless (AtEnd()) next();
    } // EnumArg

    public: bool AtEnd() const
        { return 0 == *m_pwszRunner && 0 == *m_wsz; }

    public: LPCWSTR Get() const
        { ASSERT(!AtEnd()); return m_wsz; }

    public: void Next()
        { ASSERT(!AtEnd()); next(); }

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

HINSTANCE   g_hInstance;
HINSTANCE   g_hResource;
HWND        g_hwndActiveDialog;
uint        g_TabBand__TabDragMsg;

extern StyleValues g_DefaultStyle;

static void NoReturn fatalExit(const char16*);


//////////////////////////////////////////////////////////////////////
//
// callRunningApp
//
static int callRunningApp(EnumArg* pEnumArg)
{
    Handle shShared = ::OpenFileMapping(
        FILE_MAP_READ | FILE_MAP_WRITE,
        FALSE,
        k_wszFileMapping );
    if (NULL == shShared)
    {
        fatalExit(L"OpenFileMapping");
    }

    SharedArea* p = reinterpret_cast<SharedArea*>(::MapViewOfFile(
        shShared,
        FILE_MAP_READ | FILE_MAP_WRITE,
        0,      // dwFileOffsetHigh
        0,      // dwFileOffsetLow
        k_cbFileMapping ) );
    if (NULL == p)
    {
        fatalExit(L"MapViewOfFile");
    }

    while (!pEnumArg->AtEnd()) {
        const char16* pwszArg = pEnumArg->Get();

        char16 wsz[MAX_PATH];
        char16* pwszFile;
        uint cwch = ::GetFullPathName(
            pwszArg,
            lengthof(wsz),
            wsz,
            &pwszFile );

        pEnumArg->Next();

        if (0 == cwch || cwch > lengthof(wsz))
        {
            continue;
        }

        COPYDATASTRUCT oData;
        oData.dwData = 1;
        oData.cbData = sizeof(char16) * (cwch + 1);
        oData.lpData = wsz;

        ::SendMessage(
            p->m_hwnd,
            WM_COPYDATA,
            NULL,
            reinterpret_cast<LPARAM>(&oData) );
    } // for each arg

    return 0;
} // callRunningApp


//////////////////////////////////////////////////////////////////////
//
// fatalExit
//
static void NoReturn fatalExit(const char16* pwsz)
{
    char16 wsz[100];
    ::wsprintf(wsz, L"Evita Text Editor can't start (%s).", pwsz);
    ::FatalAppExit(0, wsz);
} // fatalExit

static int MainLoop(EnumArg* pEnumArg) {
  // Initialize Default Style
  // This initialize must be before creating edit buffers.
  {
      g_DefaultStyle.m_rgfMask =
          StyleValues::Mask_Background |
          StyleValues::Mask_Color |
          StyleValues::Mask_Decoration |
          StyleValues::Mask_FontFamily |
          StyleValues::Mask_FontStyle |
          StyleValues::Mask_FontWeight |
          StyleValues::Mask_Marker |
          StyleValues::Mask_Syntax;

        #if 0
          //g_DefaultStyle->SetBackground(Color(0xF0, 0xF0, 0xF0));
          g_DefaultStyle->SetBackground(Color(247, 247, 239));
          g_DefaultStyle->SetColor(Color(0x00, 0x00, 0x00));
          g_DefaultStyle->SetMarker(Color(0x00, 0x66, 0x00));
      #else
          g_DefaultStyle.m_crBackground = Color(255, 255, 255);
          g_DefaultStyle.m_crColor      = Color(0x00, 0x00, 0x00);
          g_DefaultStyle.m_crMarker     = Color(0x00, 0x99, 0x00);
      #endif

        //#define BaseFont L"Lucida Console"
      //#define BaseFont L"Courier New"
      #define BaseFont L"Consolas, MS Gothic"

        {
          //FontSet* pFontSet = new FontSet;
          //pFontSet->Add(Font::Create(BaseFont, nFontSize, ANSI_CHARSET));
          //pFontSet->Add(Font::Create(L"Courier New", nFontSize, ANSI_CHARSET));
          //pFontSet->Add(Font::Create(L"MS Gothic", nFontSize, SHIFTJIS_CHARSET));
          g_DefaultStyle.m_pwszFontFamily = BaseFont;
          g_DefaultStyle.m_nFontSize      = 10;
      }
  }

    Application::Init();

    Frame* pFrame = Application::Get()->CreateFrame();

    while (!pEnumArg->AtEnd())
  {
      const char16* pwszArg = pEnumArg->Get();
      Buffer* pBuffer = Application::Get()->Load(pwszArg);
      EditPane* pPane = new EditPane(pFrame, pBuffer);
      pFrame->AddPane(pPane);

        pEnumArg->Next();
  } // for each arg

    // When there is no filename argument, we start lisp.
  if (!pFrame->GetFirstPane()) {
      #if USE_LISTENER
          ListenerBuffer* pBuffer = new ListenerBuffer;
          pBuffer->Start();

        #else // USE_LISTENER
          Buffer* pBuffer = new Buffer(L"*scratch*");
      #endif // USE_LISTENER

        Application::Get()->InternalAddBuffer(pBuffer);
      EditPane* pPane = new EditPane(pFrame, pBuffer);
      pFrame->AddPane(pPane);
  } // if

    pFrame->Realize();

  int iIdle = 1;
  MSG oMsg;
  for (;;) {
    if (!::PeekMessage(&oMsg, NULL, 0, 0, PM_REMOVE)) {
      bool fGotMessage = false;
      if (iIdle) {
          #if DEBUG_IDLE
            DEBUG_PRINTF("idle %d msg=0x%04X qs=0x%0x\n", 
                         iIdle, oMsg.message,
                         ::GetQueueStatus(QS_ALLEVENTS) );
          #endif // DEBUG_IDLE

          uint nCount = 0;
          while (Application::Get()->OnIdle(nCount)) {
            if (::PeekMessage(&oMsg, NULL, 0, 0, PM_REMOVE)) {
              fGotMessage = true;
              break;
            }
            nCount += 1;
         }
      }

      iIdle = 0;
      if (!fGotMessage)
        ::GetMessage(&oMsg, NULL, 0, 0);
    } // if

    if (WM_QUIT == oMsg.message)
      return static_cast<int>(oMsg.wParam);

    if (g_hwndActiveDialog) {
      if (::IsDialogMessage(g_hwndActiveDialog, &oMsg))
        continue;
    }

    ::TranslateMessage(&oMsg);
    ::DispatchMessage(&oMsg);

    if (oMsg.message != WM_PAINT && oMsg.message != WM_SYSTIMER) {
      #if DEBUG_IDLE
        DEBUG_PRINTF("Enable idle by messgage=0x%04X\n", oMsg.message);
      #endif
      iIdle = 1;
    }
  }
}

//////////////////////////////////////////////////////////////////////
//
// WinMain
//
int WINAPI WinMain(
    HINSTANCE hInstance,
    HINSTANCE,
    LPSTR,
    int )
{
    g_hInstance = hInstance;
    g_hResource = hInstance;

    EnumArg oEnumArg(::GetCommandLine());
    while (!oEnumArg.AtEnd())
    {
        const char16* pwszArg = oEnumArg.Get();

        if ('-' != *pwszArg)
        {
            break;
        }
        if (0 == ::lstrcmpW(pwszArg, L"-multiple"))
        {
            g_fMultiple = true;
        }
        else if (0 == ::lstrcmpW(pwszArg, L"-dll"))
        {
            oEnumArg.Next();
            when (oEnumArg.AtEnd()) break;
        }
        else if (0 == ::lstrcmpW(pwszArg, L"-image"))
        {
            oEnumArg.Next();
            when (oEnumArg.AtEnd()) break;
        }

        oEnumArg.Next();
    } // for each arg

    if (!g_fMultiple)
    {
        g_hEvent = ::CreateEventW(
            NULL,   // lpEventAttrs
            TRUE,   // fManualReset
            FALSE,  // fInitialState
            L"Local\\" SINGLE_INSTANCE_NAME );

        if (NULL == g_hEvent)
        {
            fatalExit(L"CreateEvent");
        }

        if (ERROR_ALREADY_EXISTS == ::GetLastError())
        {
            uint nWait = ::WaitForSingleObject(g_hEvent, 30 * 100);
            switch (nWait)
            {
            case WAIT_OBJECT_0:
                return callRunningApp(&oEnumArg);

            default:
                ::MessageBox(
                    NULL,
                    L"WaitForSignleObject",
                    k_pwszTitle,
                    MB_APPLMODAL | MB_ICONERROR );
            } // switch
        }
    } // if !g_fSingle

    // Common Control
    {
        INITCOMMONCONTROLSEX oInit;
        oInit.dwSize = sizeof(oInit);
        oInit.dwICC  = ICC_BAR_CLASSES;
        if (!::InitCommonControlsEx(&oInit))
        {
            ::MessageBox(
                NULL,
                L"InitCommonControlsEx",
                k_pwszTitle,
                MB_APPLMODAL | MB_ICONERROR );
            return 1;
        }
    }

    ::TabBand__Init(g_hInstance);

    g_TabBand__TabDragMsg = ::RegisterWindowMessage(TabBand__TabDragMsgStr);

    int iExit = BaseWindow::Init();
    if (0 != iExit) return iExit;
    int iRet = MainLoop(&oEnumArg);

    return iRet;
} // WinMain
