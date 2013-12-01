#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - winapp - Frame Window
// listener/winapp/vi_Frame.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_Frame.cpp#5 $
//
#define DEBUG_DROPFILES 0
#define DEBUG_FOCUS     1
#define DEBUG_REDRAW    1
#define DEBUG_WINDOWPOS 0
#include "./vi_Frame.h"

#include "./ed_Mode.h"

#include "./vi_defs.h"

#include "./vi_Application.h"
#include "./vi_Buffer.h"
#include "./vi_EditPane.h"
#include "./vi_Pane.h"
#include "./vi_Selection.h"
#include "./vi_Style.h"
#include "./vi_util.h"

#include "./ctrl_TabBand.h"

#include <dwmapi.h>

#define DEFPROC(mp_name, mp_rety, mp_params, mp_args) \
  typedef mp_rety (WINAPI *FnType ## mp_name) mp_params; \
  private: FnType ## mp_name m_pfn ## mp_name; \
  public: mp_rety mp_name mp_params { return m_pfn ## mp_name mp_args; } \
  private: void Install ## mp_name() { \
    m_pfn ## mp_name = reinterpret_cast<FnType ## mp_name>(Get(#mp_name)); \
  }

#define INSTALL_PROC(mp_name) \
    Install ## mp_name ()

class DllWrapper {
  public: ~DllWrapper() {
    ::FreeLibrary(m_hDll);
  }

  public: bool IsAviable() const {
    return m_hDll != nullptr;
  }

  protected: void* Get(const char* name) const {
    return m_hDll ? ::GetProcAddress(m_hDll, name) : nullptr;
  }

  protected: DllWrapper(const char16* const pwszName)
    : m_hDll(::LoadLibraryW(pwszName)) {
  }

  private: HINSTANCE const m_hDll;

  DISALLOW_COPY_AND_ASSIGN(DllWrapper);
};

// DWM(Desktop Window Manager) API
class DwmApi : public DllWrapper {
  DEFPROC(DwmExtendFrameIntoClientArea, \
    HRESULT, \
    (HWND hwnd, const MARGINS* pm), \
    (hwnd, pm))

  DEFPROC(DwmDefWindowProc, \
    BOOL, \
    (HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam, LRESULT* lp), \
    (hwnd, uMsg, wParam, lParam, lp))

  public: DwmApi::DwmApi() : DllWrapper(L"dwmapi.dll") {
    INSTALL_PROC(DwmDefWindowProc);
    INSTALL_PROC(DwmExtendFrameIntoClientArea);
  }
};

static DwmApi* s_pDwmApi;

#define USE_TABBAND_EDGE 0
extern uint g_TabBand__TabDragMsg;

/// <summary>
///   Construct this frame.
/// </summary>
Frame::Frame()
    : m_pActivePane(nullptr) {
  ::ZeroMemory(m_rgpwszMessage, sizeof(m_rgpwszMessage));
} // Frame::Frame

/// <summary>
///   Destruct this frame.
/// </summary>
Frame::~Frame() {
  Application::Get()->DeleteFrame(this);

  for (auto i = 0; i < MessageLevel_Limit; i++) {
    delete[] m_rgpwszMessage[i];
  } // for i
} // Frame::~Frame

/// <summary>
///   Activate this frame.
/// </summary>
bool Frame::Activate() {
  return ::SetForegroundWindow(*this) != 0;
} // Frame::Activate

/// <summary>
///   Add specified pane to this frame.
/// </summary>
/// <param name="pPane">A pane to be added.</param>
/// <returns>A pane parameter.</returns>
Pane* Frame::AddPane(Pane* const pPane) {
  ASSERT(pPane != nullptr);

  if (pPane->GetFrame() == this) {
    return pPane;
  }

  if (pPane->GetFrame()) {
    auto const pFrame = pPane->GetFrame();
    if (pPane->IsRealized()) {
      ::SetParent(*pPane, m_hwnd);
      pFrame->detachPane(pPane);
    } else {
      pFrame->m_oPanes.Delete(pPane);
    }
  }

  m_oPanes.Append(this, pPane);

  if (IsRealized()) {
    if (! pPane->IsRealized()) {
      pPane->Realize();
    } else {
      RECT rc;
      GetPaneRect(&rc);
      ::SetWindowPos(
          *pPane,
          nullptr,
          rc.left,
          rc.top,
          rc.right - rc.left,
          rc.bottom - rc.top,
          SWP_NOZORDER);
    } // if

    addTab(pPane);
  } // if

  return pPane;
} // Frame::AddPane

void Frame::addTab(Pane* const pPane) {
  TCITEM oItem;
  oItem.mask = TCIF_TEXT | TCIF_PARAM;
  oItem.pszText = const_cast<char16*>(pPane->GetName());
  oItem.lParam = reinterpret_cast<LPARAM>(pPane);

  if (auto const pEdit = pPane->DynamicCast<EditPane>()) {
    oItem.iImage = pEdit->GetBuffer()->GetMode()->GetIcon();
    if (oItem.iImage != -1) {
      oItem.mask |= TCIF_IMAGE;
    }
  } // if

  auto const new_item_index = TabCtrl_GetItemCount(m_hwndTabBand);
  TabCtrl_InsertItem(m_hwndTabBand, new_item_index, &oItem);
  TabCtrl_SetCurSel(m_hwndTabBand, new_item_index);
} // Frame::addTab

bool Frame::canClose() {
  if (Application::Get()->HasMultipleFrames()) {
    // Destroy this frame since we have multiple frames.
    return true;
  }

  // Can we exist applicaiton safety?
  return Application::Get()->CanExit();
} // Frame::canClose

// Description:
//  Detach pPane from this frame and
//    o Change active pane to MRU pane
//    o If no pane in frame, destory this frame.
void Frame::detachPane(Pane* const pPane) {
  auto const iItem = getTabFromPane(pPane);
  m_oPanes.Delete(pPane);
  TabCtrl_DeleteItem(m_hwndTabBand, iItem);
  // Tab control activate another tab.
  if (m_oPanes.IsEmpty()) {
    Destroy();
  }

  ASSERT(m_pActivePane != pPane);
} // Frame::detachPane

/// <summary>
///   Get active pane of this frame.
/// </summary>
Pane* Frame::GetActivePane() {
  if (m_pActivePane && m_pActivePane->GetActiveTick() != 0) {
    return m_pActivePane;
  }

  auto pActive = m_oPanes.GetFirst();
  foreach (EnumPane, oEnum, this) {
    auto const pPane = oEnum.Get();
    if (pActive->GetActiveTick() < pPane->GetActiveTick()) {
      pActive = pPane;
    }
  } // for pane

  return pActive;
} // Frame::GetActivePane

/// <summary>
///   Get width of status bar.
/// </summary>
int Frame::GetCxStatusBar() const {
    auto const cx =
        m_rc.right
        - m_rc.left
        - ::GetSystemMetrics(SM_CXVSCROLL);  // remove size grip
    return cx;
} // Frame::GetCxStatusBar

Pane* Frame::getPaneFromTab(int const iItem) const {
  Pane* pPane;
  {
    TCITEM oItem;
    oItem.mask = TCIF_PARAM;
    if (!TabCtrl_GetItem(m_hwndTabBand, iItem, &oItem)) {
      return nullptr;
    }

    pPane = reinterpret_cast<Pane*>(oItem.lParam);
  }

  foreach (EnumPane, oEnum, this) {
    if (pPane == oEnum.Get()) {
      return pPane;
    }
  } // for pane

  return nullptr;
} // Frame::getPaneFromTab

int Frame::getTabFromPane(Pane* const pPane) const {
  TCITEM oItem;
  oItem.mask = TCIF_PARAM;
  int iItem = 0;
  for (;;) {
    if (!TabCtrl_GetItem(m_hwndTabBand, iItem, &oItem)) {
      CAN_NOT_HAPPEN();
    }

    if (reinterpret_cast<Pane*>(oItem.lParam) == pPane) {
      return iItem;
    }

    iItem += 1;
  } // for
} // FrameWIndow::getTabFromPane

/// <summary>
///   Get pane rectangle.
/// </summary>
void Frame::GetPaneRect(RECT* const out_rc) {
  *out_rc = m_rc;
  out_rc->left += 2;
  #if USE_TABBAND_EDGE
    out_rc->top += m_cyTabBand + 2 + 2;
  #else
    out_rc->top += m_cyTabBand;
  #endif
  out_rc->right -= 2;
  out_rc->bottom -= m_oStatusBar.GetCy() + 2;
} // Frame::GetPaneRect

const char16* Frame::getToolTip(NMTTDISPINFO* const pDisp) const {
  auto const pPane = getPaneFromTab(static_cast<int>(pDisp->hdr.idFrom));
  if (pPane == nullptr) {
    return L"";
  }

  auto const pEdit = pPane->DynamicCast<EditPane>();
  if (pEdit == nullptr) {
    return pPane->GetName();
  }

  auto const pBuffer = pEdit->GetBuffer();

  const char16* pwszSave;
  char16 wszSave[100];
  if (*pBuffer->GetFileName() == 0) {
    pwszSave = L"Not saved";
  } else {
    // FIXME 2007-08-05 We should use localized date time format.
    FILETIME ft;
    ::FileTimeToLocalFileTime(pBuffer->GetLastWriteTime(), &ft);
    SYSTEMTIME st;
    ::FileTimeToSystemTime(&ft, &st);
    ::wsprintf(wszSave, L"%d/%d/%d %02d:%02d:%02d",
        st.wMonth,
        st.wDay,
        st.wYear,
        st.wHour,
        st.wMinute,
        st.wSecond);
    pwszSave = wszSave;
  }

  //char16 wszMod[100];
  auto const pwszModified =
    !pBuffer->IsModified()
        ? L"Not modified"
        : pBuffer->GetNoSave()
            ? L"Modified"
            : L"Not saved";

  ::wsprintf(m_wszToolTip,
      L"Name: %s\r\n"
      L"File: %s\r\n"
      L"Save: %s\r\n"
      L"%s\r\n",
      pBuffer->GetName(),
      *pBuffer->GetFileName() == 0
          ? L"No file"
          : pBuffer->GetFileName(),
      pwszSave,
      pwszModified);

    return m_wszToolTip;
} // Frame::getToolTip

bool Frame::hasFocus() const {
  return m_pActivePane && m_pActivePane->HasFocus();
} // Frame::hasFocus

void Frame::onDropFiles(HDROP const hDrop) {
  uint nIndex = 0;
  for (;;) {
    char16 wsz[MAX_PATH + 1];
    auto const cwch = ::DragQueryFile(hDrop, nIndex, wsz, lengthof(wsz));
    if (cwch == 0) {
      break;
    }

    auto const pBuffer = Application::Get()->Load(wsz);
    Pane* pPane = nullptr;
    foreach (EnumPane, oEnum, this) {
      auto const pEditPane = oEnum.Get()->DynamicCast<EditPane>();
      if (pEditPane == nullptr) {
        continue;
      }

      if (pEditPane->GetBuffer () == pBuffer) {
        pPane = pEditPane;
        break;
      }
    } // for pane

    if (pPane == nullptr) {
      pPane = new EditPane(pBuffer);
      AddPane(pPane);
    }

    pPane->Activate();

    nIndex += 1;
  } // for

  ::DragFinish(hDrop);
  Activate();
} // Frame::onDropFiles

void Frame::onDraw(HDC) {
#if USE_TABBAND_EDGE
  {
    RECT rc = m_rc;

    #if DEBUG_REDRAW
      DEBUG_PRINTF("WM_PAINT %p %dx%d+%d+%d\n",
          this,
          rc.right - rc.left, rc.bottom - rc.top, rc.left, rc.top);
     #endif

     rc.top += m_cyTabBand;
     rc.bottom = rc.top + 2;

     {
       auto const color = static_cast<COLORREF>(
          ::SendMessage(m_hwndTabBand, WM_USER, 0, 0));

        auto const hBrush = ::CreateSolidBrush(color);

        ::FillRect(hdc, &rc, hBrush);
        ::DeleteObject(hBrush);
     }
  }

  {
    RECT rc;
    GetPaneRect(&rc);
    rc.top -= 2;
    rc.left -= 2;
    rc.right += 2;
    rc.bottom +=  2;
    ::DrawEdge(hdc, &rc, EDGE_SUNKEN, BF_RECT);
  }
#endif //USE_TABBAND_EDGE
} // Frame::onDraw

/// <summary>
///   Idle processing
/// </summary>
bool Frame::OnIdle(uint const nCount) {
  class Local {
    public: static void HandleObsoleteBuffer(Buffer* const pBuffer) {
      // Prevent further obsolete checking.
      pBuffer->SetObsolete(Edit::Buffer::Obsolete_Ignore);

      auto const iAnswer = Application::Get()->Ask(
          MB_YESNO | MB_ICONWARNING | MB_SETFOREGROUND | MB_TOPMOST,
          IDS_ASK_REFRESH,
          pBuffer->GetName());

      switch (iAnswer) {
        case IDNO:
          break;

        case IDYES:
          foreach (Buffer::EnumWindow, oEnum, pBuffer) {
            oEnum.Get()->GetSelection()->PrepareForReload();
          } // for window
          pBuffer->Load(pBuffer->GetFileName());
          break;

        default:
         CAN_NOT_HAPPEN();
      } // swtich iAnser
    } // HandleObsoleteBuffer
  }; // Local

  if (nCount == 0) {
    if (hasFocus()) {
      updateTitleBar();

      if (auto const pEditPane = m_pActivePane->DynamicCast<EditPane>()) {
        auto const pBuffer = pEditPane->GetBuffer();

        switch (pBuffer->GetObsolete()) {
          case Buffer::Obsolete_Checking:
          case Buffer::Obsolete_Ignore:
          case Buffer::Obsolete_Unknown:
            break;

          case Buffer::Obsolete_No:
            pBuffer->UpdateFileStatus();
            break;

          case Buffer::Obsolete_Yes:
            Local::HandleObsoleteBuffer(pBuffer);
            break;

          default:
             CAN_NOT_HAPPEN();
        } // switch obsolete
      } // if
    } // if
  } // if

  auto fMore = false;
  foreach (EnumPane, oEnum, this) {
    auto const pPane = oEnum.Get();
    if (pPane->OnIdle(nCount)) {
      fMore = true;
    }
  } // for Pane
  return fMore;
} // Frame::OnIdle

#define DefineDllProc(mp_dll, mp_name) \
    g_pfn ## mp_name = reinterpret_cast<mp_name ## T>( \
        ::GetProcAddress(mp_dll, #mp_name));

LRESULT Frame::onMessage(
    uint const uMsg,
    WPARAM const wParam,
    LPARAM const lParam) {
  switch (uMsg) {
    case WM_ACTIVATE:
    case WM_DWMCOMPOSITIONCHANGED:
      if (s_pDwmApi->IsAviable()) {
        MARGINS margins;
        margins.cxLeftWidth = 0;
        margins.cxRightWidth = 0;
        margins.cyBottomHeight = 0;
        margins.cyTopHeight = m_cyTabBand;

        auto const hr = s_pDwmApi->DwmExtendFrameIntoClientArea(
            m_hwnd, &margins);
        ASSERT(SUCCEEDED(hr));
      }
      break;

    case WM_CLOSE:
      if (canClose()) {
        break;
      }
      return 0;

    case WM_CREATE: {
      ::DragAcceptFiles(m_hwnd, TRUE);

      s_pDwmApi = new DwmApi();
      if (s_pDwmApi->IsAviable()) {
        MARGINS oMargins = { -1 };
        s_pDwmApi->DwmExtendFrameIntoClientArea(m_hwnd, &oMargins);
      }

      {
        m_hwndTabBand = ::CreateWindowEx(
            0,
            L"TabBandClass",
            nullptr,
            WS_CHILD | WS_VISIBLE | TCS_TOOLTIPS,
            0, 0, 0, 0,
            m_hwnd,
            reinterpret_cast<HMENU>(CtrlId_TabBand),
            g_hInstance,
            nullptr);

        ::SendMessage(
           m_hwndTabBand,
           TCM_SETIMAGELIST,
           0,
           reinterpret_cast<LPARAM>(
              Application::Get()->GetIconList()));

        RECT rc;
        ::GetWindowRect(m_hwndTabBand, &rc);
        m_cyTabBand = rc.bottom - rc.top;
      }

      m_oStatusBar.Realize(m_hwnd, CtrlId_StatusBar);
      m_oTitleBar.Realize(m_hwnd);

      ::GetClientRect(m_hwnd, &m_rc);

      foreach (EnumPane, oEnum, this) {
        auto const pPane = oEnum.Get();
        pPane->Realize();
        addTab(pPane);
      } // for Pane

      if (m_oPanes.GetFirst()) {
        m_oPanes.GetFirst()->Activate();
      }
      break;
    } // WM_CREATE

    case WM_DROPFILES:
      onDropFiles(reinterpret_cast<HDROP>(wParam));
      DEBUG_PRINTF("WM_DROPFILES\n");
      break;

    case WM_GETMINMAXINFO: {
      auto pMinMax = reinterpret_cast<MINMAXINFO*>(lParam);
      pMinMax->ptMinTrackSize.x = 200;
      pMinMax->ptMinTrackSize.y = 200;
      return 0;
    } // WM_GETMIMMAXINFO

    case WM_NCDESTROY:
      delete this;
      break;

    case WM_NCHITTEST:
      if (s_pDwmApi->IsAviable()) {
        LRESULT lResult;
        if (s_pDwmApi->DwmDefWindowProc(
              m_hwnd, uMsg, wParam, lParam, &lResult)) {
            return lResult;
        }

        POINT const ptMouse = { GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam) };
        RECT rcWindow;
        ::GetWindowRect(m_hwnd, &rcWindow);

        RECT rcClient = { 0 };
        ::AdjustWindowRectEx(
            &rcClient,
            ::GetWindowLong(m_hwnd, GWL_STYLE),
            false,
            ::GetWindowLong(m_hwnd, GWL_EXSTYLE));

        if (ptMouse.y >= rcWindow.top
            && ptMouse.y < rcWindow.top - rcClient.top + m_cyTabBand) {
          return HTCAPTION;
        }
      }
      break;

    case WM_NOTIFY: {
      auto const pNotify = reinterpret_cast<NMHDR*>(lParam);
      if (TTN_NEEDTEXT == pNotify->code) {
          auto const p = reinterpret_cast<NMTTDISPINFO*>(lParam);
          ::SendMessage(p->hdr.hwndFrom, TTM_SETMAXTIPWIDTH, 0, 300);
          p->lpszText = const_cast<char16*>(getToolTip(p));
          return 0;
      }

      switch (pNotify->idFrom) {
        case CtrlId_TabBand:
          switch (pNotify->code) {
            case TABBAND_NOTIFY_CLOSE: {
                auto const iCurSel = TabCtrl_GetCurSel(m_hwndTabBand);
                if (auto const pPane = getPaneFromTab(iCurSel)) {
                    pPane->Destroy();
                }
                break;
            } // TABBAND_NOTIFY_CLOSE

            case TABBAND_NOTIFY_QUERY_CLOSE:
              return HasMultiplePanes() || canClose();

            case TCN_SELCHANGE: {
              auto const iCurSel = TabCtrl_GetCurSel(m_hwndTabBand);
              if (auto const pPane = getPaneFromTab(iCurSel)) {
                if (m_pActivePane) {
                  ::ShowWindow(*m_pActivePane, SW_HIDE);
                }

                m_pActivePane = pPane;
                ::ShowWindow(*pPane, SW_SHOW);
                ::SetFocus(*pPane);
                updateTitleBar();
              }
              break;
            } // TCN_SELCHANGE
            break;
          } // switch code
        } // switch idFrom
        return 0;
    } // WM_NOTIFY

    case WM_PAINT: {
      PAINTSTRUCT ps;
      auto const hdc = ::BeginPaint(m_hwnd, &ps);
      onDraw(hdc);
      ::EndPaint(m_hwnd, &ps);
      return 0;
    } // WM_PAINT

    case WM_PARENTNOTIFY:
      switch (wParam) {
        case WM_DESTROY: {
          auto const hwnd = reinterpret_cast<HWND>(lParam);
          foreach (EnumPane, oEnum, this) {
            if (auto const pPane = oEnum.Get()) {
              if (*pPane == hwnd) {
                detachPane(pPane);
                break;
              }
            }
          } // for pane
          break;
        } // WM_DESTROY
      } // switch wParam
      return 0;

    case WM_SETFOCUS:
      if (auto const pPane = GetActivePane()) {
        #if DEBUG_FOCUS
            DEBUG_PRINTF("WM_SETFOCUS %p pane=%p\n", this, pPane);
        #endif
        Application::Get()->SetActiveFrame(this);
        pPane->Activate();
      } // if
      return 0;

    case WM_WINDOWPOSCHANGED: {
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

      // undocumented SWP flags. See http://www.winehq.org.
      #if !defined(SWP_NOCLIENTSIZE)
          #define SWP_NOCLIENTSIZE    0x0800
          #define SWP_NOCLIENTMOVE    0x1000
      #endif // !defined(SWP_NOCLIENTSIZE)
      // Create   0x10001843  NOCLIENTMOVE NOCLIENTSIZE SHOWWINDOW NOMOV NOSIZE
      // Minimize 0x00008130
      // Restore  0x00008124
      // Move     0x00000A15
      // Destroy  0x20001897  NOCLIENTMOVE NOCLIENTSIZE HIDEWINDOW NOACTIVATE NOZORDER NOMOVE NOSIZE
      //if (wp->flags & SWP_NOSIZE) return 0;

      auto const wp = reinterpret_cast<WINDOWPOS*>(lParam);

      #if DEBUG_WINDOWPOS
        DEBUG_PRINTF("WM_WINDOWPOSCHANGED:"
            L" flags=0x%0x\r\n",
            wp->flags);
      #endif // DEBUG_WINDOWPOS

      if (wp->flags & SWP_HIDEWINDOW) {
        // We don't take care hidden window.
        return 0;
      } // if

      if ((wp->flags & 0x10000000) == 0) {
        if (wp->flags & SWP_NOSIZE) {
          return 0;
        } // if
      } // if

      if (::IsIconic(m_hwnd)) {
        // We don't take care miminize window.
        return 0;
      } // if

      ::GetClientRect(m_hwnd, &m_rc);

      // Tab Band
      {
        ::SetWindowPos(
            m_hwndTabBand,
            nullptr,
            m_rc.left,
            m_rc.top,
            m_rc.right - m_rc.left,
            m_rc.top + m_cyTabBand,
            SWP_NOZORDER);
      }

      // Status Bar
      //  message, code page, newline, line num, column, char, ins/ovr
      if (m_oStatusBar) {
          ::SetWindowPos(
              m_oStatusBar,
              nullptr,
              m_rc.left,
              m_rc.bottom - m_oStatusBar.GetCy(),
              m_rc.right - m_rc.left,
              m_oStatusBar.GetCy(),
              SWP_NOZORDER);

          ::SendMessage(m_oStatusBar, SB_SIMPLE, 1, 0);

          char16 wsz[100];
          ::wsprintf(wsz, L"Resizing... %dx%d",
              m_rc.right - m_rc.left,
              m_rc.bottom - m_rc.top);

          ::SendMessage(
              m_oStatusBar,
              SB_SETTEXT,
              SB_SIMPLEID | SBT_NOBORDERS,
              reinterpret_cast<LPARAM>(wsz));
      } // status bar

      RECT rc;
      GetPaneRect(&rc);

      foreach (EnumPane, oEnum, this) {
        auto const pPane = oEnum.Get();

        ::SetWindowPos(
            *pPane,
            HWND_TOP,
            rc.left,
            rc.top,
            rc.right  - rc.left,
            rc.bottom - rc.top,
            SWP_NOZORDER);
      } // for Pane

      //::InvalidateRect(m_hwnd, nullptr, false);
      {
        Dc hdc(m_hwnd, ::GetDC(m_hwnd));
        onDraw(hdc);
      }

      return 0;
    } // WM_WINDOWPOSCHANGED

    default:
      if (uMsg == g_TabBand__TabDragMsg) {
        return onTabDrag(
            static_cast<TabBandDragAndDrop>(wParam),
            reinterpret_cast<HWND>(lParam));
      }
      break;
  } // switch uMsg

  return BaseWindow::onMessage(uMsg, wParam, lParam);
} // Frame::onMessage

bool Frame::onTabDrag(
    TabBandDragAndDrop const eAction,
    HWND const hwndTabBand) {
  auto const pFrom = Application::Get()->FindFrame(::GetParent(hwndTabBand));

  if (pFrom == nullptr) {
    // We should not be here.
    return false;
  }

  auto const pPane = pFrom->GetActivePane();
  if (pPane == nullptr) {
    // Why is pPane nullptr?
    return false;
  }

  switch (eAction) {
    case kDrop:
      if (this != pFrom) {
        AddPane(pPane);
      }
      break;

    case kHover:
      break;

    case kThrow: {
      auto const pNewFrame = Application::Get()->CreateFrame();
      pNewFrame->Realize();
      pNewFrame->AddPane(pPane);
      break;
    }

    default:
      CAN_NOT_HAPPEN();
  }

  return true;
} // Frame::onTabDrag

/// <summary>
///   Realize this frame.
/// </summary>
void Frame::Realize() {
  int const cColumns = 80;
  int const cRows    = 40;

  DWORD dwExStyle =
      WS_EX_APPWINDOW
      | WS_EX_WINDOWEDGE;

  DWORD dwStyle =
    WS_OVERLAPPEDWINDOW
    | WS_VISIBLE;

  Font* pFont;
  {
    Dc dc(m_hwnd, ::GetDC(m_hwnd));
    pFont = FontSet::Get(dc, &g_DefaultStyle)->FindFont(dc, 'x');
  }

  RECT rc;
  rc.left   = 0;
  rc.top    = 0;
  rc.right  = pFont->GetWidth() * cColumns;
  rc.bottom = pFont->GetHeight() * cRows;

  ::AdjustWindowRectEx(&rc, dwStyle, TRUE, dwExStyle);
  rc.right += ::GetSystemMetrics(SM_CXVSCROLL) + 10;

  RECT rcWork;
  ::SystemParametersInfo(SPI_GETWORKAREA, 0, &rcWork, 0);

  auto const cx = rc.right  - rc.left;
  auto const cy = (rcWork.bottom - rcWork.top) * 4 / 5;

  // See WM_GETMINMAXINFO
  CreateWindowEx(
      dwExStyle,
      L"This is Window Text.",
      dwStyle,
      nullptr,
      CW_USEDEFAULT,
      CW_USEDEFAULT,
      cx,
      cy);

  SetStatusBar(0, L"Ready");
} // Frame::Realize

/// <summary>
///   Remove all messages in status bar.
/// </summary>
void Frame::ResetMessages() {
  ::ZeroMemory(m_rgpwszMessage, sizeof(m_rgpwszMessage));
} // Frame::ResetMessages

void Frame::SetActivePane(Pane* const pPane) {
  #if DEBUG_FOCUS
      DEBUG_PRINTF("%p new=%p cur=%p\n", this, pPane, m_pActivePane);
  #endif

  int iItem = 0;
  for (;;) {
    TCITEM oItem;
    oItem.mask = TCIF_PARAM;
    if (TabCtrl_GetItem(m_hwndTabBand, iItem, &oItem)) {
      if (oItem.lParam == reinterpret_cast<LPARAM>(pPane)) {
        if (TabCtrl_GetCurSel(m_hwndTabBand) == iItem) {
          ::SetFocus(*pPane);
        } else {
          TabCtrl_SetCurSel(m_hwndTabBand, iItem);
        }
        break;
      }
    } // if
    iItem += 1;
  } // for pane
}// Frame::SetActivePane

/// <summary>
///   Set status bar message on specified part.
/// </summary>
void Frame::SetStatusBar(int const ePart, const char16* const pwszMsg) {
  ::SendMessage(m_oStatusBar, SB_SIMPLE, 0, 0);

  ::SendMessage(
      m_oStatusBar,
      SB_SETTEXT,
      ePart | SBT_NOBORDERS,
      reinterpret_cast<LPARAM>(pwszMsg));
} // Frame::SetStatusBar

/// <summary>
///   Set status bar formatted message on specified part.
/// </summary>
void Frame::SetStatusBarf(
    int const ePart,
    const char16* pwszFormat, ...) {
  char16 wsz[1024];

  va_list args;
  va_start(args, pwszFormat);
  ::wvsprintf(wsz, pwszFormat, args);
  va_end(args);
  SetStatusBar(ePart, wsz);
} // Frame::SetStatusBar

/// <summary>
///   Set parts width of status bar.
/// </summary>
void Frame::SetStatusBarParts(int const* const prgiPart, int const cParts) {
  m_oStatusBar.SetParts(prgiPart, cParts);
} // Frame::SetStatusBar

/// <summary>
///   Show or activate specified buffer on this frame.
/// </summary>
/// <param name="pBuffer">A buffer to display or activate.</param>
/// <returns>
///   True if buffer hasn't been displayed in this frame. False if associated
///   window is activated.
/// </returns>
bool Frame::ShowBuffer(Buffer* const pBuffer) {
  foreach (EnumPane, oEnum, this) {
    if (EditPane* pPane = oEnum.Get()->DynamicCast<EditPane>()) {
      if (pPane->GetBuffer() == pBuffer) {
        pPane->Activate();
        return false;
      }
    }
  } // for pane

  auto const pPane = new EditPane(pBuffer);
  AddPane(pPane);
  pPane->Activate();
  return true;
} // Frame::ShowBuffer

/// <summary>
///   Display specified message on status bar.
/// </summary>
void Frame::ShowMessage(
    MessageLevel const iLevel,
    uint const nFormatId, ...) {
  delete[] m_rgpwszMessage[iLevel];
  m_rgpwszMessage[iLevel] = nullptr;
  if (nFormatId != 0) {
    char16 wszFormat[1024];
    ::LoadString(g_hResource, nFormatId, wszFormat, lengthof(wszFormat));

    char16 wsz[1024];

    va_list args;
    va_start(args, nFormatId);
    ::wvsprintf(wsz, wszFormat, args);
    va_end(args);

    auto const cwch = ::lstrlenW(wsz);
    auto const pwsz = new char16[cwch + 1];
    myCopyMemory(pwsz, wsz, sizeof(char16) * (cwch + 1));
    m_rgpwszMessage[iLevel] = pwsz;
  } // if

  int i = MessageLevel_Limit;
  do {
    i -= 1;
    if (char16* pwsz = m_rgpwszMessage[i]) {
      SetStatusBar(0, pwsz);
      return;
    }
  } while (i > 0);
} // Frame::ShowMessage

// [U]
/// <summary>
///   Updates title bar to display active buffer.
/// </summary>
void Frame::updateTitleBar() {
  char16 wsz[1024];
  m_pActivePane->GetTitle(wsz, lengthof(wsz));

  char16 wszTitle[1024];
  ::wsprintf(wszTitle, L"%s - %s",
      wsz,
      Application::Get()->GetTitle());

  m_oTitleBar.SetText(wszTitle, ::lstrlenW(wszTitle));

  m_pActivePane->UpdateStatusBar();
} // Frame::updateTitleBar
