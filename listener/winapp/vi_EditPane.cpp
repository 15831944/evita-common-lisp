#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - Edit Pane
// listener/winapp/vi_EditPane.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_EditPane.cpp#3 $
//
#define DEBUG_CARET     0
#define DEBUG_FOCUS     0
#define DEBUG_KEY       0
#define DEBUG_REDRAW    0
#define DEBUG_SPLIT     0
#include "./vi_EditPane.h"

#include "./ed_Mode.h"

#include "./vi_Application.h"
#include "./vi_Buffer.h"
#include "./vi_Selection.h"
#include "./vi_TextEditWindow.h"
#include "./vi_util.h"

const char16* const
k_rgwszNewline[4] = {
  L"--",
  L"LF",
  L"CR",
  L"CRLF",
};

static HCURSOR s_hVSplitCursor;

EditPane::LeafBox::~LeafBox() {
  #if DEBUG_DESTORY
      DEBUG_PRINTF(L"%p\n", this);
  #endif

  ASSERT(!m_pWindow);

  if (m_hwndVScrollBar) {
    ::DestroyWindow(m_hwndVScrollBar);
    m_hwndVScrollBar = nullptr;
  }
}

void EditPane::SplitterDrag::Start(HWND hwnd, State eState) {
  ::SetCapture(hwnd);
  m_eState = eState;
}

void EditPane::SplitterDrag::Stop() {
  ::ReleaseCapture();
  m_eState = State_None;
}

EditPane::EditPane(Buffer* pBuffer, Posn lStart)
    : m_eState(State_NotRealized) {
  auto pWindow = new TextEditWindow(this, pBuffer, lStart);
  auto pBox = new LeafBox(pWindow);
  m_oBoxes.Append(pBox);
  m_oWindows.Append(pWindow);
  m_pwszName = pBuffer->GetName();
}

EditPane::~EditPane() {
  while (m_oBoxes.GetFirst()) {
    delete m_oBoxes.GetFirst();
  }
}

void EditPane::CloseAllBut(Window* pWindow) {
  auto pRunner = m_oBoxes.GetFirst();

  while (pRunner->GetWindow() != pWindow) {
    pRunner->GetWindow()->Destroy();
    pRunner = m_oBoxes.GetFirst();
  }

  auto const pThisBox = pRunner;

  for (;;) {
    pRunner = pThisBox->GetNext();
    if (!pRunner) {
      break;
    }
    pRunner->GetWindow()->Destroy();
  }
}

static void drawSplitter(HDC hdc, RECT* prc, uint grfFlag) {
  RECT rc = *prc;
  ::FillRect(hdc, &rc, reinterpret_cast<HBRUSH>(COLOR_3DFACE + 1));
  ::DrawEdge(hdc, &rc, EDGE_RAISED, grfFlag);
}

// Draw edges between panes.
void EditPane::drawSplitters(HDC hdc) {
  auto rc = m_rc;

  if (!HasMultipleWindows()) {
    auto const cxVScroll = ::GetSystemMetrics(SM_CXVSCROLL);
    rc.left = rc.right - cxVScroll;
    rc.bottom = rc.top + k_cySplitterBig;
    drawSplitter(hdc, &rc, BF_RECT);
  } else {
    auto pAbove = static_cast<LeafBox*>(nullptr);
    foreach (EnumBox, oEnum, this) {
      LeafBox* pBox = oEnum.Get();
      if (pAbove) {
        rc.top = pAbove->GetRect()->bottom;
        rc.bottom = pBox->GetRect()->top;
        drawSplitter(hdc, &rc, BF_TOP | BF_BOTTOM);
      }
      pAbove = pBox;
    }
  }
}

void EditPane::drawSplitters() {
  Dc dc(m_hwnd, ::GetDC(m_hwnd));
  drawSplitters(dc);
}

// Returns the last active Box.
EditPane::LeafBox* EditPane::GetActiveLeafBox() const {
  auto pActive = m_oBoxes.GetFirst();
  foreach (EnumBox, oEnum, this) {
    auto const pBox = oEnum.Get();
    if (pActive->GetWindow()->GetActiveTick() < 
            pBox->GetWindow()->GetActiveTick()) {
      pActive = pBox;
    }
  }
  return pActive;
}

// Returns the last active Box.
EditPane::Window* EditPane::GetActiveWindow() const {
  auto const pBox = GetActiveLeafBox();
  return pBox ? pBox->GetWindow() : nullptr;
}

Buffer* EditPane::GetBuffer() const {
  return GetActiveWindow()->GetBuffer();
}

int EditPane::GetTitle(char16* out_wszTitle, int cchTitle) {
  auto const pBuffer = GetActiveWindow()->GetBuffer();
  auto const pwszName = pBuffer->GetName();
  auto const cwchName = ::lstrlenW(pwszName);

  auto const cwchExtra = pBuffer->IsModified() ? 3 : 1;

  auto cwch = cwchName;
  if (cchTitle < cwchName + cwchExtra) {
    cwch = cchTitle - 2;
  }

  myCopyMemory(
      out_wszTitle,
      pwszName,
      sizeof(char16) * cwch);

  auto pwch = out_wszTitle + cwch;

  if (cwch != cwchName) {
    *pwch++ = '.';
    *pwch++ = '.';
  }

  if (pBuffer->IsModified()) {
    *pwch++ = ' ';
    *pwch++ = '*';
  }

  *pwch = 0;
  return static_cast<int>(pwch - out_wszTitle);
}

bool EditPane::HasFocus() const {
  return ::GetFocus() == *GetActiveWindow();
}

EditPane::Element EditPane::hitTest(POINT pt, LeafBox** out_pBox) const {
  *out_pBox = nullptr;
  auto cxVScroll = ::GetSystemMetrics(SM_CXVSCROLL);

  if (HasMultipleWindows()) {
    foreach (EnumBox, oEnum, this) {
      auto const pBox = oEnum.Get();
      *out_pBox = pBox;

      if (::PtInRect(pBox->GetRect(), pt)) {
        if (pt.x > m_rc.right - cxVScroll) {
          return Element_VScrollBar;
        }
        return Element_Window;
      }

      if (pt.y < pBox->GetRect()->top) {
        return Element_Splitter;
      }
    }
  } else if (::PtInRect(m_oBoxes.GetFirst()->GetRect(), pt)) {
    auto const pBox = m_oBoxes.GetFirst();
    *out_pBox = pBox;

    if (pt.x < pBox->GetRect()->right - cxVScroll) {
      return Element_Window;
    }

    if (pt.y < pBox->GetRect()->top + k_cySplitterBig) {
      return Element_SplitterBig;
    }

    return Element_VScrollBar;
  }

  return Element_None;
}
Command::KeyBindEntry* EditPane::MapKey(uint nKey) {
  return GetActiveWindow()->MapKey(nKey);
}

bool EditPane::OnIdle(uint nCount) {
  auto fMore = GetActiveWindow()->OnIdle(nCount);
  foreach (EnumBox, oEnum, this) {
    if (oEnum.Get()->GetWindow()->OnIdle(nCount)) {
      fMore = true;
    }
  }
  return fMore;
}

LRESULT EditPane::onMessage(
    UINT uMsg,
    WPARAM wParam,
    LPARAM lParam) {
  switch (uMsg) {
    case WM_CREATE: {
      auto const pCreate = reinterpret_cast<CREATESTRUCT*>(lParam);
      m_eState = State_Realized;
      ::SetWindowText(m_hwnd, m_pwszName);
      m_rc.left = 0;
      m_rc.top = 0;
      m_rc.right = pCreate->cx;
      m_rc.bottom = pCreate->cy;

      foreach (EnumBox, oEnum, this) {
        auto const pBox = oEnum.Get();
        *pBox->GetRect() = m_rc;
        pBox->Realize(*this);
      }
      break;
    }

    case WM_DESTROY:
      #if DEBUG_DESTROY
       DEBUG_PRINTF("WM_DESTROY %p\n", this);
      #endif

      m_eState = State_Destroyed;
      while (m_oBoxes.GetFirst()) {
        m_oBoxes.GetFirst()->GetWindow()->Destroy();
      }
      break;

    case WM_LBUTTONDOWN: {
      Point pt(MAKEPOINTS(lParam));
      switch (hitTest(pt, &m_oSplitterDrag.m_pBox)) {
        case Element_Splitter:
          m_oSplitterDrag.Start(m_hwnd, SplitterDrag::State_Drag);
          break;

        case Element_SplitterBig:
          m_oSplitterDrag.Start(m_hwnd, SplitterDrag::State_DragSingle);
          break;
        }
        return 0;
    }

    case WM_LBUTTONUP:
      switch (m_oSplitterDrag.m_eState) {
        case SplitterDrag::State_Drag:
        case SplitterDrag::State_DragSingle: {
          auto const pBelow = m_oSplitterDrag.m_pBox;
          auto const pAbove = pBelow->GetPrev();
          if (!pAbove) {
              break;
          }

          Point pt(MAKEPOINTS(lParam));
          auto const cyMin = k_cyMinBox;
          if (pt.y - pAbove->GetRect()->top < cyMin) {
            pBelow->GetRect()->top = pAbove->GetRect()->top;
            pAbove->GetWindow()->Destroy();
            setBoxPos(pBelow);
            drawSplitters();
          } else if (pBelow->GetRect()->bottom - pt.y < k_cyMinBox) {
            pAbove->GetRect()->bottom = pBelow->GetRect()->bottom;
            pBelow->GetWindow()->Destroy();
            setBoxPos(pAbove);
            drawSplitters();
          }
          m_oSplitterDrag.Stop();
          break;
        }
      }
      return 0;

    case WM_MOUSEMOVE: {
      switch (m_oSplitterDrag.m_eState) {
        case SplitterDrag::State_Drag:
        case SplitterDrag::State_DragSingle: {
          auto const pBelow = m_oSplitterDrag.m_pBox;
          auto const pBelowPrev = pBelow->GetPrev();
          auto const pAbove = pBelowPrev
              ? pBelowPrev
              : splitVertically(pBelow, k_cySplitter);

          Point pt(MAKEPOINTS(lParam));
          if (pt.y - pAbove->GetRect()->top <= 0) {
            // Above box is too small.
          } else if (pBelow->GetRect()->bottom - (pt.y + k_cySplitter) <= 0) {
            // Below box is too small.
          } else {
            pAbove->GetRect()->bottom = pt.y;
            pBelow->GetRect()->top = pt.y + k_cySplitter;
            setBoxPos(pAbove);
            setBoxPos(pBelow);
          }

          drawSplitters();
          break;
        }
      }
      return 0;
    }

    case WM_PAINT: {
      PAINTSTRUCT ps;
      auto const hdc = ::BeginPaint(m_hwnd, &ps);
      #if DEBUG_REDRAW
        DEBUG_PRINTF(L"WM_PAINT %p (%d,%d)-(%d,%d)\n",
            this,
            ps.rcPaint.left, ps.rcPaint.top,
            ps.rcPaint.right, ps.rcPaint.bottom);
      #endif
      drawSplitters(hdc);
      ::EndPaint(m_hwnd, &ps);
      return 0;
    }

    case WM_PARENTNOTIFY:
      switch (wParam) {
        case WM_DESTROY: {
          auto const hwnd = reinterpret_cast<HWND>(lParam);
          foreach (EnumBox, oEnum, this) {
              auto const pBox = oEnum.Get();
              if (*pBox->GetWindow() != hwnd) {
                continue;
              }

              #if DEBUG_DESTROY
                DEBUG_PRINTF("WM_PARENTNOTIFY %p WM_DESTROY box=%p\n",
                    this, pBox);
              #endif

              auto const pAbove = pBox->GetPrev();
              auto const pBelow = pBox->GetNext();

              m_oWindows.Delete(pBox->GetWindow());
              m_oBoxes.Delete(pBox);

              pBox->DetachWindow();

              auto rc = *pBox->GetRect();
              delete pBox;

              if (pAbove) {
                // Extend pane above.
                pAbove->GetRect()->bottom = rc.bottom;
                setBoxPos(pAbove);
                break;
              }

              if (pBelow) {
                // Extend pane below.
                pBelow->GetRect()->top = rc.top;
                setBoxPos(pBelow);
                break;
              }

              if (State_Realized == m_eState) {
                // There is no window in this pane. So, we delete
                // this pane.
                ::DestroyWindow(*this);
              }
              break;
          }
          break;
        }
      }
      return 0;

    case WM_SETCURSOR: {
      POINT pt;
      if (!::GetCursorPos(&pt)) {
        return FALSE;
      }

      if (!::ScreenToClient(m_hwnd, &pt)) {
        return FALSE;
      }

      LeafBox* pBox;
      switch (hitTest(pt, &pBox)) {
        case Element_Splitter:
        case Element_SplitterBig:
          if (!s_hVSplitCursor) {
            s_hVSplitCursor = ::LoadCursor(
                g_hInstance,
                MAKEINTRESOURCE(IDC_VSPLIT));
          }
          ::SetCursor(s_hVSplitCursor);
          break;

        default:
          ::SetCursor(::LoadCursor(nullptr, IDC_ARROW));
          break;
      }
      return TRUE;
    }

    case WM_SETFOCUS:
      #if DEBUG_FOCUS
          DEBUG_PRINTF(L"WM_SETFOCUS %p\n", this);
      #endif

      Pane::onMessage(uMsg, wParam, lParam);
      if (auto const pWindow = GetActiveWindow()) {
        pWindow->Activate();
        GetBuffer()->UpdateFileStatus(true);
      }
      return 0;

    case WM_SIZE:
      goto resize;

    case WM_VSCROLL: {
      auto const pBox = reinterpret_cast<LeafBox*>(
          ::GetWindowLongPtr(
              reinterpret_cast<HWND>(lParam),
              GWLP_USERDATA));
      if (auto const pWindow = pBox->GetWindow()) {
        pWindow->SendMessage(WM_VSCROLL, wParam, lParam);
      }
      return 0;
    }

    case WM_WINDOWPOSCHANGED: {
      auto const wp = reinterpret_cast<const WINDOWPOS*>(lParam);
      if (wp->flags & SWP_NOSIZE) {
        return 0;
      }

      goto resize;
    }

    resize: {
      auto rcOld = m_rc;
      ::GetClientRect(*this, &m_rc);
      auto const cyNewPane = m_rc.bottom  - m_rc.top;
      auto const cyOldPane = rcOld.bottom - rcOld.top;

      if (!cyOldPane) {
        auto cBoxes = 0;
        foreach (EnumBox, oEnum, this) {
          cBoxes += 1;
        }

        if (!cBoxes) {
          return 0;
        }

        auto const cyNewWin = cyNewPane / cBoxes;
        auto yBox = m_rc.top;
        auto cySplitter = 0;
        auto pBox = static_cast<LeafBox*>(nullptr);
        foreach (EnumBox, oEnum, this) {
          pBox = oEnum.Get();
          auto const prc = pBox->GetRect();
          yBox += cySplitter;
          prc->top = yBox;
          yBox += cyNewWin;
          prc->bottom = yBox;
          cySplitter = k_cySplitter;
        }

        if (pBox) {
          pBox->GetRect()->bottom = m_rc.bottom;
        }
      } else {
        tryAgain:
          auto yBox = m_rc.top;
          auto cySplitter = 0;
          auto pBox = static_cast<LeafBox*>(nullptr);
          foreach (EnumBox, oEnum, this) {
            pBox = oEnum.Get();
            auto const prc = pBox->GetRect();
            auto const cyOldWin = prc->bottom - prc->top;
            auto const cyNewWin = cyNewPane * cyOldWin / cyOldPane;
            if (cyNewWin < k_cyMinBox) {
              pBox->GetWindow()->Destroy();
              goto tryAgain;
            }
            yBox += cySplitter;
            prc->top = yBox;
            yBox += cyNewWin;
            prc->bottom = yBox;
            cySplitter = k_cySplitter;
          }

          if (!pBox) {
            return 0;
          }
          pBox->GetRect()->bottom = m_rc.bottom;
      }

      foreach (EnumBox, oEnum, this) {
        auto const pBox = oEnum.Get();
        pBox->GetRect()->left = m_rc.left;
        pBox->GetRect()->right = m_rc.right;
        setBoxPos(pBox);
      }

      drawSplitters();
      return 0;
    }

    case TextEditWindow::WN_QueryClose:
      // Do we have multiple frame?
      if (Application::Get()->HasMultipleFrames()) {
        // We have mutliple frame. So, we have at least one frame even if
        // we destroy frame contains this pane.
        return TRUE;
      }

      if (HasMultipleWindows()) {
        // This pane won't be closed when close specified window.
        return TRUE;
      }
      return Application::Get()->CanExit();
  }

  return Pane::onMessage(uMsg, wParam, lParam);
}

void EditPane::LeafBox::Realize(HWND hwndParent) {
  m_hwndVScrollBar = ::CreateWindowExW(
        0,
        L"SCROLLBAR",
        nullptr, // title
        WS_CHILD | WS_VISIBLE | SBS_VERT,
        0, // x
        0, // y
        0, // width
        0, // height
        hwndParent, // parent
        nullptr, // menu
        g_hInstance,
        nullptr);

  ::SetWindowLongPtr(
      m_hwndVScrollBar,
      GWLP_USERDATA,
      reinterpret_cast<LONG_PTR>(this));

   m_pWindow->CreateWindowEx(0, nullptr, WS_CHILD | WS_VISIBLE, hwndParent);
   m_pWindow->SetScrollBar(m_hwndVScrollBar, SB_VERT);
}

void EditPane::setupStatusBar() {
  static const int rgiWidth[] = {
      25, // ins/ovf
      70, // posn
      40, // column
      50, // line
      32, // newline
      50, // code page
      70, // mode
      0,
  };

  int rgiRight[ARRAYSIZE(rgiWidth)];
  auto iRight = GetFrame()->GetCxStatusBar();
  for (auto i = 0u; i < ARRAYSIZE(rgiRight); i++) {
    rgiRight[ARRAYSIZE(rgiRight) - i - 1] = iRight;
    iRight -= rgiWidth[i];
  }
  GetFrame()->SetStatusBarParts(rgiRight, lengthof(rgiRight));
}

void EditPane::setBoxPos(LeafBox* pBox) const {
  auto const pWindow = pBox->GetWindow();
  auto const hwndVScrollBar = pWindow->GetScrollBarHwnd(SB_VERT);
  auto const prc = pBox->GetRect();

  #if DEBUG_SPLIT
    DEBUG_PRINTF(L"%p %p %d+%d-%d+%d\n",
        this,
        pWindow,
        prc->left, prc->top, prc->right, prc->bottom);
  #endif

  auto cxVScroll = hwndVScrollBar ? ::GetSystemMetrics(SM_CXVSCROLL) : 0;

  if (hwndVScrollBar) {
    auto const cySplitter = HasMultipleWindows() ? 0 : k_cySplitterBig;
    ::SetWindowPos(
        hwndVScrollBar,
        nullptr,
        prc->right - cxVScroll,
        prc->top + cySplitter,
        cxVScroll,
        prc->bottom - prc->top - cySplitter,
        SWP_NOZORDER);
  }

  ::SetWindowPos(
      *pWindow,
      nullptr,
      prc->left,
      prc->top,
      prc->right - prc->left - cxVScroll,
      prc->bottom - prc->top,
      SWP_NOZORDER);
}

EditPane::Window* EditPane::SplitVertically() {
  auto const pBelow = GetActiveLeafBox();
  auto const prcBelow = pBelow->GetRect();

  // Active Box is too small to split.
  auto const cyBox = prcBelow->bottom - prcBelow->top;
  if (cyBox < k_cyMinBox * 2 + k_cySplitter) {
    return nullptr;
  }

  splitVertically(pBelow, cyBox / 2);
  pBelow->GetWindow()->MakeSelectionVisible();
  return pBelow->GetWindow();
}

EditPane::LeafBox* EditPane::splitVertically(LeafBox* pBelow, int cyBox) {
  auto const prcBelow = pBelow->GetRect();
  ASSERT(prcBelow->bottom - prcBelow->top > cyBox);

  auto const pWindow = new Window(
      this,
      pBelow->GetWindow()->GetBuffer(),
      pBelow->GetWindow()->GetStart());

  auto const pSelection = pBelow->GetWindow()->GetSelection();

  pWindow->GetSelection()->SetRange(
      pSelection->GetStart(),
      pSelection->GetEnd());

  pWindow->GetSelection()->SetStartIsActive(
      pSelection->IsStartActive());

  auto const pAbove = new LeafBox(pWindow);
  auto const prcAbove = pAbove->GetRect();

  m_oBoxes.InsertBefore(pAbove, pBelow);
  m_oWindows.InsertBefore(pWindow, pBelow->GetWindow());
  pAbove->Realize(*this);

  prcAbove->left = prcBelow->left;
  prcAbove->right = prcBelow->right;
  prcAbove->top = prcBelow->top;
  prcAbove->bottom = prcBelow->top + cyBox;

  prcBelow->top = prcAbove->bottom + k_cySplitter;

  setBoxPos(pBelow);
  setBoxPos(pAbove);

  drawSplitters();
  return pAbove;
}

void EditPane::UpdateStatusBar() {
  setupStatusBar();

  auto const pBuffer = GetActiveWindow()->GetBuffer();

  GetFrame()->ShowMessage(
      MessageLevel_Idle,
      (
        pBuffer->IsNotReady()
            ? IDS_STATUS_BUSY
            : GetActiveWindow()->HasFocus()
                ? IDS_STATUS_READY
                : 0
     ));

  GetFrame()->SetStatusBarf(
      StatusBarPart_Mode,
      pBuffer->GetMode()->GetName());

  GetFrame()->SetStatusBarf(
      StatusBarPart_CodePage,
      L"CP%u",
      pBuffer->GetCodePage());

  GetFrame()->SetStatusBarf(
      StatusBarPart_Newline,
      k_rgwszNewline[pBuffer->GetNewline()]);

  auto const pSelection = GetActiveWindow()->GetSelection();

  // FIXME 2007-07-18 yosi We should use lazy evaluation object for
  // computing line number of column or cache.
  Selection::Information oInfo;
  pSelection->GetInformation(&oInfo);

  GetFrame()->SetStatusBarf(
      StatusBarPart_LineNumber,
      L"Ln %d%s",
      oInfo.m_lLineNum,
      oInfo.m_fLineNum ? L"" : L"+");

  GetFrame()->SetStatusBarf(
      StatusBarPart_Column,
      L"Cn %d%s",
      oInfo.m_lColumn,
      oInfo.m_fColumn ? L"" : L"+");

  GetFrame()->SetStatusBarf(
      StatusBarPart_Posn,
      L"Ch %d",
      pSelection->IsStartActive() ?
          pSelection->GetStart() : pSelection->GetEnd());

  // FIXME 2007-07-25 yosi@msn.com We need to show "OVR" if
  // we are in overwrite mode.
  GetFrame()->SetStatusBarf(
      StatusBarPart_Insert,
      pBuffer->IsReadOnly() ? L"R/O" : L"INS",
      pBuffer->GetStart());
}
