#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - winapp - Text Edit Window
// listener/winapp/vi_TextEditWindow.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_TextEditWindow.cpp#3 $
//
#define DEBUG_AUTOSCROLL 0
#define DEBUG_CARET 0
#define DEBUG_FOCUS _DEBUG
#define DEBUG_IDLE 0
#define DEBUG_KEY 0
#define DEBUG_PAINT _DEBUG
#define DEBUG_REDRAW 0
#define DEBUG_RESIZE _DEBUG
#define DEBUG_SCROLL 0
#define DEBUG_SHOW_HIDE _DEBUG
#include "./vi_TextEditWindow.h"

#include "./ed_Style.h"
#include "./gfx_base.h"
#include "./vi_Application.h"
#include "./vi_Buffer.h"
#include "./vi_EditPane.h"
#include "./vi_Frame.h"
#include "./vi_Selection.h"
#include "./vi_util.h"

extern HWND g_hwndActiveDialog;

namespace Command {
uint TranslateKey(uint);
}

namespace {
//////////////////////////////////////////////////////////////////////
//
// Caret
//
// Description:
// Represents caret in per-thread queue. To blink caret, we must track
// caret size. If we call CreateCaret, caret doesn't blink.
//
class Caret {
  private: SIZE m_size;
  private: POINT m_pt;
  private: HWND m_hwnd;
  private: bool m_fShow;

  public: Caret()
    : m_hwnd(nullptr),
      m_fShow(false) {
  }

  public: void Destroy() {
    m_hwnd = nullptr;
    ::DestroyCaret();
    #if DEBUG_CARET
        DEBUG_PRINTF("%p\n", this);
    #endif // DEBUG_CARET
    m_size.cx = -1;
    m_pt.x = -1;
  }

  // Hide - Hide caret
  public: void Hide() {
    if (!m_hwnd)
      return;
    if (!m_fShow)
      return;
    #if DEBUG_CARET
      DEBUG_PRINTF("%p\n", this);
    #endif // DEBUG_CARET

    ::HideCaret(m_hwnd);
    m_fShow = false;
  }

  // IsDirty - Returns true if position or size of caret is changed
  // since last show.
  public: bool isDirty(HWND hwnd, SIZE size) const {
    if (m_hwnd != hwnd) {
      #if DEBUG_CARET
          DEBUG_PRINTF("m_hwnd\n");
      #endif
      return true;
    }

    if (m_size.cx != size.cx) {
      #if DEBUG_CARET
          DEBUG_PRINTF("m_size.cx %d %d\n", m_size.cx, size.cx);
      #endif
      return true;
    }

    if (m_size.cy != size.cy) {
      #if DEBUG_CARET
          DEBUG_PRINTF("m_size.cy\n");
      #endif
      return true;
    }

    return false;
  }

  // Show - Show caret at pt with size.
  public: void Show(HWND hwnd, SIZE size, POINT pt) {
    if (isDirty(hwnd, size)) {
      m_hwnd = hwnd;
      m_size = size;
      ::CreateCaret(m_hwnd, nullptr, size.cx, size.cy);
      #if DEBUG_CARET
        DEBUG_PRINTF("Create %dx%d\r\n", size.cx, size.cy);
      #endif // DEBUG_CARET
    }

    if (m_pt.x != pt.x || m_pt.y != pt.y) {
      m_pt = pt;
      ::SetCaretPos(m_pt.x, m_pt.y);
      #if DEBUG_CARET
        DEBUG_PRINTF("SetCaretPos (%d, %d)\r\n", pt.x, pt.y);
      #endif // DEBUG_CARET
    }

    #if DEBUG_CARET
      DEBUG_PRINTF("ShowCaret: (%d, %d) %dx%d\n",
          m_pt.x, m_pt.y, m_size.cx, m_size.cy);
    #endif // DEBUG_CARET
    ::ShowCaret(m_hwnd);
    m_fShow = true;
  }
};

Caret g_oCaret;

enum MyTimerId {
  MyTimerId_AutoScroll = 1,
  MyTimerId_Blink = 2,
};

static int s_active_tick;

} // namespace

void TextEditWindow::AutoScroll::Continue(HWND hwnd) {
  #if DEBUG_AUTOSCROLL
    DEBUG_PRINTF("\n");
  #endif // DEBUG_AUTOSCROLL

  m_nTimerId = MyTimerId_AutoScroll;
  ::SetTimer(hwnd, m_nTimerId, 50, nullptr);
}

void TextEditWindow::AutoScroll::Start(HWND hwnd, int iDir) {
  if (!m_nTimerId) {
    m_nStartTick = ::GetTickCount();
    Continue(hwnd);
  }
  m_iDirection = iDir;
}

void TextEditWindow::AutoScroll::Stop(HWND hwnd) {
  if (!m_nTimerId)
    return;

  #if DEBUG_AUTOSCROLL
    DEBUG_PRINTF("id=%d\n", m_nTimterid);
  #endif // DEBUG_AUTOSCROLL

  ::KillTimer(hwnd, m_nTimerId);
  m_nTimerId = 0;
}

TextEditWindow::TextEditWindow(void* pvHost, Buffer* pBuffer, Posn lStart)
    : m_eDragMode(DragMode_None),
      m_fBlink(false),
      m_fHasFocus(false),
      m_gfx(nullptr),
      m_lCaretPosn(-1),
      m_nActiveTick(0),
      m_nBlinkTimerId(0),
      m_pBlink(pBuffer->CreateRange(0,0)),
      m_pPage(new Page()),
      ALLOW_THIS_IN_INITIALIZER_LIST(selection_(
          new(pBuffer->GetHeap()) Selection(this, pBuffer))),
      m_pViewRange(pBuffer->CreateRange(lStart)),
      #if SUPPORT_IME
        m_fImeTarget(false),
        m_lImeEnd(0),
        m_lImeStart(0),
      #endif // SUPPORT_IME
      m_pvHost(pvHost),
      show_count_(0) {
  pBuffer->AddWindow(this);
}

TextEditWindow::~TextEditWindow() {
  // TODO: We should not use m_hwnd in TextEditWindow.
  m_hwnd = nullptr;
  GetSelection()->GetBuffer()->RemoveWindow(this);
}

void TextEditWindow::Activate() {
  m_fHasFocus = true;
}

void TextEditWindow::Blink(Posn lPosn, uint nMillisecond) {
  m_pBlink->SetRange(lPosn, lPosn);
  m_fBlink = true;
  Redraw();
  if (!m_nBlinkTimerId) {
    m_nBlinkTimerId = MyTimerId_Blink;
    ::SetTimer(m_hwnd, m_nBlinkTimerId, nMillisecond, nullptr);
  }
}

Posn TextEditWindow::computeGoalX(float xGoal, Posn lGoal) {
  if (xGoal < 0)
    return lGoal;

  Page::Line* pLine = nullptr;

  if (!m_pPage->IsDirty(m_rc, *selection_))
    pLine = m_pPage->FindLine(lGoal);

  if (pLine)
    return pLine->MapXToPosn(*m_gfx, xGoal);

  auto lStart = GetBuffer()->ComputeStartOf(Unit_Paragraph, lGoal);
  Page oPage;
  gfx::RectF page_rect(m_rc);
  for (;;) {
    auto const pLine = oPage.FormatLine(*m_gfx, page_rect,
                                        *selection_, lStart);
    auto const lEnd = pLine->GetEnd();
    if (lGoal < lEnd)
      return pLine->MapXToPosn(*m_gfx, xGoal);
    lStart = lEnd;
  }
}

Count TextEditWindow::ComputeMotion(Unit eUnit, Count n,
                                    const gfx::PointF& pt,
                                    Posn* inout_lPosn)  {
  switch (eUnit) {
    case Unit_Line:
      if (n > 0) {
        auto const lBufEnd = GetBuffer()->GetEnd();
        auto lGoal = *inout_lPosn;
        auto k = 0;
        for (k = 0; k < n; k++) {
          lGoal = EndOfLine(lGoal);
          if (lGoal >= lBufEnd)
            break;
          lGoal += 1;
        }
        *inout_lPosn = computeGoalX(pt.x, min(lGoal, lBufEnd));
        return k;
      } else if (n < 0) {
        n = -n;

        auto const lBufStart = GetBuffer()->GetStart();
        auto lStart = *inout_lPosn;
        auto k = 0;
        for (k = 0; k < n; k++) {
          lStart = StartOfLine(lStart);
          if (lStart <= lBufStart)
            break;
          lStart -= 1;
        }

        *inout_lPosn = computeGoalX(pt.x, max(lStart, lBufStart));
        return k;
      }
      return 0;

    case Unit_Screen: {
      auto k = LargeScroll(0, n, false);
      if (k > 0) {
        auto const lStart = m_pPage->GetStart();
        m_pViewRange->SetRange(lStart, lStart);
        *inout_lPosn = MapPointToPosn(pt);
      } else if (n > 0) {
        *inout_lPosn = GetEnd();
        k = 1;
      } else if (n < 0) {
        *inout_lPosn = GetStart();
        k = 1;
      }
      return k;
    }

    case Unit_Window:
      if (n > 0) {
        *inout_lPosn = GetEnd();
        return 1;
      }
      if (n < 0) {
        *inout_lPosn = GetStart();
        return 1;
      }
      return 0;

    default:
      return GetBuffer()->ComputeMotion(eUnit, n, inout_lPosn);
  }
}

void TextEditWindow::Destroy() {
  GetHost<EditPane>()->WillDestroyWindow(*this);
  delete this;
}

Posn TextEditWindow::EndOfLine(Posn lPosn) {
  if (!m_pPage->IsDirty(m_rc, *selection_)) {
    auto const pLine = m_pPage->FindLine(lPosn);
    if (pLine)
      return pLine->GetEnd() - 1;
  }

  auto const lBufEnd = selection_->GetBuffer()->GetEnd();
  return lPosn >= lBufEnd ? lBufEnd : endOfLineAux(*m_gfx, lPosn);
}

Posn TextEditWindow::endOfLineAux(const gfx::Graphics& gfx, Posn lPosn) {
  auto const lBufEnd = selection_->GetBuffer()->GetEnd();
  if (lPosn >= lBufEnd)
    return lBufEnd;

  Page oPage;
  gfx::RectF page_rect(m_rc);
  auto lStart = selection_->GetBuffer()->ComputeStartOf(Unit_Paragraph, lPosn);
  for (;;) {
    auto const pLine = oPage.FormatLine(gfx, page_rect, *selection_, lStart);
    lStart = pLine->GetEnd();
    if (lPosn < lStart)
      return lStart - 1;
  }
}

void TextEditWindow::format(const gfx::Graphics& gfx, Posn lStart) {
  m_pPage->Format(gfx, gfx::RectF(m_rc), *selection_, lStart);
}

Buffer* TextEditWindow::GetBuffer() const {
  return selection_->GetBuffer();
}

Count TextEditWindow::GetColumn(Posn lPosn) {
  auto const lStart = StartOfLine(lPosn);
  return lPosn - lStart;
}

HCURSOR TextEditWindow::GetCursorAt(const Point&) const {
  return ::LoadCursor(nullptr, MAKEINTRESOURCE(IDC_IBEAM));
}


// For Selection.MoveDown Screen
Posn TextEditWindow::GetEnd() {
  updateScreen();
  return m_pPage->GetEnd();
}

HWND TextEditWindow::GetScrollBarHwnd(int nBar) const {
  if (nBar == SB_VERT)
    return m_oVertScrollBar.GetHwnd();
  return nullptr;
}

//For Selection.MoveUp Screen
Posn TextEditWindow::GetStart() {
  updateScreen();
 return m_pPage->GetStart();
}

void TextEditWindow::Hide() {
  #if DEBUG_SHOW_HIDE
    DEBUG_PRINTF("%p show=%d |%ls|\n", this, show_count_,
                 GetBuffer()->GetName());
  #endif
  show_count_ = 0;
}

int TextEditWindow::LargeScroll(int, int iDy, bool fRender) {
  updateScreen();

  if (iDy < 0) {
    // Scroll Down -- place top line out of window.
    iDy = -iDy;

    auto const lBufStart = selection_->GetBuffer()->GetStart();
    int k;
    for (k = 0; k < iDy; k++) {
      auto const lStart = m_pPage->GetStart();
      if (lStart == lBufStart)
        break;

      // Scroll down until page start goes out to page.
      do {
        #if DEBUG_SCROLL
          DEBUG_PRINTF("scroll down lStart=%d\n", lStart);
        #endif
        if (!m_pPage->ScrollDown(*m_gfx))
          break;
      } while (m_pPage->GetEnd() != lStart);
    }

     if (fRender && k > 0)
       Render();
      return k;
  } else if (iDy > 0) {
    // Scroll Up -- format page from page end.
    const Posn lBufEnd = selection_->GetBuffer()->GetEnd();
    int k;
    for (k = 0; k < iDy; k++) {
      auto const lStart = m_pPage->GetEnd();
      if (lStart >= lBufEnd)
        break;
      #if DEBUG_SCROLL
        DEBUG_PRINTF("scroll up lStart=%d\n", lStart);
      #endif
      format(*m_gfx, lStart);
    }

    if (fRender && k > 0)
      Render();
    return k;
  }
  return 0;
}

Command::KeyBindEntry* TextEditWindow::MapKey(uint nKey) {
  return GetBuffer()->MapKey(nKey);
}

void TextEditWindow::MakeSelectionVisible() {
  #if DEBUG_CARET
    DEBUG_PRINTF("%p [%d,%d]\n",
        this, selection_->GetStart(), selection_->GetEnd());
  #endif

  m_lCaretPosn = -1;
  redraw(true);
}

Posn TextEditWindow::MapPointToPosn(const gfx::PointF pt) {
  updateScreen();
  return m_pPage->MapPointToPosn(*m_gfx, pt);
}

// Description:
// Maps position specified buffer position and returns height
// of caret, If specified buffer position isn't in window, this function
// returns 0.
gfx::RectF TextEditWindow::MapPosnToPoint(Posn lPosn) {
  updateScreen();
  for (;;) {
    if (auto rect = m_pPage->MapPosnToPoint(*m_gfx, lPosn))
      return rect;
    m_pPage->ScrollToPosn(*m_gfx, lPosn);
  }
}

bool TextEditWindow::OnIdle(uint count) {
  auto const more = GetBuffer()->OnIdle(count);

  #if DEBUG_IDLE
    DEBUG_PRINTF("%p count=%d more=%d\n", this, count, more);
  #endif

  gfx::Graphics::DrawingScope drawing_scope(*m_gfx);
  Redraw();
  return more;
}

void TextEditWindow::OnLeftButtonDown(uint flags, const Point& point) {
  auto const lPosn = MapPointToPosn(point);
  if (lPosn < 0) {
    // Click outside window. We do nothing.
    return;
  }

  #if DEBUG_FOCUS
    DEBUG_PRINTF("WM_LBUTTONDOWN: p=%d\r\n", lPosn);
  #endif

  if (!m_fHasFocus) {
    m_fHasFocus = true;
    if (lPosn >= GetSelection()->GetStart() &&
        lPosn < GetSelection()->GetEnd()) {
     return;
    }
  }

  GetSelection()->MoveTo(lPosn, flags & MK_SHIFT);

  if (flags & MK_CONTROL) {
    selectWord(lPosn);
  } else {
    m_eDragMode = DragMode_Selection;
    ::SetCapture(m_hwnd);
  }
}

void TextEditWindow::OnLeftButtonUp(uint, const Point&) {
  if (m_eDragMode == DragMode_None)
    return;
  ::ReleaseCapture();
  stopDrag();
}

LRESULT TextEditWindow::onMessage(UINT uMsg, WPARAM wParam, LPARAM lParam) {
  return BaseWindow::onMessage(uMsg, wParam, lParam);
}

void TextEditWindow::OnMouseMove(uint, const Point& point) {
  if (m_eDragMode == DragMode_None) {
    // We have nothing to do if mouse isn't dragged.
    return;
  }

  if (::GetCapture() != m_hwnd){
    // Someone takes mouse capture.
    stopDrag();
    return;
  }

  auto const lPosn = MapPointToPosn(point);
  if (lPosn >= 0)
    selection_->MoveTo(lPosn, true);

  #if DEBUG_FORMAT
    DEBUG_PRINTF("WM_MOUSEMOVE: %d@%d\n", pt.x, pt.y);
  #endif // DEBUG_FORMAT

  if (point.y < m_rc.top)
    m_oAutoScroll.Start(m_hwnd, -1);
  else if (point.y > m_rc.bottom)
    m_oAutoScroll.Start(m_hwnd, 1);
  else
    m_oAutoScroll.Stop(m_hwnd);
}

TextEditWindow::MessageResult TextEditWindow::ForwardMessage(
    uint uMsg, WPARAM wParam, LPARAM lParam) {
  if (WM_SYSTIMER == uMsg) { // WM_SYSTIMER for blinking caret
    DEBUG_PRINTF("WM_SYSTIMER %p\r\n", this);
  }

  switch (uMsg) {
    case WM_CHAR: {
      // Ctrl+<key> is handled by WM_KEYDOWN.
      char16 wch = static_cast<char16>(wParam);
      if (wch >= 0x20) {
        m_fBlink = false;
        Application::Get()->Execute(this, wch, HIWORD(lParam) & KF_REPEAT);
      }
      break;
    }

    case WM_DESTROY:
      #if DEBUG_DESTROY
        DEBUG_PRINTF("WM_DESTROY %p\n", this);
      #endif
      m_nActiveTick = 0;
      break;

    case WM_APPCOMMAND:
      #if DEBUG_KEY
          DEBUG_PRINTF("WM_APPCOMMAND %p %x\n", this, lParam);
      #endif
      return TRUE;

    case WM_KEYDOWN: {
      #if DEBUG_KEY
        DEBUG_PRINTF("WM_KEYDOWN VKey=0x%0X 0x%04X 0x%04X\r\n",
            wParam, HIWORD(lParam), LOWORD(lParam));
      #endif

      auto const nVKey = static_cast<uint>(wParam);
      auto const nKey = Command::TranslateKey(nVKey);
      if (nKey) {
        m_fBlink = false;
        Application::Get()->Execute(this, nKey, HIWORD(lParam) & KF_REPEAT);
        return 0;
      }
      break;
    }

    case WM_KILLFOCUS:
      #if DEBUG_FOCUS
        DEBUG_PRINTF("WM_KILLFOCUS %p focus=%d |%ls|\n",
            this, m_fHasFocus, GetBuffer()->GetName());
      #endif // DEBUG_FOCUS

      m_fHasFocus = false;
      g_oCaret.Destroy();
      break;

    case WM_LBUTTONDBLCLK: {
      Point pt(MAKEPOINTS(lParam));
      auto const lPosn = MapPointToPosn(pt);
      if (lPosn >= 0)
        selectWord(lPosn);
      return 0;
    }

    case WM_LBUTTONDOWN:
      OnLeftButtonUp(static_cast<uint>(wParam), MAKEPOINTS(lParam));
      return 0;

    case WM_LBUTTONUP:
      OnLeftButtonUp(static_cast<uint>(wParam), MAKEPOINTS(lParam));
      return 0;

    case WM_MOUSEMOVE:
      OnMouseMove(static_cast<uint>(wParam), MAKEPOINTS(lParam));
      return 0;

    case WM_MOUSEWHEEL:
      SmallScroll(0, GET_WHEEL_DELTA_WPARAM(wParam) > 0 ? -2 : 2);
      return 0;

    case WM_USER: {
      gfx::Graphics::DrawingScope drawing_scope(*m_gfx);
      m_pPage->Reset();
      m_pPage->Render(*m_gfx);
      return 0;
    }

    case WM_PAINT: {
      #if DEBUG_PAINT
        DEBUG_PRINTF("~~~~~~~~~~ WM_PAINT Start\n");
      #endif
      // Note: We don't need to show/hide caret. See MSDN/Carets/Using
      // Carets/Hiding a Caret
      {
        RECT rc;
        if (::GetUpdateRect(m_hwnd, &rc, false)) {
          DEBUG_PRINTF("update_rect=(%d,%d)-(%d,%d)\n", rc.left, rc.top,
                       rc.right, rc.bottom);
        } else {
          DEBUG_PRINTF("GetUpdateRectFailed\n");
        }

        gfx::Graphics::DrawingScope drawing_scope(*m_gfx);
        (*m_gfx)->Clear(gfx::ColorF(gfx::ColorF::Red));
        m_pPage->Reset();
        m_pPage->Render(*m_gfx);
      }
      ::ValidateRect(*this, nullptr);
      #if DEBUG_PAINT
        DEBUG_PRINTF("~~~~~~~~~~ WM_PAINT End\n");
      #endif
      return 0;
    }

    case WM_SETCURSOR:
      if (HTCLIENT == LOWORD(lParam)) {
        ::SetCursor(GetCursorAt(Point()));
        return 1;
      }
      break;

    case WM_SETFOCUS:
      #if DEBUG_FOCUS
        DEBUG_PRINTF("WM_SETFOCUS %p focus=%d |%ls|\n",
            this, m_fHasFocus, GetBuffer()->GetName());
      #endif // DEBUG_FOCUS
      s_active_tick += 1;
      m_nActiveTick = s_active_tick;
      m_fHasFocus = true;
      return 0;

    case WM_SIZE:
      #if DEBUG_RESIZE
        DEBUG_PRINTF("WM_SIZE %p %dx%d\n",
            this, LOWORD(lParam), HIWORD(lParam));
      #endif
      return 0;

    case WM_TIMER: {
      #if DEBUG_AUTOSCROLL
      DEBUG_PRINTF("WM_TIMER: %d dir=%d\n",
          wParam, m_oAutoScroll.m_iDirection);
      #endif // DEBUG_AUTOSCROLL

      switch (wParam) {
        case MyTimerId_AutoScroll:
          if (m_oAutoScroll.m_nTimerId) {
              auto const iDuration = ::GetTickCount() -
                  m_oAutoScroll.m_nStartTick;
              auto lCount = iDuration / 500;
              lCount = max(lCount, 1);
              lCount = min(lCount, 20);

              Count k;
              if (m_oAutoScroll.m_iDirection > 0) {
                  k = selection_->MoveDown(Unit_Line, lCount, true);
              } else {
                  k = selection_->MoveUp(Unit_Line, lCount, true);
              }

              if (!k) {
                m_oAutoScroll.Stop(m_hwnd);
              } else {
                m_oAutoScroll.Continue(m_hwnd);
              }
          }
          break;

        case MyTimerId_Blink:
          ::KillTimer(m_hwnd, wParam);
          m_nBlinkTimerId = 0;
          m_fBlink = false;
          break;
      }
      break;
    }

    case WM_VSCROLL:
      onVScroll(LOWORD(wParam));
      return 0;

    case WM_WINDOWPOSCHANGED: {
      // DefWindowProc sents WM_SIZE and WM_MOVE, so handling
      // WM_WINDPOSCHANGED is faster than DefWindowProc.
      auto const wp = reinterpret_cast<const WINDOWPOS*>(lParam);
      if (wp->flags & SWP_NOSIZE)
        return 0;
      #if DEBUG_REDRAW || DEBUG_RESIZE
        DEBUG_PRINTF("WM_WINDOWPOSCHANGED %p 0x%X %dx%d+%d+%d\n",
                     this, wp->flags, wp->cx, wp->cy, wp->x, wp->y);
      #endif
      Rect rect;
      ::GetClientRect(m_hwnd, &rect);
      Resize(rect);
      return 0;
    }

    #if SUPPORT_IME
    case WM_IME_COMPOSITION:
      onImeComposition(lParam);
      return 0;

    case WM_IME_ENDCOMPOSITION:
      m_fImeTarget = false;
      return 0;

    case WM_IME_REQUEST:
      if (IMR_RECONVERTSTRING == wParam) {
          return setReconvert(
              reinterpret_cast<RECONVERTSTRING*>(lParam),
              GetSelection()->GetStart(),
              GetSelection()->GetEnd());
      }
      break;

    case WM_IME_SETCONTEXT:
      // We draw composition string instead of IME. So, we don't
      // need default composition window.
      lParam &= ~ISC_SHOWUICOMPOSITIONWINDOW;
      break;

    case WM_IME_STARTCOMPOSITION:
      if (!m_fImeTarget) {
        m_lImeStart = GetSelection()->GetStart();
        m_lImeEnd = m_lImeStart;
        m_fImeTarget = false;
      }
      return 0;
    #endif // SUPPORT_IME
  }
  return MessageResult();
}

void TextEditWindow::onVScroll(uint nCode) {
  switch (nCode) {
    case SB_ENDSCROLL: // 8
      break;

    case SB_LINEDOWN: // 1
      SmallScroll(0, 1);
      break;

    case SB_LINEUP: // 0
      SmallScroll(0, -1);
      break;

    case SB_PAGEDOWN: // 3
      LargeScroll(0, 1);
      break;

    case SB_PAGEUP: // 2
      LargeScroll(0, -1);
      break;

    case SB_THUMBPOSITION: // 4
      return;

    case SB_THUMBTRACK: { // 5
      SCROLLINFO oInfo;
      oInfo.cbSize = sizeof(oInfo);
      oInfo.fMask = SIF_ALL;
      if (m_oVertScrollBar.GetInfo(&oInfo)) {
        auto const lStart = startOfLineAux(*m_gfx, oInfo.nTrackPos);
        format(*m_gfx, lStart);
        Render();
      }
      break;
    }

    default:
      return;
  }
}

void TextEditWindow::Realize(HWND hwnd, const gfx::Graphics& gfx,
                             const Rect& rect) {
  ASSERT(!m_gfx);
  ASSERT(!show_count_);
  m_hwnd = hwnd;
  m_gfx = &gfx;
  show_count_ = 1;
  Resize(rect);
}

void TextEditWindow::Redraw() {
  auto fSelectionActive = m_fHasFocus;

  if (g_hwndActiveDialog) {
    auto const edit_pane = Application::Get()->GetActiveFrame()->
        GetActivePane()->DynamicCast<EditPane>();
    if (edit_pane)
      fSelectionActive = edit_pane->GetActiveWindow() == this;
  }
  redraw(fSelectionActive);
}

void TextEditWindow::redraw(bool fSelectionIsActive) {
  #if DEBUG_REDRAW
    DEBUG_PRINTF("~~~~~~~~~~ Start selection_is_active=%d\n",
                 fSelectionIsActive);
  #endif
  Posn lCaretPosn;
  Posn lSelStart;
  Posn lSelEnd;

  if (m_fBlink) {
    lSelStart = m_pBlink->GetStart();
    lSelEnd = m_pBlink->GetEnd();
  } else {
    lSelStart = selection_->GetStart();
    lSelEnd = selection_->GetEnd();
  }

  if (fSelectionIsActive) {
    lCaretPosn = selection_->IsStartActive() ? lSelStart : lSelEnd;

    // EvEdit 1.0's highlight color
    //selection_->SetColor(Color(0, 0, 0));
    //selection_->SetBackground(Color(0xCC, 0xCC, 0xFF));

    // We should not use GetSysColor. If we want to use here
    // default background must be obtained from GetSysColor.
    //selection_->SetColor(::GetSysColor(COLOR_HIGHLIGHTTEXT));
    //selection_->SetBackground(::GetSysColor(COLOR_HIGHLIGHT));

    // We use Vista's highlight color.
    selection_->SetColor(Color(255, 255, 255));
    selection_->SetBackground(Color(51, 153, 255));
  } else {
    lCaretPosn = m_lCaretPosn;

    Posn lEnd = GetBuffer()->GetEnd();
    if (lSelStart == lEnd && lSelEnd == lEnd)
      lCaretPosn = lEnd;

     selection_->SetColor(Color(67, 78, 84));
     selection_->SetBackground(Color(191, 205, 219));
  }

  {
    auto const lStart = m_pViewRange->GetStart();
    if (m_pPage->IsDirty(m_rc, *selection_, fSelectionIsActive)) {
      #if DEBUG_REDRAW
        DEBUG_PRINTF("Page %p is dirty. lStart=%d\n", m_pPage, lStart);
      #endif // DEBUG_REDRAW
      format(*m_gfx, startOfLineAux(*m_gfx, lStart));

      if (m_lCaretPosn != lCaretPosn) {
        // FIXME 2007-05-12 Fill the page with lines.
        #if DEBUG_REDRAW
          DEBUG_PRINTF("ScrollToPosn %d\n", lCaretPosn);
        #endif
        m_pPage->ScrollToPosn(*m_gfx, lCaretPosn);
        m_lCaretPosn = lCaretPosn;
      }
    } else if (m_lCaretPosn != lCaretPosn) {
        m_pPage->ScrollToPosn(*m_gfx, lCaretPosn);
        m_lCaretPosn = lCaretPosn;
    } else if (m_pPage->GetStart() != lStart) {
        #if DEBUG_REDRAW
        DEBUG_PRINTF("Page %p change start %d to %d\n",
            m_pPage, m_pPage->GetStart(), lStart);
        #endif // DEBUG_REDRAW
        format(*m_gfx, startOfLineAux(*m_gfx, lStart));
    }
  }

  render(*m_gfx);

  #if DEBUG_REDRAW
    DEBUG_PRINTF("~~~~~~~~~~ End Page=[%d,%d]\n",
        m_pPage->GetStart(), m_pPage->GetEnd());
  #endif
}

void TextEditWindow::Render() {
  gfx::Graphics::DrawingScope drawing_scope(*m_gfx);
  render(*m_gfx);
}

void TextEditWindow::render(const gfx::Graphics& gfx) {
  if (show_count_ <= 0)
    return;

  if (m_fHasFocus)
    g_oCaret.Hide();

  if (gfx.drawing()) {
    m_pPage->Render(gfx);
  } else {
    gfx::Graphics::DrawingScope drawing_scope(gfx);
    m_pPage->Render(gfx);
  }

  {
    auto const lStart = m_pPage->GetStart();
    m_pViewRange->SetRange(lStart, lStart);
    updateScrollBar();
  }

  if (!m_fHasFocus)
    return;

  const auto rect = m_pPage->MapPosnToPoint(gfx, m_lCaretPosn);
  if (!rect)
    return;
  ASSERT(rect.height() > 0);
  SIZE size = {
    max(::GetSystemMetrics(SM_CXBORDER), 2),
    static_cast<int>(rect.height())
  };

  POINT pt = {
    static_cast<int>(rect.left),
    static_cast<int>(rect.top)
  };

  #if SUPPORT_IME
    if (m_fImeTarget) {
        if (showImeCaret(size, pt))
          return;
        DEBUG_PRINTF("showImeCaret failed.\r\n");
    }
  #endif // SUPPORT_IME
  g_oCaret.Show(m_hwnd, size, pt);
}

void TextEditWindow::Resize(const Rect& rect) {
  #if DEBUG_RESIZE
    DEBUG_PRINTF("%p (%d,%d)x(%d,%d) show=%d |%ls|\n",
        this,
        rect.left, rect.top, rect.width(), rect.height(),
        show_count_,
        GetBuffer()->GetName());
  #endif
  m_rc = rect;
  m_pPage->Reset();
  Redraw();
}

void TextEditWindow::selectWord(Posn lPosn) {
  auto const pSelection = GetSelection();
  pSelection->SetStart(lPosn);
  pSelection->StartOf(Unit_Word);
  pSelection->EndOf(Unit_Word, true);
  pSelection->SetStartIsActive(false);
}

void TextEditWindow::SetScrollBar(HWND hwnd, int nBar) {
  if (nBar == SB_VERT)
    m_oVertScrollBar.Set(hwnd, hwnd == *this ? SB_VERT : SB_CTL);
}

void TextEditWindow::Show() {
  #if DEBUG_SHOW_HIDE
    DEBUG_PRINTF("%p show=%d |%ls|\n", this, show_count_,
                 GetBuffer()->GetName());
  #endif
  ++show_count_;
  if (show_count_ == 1) {
    gfx::Graphics::DrawingScope drawing_scope(*m_gfx);
    Redraw();
  }
}

int TextEditWindow::SmallScroll(int, int iDy) {
  updateScreen();

  if (iDy < 0) {
    iDy = -iDy;

    auto const lBufStart = selection_->GetBuffer()->GetStart();
    auto lStart = m_pPage->GetStart();
    int k;
    for (k = 0; k < iDy; k++) {
      if (lStart == lBufStart)
        break;
        lStart = startOfLineAux(*m_gfx, lStart - 1);
    }

    if (k > 0) {
      #if DEBUG_FORMAT
        DEBUG_PRINTF("down lStart=%d\n", lStart);
      #endif // DEBUG_FORMAT
      format(*m_gfx, lStart);
      Render();
    }
    return k;
  }

  if (iDy > 0) {
    auto const lBufEnd = selection_->GetBuffer()->GetEnd();
    int k;
    for (k = 0; k < iDy; k++) {
      if (m_pPage->GetEnd() >= lBufEnd) {
          // Make sure whole line of buffer end is visible.
          m_pPage->ScrollToPosn(*m_gfx, lBufEnd);
          k += 1;
          break;
      }

      if (!m_pPage->ScrollUp(*m_gfx))
        break;
    }

    if (k > 0)
        Render();
    return k;
  }

  return 0;
}

Posn TextEditWindow::StartOfLine(Posn lPosn) {
  return lPosn <= 0 ? 0 : startOfLineAux(*m_gfx, lPosn);
}

// See Also:
// EditPange::endOfLineAux
// Description:
// Returns start position of window line of specified position.
Posn TextEditWindow::startOfLineAux(const gfx::Graphics& gfx, Posn lPosn) {
  if (!m_pPage->IsDirty(m_rc, *selection_)) {
    auto const pLine = m_pPage->FindLine(lPosn);
    if (pLine)
      return pLine->GetStart();
  }

  auto lStart = selection_->GetBuffer()->ComputeStartOf(
      Unit_Paragraph,
      lPosn);
  if (!lStart)
    return 0;

  Page oPage;
  gfx::RectF page_rect(m_rc);
  for (;;) {
    auto const pLine = oPage.FormatLine(gfx, page_rect, *selection_,
                                        lStart);

    auto const lEnd = pLine->GetEnd();
    if (lPosn < lEnd)
      return pLine->GetStart();
    lStart = lEnd;
  }
}

void TextEditWindow::stopDrag() {
  m_oAutoScroll.Stop(m_hwnd);
 m_eDragMode = DragMode_None;
}

void TextEditWindow::updateScreen() {
  if (!m_pPage->IsDirty(m_rc, *selection_))
    return;

  Posn lStart = m_pViewRange->GetStart();
  lStart = startOfLineAux(*m_gfx, lStart);
  #if DEBUG_REDRAW
    DEBUG_PRINTF("dirty page lStart=%d\n", lStart);
  #endif // DEBUG_REDRAW
  format(*m_gfx, lStart);
}

void TextEditWindow::updateScrollBar() {
  if (!m_pPage->GetBuffer())
    return;

  auto const lBufEnd = m_pPage->GetBuffer()->GetEnd() + 1;

  SCROLLINFO oInfo;
  oInfo.cbSize = sizeof(oInfo);
  oInfo.fMask = SIF_POS | SIF_RANGE | SIF_PAGE | SIF_DISABLENOSCROLL;
  oInfo.nPage = m_pPage->GetEnd() - m_pPage->GetStart();
  oInfo.nMin = 0;
  oInfo.nMax = lBufEnd;
  oInfo.nPos = m_pPage->GetStart();

  if (static_cast<Count>(oInfo.nPage) >= lBufEnd) {
    // Current screen shows entire buffer. We disable scroll bar.
    oInfo.nMax = 0;
  }

  m_oVertScrollBar.SetInfo(&oInfo, true);
}

#if SUPPORT_IME

// See Also Caret::Show for moving candidate window.

#include <imm.h>
#pragma comment(lib, "imm32.lib")

extern StyleValues g_DefaultStyle;
StyleValues g_rgpImeStyleConverted[2];
StyleValues g_pImeStyleInput;
StyleValues g_pImeStyleTargetConverted;
StyleValues g_pImeStyleTargetNotConverted;

#define GCS_COMPSTRATTR (GCS_COMPSTR | GCS_COMPATTR | GCS_CURSORPOS)

class Imc {
  private: HWND m_hwnd;
  private: HIMC m_himc;

  public: Imc(HWND hwnd) : m_hwnd(hwnd), m_himc(::ImmGetContext(hwnd)) {}

  public: ~Imc() {
    if (m_himc)
      ::ImmReleaseContext(m_hwnd, m_himc);
  }

  public: operator HIMC() const { return m_himc; }
};

void TextEditWindow::onImeComposition(LPARAM lParam) {
  Imc imc(m_hwnd);
  if (!imc)
    return;

  Edit::UndoBlock oUndo(GetSelection(), L"IME");

  char16 rgwch[1024];
  // If IME has result string, we can insert it into buffer.
  if (lParam & GCS_RESULTSTR) {
    // Remove previous composition string. If user inputs "Space",
    // IME set GCS_RESULTSTR without composition.
    if (m_lImeStart != m_lImeEnd) {
      GetSelection()->SetRange(m_lImeStart, m_lImeEnd);
      GetSelection()->SetText(L"", 0);
    }

    // Get result string
    auto const cwch = ::ImmGetCompositionString(
        imc, GCS_RESULTSTR, rgwch, sizeof(rgwch)) / sizeof(char16);

    // Insert result string into buffer
    if (cwch >= 1) {
      GetSelection()->SetText(rgwch, cwch);
      GetSelection()->Collapse(Collapse_End);
      m_lImeEnd = GetSelection()->GetEnd();
      m_lImeStart = m_lImeEnd;
    }
  }

  // IME has composition string
  if ((lParam & GCS_COMPSTRATTR) == GCS_COMPSTRATTR) {
    // Remove previous composition string
    if (m_lImeStart != m_lImeEnd) {
        GetSelection()->SetRange(m_lImeStart, m_lImeEnd);
        GetSelection()->SetText(L"", 0);
        m_lImeEnd = m_lImeStart;
    }

    // Get composition string
    auto const cwch = ::ImmGetCompositionString(
        imc, GCS_COMPSTR, rgwch, sizeof(rgwch)) / sizeof(char16);

    // Get composition attributes
    char rgbAttr[lengthof(rgwch)];
    auto const cbAttr = ::ImmGetCompositionString(
        imc, GCS_COMPATTR, rgbAttr, sizeof(rgbAttr));
    if (cbAttr != static_cast<int>(cwch)) {
      DEBUG_PRINTF("GCCS_COMPATTR\n");
      return;
    }

    auto const lCursor = ::ImmGetCompositionString(
          imc, GCS_CURSORPOS, nullptr, 0);
    if (lCursor < 0) {
      DEBUG_PRINTF("GCCS_CURSORPOS\n");
      return;
    }

    uint32 rgnClause[100];
    ::ImmGetCompositionString(imc, GCS_COMPCLAUSE, rgnClause,
                              sizeof(rgnClause));

    GetSelection()->SetText(rgwch, cwch);
    GetSelection()->Collapse(Collapse_End);
    m_lImeEnd = GetSelection()->GetEnd();
    GetSelection()->SetRange(m_lImeStart + lCursor, m_lImeStart + lCursor);

    if (!g_pImeStyleInput.m_rgfMask) {
          // Converted[0]
          g_rgpImeStyleConverted[0].m_rgfMask =
              StyleValues::Mask_Decoration;

          g_rgpImeStyleConverted[0].m_eDecoration =
              TextDecoration_ImeInactiveA;

          // Converted[1]
          g_rgpImeStyleConverted[1].m_rgfMask =
              StyleValues::Mask_Decoration;

          g_rgpImeStyleConverted[1].m_eDecoration =
              TextDecoration_ImeInactiveB;

          // Input
          g_pImeStyleInput.m_rgfMask =
              StyleValues::Mask_Decoration;

          g_pImeStyleInput.m_eDecoration =
              TextDecoration_ImeInput;

          // Target Converted
          g_pImeStyleTargetConverted.m_rgfMask =
              StyleValues::Mask_Decoration;

          g_pImeStyleTargetConverted.m_eDecoration =
              TextDecoration_ImeActive;

          // Target Not Converted
          g_pImeStyleTargetNotConverted.m_rgfMask =
              StyleValues::Mask_Background |
              StyleValues::Mask_Color |
              StyleValues::Mask_Decoration;

          #if 0
              g_pImeStyleTargetNotConverted.m_crBackground =
                  g_DefaultStyle.GetColor();

              g_pImeStyleTargetNotConverted.m_crColor =
                  g_DefaultStyle.GetBackground();
          #else
              g_pImeStyleTargetNotConverted.m_crBackground =
                  GetSelection()->GetBackground();

              g_pImeStyleTargetNotConverted.m_crColor =
                  GetSelection()->GetColor();
          #endif

          g_pImeStyleTargetNotConverted.m_eDecoration =
              TextDecoration_None;
      }

      m_fImeTarget = false;
      Posn lEnd = m_lImeStart + cwch;
      Posn lPosn = m_lImeStart;
      int iClause = 0;
      int iConverted = 0;
      while (lPosn < lEnd) {
        StyleValues* pStyle;
        switch (rgbAttr[lPosn - m_lImeStart]) {
          case ATTR_INPUT:
          case ATTR_INPUT_ERROR:
            pStyle = &g_pImeStyleInput;
            iConverted = 0;
            break;

          case ATTR_TARGET_CONVERTED:
            pStyle = &g_pImeStyleTargetConverted;
            m_fImeTarget = true;
            iConverted = 0;
            break;

          case ATTR_TARGET_NOTCONVERTED:
            pStyle = &g_pImeStyleTargetNotConverted;
            m_fImeTarget = true;
            iConverted = 0;
            break;

          case ATTR_CONVERTED:
            pStyle = &g_rgpImeStyleConverted[iConverted];
            iConverted = 1 - iConverted;
            break;

          default:
            pStyle = &g_pImeStyleInput;
            break;
        }

        iClause += 1;
        Posn lNext = m_lImeStart + rgnClause[iClause];
        GetBuffer()->SetStyle(lPosn, lNext, pStyle);
        lPosn = lNext;
      }
  }

  ////////////////////////////////////////////////////////////
  //
  // We have already insert composed string. So, we don't
  // need WM_IME_CHAR and WM_CHAR messages to insert
  // composed string.
  if (lParam & GCS_RESULTSTR)
  {
      m_fImeTarget = false;
      return;
  }

  // Composition was canceled.
  if (0 == lParam)
  {
      m_fImeTarget = false;

      // Remove previous composition string
      GetSelection()->SetRange(m_lImeStart, m_lImeEnd);
      GetSelection()->SetText(L"", 0);

      // if (m_fCancelButLeave)
      {
          long cwch = ::ImmGetCompositionString(
              imc,
              GCS_COMPSTR,
              rgwch,
              sizeof(rgwch)) / sizeof(char16);
          if (cwch >= 1)
          {
              GetSelection()->SetText(rgwch, cwch);
          }
      }

      m_lImeEnd = m_lImeStart;
  }
}

// Note:
// o IME2000 ignores string after newline.
// o We should limit number of characters to be reconverted.
//
void TextEditWindow::Reconvert(Posn lStart, Posn lEnd) {
  BOOL fSucceeded;

  auto const cb = setReconvert(nullptr, lStart, lEnd);
  if (!cb)
    return;

  auto const pb = new char[cb];
  if (!pb)
    return;

  auto const p = reinterpret_cast<RECONVERTSTRING*>(pb);
  setReconvert(p, lStart, lEnd);

  Imc imc(m_hwnd);
  fSucceeded = ::ImmSetCompositionString(
      imc,
      SCS_QUERYRECONVERTSTRING,
      p,
      cb,
      nullptr,
      0);
  if (!fSucceeded) {
      DEBUG_PRINTF("SCS_QUERYRECONVERTSTRING\n");
      goto exit;
  }

  m_lImeStart = lStart + p->dwCompStrOffset / 2;
  m_lImeEnd = m_lImeStart + p->dwCompStrLen;
  m_fImeTarget = true;

  fSucceeded = ::ImmSetCompositionString(
      imc,
      SCS_SETRECONVERTSTRING,
      p,
      cb,
      nullptr,
      0);
  if (!fSucceeded) {
    DEBUG_PRINTF("SCS_SETRECONVERTSTRING\n");
    goto exit;
  }

  exit:
    delete[] pb;
}

uint TextEditWindow::setReconvert(RECONVERTSTRING* p, Posn lStart,
                                  Posn lEnd) {
  auto const cwch = lEnd - lStart;
  if (!p || !cwch)
    return 0;

  auto const cb = sizeof(RECONVERTSTRING) + sizeof(char16) * (cwch + 1);
  p->dwSize = cb;
  p->dwVersion = 0;
  p->dwStrLen = cwch;
  p->dwStrOffset = sizeof(RECONVERTSTRING);
  p->dwCompStrLen = cwch; // # of characters
  p->dwCompStrOffset = 0; // byte offset
  p->dwTargetStrLen = p->dwCompStrLen;
  p->dwTargetStrOffset = p->dwCompStrOffset;

  auto const pwch = reinterpret_cast<char16*>(
      reinterpret_cast<char*>(p) + p->dwStrOffset);
  GetBuffer()->GetText(pwch, lStart, lEnd);
  pwch[cwch] = 0;
  return cb;
}

// Set left top coordinate of IME candiate window.
BOOL TextEditWindow::showImeCaret(SIZE sz, POINT pt) {
  Imc imc(m_hwnd);
  if (!imc)
    return FALSE;

  CANDIDATEFORM oCF;
  oCF.dwIndex = 0;
  oCF.dwStyle = CFS_EXCLUDE;
  oCF.ptCurrentPos.x = pt.x;
  oCF.ptCurrentPos.y = pt.y + sz.cy;
  oCF.rcArea.left = pt.x;
  oCF.rcArea.top = pt.y;
  oCF.rcArea.right = pt.x;
  oCF.rcArea.bottom = pt.y + sz.cy;

  return ::ImmSetCandidateWindow(imc, &oCF);
}

#endif // SUPPORT_IME
