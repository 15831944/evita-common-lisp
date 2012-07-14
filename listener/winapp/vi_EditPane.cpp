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
#define DEBUG_CARET 0
#define DEBUG_FOCUS 0
#define DEBUG_KEY 0
#define DEBUG_REDRAW 0
#define DEBUG_SPLIT 1
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

static void DrawSplitter(HDC hdc, RECT* prc, uint grfFlag) {
  auto rc = *prc;
  ::FillRect(hdc, &rc, reinterpret_cast<HBRUSH>(COLOR_3DFACE + 1));
  ::DrawEdge(hdc, &rc, EDGE_RAISED, grfFlag);
}

void EditPane::Box::Realize(HWND, const Rect& rect) {
  rect_ = rect;
}

void EditPane::Box::SetRect(const Rect& rect) {
  rect_ = rect;
}

EditPane::LayoutBox::LayoutBox(LayoutBox* outer)
    : Box(outer),
      hwndParent_(nullptr) {}

EditPane::LayoutBox::~LayoutBox() {
  while (boxes_.GetFirst()) {
    delete boxes_.GetFirst();
  }
}

EditPane::LeafBox* EditPane::LayoutBox::GetActiveLeafBox() const {
  class Local {
    public: static LeafBox* SelectActiveBox(LeafBox* box1, LeafBox* box2) {
      return box1 && box2
          ? activeTick(*box1) > activeTick(*box2) ? box1 : box2
          : box1 ? box1 : box2;
    }

    private: static int activeTick(const LeafBox& box) {
      return box.GetWindow()->GetActiveTick();
    }
  };

  auto candiate = static_cast<LeafBox*>(nullptr);
  foreach (BoxList::Enum, it, boxes_) {
    candiate = Local::SelectActiveBox(candiate, it->GetActiveLeafBox());
  }
  return candiate;
}

uint EditPane::LayoutBox::CountLeafBox() const {
  auto count = 0u;
  foreach (BoxList::Enum, it, boxes_) {
    count += it->CountLeafBox();
  }
  return count;
}

void EditPane::LayoutBox::Destroy() {
  while (auto const box = boxes_.GetFirst()) {
    box->Destroy();
  }
}

EditPane::LeafBox* EditPane::LayoutBox::GetLeafBox(HWND hwnd) const {
  foreach (BoxList::Enum, it, boxes_) {
    if (auto const box = it->GetLeafBox(hwnd)) {
      return box;
    }
  }
  return nullptr;
}

bool EditPane::LayoutBox::OnIdle(uint count) {
  auto more = false;
  foreach (BoxList::Enum, it, boxes_) {
    if (it->OnIdle(count)) {
      more = true;
    }
  }
  return more;
}

void EditPane::LayoutBox::Realize(HWND hwndParent, const Rect& rect) {
  Box::Realize(hwndParent, rect);
  hwndParent_ = hwndParent;
}

void EditPane::LayoutBox::UpdateSplitters() {
  Dc dc(hwndParent_, ::GetDC(hwndParent_));
  DrawSplitters(dc);
}

EditPane::LeafBox::~LeafBox() {
  DEBUG_PRINTF("%p\n", this);
  ASSERT(!m_pWindow);

  if (m_hwndVScrollBar) {
    ::SetWindowLongPtr(m_hwndVScrollBar, GWLP_USERDATA, 0);
    ::DestroyWindow(m_hwndVScrollBar);
    m_hwndVScrollBar = nullptr;
  }
}

void EditPane::LeafBox::Destroy() {
    GetWindow()->Destroy();
}

EditPane::LeafBox* EditPane::LeafBox::GetLeafBox(HWND hwnd) const {
    return *GetWindow() == hwnd ? const_cast<LeafBox*>(this) : nullptr;
}

EditPane::HitTestResult EditPane::LeafBox::HitTest(Point pt) const {
    if (!::PtInRect(&rect(), pt)) {
    return HitTestResult(HitTestResult::None);
  }

  auto const cxVScroll = ::GetSystemMetrics(SM_CXVSCROLL);
  return HitTestResult(
      pt.x < rect().right - cxVScroll
        ? HitTestResult::Window
        : !HasSibling() && pt.y < rect().top + k_cySplitterBig
            ? HitTestResult::VSplitterBig
            : HitTestResult::VSplitter,
      this);
}

bool EditPane::LeafBox::OnIdle(uint count) {
    return GetWindow()->OnIdle(count);
}

void EditPane::LeafBox::Realize(HWND hwndParent, const Rect& rect) {
  Box::Realize(hwndParent, rect);

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

void EditPane::LeafBox::SetRect(const Rect& rect) {
    Box::SetRect(rect);
  auto const pWindow = GetWindow();
  auto const hwndVScrollBar = pWindow->GetScrollBarHwnd(SB_VERT);
  auto const prc = &rect;

  #if DEBUG_SPLIT
    DEBUG_PRINTF("%p %p %d+%d-%d+%d\n",
        this,
        pWindow,
        prc->left, prc->top, prc->right, prc->bottom);
  #endif

  auto cxVScroll = hwndVScrollBar ? ::GetSystemMetrics(SM_CXVSCROLL) : 0;

  if (hwndVScrollBar) {
    auto const cySplitter = HasSibling() ? 0 : k_cySplitterBig;
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

EditPane::VirticalLayoutBox::VirticalLayoutBox(LayoutBox* outer)
    : LayoutBox(outer) {}

void EditPane::VirticalLayoutBox::DrawSplitters(HDC hdc) {
  auto rc = rect();

  if (boxes_.GetFirst() == boxes_.GetLast()) {
    auto const cxVScroll = ::GetSystemMetrics(SM_CXVSCROLL);
    rc.left = rc.right - cxVScroll;
    rc.bottom = rc.top + k_cySplitterBig;
    DrawSplitter(hdc, &rc, BF_RECT);
    boxes_.GetFirst()->DrawSplitters(hdc);
    return;
  }

  foreach (BoxList::Enum, it, boxes_) {
    auto const box = it.Get();
    box->DrawSplitters(hdc);
    if (auto const above_box = box->GetPrev()) {
      rc.top = above_box->rect().bottom;
      rc.bottom = box->rect().top;
      DrawSplitter(hdc, &rc, BF_TOP | BF_BOTTOM);
    }
  }
}

EditPane::HitTestResult EditPane::VirticalLayoutBox::HitTest(
    Point pt) const {
  if (!::PtInRect(&rect(), pt)) {
    return HitTestResult(HitTestResult::None);
  }

  foreach (BoxList::Enum, it, boxes_) {
    auto const result = it->HitTest(pt);
    if (result.type != HitTestResult::None) {
      return result;
    }

    if (auto const above = it->GetPrev()) {
      RECT splitterRect;
      splitterRect.left = rect().left;
      splitterRect.right = rect().right;
      splitterRect.top = above->rect().bottom;
      splitterRect.bottom = it->rect().top;
      if (::PtInRect(&splitterRect, pt)) {
        return HitTestResult(HitTestResult::VSplitter, it.Get());
      }
    }
  }

  return HitTestResult(HitTestResult::None);
}

void EditPane::VirticalLayoutBox::MoveSplitter(
    const Point& pt,
    Box& below_box) {
  auto const pBelow = &below_box;
  auto const pBelowPrev = pBelow->GetPrev();
  auto const pAbove = pBelowPrev
      ? pBelowPrev
      : &Split(*pBelow, k_cySplitter);

  if (pt.y - pAbove->rect().top <= 0) {
    // Above box is too small.
  } else if (pBelow->rect().bottom - (pt.y + k_cySplitter) <= 0) {
    // Below box is too small.
  } else {
    pAbove->rect().bottom = pt.y;
    pBelow->rect().top = pt.y + k_cySplitter;
    pAbove->SetRect(pAbove->rect());
    pBelow->SetRect(pBelow->rect());
  }

  UpdateSplitters();
}

void EditPane::VirticalLayoutBox::Realize(
    HWND hwndParent,
    const Rect& rect) {
  LayoutBox::Realize(hwndParent, rect);

  auto const num_boxes = boxes_.Count();
  if (!num_boxes) {
    return;
  }

  if (num_boxes == 1) {
    boxes_.GetFirst()->Realize(hwndParent, rect);
    return;
  }

  auto const height = rect.bottom - rect.top;
  auto const content_height = height - k_cySplitter * (num_boxes - 1);
  auto const box_height = content_height / num_boxes;
  RECT elemRect(rect);
  foreach (BoxList::Enum, it, boxes_) {
    elemRect.bottom = rect.top + box_height;
    it->Realize(hwndParent, elemRect);
    elemRect.top = elemRect.bottom + k_cySplitter;
  }
}

void EditPane::VirticalLayoutBox::Remove(LeafBox& box) {
  auto const pAbove = box.GetPrev();
  auto const pBelow = box.GetNext();

  boxes_.Delete(&box);
  box.DetachWindow();

   auto rc = box.rect();

   if (pAbove) {
     // Extend pane above.
      RECT rect = pAbove->rect();
      rect.bottom = rc.bottom;
      pAbove->SetRect(rect);
   } else if (pBelow) {
     // Extend pane below.
     RECT rect = pBelow->rect();
     rect.top = rc.top;
     pBelow->SetRect(rect);
   }

   delete &box;
}

void EditPane::VirticalLayoutBox::SetRect(const Rect& newRect) {
  RECT rcOld = rect();
  LayoutBox::SetRect(newRect);
  auto const num_boxes = boxes_.Count();
  if (!num_boxes) {
    return;
  }

  if (num_boxes == 1) {
    boxes_.GetFirst()->SetRect(newRect);
    return;
  }

  auto const cyNewPane = rect().bottom  - rect().top;
  auto const cyOldPane = rcOld.bottom - rcOld.top;

  if (!cyOldPane) {
    auto const cBoxes = boxes_.Count();
    if (!cBoxes) {
      return;
    }

    auto const cyNewWin = cyNewPane / cBoxes;
    auto yBox = rect().top;
    auto cySplitter = 0;
    auto pBox = static_cast<Box*>(nullptr);
    foreach (BoxList::Enum, oEnum, boxes_) {
      pBox = oEnum.Get();
      auto const prc = &pBox->rect();
      yBox += cySplitter;
      prc->top = yBox;
      yBox += cyNewWin;
      prc->bottom = yBox;
      cySplitter = k_cySplitter;
    }

    if (pBox) {
      pBox->rect().bottom = rect().bottom;
    }
  } else {
    tryAgain:
      auto yBox = rect().top;
      auto cySplitter = 0;
      auto pBox = static_cast<Box*>(nullptr);
      foreach (BoxList::Enum, oEnum, boxes_) {
        pBox = oEnum.Get();
        auto const prc = &pBox->rect();
        auto const cyOldWin = prc->bottom - prc->top;
        auto const cyNewWin = cyNewPane * cyOldWin / cyOldPane;
        if (cyNewWin < k_cyMinBox) {
          pBox->Destroy();
          goto tryAgain;
        }
        yBox += cySplitter;
        prc->top = yBox;
        yBox += cyNewWin;
        prc->bottom = yBox;
        cySplitter = k_cySplitter;
      }

      if (!pBox) {
        return;
      }
      pBox->rect().bottom = rect().bottom;
  }

  foreach (BoxList::Enum, oEnum, boxes_) {
    auto const pBox = oEnum.Get();
    auto newRect = pBox->rect();
    newRect.left = rect().left;
    newRect.right = rect().right;
    pBox->SetRect(newRect);
  }

  UpdateSplitters();
}

EditPane::LeafBox& EditPane::VirticalLayoutBox::Split(
    Box& below,
    int cyBox) {
  auto pBelow = below.GetActiveLeafBox();
  auto const prcBelow = &pBelow->rect();
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

  auto const pAbove = new LeafBox(this, pWindow);
  auto const prcAbove = &pAbove->rect();

  boxes_.InsertBefore(pAbove, pBelow);

  prcAbove->left = prcBelow->left;
  prcAbove->right = prcBelow->right;
  prcAbove->top = prcBelow->top;
  prcAbove->bottom = prcBelow->top + cyBox;

  pAbove->Realize(hwndParent_, *prcAbove);

  prcBelow->top = prcAbove->bottom + k_cySplitter;
  pBelow->SetRect(pBelow->rect());

  UpdateSplitters();

  return *pAbove;
}

void EditPane::VirticalLayoutBox::StopSplitter(
    const Point& pt,
    Box& below_box) {
  if (!below_box.GetPrev()) {
    return;
  }

  auto& above_box = *below_box.GetPrev();

  auto const cyMin = k_cyMinBox;
  if (pt.y - above_box.rect().top < cyMin) {
    below_box.rect().top = above_box.rect().top;
    above_box.Destroy();
    below_box.SetRect(below_box.rect());
    UpdateSplitters();
  } else if (below_box.rect().bottom - pt.y < k_cyMinBox) {
    above_box.rect().bottom = below_box.rect().bottom;
    below_box.Destroy();
    above_box.SetRect(above_box.rect());
    UpdateSplitters();
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
    : m_eState(State_NotRealized),
      root_box_(new VirticalLayoutBox(nullptr)) {
  auto pWindow = new TextEditWindow(this, pBuffer, lStart);
  auto& box = *new LeafBox(root_box_, pWindow);
  root_box_->Add(box);
  m_oWindows.Append(pWindow);
  m_pwszName = pBuffer->GetName();
}

EditPane::~EditPane() {
  delete root_box_;
}

void EditPane::CloseAllBut(Window* window) {
  if (auto const leaf_box = root_box_->GetLeafBox(*window)) {
    leaf_box->Destroy();
  }
}

// Returns the last active Box.
EditPane::LeafBox* EditPane::GetActiveLeafBox() const {
  return root_box_->GetActiveLeafBox();
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

Command::KeyBindEntry* EditPane::MapKey(uint nKey) {
  return GetActiveWindow()->MapKey(nKey);
}

bool EditPane::OnIdle(uint count) {
  return root_box_->OnIdle(count);
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
      root_box_->Realize(*this, m_rc);
      break;
    }

    case WM_DESTROY:
      #if DEBUG_DESTROY
       DEBUG_PRINTF("WM_DESTROY %p\n", this);
      #endif

      m_eState = State_Destroyed;
      root_box_->Destroy();
      break;

    case WM_LBUTTONDOWN: {
      Point pt(MAKEPOINTS(lParam));
      auto const result = root_box_->HitTest(pt);
      switch (result.type) {
        case HitTestResult::VSplitter:
          m_oSplitterDrag.m_pBox = result.box;
          m_oSplitterDrag.Start(m_hwnd, SplitterDrag::State_Drag);
          break;

        case HitTestResult::VSplitterBig:
          m_oSplitterDrag.m_pBox = result.box;
          m_oSplitterDrag.Start(m_hwnd, SplitterDrag::State_DragSingle);
          break;
        }
        return 0;
    }

    case WM_LBUTTONUP:
      switch (m_oSplitterDrag.m_eState) {
        case SplitterDrag::State_Drag:
        case SplitterDrag::State_DragSingle:
          m_oSplitterDrag.m_pBox->outer()->StopSplitter(
            MAKEPOINTS(lParam),
            *m_oSplitterDrag.m_pBox);
          m_oSplitterDrag.Stop();
          break;
      }
      return 0;

    case WM_MOUSEMOVE: {
      switch (m_oSplitterDrag.m_eState) {
        case SplitterDrag::State_Drag:
        case SplitterDrag::State_DragSingle: {
          m_oSplitterDrag.m_pBox->outer()->MoveSplitter(
            MAKEPOINTS(lParam),
            *m_oSplitterDrag.m_pBox);
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
      root_box_->DrawSplitters(hdc);
      ::EndPaint(m_hwnd, &ps);
      return 0;
    }

    case WM_PARENTNOTIFY:
      if (wParam == WM_DESTROY) {
        auto const hwnd = reinterpret_cast<HWND>(lParam);
        if (auto const box = root_box_->GetLeafBox(hwnd)) {
          DEBUG_PRINTF("WM_PARENTNOTIFY: box=%p\n", box);
          m_oWindows.Delete(box->GetWindow());
          box->outer()->Remove(*box);
          if (State_Realized == m_eState) {
            // There is no window in this pane. So, we delete
            // this pane.
            if (!root_box_->CountLeafBox()) {
              ::DestroyWindow(*this);
            }
          }
        }
      }
      return 0;

    case WM_SETCURSOR: {
      Point pt;
      if (!::GetCursorPos(&pt)) {
        return FALSE;
      }

      if (!::ScreenToClient(m_hwnd, &pt)) {
        return FALSE;
      }

      switch (root_box_->HitTest(pt).type) {
        case HitTestResult::VSplitter:
        case HitTestResult::VSplitterBig:
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
      Resize();
      return 0;

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

      Resize();
      return 0;
    }

    case TextEditWindow::WN_QueryClose:
      // Do we have multiple frame?
      if (Application::Get()->HasMultipleFrames()) {
        // We have mutliple frame. So, we have at least one frame even if
        // we destroy frame contains this pane.
        return TRUE;
      }

      if (root_box_->CountLeafBox() > 1) {
        // This pane won't be closed when close specified window.
        return TRUE;
      }
      return Application::Get()->CanExit();
  }

  return Pane::onMessage(uMsg, wParam, lParam);
}

void EditPane::Resize() {
  ::GetClientRect(*this, &m_rc);
  root_box_->SetRect(m_rc);
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

EditPane::Window* EditPane::SplitVertically() {
  auto const pBelow = GetActiveLeafBox();
  ASSERT(!!pBelow);
  auto const prcBelow = pBelow->rect();

  // Active Box is too small to split.
  auto const cyBox = prcBelow.bottom - prcBelow.top;
  if (cyBox < k_cyMinBox * 2 + k_cySplitter) {
    return nullptr;
  }

  auto& leaf_box = pBelow->outer()->Split(*pBelow, cyBox / 2);
  pBelow->GetWindow()->MakeSelectionVisible();
  m_oWindows.InsertBefore(leaf_box.GetWindow(), pBelow->GetWindow());
  return pBelow->GetWindow();
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
