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
#define DEBUG_REDRAW 0
#define DEBUG_RESIZE _DEBUG
#define DEBUG_SPLIT 0
#include "./vi_EditPane.h"

#include "./ed_Mode.h"
#include "./gfx_base.h"
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

static HCURSOR s_hHSplitCursor;
static HCURSOR s_hVSplitCursor;

class EditPane::Box : public DoubleLinkedNode_<EditPane::Box>,
                      public RefCounted_<EditPane::Box> {
    private: bool is_removed_;
    private: LayoutBox* outer_;
    private: Rect rect_;
    protected: Box(LayoutBox*);
    public: virtual ~Box();
    public: bool is_removed() const { return is_removed_; }
    public: virtual Box* first_child() const { return nullptr; }
    public: Box* next_sibling() const { return GetNext(); }
    public: Box* previous_sibling() const { return GetPrev(); }
    public: LayoutBox* outer() const { return outer_; }
    public: const Rect& rect() const { return rect_; }
    public: Rect& rect() { return rect_; }
    public: void set_outer(LayoutBox& outer) { outer_ = &outer; }
    public: virtual void CloseAllBut(Window*) = 0;
    public: virtual uint CountLeafBox() const = 0;
    public: virtual void Destroy() = 0;
    public: virtual void DrawSplitters(const gfx::Graphics&) { }
    public: virtual LeafBox* GetActiveLeafBox() const = 0;
    public: virtual LeafBox* GetFirstLeafBox() const = 0;
    public: virtual LeafBox* GetLeafBox(HWND) const = 0;

    public: virtual HitTestResult HitTest(Point) const = 0;
    public: virtual void Hide() {}

    public: virtual bool IsLeafBox() const = 0;

    public: virtual bool OnIdle(uint) = 0;

    public: virtual void Realize(HWND, const Rect&);
    public: virtual void Redraw() const {}
    public: void Removed();

    public: virtual void SetRect(const Rect&);
    public: virtual void Show() {}

    DISALLOW_COPY_AND_ASSIGN(Box);
};

class EditPane::LayoutBox : public EditPane::Box {
  protected: typedef DoubleLinkedList_<Box> BoxList;
  protected: BoxList boxes_;
  protected: HWND hwndParent_;
  protected: LayoutBox(LayoutBox*);
  public: virtual ~LayoutBox();
  public: virtual Box* first_child() const override final {
    return boxes_.GetFirst();
  }
  public: void Add(Box& box);
  public: virtual void CloseAllBut(Window*) override final;
  public: virtual uint CountLeafBox() const override final;
  public: virtual void Destroy() override final;
  protected: virtual void DidRemoveBox(Box*, Box*, const Rect&) = 0;
  public: virtual void MoveSplitter(const Point&, Box&) = 0;
  public: virtual LeafBox* GetActiveLeafBox() const override final;
  public: virtual LeafBox* GetFirstLeafBox() const override final;
  public: virtual LeafBox* GetLeafBox(HWND) const override final;

  // [H]
  public: virtual void Hide() override {
    for (auto& box: boxes_)
      box.Hide();
  }

  public: virtual bool IsLeafBox() const override final { return false; }
  public: bool IsSingle() const;
  public: virtual bool IsVerticalLayoutBox() const = 0;
  public: virtual bool OnIdle(uint) override final;
  public: virtual void Realize(HWND, const Rect&) override;
  public: virtual void Redraw() const override final;
  public: void RemoveBox(Box&);
  public: void Replace(Box&, Box&);
  public: virtual LeafBox& Split(Box&, int) = 0;

  public: virtual void StopSplitter(const Point&, Box&) = 0;

  public: virtual void Show() override {
    for (auto& box: boxes_)
      box.Show();
  }

  // [U]
  protected: void UpdateSplitters();
};

class EditPane::LeafBox final : public EditPane::Box {
  private: HWND m_hwndVScrollBar;
  private: Window* m_pWindow;

  public: LeafBox(LayoutBox* outer, Window* pWindow)
    : Box(outer),
      m_hwndVScrollBar(nullptr),
      m_pWindow(pWindow) {
  }

  public: virtual ~LeafBox();

  // [C]
  public: virtual void CloseAllBut(Window*) override;
  public: virtual uint CountLeafBox() const  override final { return 1; }

  // [D]
  public: virtual void Destroy() override;
  public: void DetachWindow();

  public: void EnsureInHorizontalLayoutBox();
  public: void EnsureInVerticalLayoutBox();

  // [G]
  public: virtual LeafBox* GetActiveLeafBox() const override;
  public: virtual LeafBox* GetFirstLeafBox() const override;
  public: virtual LeafBox* GetLeafBox(HWND) const override;
  public: Window* GetWindow() const { return m_pWindow; }

  // [H]
  private: bool HasSibling() const { return GetNext() || GetPrev(); }
  public: virtual HitTestResult HitTest(Point) const override;

  public: virtual void Hide() {
    if (m_hwndVScrollBar)
      ::ShowWindow(m_hwndVScrollBar, SW_HIDE);
    ::ShowWindow(*m_pWindow, SW_HIDE);
  }

  // [I]
  public: virtual bool IsLeafBox() const override final { return true; }

  // [O]
  public: virtual bool OnIdle(uint) override;

  // [R]
  public: virtual void Realize(HWND, const Rect&) override;
  public: virtual void Redraw() const override;

  // [S]
  public: virtual void SetRect(const Rect&) override;

  public: virtual void Show() {
    if (m_hwndVScrollBar)
      ::ShowWindow(m_hwndVScrollBar, SW_SHOW);
    ::ShowWindow(*m_pWindow, SW_SHOW);
  }
};

class EditPane::HorizontalLayoutBox final : public EditPane::LayoutBox {
  public: HorizontalLayoutBox(LayoutBox*);
  public: virtual ~HorizontalLayoutBox();
  protected: virtual void DidRemoveBox(Box*, Box*, const Rect&) override;
  public: virtual HitTestResult HitTest(Point) const override;
  public: virtual void DrawSplitters(const gfx::Graphics&) override;
  public: virtual bool IsVerticalLayoutBox() const override;
  public: virtual void MoveSplitter(const Point&, Box&) override;
  public: virtual void Realize(HWND, const Rect&) override;
  public: virtual void SetRect(const Rect&) override;
  public: virtual LeafBox& Split(Box&, int) override;
  public: virtual void StopSplitter(const Point&, Box&) override;
};

class EditPane::VerticalLayoutBox final : public LayoutBox {
  public: VerticalLayoutBox(LayoutBox*);
  public: virtual ~VerticalLayoutBox();
  protected: virtual void DidRemoveBox(Box*, Box*, const Rect&) override;
  public: virtual HitTestResult HitTest(Point) const override;
  public: virtual void DrawSplitters(const gfx::Graphics&) override;
  public: virtual bool IsVerticalLayoutBox() const override;
  public: virtual void MoveSplitter(const Point&, Box&) override;
  public: virtual void Realize(HWND, const Rect&) override;
  public: virtual void SetRect(const Rect&) override;
  public: virtual LeafBox& Split(Box&, int) override;
  public: virtual void StopSplitter(const Point&, Box&) override;
};

#if 0
class EditPane::BoxWalker final {
  public: class Iterator final {
    private: const EditPane* tree_owner_;
    private: Box* runner_;
    public: Iterator(const EditPane& tree_owner, Box* runner)
        : tree_owner_(&tree_owner),
          runner_(runner) {
    }
    public: bool operator==(const Iterator& other) const {
      ASSERT(tree_owner_ == other.tree_owner_);
      return runner_ == other.runner_;
    }
    public: bool operator!=(const Iterator& other) const {
      ASSERT(tree_owner_ == other.tree_owner_);
      return runner_ != other.runner_;
    }
    public: Box& operator*() const {
      ASSERT(runner_);
      return *runner_;
    }
    public: BoxWalker& operator++() {
      ASSERT(runner_);
      if (auto const child = runner_->first_child())
        runner_= child;
      else if (auto const next_sibling = runner_->next_sibling())
        runner_ = next_sibling;
      else
        runner_ = runner_->outer();
    }
  };
  private: const EditPane* tree_owner_;
  public: BoxWalker(const EditPane& tree_owner) : tree_owner_(&tree_owner) {}
  public: Iterator begin() const {
    return Iterator(*tree_owner_, &tree_owner_->root_box());
  }
  public: Iterator end() const {
    return Iterator(*tree_owner_, nullptr);
  }
};
#endif

namespace {
void DrawSplitter(const gfx::Graphics& gfx, RECT* prc,
                         uint /*grfFlag*/) {
  auto rc = *prc;
  gfx::Brush fillBrush(gfx, gfx::sysColor(COLOR_3DFACE));
  gfx.FillRectangle(fillBrush, rc);
  //::DrawEdge(gfx, &rc, EDGE_RAISED, grfFlag);
}
} // namesapce

//
// EditPane
//

EditPane::HitTestResult::HitTestResult()
    : box(nullptr), type(None) {}

EditPane::HitTestResult::HitTestResult(Type type, const Box& box)
    : box(const_cast<Box*>(&box)), type(type) {
  ASSERT(type != None);
}

EditPane::Box::Box(LayoutBox* outer)
    : is_removed_(false),
      outer_(outer) {}

EditPane::Box::~Box() {
  ASSERT(is_removed_);
  ASSERT(!outer_);
  ASSERT(!GetNext());
  ASSERT(!GetPrev());
}

void EditPane::Box::Realize(HWND, const Rect& rect) {
  rect_ = rect;
}

void EditPane::Box::Removed() {
  DEBUG_PRINTF("%p\n", this);
  ASSERT(!is_removed());
  is_removed_ = true;
  outer_ = nullptr;
}

void EditPane::Box::SetRect(const Rect& rect) {
  rect_ = rect;
}

// HorizontalLayoutBox
EditPane::HorizontalLayoutBox::HorizontalLayoutBox(LayoutBox* outer)
    : LayoutBox(outer) {}

EditPane::HorizontalLayoutBox::~HorizontalLayoutBox() {
  DEBUG_PRINTF("%p\n", this);
}

void EditPane::HorizontalLayoutBox::DidRemoveBox(
    Box* const pAbove,
    Box* const pBelow,
    const Rect& rc) {
  if (pAbove) {
    // Extend pane above.
    RECT rect = pAbove->rect();
    rect.right = rc.right;
    pAbove->SetRect(rect);
  } else if (pBelow) {
    // Extend pane below.
    RECT rect = pBelow->rect();
    rect.left = rc.left;
    pBelow->SetRect(rect);
  }
}

void EditPane::HorizontalLayoutBox::DrawSplitters(const gfx::Graphics& gfx) {
  if (boxes_.GetFirst() == boxes_.GetLast()) {
    return;
  }

  auto rc = rect();
  foreach (BoxList::Enum, it, boxes_) {
    auto const box = it.Get();
    box->DrawSplitters(gfx);
    if (auto const right_box = box->GetPrev()) {
      rc.left = right_box->rect().right;
      rc.right = box->rect().left;
      DrawSplitter(gfx, &rc, BF_LEFT | BF_RIGHT);
    }
  }
}

EditPane::HitTestResult EditPane::HorizontalLayoutBox::HitTest(
    Point pt) const {
  if (!::PtInRect(&rect(), pt)) {
    return HitTestResult();
  }

  foreach (BoxList::Enum, it, boxes_) {
    auto const result = it->HitTest(pt);
    if (result.type != HitTestResult::None) {
      return result;
    }

    if (auto const left_box = it->GetPrev()) {
      RECT splitterRect;
      splitterRect.top = rect().top;
      splitterRect.bottom = rect().bottom;
      splitterRect.left = left_box->rect().right;
      splitterRect.right = it->rect().left;
      if (::PtInRect(&splitterRect, pt)) {
        return HitTestResult(HitTestResult::HSplitter, *it.Get());
      }
    }
  }

  return HitTestResult();
}

bool EditPane::HorizontalLayoutBox::IsVerticalLayoutBox() const {
  return false;
}

void EditPane::HorizontalLayoutBox::MoveSplitter(
    const Point& pt,
    Box& right_box) {
  auto& left_box = right_box.GetPrev()
      ? *right_box.GetPrev()
      : Split(right_box, k_cxSplitter);

  if (pt.x - left_box.rect().left <= 0) {
    // Above box is too small.
  } else if (right_box.rect().right - pt.x <= k_cxMinBox) {
    // Below box is too small.
  } else {
    left_box.rect().right = pt.x;
    right_box.rect().left = pt.x + k_cxSplitter;
    left_box.SetRect(left_box.rect());
    right_box.SetRect(right_box.rect());
  }

  UpdateSplitters();
}

void EditPane::HorizontalLayoutBox::Realize(
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

  auto const width = rect.right - rect.left;
  auto const content_width = width - k_cxSplitter * (num_boxes - 1);
  auto const box_width = content_width / num_boxes;
  RECT elemRect(rect);
  foreach (BoxList::Enum, it, boxes_) {
    elemRect.right = rect.left + box_width;
    it->Realize(hwndParent, elemRect);
    elemRect.left = elemRect.right + k_cxSplitter;
  }
}

void EditPane::HorizontalLayoutBox::SetRect(const Rect& newRect) {
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

  auto const cxNewPane = rect().right  - rect().left;
  auto const cxOldPane = rcOld.right - rcOld.left;

  if (!cxOldPane) {
    auto const cBoxes = boxes_.Count();
    if (!cBoxes) {
      return;
    }

    auto const cxNewWin = cxNewPane / cBoxes;
    auto xBox = rect().left;
    auto cxSplitter = 0;
    auto pBox = static_cast<Box*>(nullptr);
    foreach (BoxList::Enum, oEnum, boxes_) {
      pBox = oEnum.Get();
      auto const prc = &pBox->rect();
      xBox += cxSplitter;
      prc->left = xBox;
      xBox += cxNewWin;
      prc->right = xBox;
      cxSplitter = k_cxSplitter;
    }

    if (pBox) {
      pBox->rect().right = rect().right;
    }
  } else {
    ScopedRefCount_<LayoutBox> protect(*this);

    tryAgain:
      auto xBox = rect().left;
      auto cxSplitter = 0;
      auto pBox = static_cast<Box*>(nullptr);
      foreach (BoxList::Enum, oEnum, boxes_) {
        pBox = oEnum.Get();
        auto const prc = &pBox->rect();
        auto const cxOldWin = prc->right - prc->left;
        auto const cxNewWin = cxNewPane * cxOldWin / cxOldPane;
        if (cxNewWin < k_cxMinBox) {
          pBox->Destroy();
          if (is_removed())
            return;
          goto tryAgain;
        }
        xBox += cxSplitter;
        prc->left = xBox;
        xBox += cxNewWin;
        prc->right = xBox;
        cxSplitter = k_cxSplitter;
      }

      if (!pBox) {
        return;
      }
      pBox->rect().right = rect().right;
  }

  foreach (BoxList::Enum, oEnum, boxes_) {
    auto const pBox = oEnum.Get();
    auto newRect = pBox->rect();
    newRect.top = rect().top;
    newRect.bottom = rect().bottom;
    pBox->SetRect(newRect);
  }

  UpdateSplitters();
}

EditPane::LeafBox& EditPane::HorizontalLayoutBox::Split(
    Box& below,
    int cxBox) {
  auto pBelow = below.GetActiveLeafBox();
  auto const prcBelow = &pBelow->rect();
  ASSERT(prcBelow->right - prcBelow->left > cxBox);

  auto const pWindow = new TextEditWindow(
      pBelow->GetWindow()->GetHost<EditPane>(),
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
  pAbove->AddRef();

  prcAbove->top = prcBelow->top;
  prcAbove->bottom = prcBelow->bottom;
  prcAbove->left = prcBelow->left;
  prcAbove->right = prcBelow->left + cxBox;

  pAbove->Realize(hwndParent_, *prcAbove);

  prcBelow->left = prcAbove->right + k_cxSplitter;
  pBelow->SetRect(pBelow->rect());

  UpdateSplitters();

  return *pAbove;
}

void EditPane::HorizontalLayoutBox::StopSplitter(
    const Point& pt,
    Box& below_box) {
  DEBUG_PRINTF("%p\n", this);
  if (!below_box.GetPrev()) {
    return;
  }

  ScopedRefCount_<LayoutBox> protect(*this);
  auto& above_box = *below_box.GetPrev();
  auto const cxMin = k_cxMinBox;
  if (pt.x - above_box.rect().left < cxMin) {
    below_box.rect().left = above_box.rect().left;
    above_box.Destroy();
    below_box.SetRect(below_box.rect());
    UpdateSplitters();
  } else if (below_box.rect().right - pt.x < k_cxMinBox) {
    above_box.rect().right = below_box.rect().right;
    below_box.Destroy();
    above_box.SetRect(above_box.rect());
    UpdateSplitters();
  }
}

// LayoutBox
EditPane::LayoutBox::LayoutBox(LayoutBox* outer)
    : Box(outer),
      hwndParent_(nullptr) {}

EditPane::LayoutBox::~LayoutBox() {
  ASSERT(boxes_.IsEmpty());
}

void EditPane::LayoutBox::Add(Box& box) {
  ASSERT(!is_removed());
  boxes_.Append(&box);
  box.AddRef();
}

EditPane::LeafBox* EditPane::LayoutBox::GetActiveLeafBox() const {
  ASSERT(!is_removed());
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

EditPane::LeafBox* EditPane::LayoutBox::GetFirstLeafBox() const {
  ASSERT(!is_removed());
  return boxes_.GetFirst() ? boxes_.GetFirst()->GetFirstLeafBox() : nullptr;
}

void EditPane::LayoutBox::CloseAllBut(Window* window) {
  ASSERT(!is_removed());
  auto runner = boxes_.GetFirst();
  while (runner) {
    auto const next = runner->GetNext();
    runner->CloseAllBut(window);
    runner = next;
  }
}

uint EditPane::LayoutBox::CountLeafBox() const {
  ASSERT(!is_removed());
  auto count = 0u;
  foreach (BoxList::Enum, it, boxes_) {
    count += it->CountLeafBox();
  }
  return count;
}

void EditPane::LayoutBox::Destroy() {
  DEBUG_PRINTF("%p\n", this);
  ASSERT(!is_removed());
  auto runner = boxes_.GetFirst();
  while (runner) {
    auto const next = runner->GetNext();
    runner->Destroy();
    runner = next;
  }
}

EditPane::LeafBox* EditPane::LayoutBox::GetLeafBox(HWND hwnd) const {
  ASSERT(!is_removed());
  foreach (BoxList::Enum, it, boxes_) {
    if (auto const box = it->GetLeafBox(hwnd)) {
      return box;
    }
  }
  return nullptr;
}

bool EditPane::LayoutBox::IsSingle() const {
  ASSERT(!is_removed());
  return !boxes_.IsEmpty() && boxes_.GetFirst() == boxes_.GetLast();
}

bool EditPane::LayoutBox::OnIdle(uint count) {
  ASSERT(!is_removed());
  auto more = false;
  foreach (BoxList::Enum, it, boxes_) {
    if (it->OnIdle(count)) {
      more = true;
    }
  }
  return more;
}

void EditPane::LayoutBox::Realize(HWND hwndParent, const Rect& rect) {
  ASSERT(!is_removed());
  Box::Realize(hwndParent, rect);
  hwndParent_ = hwndParent;
}

void EditPane::LayoutBox::Redraw() const {
  for (auto& box : boxes_) {
    box.Redraw();
  }
}

void EditPane::LayoutBox::RemoveBox(Box& box) {
  ASSERT(!is_removed());
  auto const pAbove = box.GetPrev();
  auto const pBelow = box.GetNext();
  boxes_.Delete(&box);
  auto const rc = box.rect();
  box.Removed();
  box.Release();
  DidRemoveBox(pAbove, pBelow, rc);

  if (!outer()) {
    return;
  }

  if (boxes_.IsEmpty()) {
    outer()->RemoveBox(*this);
    return;
  }

  if (boxes_.GetFirst() != boxes_.GetLast()) {
    return;
  }

  auto& first_box = *boxes_.GetFirst();
  boxes_.Delete(&first_box);
  auto const outer = this->outer();
  Removed();
  outer->Replace(first_box, *this);
  first_box.Release();
  return;
}

void EditPane::LayoutBox::Replace(Box& new_box, Box& old_box) {
  ASSERT(!is_removed());
  boxes_.InsertBefore(&new_box, &old_box);
  new_box.set_outer(*this);
  new_box.AddRef();
  boxes_.Delete(&old_box);
  old_box.Release();
}

void EditPane::LayoutBox::UpdateSplitters() {
  if (is_removed()) {
    return;
  }

  auto const pane = EditPane::FromHwnd(hwndParent_);
  auto& gfx = pane->gfx();
  gfx::Graphics::DrawingScope scope(gfx);
  DrawSplitters(gfx);
}

// LeafBox
EditPane::LeafBox::~LeafBox() {
  DEBUG_PRINTF("%p\n", this);
  ASSERT(!m_pWindow);

  if (m_hwndVScrollBar) {
    ::SetWindowLongPtr(m_hwndVScrollBar, GWLP_USERDATA, 0);
    ::DestroyWindow(m_hwndVScrollBar);
    m_hwndVScrollBar = nullptr;
  }
}

void EditPane::LeafBox::CloseAllBut(Window* window) {
  if (GetWindow() != window) {
    GetWindow()->Destroy();
  }
}

void EditPane::LeafBox::Destroy() {
    GetWindow()->Destroy();
}

void EditPane::LeafBox::DetachWindow() {
  m_pWindow = nullptr;
}

void EditPane::LeafBox::EnsureInHorizontalLayoutBox() {
  if (!outer()->IsVerticalLayoutBox()) {
    return;
  }

  auto& layout_box = *new HorizontalLayoutBox(outer());
  ScopedRefCount_<LeafBox> protect(*this);
  outer()->Replace(layout_box, *this);
  layout_box.Realize(::GetParent(*GetWindow()), rect());
  layout_box.Add(*this);
  set_outer(layout_box);
}

void EditPane::LeafBox::EnsureInVerticalLayoutBox() {
  if (outer()->IsVerticalLayoutBox()) {
    return;
  }

  auto& layout_box = *new VerticalLayoutBox(outer());
  ScopedRefCount_<LeafBox> protect(*this);
  outer()->Replace(layout_box, *this);
  layout_box.Realize(::GetParent(*GetWindow()), rect());
  layout_box.Add(*this);
  set_outer(layout_box);
}

EditPane::LeafBox* EditPane::LeafBox::GetActiveLeafBox() const {
  return const_cast<LeafBox*>(this);
}

EditPane::LeafBox* EditPane::LeafBox::GetFirstLeafBox() const {
  return const_cast<LeafBox*>(this);
}

EditPane::LeafBox* EditPane::LeafBox::GetLeafBox(HWND hwnd) const {
    return *GetWindow() == hwnd ? const_cast<LeafBox*>(this) : nullptr;
}

EditPane::HitTestResult EditPane::LeafBox::HitTest(Point pt) const {
  if (!::PtInRect(&rect(), pt))
    return HitTestResult();

  auto const cxVScroll = ::GetSystemMetrics(SM_CXVSCROLL);
  if (pt.x < rect().right - cxVScroll)
    return HitTestResult(HitTestResult::Window, *this);

  if (!HasSibling() && pt.y < rect().top + k_cySplitterBig)
    return HitTestResult(HitTestResult::VSplitterBig, *this);

  return HitTestResult();
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

 ::SetWindowLongPtr(m_hwndVScrollBar, GWLP_USERDATA,
      reinterpret_cast<LONG_PTR>(this));

 m_pWindow->CreateWindowEx(0, nullptr, WS_CHILD | WS_VISIBLE, hwndParent);
 m_pWindow->SetScrollBar(m_hwndVScrollBar, SB_VERT);
 SetRect(rect);
}

void EditPane::LeafBox::Redraw() const {
  // TODO: We should assign number to redraw text editor window.
  ::SendMessage(*m_pWindow, WM_USER, 0, 0);
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

EditPane::VerticalLayoutBox::VerticalLayoutBox(LayoutBox* outer)
    : LayoutBox(outer) {}

EditPane::VerticalLayoutBox::~VerticalLayoutBox() {
  DEBUG_PRINTF("%p\n", this);
}

void EditPane::VerticalLayoutBox::DidRemoveBox(
    Box* const pAbove,
    Box* const pBelow,
    const Rect& rc) {
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
}

void EditPane::VerticalLayoutBox::DrawSplitters(const gfx::Graphics& gfx) {
  auto rc = rect();

  if (boxes_.GetFirst() == boxes_.GetLast()) {
    auto const cxVScroll = ::GetSystemMetrics(SM_CXVSCROLL);
    rc.left = rc.right - cxVScroll;
    rc.bottom = rc.top + k_cySplitterBig;
    DrawSplitter(gfx, &rc, BF_RECT);
    boxes_.GetFirst()->DrawSplitters(gfx);
    return;
  }

  foreach (BoxList::Enum, it, boxes_) {
    auto const box = it.Get();
    box->DrawSplitters(gfx);
    if (auto const above_box = box->GetPrev()) {
      rc.top = above_box->rect().bottom;
      rc.bottom = box->rect().top;
      DrawSplitter(gfx, &rc, BF_TOP | BF_BOTTOM);
    }
  }
}

EditPane::HitTestResult EditPane::VerticalLayoutBox::HitTest(
    Point pt) const {
  if (!::PtInRect(&rect(), pt)) {
    return HitTestResult();
  }

  foreach (BoxList::Enum, it, boxes_) {
    auto const result = it->HitTest(pt);
    if (result.type != HitTestResult::None) {
      return result;
    }

    if (auto const above_box = it->GetPrev()) {
      RECT splitterRect;
      splitterRect.left = rect().left;
      splitterRect.right = rect().right;
      splitterRect.top = above_box->rect().bottom;
      splitterRect.bottom = it->rect().top;
      if (::PtInRect(&splitterRect, pt)) {
        return HitTestResult(HitTestResult::VSplitter, *it.Get());
      }
    }
  }

  return HitTestResult();
}

bool EditPane::VerticalLayoutBox::IsVerticalLayoutBox() const {
  return true;
}

void EditPane::VerticalLayoutBox::MoveSplitter(
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

void EditPane::VerticalLayoutBox::Realize(
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

void EditPane::VerticalLayoutBox::SetRect(const Rect& newRect) {
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
    ScopedRefCount_<LayoutBox> protect(*this);
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
          if (is_removed()) {
            return;
          }
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

EditPane::LeafBox& EditPane::VerticalLayoutBox::Split(
    Box& below,
    int cyBox) {
  auto pBelow = below.GetActiveLeafBox();
  auto const prcBelow = &pBelow->rect();
  ASSERT(prcBelow->bottom - prcBelow->top > cyBox);

  auto const pWindow = new TextEditWindow(
      pBelow->GetWindow()->GetHost<EditPane>(),
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
  pAbove->AddRef();

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

void EditPane::VerticalLayoutBox::StopSplitter(
    const Point& pt,
    Box& below_box) {
  DEBUG_PRINTF("%p\n", this);
  if (!below_box.GetPrev()) {
    return;
  }

  ScopedRefCount_<LayoutBox> protect(*this);
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

// EditPane::SplitterDrag
EditPane::SplitterDrag::SplitterDrag()
    : m_eState(State_None),
      m_pBox(nullptr) {}

EditPane::SplitterDrag::~SplitterDrag() {
  ASSERT(!m_pBox);
}

void EditPane::SplitterDrag::End(const Point& point) {
  switch (m_eState) {
    case SplitterDrag::State_Drag:
    case SplitterDrag::State_DragSingle:
      m_pBox->outer()->StopSplitter(point, *m_pBox);
      Stop();
      break;
  }
}

void EditPane::SplitterDrag::Move(const Point& point) {
  switch (m_eState) {
    case SplitterDrag::State_Drag:
    case SplitterDrag::State_DragSingle:
      m_pBox->outer()->MoveSplitter(point, *m_pBox);
      break;
  }
}

void EditPane::SplitterDrag::Start(HWND hwnd, State eState, Box& box) {
  ASSERT(!!box.outer());
  ::SetCapture(hwnd);
  m_eState = eState;
  m_pBox = &box;
  box.AddRef();
}

void EditPane::SplitterDrag::Stop() {
  if (m_eState != State_None) {
    ASSERT(!!m_pBox);
    ::ReleaseCapture();
    m_eState = State_None;
    m_pBox->Release();
    m_pBox = nullptr;
  }
  ASSERT(!m_pBox);
}

EditPane::EditPane(Buffer* pBuffer, Posn lStart)
    : m_eState(State_NotRealized),
      root_box_(*new VerticalLayoutBox(nullptr)) {
  auto pWindow = new TextEditWindow(this, pBuffer, lStart);
  ScopedRefCount_<LeafBox> box(*new LeafBox(root_box_, pWindow));
  root_box_->Add(*box);
  m_pwszName = pBuffer->GetName();
}

EditPane::~EditPane() {
  root_box_->Removed();
}

void EditPane::Activate() {
  Pane::Activate();
  auto const window = GetActiveWindow();
  if (!window)
    return;
  window->Activate();
  window->GetBuffer()->UpdateFileStatus(true);
}

void EditPane::CloseAllBut(Window* window) {
  root_box_->CloseAllBut(window);
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

void EditPane::Hide() {
  root_box_->Hide();
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
      //gfx_->Init(m_hwnd);
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
        case HitTestResult::HSplitter:
        case HitTestResult::VSplitter:
          m_oSplitterDrag.Start(m_hwnd, SplitterDrag::State_Drag, *result.box);
          break;

        case HitTestResult::VSplitterBig:
          m_oSplitterDrag.Start(
              m_hwnd,
              SplitterDrag::State_DragSingle,
              *result.box);
          break;
        }
        return 0;
    }

    case WM_LBUTTONUP:
      m_oSplitterDrag.End(MAKEPOINTS(lParam));
      return 0;

    case WM_MOUSEMOVE:
      m_oSplitterDrag.Move(MAKEPOINTS(lParam));
      return 0;

    case WM_ERASEBKGND:
      DEBUG_PRINTF("WM_ERASEBKGND %p\n", this);
      return TRUE;

#if 0
    case WM_PAINT: {
      DEBUG_PRINTF("WM_PAINT Start %p\n", this);
      {
        gfx::Graphics::DrawingScope scope(*gfx_);
        //(*gfx_)->Clear(gfx::ColorF(gfx::ColorF::Pink));
        root_box_->DrawSplitters(*gfx_);
      }
      root_box_->Redraw();
      {
        RECT rc;
        if (::GetUpdateRect(m_hwnd, &rc, false)) {
          DEBUG_PRINTF("update_rect=(%d,%d)-(%d,%d)\n", rc.left, rc.top,
                       rc.right, rc.bottom);
        } else {
          DEBUG_PRINTF("GetUpdateRectFailed\n");
        }
      }
      ::ValidateRect(m_hwnd, nullptr);
      DEBUG_PRINTF("WM_PAINT End %p\n", this);
      return 0;
    }
#endif

    case WM_PARENTNOTIFY:
      switch (LOWORD(wParam)) {
        case WM_CREATE: {
          auto const hwnd = reinterpret_cast<HWND>(lParam);
          if (auto const box = root_box_->GetLeafBox(hwnd)) {
            DEBUG_PRINTF("WM_PARENTNOTIFY: CREATE box=%p\n", box);
            auto const next_leaf_box = box->GetNext()
                ? box->GetNext()->GetFirstLeafBox()
                : nullptr;
            auto const next_window = next_leaf_box
                ? next_leaf_box->GetWindow()
                : nullptr;
            if (next_window) {
              m_oWindows.InsertBefore(box->GetWindow(), next_window);
            } else {
              m_oWindows.Append(box->GetWindow());
            }
          }
          break;
        }

        case WM_DESTROY: {
          auto const hwnd = reinterpret_cast<HWND>(lParam);
          if (auto const box = root_box_->GetLeafBox(hwnd)) {
            DEBUG_PRINTF("WM_PARENTNOTIFY: DESTROY box=%p\n", box);
            m_oWindows.Delete(box->GetWindow());
            box->DetachWindow();
            auto const outer = box->outer();
            outer->RemoveBox(*box);
            if (State_Realized == m_eState) {
              if (!root_box_->CountLeafBox()) {
                // There is no window in this pane. So, we delete
                // this pane.
                ::DestroyWindow(*this);
              }
            }
          }
          break;
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
        case HitTestResult::HSplitter:
        case HitTestResult::HSplitterBig:
          if (!s_hHSplitCursor) {
            s_hHSplitCursor = ::LoadCursor(
                g_hInstance,
                MAKEINTRESOURCE(IDC_HSPLIT));
          }
          ::SetCursor(s_hHSplitCursor);
          break;

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
      DEBUG_PRINTF("WM_SETFOCUS %p\n", this);
      Pane::onMessage(uMsg, wParam, lParam);
      if (auto const pWindow = GetActiveWindow()) {
        pWindow->Activate();
        GetBuffer()->UpdateFileStatus(true);
      }
      return 0;

#if 0
    case WM_SIZE:
      Resize();
      return 0;
#endif

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
      if (wp->flags & SWP_NOSIZE)
        return 0;

      #if DEBUG_REDRAW || DEBUG_RESIZE
        DEBUG_PRINTF("WM_WINDOWPOSCHANGED %p 0x%X %dx%d+%d+%d\n",
                     this, wp->flags, wp->cx, wp->cy, wp->x, wp->y);
      #endif

      //Resize();
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


void EditPane::Realize(Frame& frame, const gfx::Graphics& gfx) {
  ASSERT(!IsRealized());
  m_hwnd = frame;
  gfx_ = &gfx;
  m_eState = State_Realized;
  frame.GetPaneRect(&m_rc);
  root_box_->Realize(*this, m_rc);
}

void EditPane::Resize(const RECT& rc) {
  m_rc = rc;
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

void EditPane::Show() {
  root_box_->Show();
}

EditPane::Window* EditPane::SplitHorizontally() {
  auto const right_box = GetActiveLeafBox();
  ASSERT(!!right_box);
  auto const right_rect = right_box->rect();

  // Active Box is too small to split.
  auto const cxBox = right_rect.right - right_rect.left;
  if (cxBox < k_cxMinBox * 2 + k_cxSplitter) {
    return nullptr;
  }

  right_box->EnsureInHorizontalLayoutBox();
  right_box->outer()->Split(*right_box, cxBox / 2);
  right_box->GetWindow()->MakeSelectionVisible();
  return right_box->GetWindow();
}

EditPane::Window* EditPane::SplitVertically() {
  auto const below_box = GetActiveLeafBox();
  ASSERT(!!below_box);
  auto const belowRect = below_box->rect();

  // Active Box is too small to split.
  auto const cyBox = belowRect.bottom - belowRect.top;
  if (cyBox < k_cyMinBox * 2 + k_cySplitter) {
    return nullptr;
  }

  below_box->EnsureInVerticalLayoutBox();
  below_box->outer()->Split(*below_box, cyBox / 2);
  below_box->GetWindow()->MakeSelectionVisible();
  return below_box->GetWindow();
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
