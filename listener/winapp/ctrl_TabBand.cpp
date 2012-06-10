#include "precomp.h"
// Note: You must include GdiPlus.h in precomp.h.
//////////////////////////////////////////////////////////////////////////////
//
// Tab Band Control
// ctrl_TabBand.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/ctrl_TabBand.cpp#2 $
//
#define DEBUG_DRAG    1
#define DEBUG_HOVER   0
#define DEBUG_MESSAGE   0
#define DEBUG_TOOLTIP   0
#include "./ctrl_tabBand.h"

#undef override

// C4481: nonstandard extension used: override specifier 'override'
#pragma warning(disable: 4481)
#pragma comment(lib, "gdiplus.lib")
#pragma comment(lib, "msimg32.lib")
#pragma comment(lib, "user32.lib")
#pragma comment(lib, "uxtheme.lib")

#pragma warning(disable:4355) // 'this' : used in base member initializer list

#define WC_TABBANDCLASS  L"TabBandClass"

#include <dwmapi.h>

static HINSTANCE g_hInstance;

class ScopedGetDC {
  private: HDC m_hdc;
  private: HWND m_hwnd;

  public: ScopedGetDC(HWND hwnd) :
      m_hdc(::GetDC(hwnd)),
      m_hwnd(hwnd) {
    ASSERT(m_hdc != nullptr);
    ASSERT(m_hwnd != nullptr);
  }

  public: ~ScopedGetDC() {
    ::ReleaseDC(m_hwnd, m_hdc);
  }

  public: operator HDC() const { return m_hdc; }
}; // ScopedGetDC

//////////////////////////////////////////////////////////////////////
//
// Select
//
class ScopedSelect {
  private: HDC m_hdc;
  private: HGDIOBJ m_old;

  public: ScopedSelect(HDC hdc, HFONT hFont) :
    m_hdc(hdc) {
    m_old = ::SelectObject(hdc, hFont);
  }

  public: ScopedSelect(HDC hdc, HGDIOBJ hObj) :
      m_hdc(hdc) {
    m_old = ::SelectObject(hdc, hObj);
  }

  public: ~ScopedSelect() {
    ::SelectObject(m_hdc, m_old);
  }
}; // Select

template<class Item_>
class DoubleLinkedList_;

template<class Item_>
class DoubleLinkedNode_ {
  friend class DoubleLinkedList_<Item_>;

  private: Item_* m_pNext;
  private: Item_* m_pPrev;

  public: DoubleLinkedNode_() :
    m_pNext(nullptr),
    m_pPrev(nullptr) {}

  public: Item_* GetNext() const { return m_pNext; }
  public: Item_* GetPrev() const { return m_pPrev; }
}; // DoubleLinkedNode_

template<class Item_>
class DoubleLinkedList_ {
  protected: typedef DoubleLinkedList_<Item_> List_;

  private: Item_* m_pFirst;
  private: Item_* m_pLast;

  public: DoubleLinkedList_() :
    m_pFirst(nullptr),
    m_pLast(nullptr) {}

  // [A]
  public: Item_* Append(Item_* pItem)
  {
    pItem->m_pNext = nullptr;
    pItem->m_pPrev = m_pLast;

    if (!m_pFirst) {
      m_pFirst = pItem;
    }

    if (m_pLast) {
      m_pLast->m_pNext = pItem;
    }

    return m_pLast = pItem;
  }

  // [D]
  public: Item_* Delete(Item_* pItem) {
    auto const pNext = pItem->m_pNext;
    auto const pPrev = pItem->m_pPrev;
    if (!pNext) {
      m_pLast = pPrev;
    } else {
      pNext->m_pPrev = pPrev;
    }

    if (!pPrev) {
      m_pFirst = pNext;
    } else {
      pPrev->m_pNext = pNext;
    }

    pItem->m_pNext = nullptr;
    pItem->m_pPrev = nullptr;

    return pItem;
  }

  // [E]
  public: class Enum {
    private: Item_* m_pRunner;
    public: Enum(List_* p) : m_pRunner(p->m_pFirst) {}
    public: Enum(const List_* p) : m_pRunner(p->m_pFirst) {}
    public: bool AtEnd() const { return m_pRunner == nullptr; }
    public: Item_* Get() { return m_pRunner; }

    public: void Next() {
      ASSERT(!AtEnd());
      m_pRunner = m_pRunner->m_pNext;
    }
  }; // Enum

  // [G]
  public: Item_* GetFirst() const { return m_pFirst; }
  public: Item_* GetLast()  const { return m_pLast; }

  // [I]
  public: Item_* InsertBefore(Item_* pItem, Item_* pRefItem) {
    if (!pRefItem) {
      return Append(pItem);
    }

    auto const pPrev = pRefItem->m_pPrev;
    if (!pPrev) {
      m_pFirst = pItem;
    } else {
      pPrev->m_pNext = pItem;
    }

    pItem->m_pPrev = pPrev;
    pItem->m_pNext = pRefItem;

    pRefItem->m_pPrev = pItem;
    return pItem;
  }

  public: bool IsEmpty() const {
    return m_pFirst == nullptr;
  }
}; // DoubleLinkedList_

enum DCHF {
  DCHF_DirLeft = 0, // <<
  DCHF_DirRight = 1, // >>
  DCHF_DirUp = 2, // ^
  DCHF_DirDown = 3, // V
  DCHF_DirMask = 3,
  DCHF_Pushed = 1 << 2,
  DCHF_Large = 1 << 3,
}; // DCHF

// For GDI+
static uint s_cInstances;
static ULONG_PTR s_GdiplusToken;

static inline void
SetVertex(TRIVERTEX* pVertex, int x, int y, DWORD rgb) {
  pVertex->x = x;
  pVertex->y = y;
  pVertex->Red = static_cast<COLOR16>(((rgb >>  0) & 0xFF) << 8);
  pVertex->Green = static_cast<COLOR16>(((rgb >>  8) & 0xFF) << 8);
  pVertex->Blue = static_cast<COLOR16>(((rgb >> 16) & 0xFF) << 8);
  pVertex->Alpha = 0x0000;
}

//////////////////////////////////////////////////////////////////////
//
// DrawCheveron
//
void DrawChevron(HDC hdc, RECT* prc, uint rgfFlag)
{
  RECT rc = *prc;
  const int cxEdge = 2;
  const int cxLarge = 4;
  int w = (rgfFlag & DCHF_Large) ? cxLarge : cxEdge;

  switch (rgfFlag & DCHF_DirMask) {
    case DCHF_DirDown: {
      int x = (rc.left + rc.right) / 2;
      int y = rc.top + 1;

      for (int k = -w; k < 0; k++) {
        ::PatBlt(hdc, x + k, y, 1, w, PATCOPY);
        ::PatBlt(hdc, x + k, y + w * 2, 1, w, PATCOPY);
        y += 1;
      }

      for (int k = 0; k <= w; k++) {
        ::PatBlt(hdc, x + k, y, 1, w, PATCOPY);
        ::PatBlt(hdc, x + k, y + w * 2, 1, w, PATCOPY);
        y -= 1;
      }

      break;
    }

    case DCHF_DirLeft: {
      int x = (rc.left + rc.right - w * 4) / 2;
      int y = (rc.top + rc.bottom) / 2;

      //  ** **
      //   ** **
      //  ** **
      //   ** **
      //  ** **
      for (int k = -w; k < 0; k++) {
        ::PatBlt(hdc, x, y + k, w, 1, PATCOPY);
        ::PatBlt(hdc, x + w * 2, y + k, w, 1, PATCOPY);
        x += 1;
      }

      for (int k = 0; k <= w; k++) {
        ::PatBlt(hdc, x, y + k, w, 1, PATCOPY);
        ::PatBlt(hdc, x + w * 2, y + k, w, 1, PATCOPY);
        x -= 1;
      }
      break;
    }

    case DCHF_DirRight: {
      int x = (rc.left + rc.right - w * 4) / 2;
      int y = (rc.top + rc.bottom) / 2;

      // ** **
      //   ** **
      //   ** **
      //  ** **
      //  ** **
      for (int k = -w; k < 0; k++) {
        ::PatBlt(hdc, x, y + k, w, 1, PATCOPY);
        ::PatBlt(hdc, x + w * 2, y + k, w, 1, PATCOPY);
        x += 1;
      }

      for (int k = 0; k <= w; k++) {
        ::PatBlt(hdc, x, y + k, w, 1, PATCOPY);
        ::PatBlt(hdc, x + w * 2, y + k, w, 1, PATCOPY);
        x -= 1;
      }
      break;
    }
  }
}

//////////////////////////////////////////////////////////////////////
//
// Element
//
class Element : public DoubleLinkedNode_<Element> {
  public: enum State {
    State_Normal,
    State_Selected,
  }; // State

  protected: State m_eState;
  protected: bool  m_fHover;
  protected: bool  m_fShow;
  protected: HIMAGELIST  m_hImageList;
  protected: Gdiplus::Font* m_pFont;
  protected: Element* m_pParent;
  protected: RECT m_rc;

  // ctor
  protected: Element(Element* pParent)
      : m_eState(State_Normal)
      , m_fHover(false)
      , m_fShow(true)
      , m_hImageList(nullptr)
      , m_pFont(nullptr)
      , m_pParent(pParent) {
    m_rc.left = m_rc.top = m_rc.right = m_rc.top = 0;
  }

  public: virtual ~Element() {
    delete m_pFont;
  }

  // [D]
  public: virtual void Draw(HDC) const = 0;
  public: virtual void DrawTheme(Gdiplus::Graphics&, HTHEME) const = 0;

  protected: static void fillRect(HDC hdc, int x, int y, int cx, int cy) {
    RECT rc;
    rc.left = x;
    rc.right = x + cx;
    rc.top = y;
    rc.bottom = y + cy;
    ::FillRect(hdc, &rc, reinterpret_cast<HBRUSH>(COLOR_BTNTEXT + 1));
  }

  public: template<class T> T* DynamicCast() {
    return Is<T>() ? StaticCast<T>() : nullptr;
  }

  // [G]
  public: virtual const char16* GetClass() const = 0;

  public: Gdiplus::Font* GetFont() const {
    const Element* pRunner = this;
    for (;;) {
      if (auto const pFont = pRunner->m_pFont) {
        return pFont;
      }

      pRunner = pRunner->GetParent();
    }
  }

  public: bool GetHover() const { return m_fHover; }

  public: HIMAGELIST GetImageList() const {
    const Element* pRunner = this;
    do {
      auto const hImageList = pRunner->m_hImageList;
      if (hImageList) {
        return hImageList;
      }

      pRunner = pRunner->GetParent();
    } while (pRunner);
    return nullptr;
  }

  public: Element* GetNextShow() const {
    for (
        Element* pRunner = GetNext();
        pRunner;
        pRunner = pRunner->GetNext()) {
      if (pRunner->m_fShow) {
        return pRunner;
      }
    }
    return nullptr;
  }

  public: Element* GetPrevShow() const {
    for (
        Element* pRunner = GetPrev();
        nullptr != pRunner;
        pRunner = pRunner->GetPrev()) {
      if (pRunner->m_fShow) {
        return pRunner;
      }
    }
    return nullptr;
  }

  public: RECT*  GetRect() { return &m_rc; }
  public: Element* GetParent() const { return m_pParent; }
  public: State  GetState() const { return m_eState; }

  // [H]
  public: virtual Element* HitTest(POINT pt) const {
    if (!::PtInRect(&m_rc, pt)) {
      return nullptr;
    }

    return const_cast<Element*>(this);
  }

  // [I]
  public: void Invalidate(HWND hwnd)
    {  ::InvalidateRect(hwnd, &m_rc, false); }

  public: template<class T> bool Is() const
    { return T::GetClass_() == GetClass(); }

  // IsHover
  public: bool IsHover() const {
    return m_fHover;
  }

  // IsSelected
  public: bool IsSelected() const {
    return State_Selected == m_eState;
  }

  public: bool IsShow() const {
    return m_fShow;
  }

  // [S]
  public: Gdiplus::Font* SetFont(Gdiplus::Font* p) {
    return m_pFont = p;
  }

  public: bool SetHover(bool f) {
    return m_fHover = f;
  }

  public: HIMAGELIST SetImageList(HIMAGELIST h) {
    return m_hImageList = h;
  }

  public: Element* SetParent(Element* p) {
    return m_pParent = p;
  }

  public: State SetState(State e) {
    m_eState = e;
    update();
    return m_eState;
  }

  public: bool Show(bool f) {
    return m_fShow = f;
  }

  public: template<class T> T* StaticCast() {
    ASSERT(Is<T>()); return reinterpret_cast<T*>(this);
  }

  // [U]
  protected: virtual void update() {}
}; // Element

//////////////////////////////////////////////////////////////////////
//
// MyFond
//
class MyFont {
  private: HFONT  m_hFont;
  private: int  m_iDescent;
  private: int  m_iHeight;

  // ctor
  public: MyFont() :
    m_hFont(nullptr),
    m_iDescent(0),
    m_iHeight(0) {}

  public: ~MyFont() {
    if (m_hFont) {
      ::DeleteObject(m_hFont);
    }
  }

  // operator
  public: operator HFONT() const { return m_hFont; }

  // [G]
  public: int GetDescent() const { return m_iDescent; }
  public: int GetHeight()  const { return m_iHeight; }

  // [i]
  public: bool Init(HDC hdc, const LOGFONT* pLogFont) {
    if (m_hFont) {
      ::DeleteObject(m_hFont);
    }

    m_hFont = ::CreateFontIndirect(pLogFont);
    if (!m_hFont) {
      return false;
    }

    ScopedSelect oSelect(hdc, m_hFont);

    TEXTMETRIC tm;
    if (!::GetTextMetrics(hdc, &tm)) {
      return false;
    }

    m_iHeight = tm.tmHeight + tm.tmInternalLeading;
    m_iDescent = tm.tmDescent;

    return true;
  }
}; // MyFont

//////////////////////////////////////////////////////////////////////
//
// TabBand Design Parameters
//
enum TabBandDesignParams {
  k_cxMargin = 3,
  k_cyMargin = 2,
  k_cxListButton = 16,
  k_cxEdge = 2,
  k_cxBorder = 3,
  k_cxPad = 3,
  k_cxMinTab = 140,
  k_cyBorder = 5,
  k_cyIcon = 16,
}; // TabBandDesignParams

//////////////////////////////////////////////////////////////////////
//
// CloseBox
//
class CloseBox : public Element {
  public: static const char16*  GetClass_() { return L"CloseBox"; }

  public: virtual const char16* GetClass() const override {
    return GetClass_();
  }

  public: enum Design {
    Height = 16,
    Width = 17,
  }; // Desgin

  // ctor
  public: CloseBox(Element* pParent) :
    Element(pParent) {}

  // [D]

  private: void drawMark(Gdiplus::Graphics& g, Gdiplus::Color cr) const {
    g.SetSmoothingMode(Gdiplus::SmoothingModeNone);

    Gdiplus::SolidBrush brush(cr);

    RECT rc = m_rc;
    rc.left += 4;
    rc.top  += 4;

    #define hline(x, y, cx, cy) \
      g.FillRectangle( \
        &brush, \
        m_rc.left + x, m_rc.top + y, \
        cx, cy);

    hline( 4, 4, 3, 1);
    hline(10, 4, 3, 1);

    hline( 5, 5, 3, 1);
    hline( 9, 5, 3, 1);

    hline( 6, 6, 5, 1);

    hline( 7, 7, 3, 2);  // center

    hline( 6, 9, 5, 1);

    hline( 5, 10, 3, 1);
    hline( 9, 10, 3, 1);

    hline( 4, 11, 3, 1);
    hline(10, 11, 3, 1);

    #undef hline
  }

  public: virtual void Draw(HDC hdc) const {
    // 01234567890123
    // ----ooo---ooo--- 4
    // -----ooo-ooo---- 5
    // ------ooooo----- 6
    // -------ooo------ 7
    // -------ooo------ 8
    // ------ooooo----- 9
    // -----ooo-ooo---- 10
    // ----ooo---ooo--- 11
    Gdiplus::Graphics g(hdc);
    drawMark(g, Gdiplus::Color(0, 0, 0));
  }

  public: virtual void DrawTheme(Gdiplus::Graphics& g, HTHEME) const {
    if (!IsHover()) {
      drawMark(g, Gdiplus::Color(150, 150, 150));

    } else {
      g.SetSmoothingMode(Gdiplus::SmoothingModeAntiAlias);

      // Fill background
      {
        Gdiplus::SolidBrush brush(Gdiplus::Color(237, 237, 239));

        RECT rc = m_rc;
        rc.left   += 0;
        rc.top  += 0;
        rc.right  -= 1;
        rc.bottom -= 1;

        g.FillRectangle(
          &brush,
          rc.left,
          rc.top,
          rc.right  - rc.left,
          rc.bottom - rc.top);
      }

      drawMark(g, Gdiplus::Color(184, 60, 61));

      g.SetSmoothingMode(Gdiplus::SmoothingModeAntiAlias);

      // Draw edge
      {
        RECT rc = m_rc;
        rc.right  -= 1;
        rc.bottom -= 1;

        Gdiplus::Pen pen(Gdiplus::Color(150, 150, 150));
        Gdiplus::Point rgPoint[8];
        rgPoint[0] = Gdiplus::Point(rc.left  + 1, rc.top  + 0);
        rgPoint[1] = Gdiplus::Point(rc.right - 1, rc.top  + 0);
        rgPoint[2] = Gdiplus::Point(rc.right + 0, rc.top  + 1);
        rgPoint[3] = Gdiplus::Point(rc.right + 0, rc.bottom - 1);
        rgPoint[4] = Gdiplus::Point(rc.right - 1, rc.bottom + 0);
        rgPoint[5] = Gdiplus::Point(rc.left  + 1, rc.bottom + 0);
        rgPoint[6] = Gdiplus::Point(rc.left  + 0, rc.bottom - 1);
        rgPoint[7] = Gdiplus::Point(rc.left  + 0, rc.top  + 1);
        g.DrawLines(&pen, rgPoint, lengthof(rgPoint));
      }
    }
  }
}; // CloseBox

//////////////////////////////////////////////////////////////////////
//
// Item
//  Represents tab item.
//
class Item : public Element {
  private: enum Design {
    k_cxCloseBoxMargin = 3,
    k_cyCloseBoxMargin = 9,
    k_cyDescent = 4,
  }; // Design

  public: static const char16*  GetClass_() { return L"Item"; }
  public: virtual const char16* GetClass()  const { return GetClass_(); }

  public: int m_cwch;
  public: char16* m_pwsz;
  public: int m_iItem;
  public: int m_iImage;
  public: LPARAM m_lParam;
  private: RECT m_rcLabel;
  private: CloseBox m_oCloseBox;
  public: uint m_rgfState;

  // ctor
  public: Item(Element* pParent, int iItem, const TCITEM* pTcItem) :
      m_cwch(0),
      m_iImage(-1),
      m_iItem(iItem),
      m_rgfState(0),
      m_oCloseBox(this),
      m_pwsz(nullptr),
      Element(pParent) {
    SetItem(pTcItem);
  }

  // dtor
  public: ~Item() {
    delete [] m_pwsz;
  }

  public: void ComputeLayout() {
    m_rcLabel = m_rc;

    auto const prc = m_oCloseBox.GetRect();
    *prc = m_rc;

    if (IsSelected()) {
      prc->right  -= k_cxCloseBoxMargin;
      prc->left = prc->right - CloseBox::Width;
      prc->top  += k_cyCloseBoxMargin;
      prc->bottom = prc->top + CloseBox::Height;
      //m_rcLabel.top -= 2;
    } else {
      prc->left = prc->right;
    }

    m_rcLabel.right = prc->left;

    m_rcLabel.left += k_cxBorder + k_cxEdge;
    m_rcLabel.right -= k_cxBorder + k_cxEdge;
    m_rcLabel.top  += 6 + 4;
    m_rcLabel.bottom = m_rcLabel.top + 12;

    if (m_iImage >= 0) {
      m_rcLabel.left += 16 + 4;
    }
  }

  // [D]
  // Draw
  public: virtual void Draw(HDC hdc) const override {
    if (IsSelected()) {
      drawSelected(hdc);
    } else {
      drawNormal(hdc);
    }
  }

  // Draw
  public: virtual void DrawTheme(
      Gdiplus::Graphics& g,
      HTHEME hTheme) const override {
    #if DEBUG_HOVER
      DEBUG_PRINTF("%p sel=%d %ls\n",
        this,
        IsSelected(),
        m_pwsz);
    #endif

    if (IsSelected()) {
      drawThemeSelected(g, hTheme);
    } else {
      drawThemeNormal(g, hTheme);
    }
  }

  private: void drawContent(HDC hdc) const {
    // Label Icon
    if (m_iImage >= 0) {
      if (auto const hImageList = GetImageList()) {
        ::ImageList_Draw(
            hImageList,
            m_iImage,
            hdc,
            m_rcLabel.left - 20,
            m_rc.top + 8,
            ILD_TRANSPARENT);
      }
    }

    // Label Text
    {
      ::SetTextAlign(hdc, TA_TOP | TA_LEFT | TA_NOUPDATECP);
      ::SetBkMode(hdc, TRANSPARENT);
      ::SetTextColor(hdc, ::GetSysColor(COLOR_BTNTEXT));
      ::SetTextColor(hdc, 0xFF);

      RECT rc = m_rc;
      rc.left += 4 + 16 + 4;
      rc.right -= 4;
      rc.top += 8;
      rc.bottom = rc.bottom - 2;

      ::ExtTextOut(
        hdc,
        rc.left,
        rc.top,
        ETO_CLIPPED,
        &rc,
        m_pwsz,
        m_cwch,
        nullptr);
    }
  }

  private: void drawNormal(HDC hdc) const {
    {
        RECT rc = m_rc;
        rc.bottom -= 3;
        ::FillRect(hdc, &rc, reinterpret_cast<HBRUSH>(COLOR_3DFACE + 1));
    }

    drawContent(hdc);
  }

  private: void drawSelected(HDC hdc) const {
    #if 0
    {
      TRIVERTEX rgvert[2];
      GRADIENT_RECT grc;

      RECT rc = m_rc;
      rc.top += 4;
      rc.bottom -= 2;

      SetVertex(
        &rgvert[0],
        rc.left,
        rc.top,
        ::GetSysColor(COLOR_3DHIGHLIGHT));

      SetVertex(
        &rgvert[1],
        rc.right,
        rc.bottom,
        ::GetSysColor(COLOR_3DFACE));

      grc.UpperLeft = 0;
      grc.LowerRight = 1;

      ::GradientFill(
        hdc,
        rgvert,
        2,
        &grc,
        1,
        GRADIENT_FILL_RECT_V);

      ::DrawEdge(hdc, &rc, EDGE_ETCHED, BF_TOP | BF_LEFT | BF_RIGHT);
    }
    #else
    {
        RECT rc = m_rc;
        rc.bottom -= 2;
        ::FillRect(hdc, &rc, reinterpret_cast<HBRUSH>(COLOR_3DLIGHT + 1));
    }
    #endif

    {
        RECT rc;
        rc.left = GetParent()->GetRect()->left;
        rc.right = m_rc.left;
        rc.top = m_rc.bottom - 3;
        rc.bottom = m_rc.bottom;
        ::DrawEdge(hdc, &rc, EDGE_ETCHED, BF_TOP);
    }

    {
        RECT rc;
        rc.left = m_rc.right - 1;
        rc.right = GetParent()->GetRect()->right;
        rc.top = m_rc.bottom - 3;
        rc.bottom = m_rc.bottom;
        ::DrawEdge(hdc, &rc, EDGE_ETCHED, BF_TOP);
    }

    drawContent(hdc);
    m_oCloseBox.Draw(hdc);
  }

  private: void drawThemeContents(Gdiplus::Graphics& g) const {
    g.SetSmoothingMode(Gdiplus::SmoothingModeNone);

    // Draw label
    {
      Gdiplus::SolidBrush brush(Gdiplus::Color(0, 0, 0));
      g.DrawString(
        m_pwsz,
        m_cwch,
        GetFont(),
        Gdiplus::PointF(
          static_cast<Gdiplus::REAL>(m_rcLabel.left),
          static_cast<Gdiplus::REAL>(m_rcLabel.top)),
        &brush);
    }

    if (m_iImage >= 0) {
      if (auto const hImageList = GetImageList()) {
        auto const hdc = g.GetHDC();
        ::ImageList_Draw(
            hImageList,
            m_iImage,
            hdc,
            m_rcLabel.left - 20,
            m_rcLabel.top,
            ILD_TRANSPARENT);
        g.ReleaseHDC(hdc);
      }
    }
  }

  // drawThemeNormal
  private: void drawThemeNormal(Gdiplus::Graphics& g, HTHEME) const {
    g.SetSmoothingMode(Gdiplus::SmoothingModeAntiAlias);

    RECT rc = m_rc;
    rc.top += 4;
    rc.bottom -= 2;

    Gdiplus::Point rgPoint[9];

    rgPoint[0] = Gdiplus::Point(rc.right, rc.bottom - 1);

    Element* pNext = GetNextShow();
    if (pNext && pNext->IsSelected()) {
      rgPoint[1] = Gdiplus::Point(rc.right - 1, rc.bottom);
    } else {
      rgPoint[1] = Gdiplus::Point(rc.right, rc.bottom);
    }

    Element* pPrev = GetPrevShow();
    if (pPrev && pPrev->IsSelected()) {
      rgPoint[2] = Gdiplus::Point(rc.left + 1, rc.bottom);
    } else {
      rgPoint[2] = Gdiplus::Point(rc.left, rc.bottom);
    }

    rgPoint[3] = Gdiplus::Point(rc.left, rc.bottom - 1);
    rgPoint[4] = Gdiplus::Point(rc.left, rc.top + 2);
    rgPoint[5] = Gdiplus::Point(rc.left  + 2, rc.top);
    rgPoint[6] = Gdiplus::Point(rc.right - 2, rc.top);
    rgPoint[7] = Gdiplus::Point(rc.right, rc.top + 2);
    rgPoint[8] = Gdiplus::Point(rc.right, rc.bottom - 1);

    Gdiplus::GraphicsPath path;
    path.AddLines(rgPoint, lengthof(rgPoint));

    // Fill highlight
    if (IsHover()) {
      auto const opacity = 255;

      Gdiplus::LinearGradientBrush fillBrush(
        Gdiplus::Point(0, rc.top + 0),
        Gdiplus::Point(0, rc.top + 27),
        Gdiplus::Color(opacity, 242, 245, 250),
        Gdiplus::Color(opacity, 153, 198, 238));

      fillBrush.SetBlendTriangularShape(0.5f, 1.0f);
      g.FillRectangle(
        &fillBrush,
        rc.left,
        rc.top,
        rc.right  - rc.left,
        27);

    } else {
      auto const opacity = 255 * 70 / 100;
      Gdiplus::LinearGradientBrush fillBrush(
        Gdiplus::Point(0, 13),
        Gdiplus::Point(0, 27 + 13 - 1),
        Gdiplus::Color(opacity, 207, 215, 235),
        Gdiplus::Color(opacity, 242, 245, 250));

      g.FillRectangle(
        &fillBrush,
        rc.left,
        rc.top,
        rc.right  - rc.left,
        27);
    }

    drawThemeContents(g);

    // Draw edge
    {
      Gdiplus::Pen linePen(Gdiplus::Color(143, 149, 161));
      g.SetSmoothingMode(Gdiplus::SmoothingModeAntiAlias);
      g.DrawPath(&linePen, &path);
    }
  }

  // drawThemeSelected
  private: void drawThemeSelected(Gdiplus::Graphics& g, HTHEME hTheme) const {
    RECT rc = m_rc;
    rc.top += 2;
    rc.bottom -= 2;

    Gdiplus::GraphicsPath path;
    {
      auto const kR = 3;
      Gdiplus::Point rgPoint[10];
      rgPoint[0] = Gdiplus::Point(GetParent()->GetRect()->left, rc.bottom + 0);
      rgPoint[1] = Gdiplus::Point(rc.left  - 1, rc.bottom + 0);
      rgPoint[2] = Gdiplus::Point(rc.left  + 0, rc.bottom - kR);
      rgPoint[3] = Gdiplus::Point(rc.left  + 0, rc.top  + kR);
      rgPoint[4] = Gdiplus::Point(rc.left  + kR, rc.top  + 0);
      rgPoint[5] = Gdiplus::Point(rc.right - kR, rc.top  + 0);
      rgPoint[6] = Gdiplus::Point(rc.right + 0, rc.top  + kR);
      rgPoint[7] = Gdiplus::Point(rc.right + 0, rc.bottom - kR);
      rgPoint[8] = Gdiplus::Point(rc.right + 1, rc.bottom + 0);
      rgPoint[9] = Gdiplus::Point(GetParent()->GetRect()->right, rc.bottom + 0);

      path.AddLines(rgPoint, lengthof(rgPoint));
      //path.CloseAllFigures();
    }

    g.SetSmoothingMode(Gdiplus::SmoothingModeNone);

    {
      RECT rc = m_rc;
      rc.top += 3;
      rc.bottom = rc.top + 10;
      Gdiplus::LinearGradientBrush fillBrush(
        Gdiplus::Point(0, rc.top),
        Gdiplus::Point(0, rc.bottom),
        Gdiplus::Color(252, 253, 253),
        Gdiplus::Color(231, 245, 251));
      g.SetSmoothingMode(Gdiplus::SmoothingModeAntiAlias);
      g.FillRectangle(
        &fillBrush,
        m_rc.left,
        rc.top,
        m_rc.right - m_rc.left,
        rc.bottom - rc.top);
    }

    {
      //g.SetSmoothingMode(Gdiplus::SmoothingModeNone);
      RECT rc = m_rc;
      rc.top += 3;

      rc.top += 9;
      Gdiplus::LinearGradientBrush fillBrush(
        Gdiplus::Point(0, rc.top),
        Gdiplus::Point(0, rc.bottom),
        Gdiplus::Color(207, 231, 250),
        Gdiplus::Color(183, 200, 246));

      g.SetSmoothingMode(Gdiplus::SmoothingModeAntiAlias);
      g.FillRectangle(
        &fillBrush,
        m_rc.left,
        rc.top,
        m_rc.right - m_rc.left,
        rc.bottom - rc.top);
    }

    drawThemeContents(g);

    m_oCloseBox.DrawTheme(g, hTheme);

    // Draw edge
    {
      g.SetSmoothingMode(Gdiplus::SmoothingModeAntiAlias);
      Gdiplus::Pen linePen(Gdiplus::Color(143, 149, 161));
      //Gdiplus::Pen linePen(Gdiplus::Color(255, 0, 0));
      g.DrawPath(&linePen, &path);
    }
  }

  // [G]

  // GetLabel
  public: const char16* GetLabel() const { return m_pwsz; }

  // [H]
  // HasClose
  public: bool HasClose() const {
    return IsSelected();
  }

  // HitTest
  public: virtual Element* HitTest(POINT pt) const override {
    if (auto const pHit = m_oCloseBox.HitTest(pt)) {
      return pHit;
    }
    return Element::HitTest(pt);

  }

  // [S]
  // SetItem
  public: void SetItem(const TCITEM* pTcItem) {
    if (pTcItem->mask & TCIF_IMAGE) {
      m_iImage = pTcItem->iImage;
    }

    if (pTcItem->mask & TCIF_PARAM) {
      m_lParam = pTcItem->lParam;
    }

    if (pTcItem->mask & TCIF_STATE) {
      m_rgfState &= ~pTcItem->dwStateMask;
      m_rgfState |= pTcItem->dwState | pTcItem->dwStateMask;
    }

    if (pTcItem->mask & TCIF_TEXT) {
      m_cwch = ::lstrlenW(pTcItem->pszText);
      m_pwsz = new char16[m_cwch + 1];
      if (!m_pwsz) {
        return;
      }

      ::CopyMemory(
        m_pwsz,
        pTcItem->pszText,
        sizeof(char16) * (m_cwch + 1));
    }
  }

  // [U]
  protected: void update() override {
    ComputeLayout();
  }
}; // Item

//////////////////////////////////////////////////////////////////////
//
// ListButton
//
//
//    [4]    [5]
//    --oooooooooo--
//    -o----------o-
// [3]o------------o [6]
//    o------------o
//    o------------o
//    o------------o
//    o------------o
//    o-----------o- [0]
//    oooooooooooo--
//  [2]           [1]
//
class ListButton : public Element {
  public: static  const char16*  GetClass_() { return L"ListButton"; }

  public: virtual const char16* GetClass()  const override {
    return GetClass_();
  }

  // ctor
  public: ListButton(Element* pParent) :
    Element(pParent) {}

  // [D]
  public: virtual void Draw(HDC hdc) const override {
    ASSERT(IsShow());
    ASSERT(m_rc.left != m_rc.right);

    ::FillRect(hdc, &m_rc, reinterpret_cast<HBRUSH>(COLOR_3DFACE + 1));

    #if 0
    {
      RECT rc = m_rc;

      rc.top += 2;
      rc.bottom -= 2;

      ::DrawEdge(hdc, &rc, EDGE_BUMP, BF_TOP | BF_LEFT | BF_RIGHT);
    }
    #endif

    // Draw triangle
    {
        auto const x = (m_rc.right - m_rc.left - 4) / 2 + m_rc.left;
        auto const y = (m_rc.bottom - m_rc.top) / 2 + m_rc.top;
        fillRect(hdc, x + 0, y + 0, 5, 1);
        fillRect(hdc, x + 1, y + 1, 3, 1);
        fillRect(hdc, x + 2, y + 2, 1, 1);
    }
  }

  private: void drawDownArrow(Gdiplus::Graphics& g) const {
    auto const x = (m_rc.right - m_rc.left - 4) / 2 + m_rc.left;
    auto const y = (m_rc.bottom - m_rc.top) / 2 + m_rc.top;
    auto const color = ::GetSysColor(COLOR_BTNTEXT);
    Gdiplus::SolidBrush arrowBrush(
      Gdiplus::Color(GetRValue(color), GetGValue(color), GetBValue(color)));
    g.SetSmoothingMode(Gdiplus::SmoothingModeNone);
    g.FillRectangle(&arrowBrush, x + 0, y + 0, 5, 1);
    g.FillRectangle(&arrowBrush, x + 1, y + 1, 3, 1);
    g.FillRectangle(&arrowBrush, x + 2, y + 2, 1, 1);
  }

  public: virtual void DrawTheme(
      Gdiplus::Graphics& g,
      HTHEME) const override {
    ASSERT(IsShow());
    ASSERT(m_rc.left != m_rc.right);

    RECT rc = m_rc;
    rc.top += 4;
    rc.bottom -= 2;

    Gdiplus::GraphicsPath path;
    {
      Gdiplus::Point rgPoint[8];

      rgPoint[0] = Gdiplus::Point(rc.right, rc.bottom - 1);

      auto const pNext = GetNextShow();
      if (pNext && pNext->IsSelected()) {
        rgPoint[1] = Gdiplus::Point(rc.right - 1, rc.bottom);
      } else {
        rgPoint[1] = Gdiplus::Point(rc.right, rc.bottom);
      }

      rgPoint[2] = Gdiplus::Point(rc.left, rc.bottom);
      rgPoint[3] = Gdiplus::Point(rc.left, rc.top + 2);
      rgPoint[4] = Gdiplus::Point(rc.left  + 2, rc.top);
      rgPoint[5] = Gdiplus::Point(rc.right - 2, rc.top);
      rgPoint[6] = Gdiplus::Point(rc.right, rc.top + 2);
      rgPoint[7] = Gdiplus::Point(rc.right, rc.bottom - 1);

      path.AddLines(rgPoint, lengthof(rgPoint));
      path.CloseAllFigures();
    }

    // Fill background
    {
      Gdiplus::LinearGradientBrush fillBrush(
        Gdiplus::Point(0, rc.top),
        Gdiplus::Point(0, m_rc.bottom),
        Gdiplus::Color(253, 253, 253),
        Gdiplus::Color(207, 231, 250));
      g.FillPath(&fillBrush, &path);
    }

    // Fill highlight
    {
      Gdiplus::LinearGradientBrush fillBrush(
        Gdiplus::Point(0, 13),
        Gdiplus::Point(0, 27 + 13 - 1),
        IsHover() ?
          Gdiplus::Color(153, 198, 238) :
          Gdiplus::Color(207, 215, 235),
        IsHover() ?
          Gdiplus::Color(242, 245, 250) :
          Gdiplus::Color(242, 245, 250));

      g.FillRectangle(
        &fillBrush,
        rc.left,
        rc.top,
        rc.right - rc.left,
        27);
    }

    drawDownArrow(g);

    // Draw edge
    {
      g.SetSmoothingMode(Gdiplus::SmoothingModeAntiAlias);
      Gdiplus::Pen linePen(Gdiplus::Color(160, 160, 160));
      g.DrawPath(&linePen, &path);
    }
  }
}; // ListButton

//////////////////////////////////////////////////////////////////////
//
// Cursor for Tab Drag
//
static HCURSOR s_hDragTabCursor;

// Window Message for Tab Dragging.
static uint s_nTagDragMsg;

// Send TabDragMsg to window which can handle it.
static HWND handleTabDragAndDrop(
    HWND const hwndTabBand,
    POINT const ptClient,
    TabBandDragAndDrop const eAction) {
  auto ptScreen = ptClient;
  if (!::ClientToScreen(hwndTabBand, &ptScreen)) {
    return nullptr;
  }

  auto hwnd = ::WindowFromPoint(ptScreen);
  if (!hwnd) {
    return nullptr;
  }

  if (s_nTagDragMsg == 0) {
    s_nTagDragMsg = ::RegisterWindowMessage(TabBand__TabDragMsgStr);
    if (s_nTagDragMsg == 0) {
      DEBUG_PRINTF("Failed RegisterWindowMessage %ls\n",
        TabBand__TabDragMsgStr);
      return nullptr;
    }
  }

  do {
    auto const iAnswer = ::SendMessage(
      hwnd,
      s_nTagDragMsg,
      eAction,
      reinterpret_cast<LPARAM>(hwndTabBand));

    if (iAnswer) {
      return hwnd;
    }

    hwnd = ::GetParent(hwnd);
  } while (hwnd);

  if (eAction == kDrop) {
    auto const hwnd = ::GetParent(hwndTabBand);
    auto const iAnswer = ::SendMessage(
        hwnd,
        s_nTagDragMsg,
        kThrow,
        reinterpret_cast<LPARAM>(hwndTabBand));
    return iAnswer ? hwnd : nullptr;
  }

  return nullptr;
}

//////////////////////////////////////////////////////////////////////
//
// loadDragTabCursor
//
// Description:
//  Load cursor for Tab Drag and Drop.
//
static void loadDragTabCursor() {
  if (s_hDragTabCursor) {
    return;
  }

  s_hDragTabCursor = ::LoadCursor(nullptr, MAKEINTRESOURCE(IDC_ARROW));

  HMODULE hDll = ::LoadLibraryEx(
    L"ieframe.dll",
    nullptr,
    LOAD_LIBRARY_AS_DATAFILE);
  if (!hDll) {
    return;
  }

  if (auto const hCursor = ::LoadCursor(hDll, MAKEINTRESOURCE(643))) {
    if (auto const hCursor2 = CopyCursor(hCursor)) {
      s_hDragTabCursor = hCursor2;
    }
  }

  ::FreeLibrary(hDll);
}

//////////////////////////////////////////////////////////////////////
//
// TabBand class

//

// Member variables:
//  m_cxTab
//    Width of tab. m_cxTab and m_xTab are update by computeLayout.
//    We use these variables to avoid redraw.
//  m_xTab
//    Left point of the first tab. m_xTab can be negative if width of
//    Tab Band control is smaller than total number of tabs times
//    m_cxTab.
//
class TabBand : public Element {
  private: enum Constants {
    k_TabListId = 1000,
    k_TabViewId,
    k_ScrollLeft,
    k_ScrollRight,
  }; // Constatns

  private: enum Drag {
    Drag_None,

    Drag_Tab,
    Drag_Start,
  }; // Drag

  public: static const char16*  GetClass_() { return L"TabBand"; }

  public: virtual const char16* GetClass()  const override {
    return GetClass_();
  }

  private: typedef DoubleLinkedList_<Element> Elements;

  private: int m_cItems;
  private: int m_cxTab;
  private: int m_cxMinTab;
  private: Drag m_eDrag;
  private: bool m_fMouseTracking;
  private: HMENU m_hTabListMenu;
  private: HTHEME m_hTheme;
  private: HWND m_hwnd;
  private: HWND m_hwndToolTips;
  private: int m_iFocus;
  private: uint m_nStyle;
  private: MyFont m_oFont;
  private: ListButton m_oListButton;
  private: Elements m_oElements;
  private: Item* m_pDragItem;
  private: Element* m_pHover;
  private: Item* m_pInsertBefore;
  private: Item* m_pSelected;
  private: POINT m_ptDragStart;
  private: int m_xTab;

  // ctor
  private: TabBand(HWND hwnd) :
      m_cItems(0),
      m_cxTab(0),
      m_cxMinTab(k_cxMinTab),
      m_eDrag(Drag_None),
      m_fMouseTracking(false),
      m_hTabListMenu(nullptr),
      m_hTheme(nullptr),
      m_hwnd(hwnd),
      m_hwndToolTips(nullptr),
      m_iFocus(-1),
      m_nStyle(0),
      m_oListButton(this),
      m_pDragItem(nullptr),
      m_pHover(nullptr),
      m_pInsertBefore(nullptr),
      m_pSelected(nullptr),
      m_xTab(0),
      Element(nullptr) {
    if (::IsThemeActive()) {
      m_hTheme = ::OpenThemeData(hwnd, L"Button");
    }
    m_oElements.Append(&m_oListButton);
  }

  // dotr
  private: ~TabBand() {
    if (m_hwndToolTips && (m_nStyle & TCS_TOOLTIPS) != 0) {
      ::DestroyWindow(m_hwndToolTips);
    }

    if (m_hTabListMenu) {
      ::DestroyMenu(m_hTabListMenu);
    }

    if (m_hTheme) {
      ::CloseThemeData(m_hTheme);
    }
  }

  // [C]
  private: bool changeFont(HDC hdc) {
    LOGFONT lf;

    if (!::SystemParametersInfo(
        SPI_GETICONTITLELOGFONT,
        sizeof(lf),
        &lf, 0)) {
      return false;
    }

    lf.lfHeight = -13;

    if (!m_oFont.Init(hdc, &lf)) {
      return false;
    }

    auto const pFont = new Gdiplus::Font(hdc, m_oFont);
    return SetFont(pFont) != nullptr;
  }

  private: bool computeLayout() {
    if (m_cItems == 0) {
      m_cxTab = -1;
      m_xTab = -1;
      return false;
    }

    *m_oListButton.GetRect() = m_rc;

    m_oListButton.GetRect()->left = m_rc.left + k_cxMargin;

    auto x = m_oListButton.GetRect()->left;

    if (m_cItems >= 2) {
      m_oListButton.Show(true);
      x += k_cxListButton;
    } else {
      m_oListButton.Show(false);
    }

    m_oListButton.GetRect()->right = x;

    int cxTabs = m_rc.right - x - k_cxMargin;

    int cxTab = cxTabs / m_cItems;
      cxTab = min(cxTab, m_cxMinTab * 2);

    if (cxTab >= m_cxMinTab) {
      // We can show all tabs.
    } else {
      // How many tabs do we show in min width?
      int cVisibles = cxTabs / m_cxMinTab;
      if (cVisibles == 0) {
        cVisibles = 1;
      }
      cxTab = cxTabs / cVisibles;
    }

    bool fChanged = m_cxTab != cxTab || m_xTab != x;

    m_cxTab = cxTab;
    m_xTab = x;

    Item* pStartItem = nullptr;
    foreach (Elements::Enum, oEnum, &m_oElements) {
      Item* pItem = oEnum.Get()->DynamicCast<Item>();
      if (pItem) {
        pStartItem = pItem;
        break;
      }
    }

    ASSERT(pStartItem != nullptr);

    do {
      bool fShow = false;
      x = m_xTab;
      foreach (Elements::Enum, oEnum, &m_oElements) {
        Item* pItem = oEnum.Get()->DynamicCast<Item>();
        if (!pItem) {
          continue;
        }

        if (pItem == pStartItem) {
          fShow = true;
        }

        pItem->Show(fShow);

        if (!fShow) {
          continue;
        }

        RECT* prc = pItem->GetRect();
        prc->left = x;
        prc->right = x + cxTab;
        prc->top = m_rc.top;
        prc->bottom = m_rc.bottom;
        pItem->ComputeLayout();

        x += cxTab;

        fShow = x + cxTab < m_rc.right;
      }

      if (!m_pSelected) {
        // No tab is selected. So, we display from the first tab.
        break;
      }

      if (m_pSelected->IsShow()) {
        // Selected tab is visible. So, we don't need to scroll tabs.
        break;
      }

      Element* pNext = pStartItem;

      for (;;) {
        pNext = pNext->GetNext();
        if (!pNext) {
          pStartItem = nullptr;
          break;
        }

        pStartItem = pNext->DynamicCast<Item>();
        if (pStartItem) {
          break;
        }
      }
    } while (pStartItem);

    if (m_hwndToolTips) {
      TOOLINFO ti;
      ti.cbSize = sizeof(ti);
      ti.hwnd = m_hwnd;
      ti.uFlags = 0;

      foreach (Elements::Enum, oEnum, &m_oElements) {
        auto const pElement = oEnum.Get();
        if (pElement->Is<Item>()) {
          ti.uId = pElement->StaticCast<Item>()->m_iItem;

        } else if (pElement->Is<ListButton>()) {
          ti.uId = k_TabListId;
        } else {
          continue;
        }

        if (pElement->IsShow()) {
          ti.rect = *pElement->GetRect();
        } else {
          ti.rect.left = ti.rect.right = -1;
          ti.rect.top = ti.rect.bottom = -1;
        }

        ::SendMessage(
            m_hwndToolTips,
            TTM_NEWTOOLRECT,
            0,
            reinterpret_cast<LPARAM>(&ti));
      }
    }

    return fChanged;
  }

  // [D]
  private: virtual void Draw(HDC hdc) const override {
    ::FillRect(hdc, &m_rc, reinterpret_cast<HBRUSH>(COLOR_3DFACE + 1));

    ScopedSelect oSelectFont(hdc, m_oFont);

    foreach (Elements::Enum, oEnum, &m_oElements) {
      auto const pElement = oEnum.Get();
      if (pElement->IsShow()) {
        pElement->Draw(hdc);
      }
    }
  }

  // DrawTheme
  private: virtual void DrawTheme(
      Gdiplus::Graphics& g,
      HTHEME hTheme) const override {

    ASSERT(hTheme != nullptr);
    g.Clear(Gdiplus::Color(0x00171717));
#if 0
    #if 1
    {
      g.SetCompositingMode(Gdiplus::CompositingModeSourceCopy);
      DWORD argb;
      BOOL opaque;
      ::DwmGetColorizationColor(&argb, &opaque);
      Gdiplus::Color color(argb & 0xFFFFFF);
      Gdiplus::SolidBrush brush(color);
      g.FillRectangle(
        &brush,
        m_rc.left,
        m_rc.top,
        m_rc.right - m_rc.left,
        m_rc.bottom - m_rc.top);
      ASSERT(g.GetCompositingMode() == Gdiplus::CompositingModeSourceCopy);
      g.SetCompositingMode(Gdiplus::CompositingModeSourceOver);
    }
    #else
    {
      Gdiplus::LinearGradientBrush fillBrush(
        Gdiplus::Point(0, m_rc.top + 14),
        Gdiplus::Point(0, m_rc.bottom - m_rc.top),
        Gdiplus::Color(207, 215, 235),
        Gdiplus::Color(242, 245, 250));

      g.FillRectangle(
        &fillBrush,
        m_rc.left,
        m_rc.top,
        m_rc.right - m_rc.left,
        m_rc.bottom - m_rc.top);
    }
    #endif
#endif
#if 1
    foreach (Elements::Enum, oEnum, &m_oElements) {
      auto const pElement = oEnum.Get();
      if (pElement->IsShow()) {
        pElement->DrawTheme(g, hTheme);
      }
    }
#endif
  }

  private: static void drawInsertMarker(HDC hdc, RECT* prc) {
    auto rc = * prc;
    rc.top += 5;
    rc.bottom -= 7;

    for (int w = 1; w <= 7; w += 2) {
      fillRect(hdc, rc.left, rc.top, w, 1);
      fillRect(hdc, rc.left, rc.bottom, w, 1);

      rc.top  -= 1;
      rc.left   -= 1;
      rc.bottom += 1;
    }
  }

  // [F]
  // findItem
  private: Item* findItem(int iItem) const {
    if (iItem < 0 || iItem >= m_cItems) {
      return nullptr;
    }

    foreach (Elements::Enum, oEnum, &m_oElements) {
      auto const pItem = oEnum.Get()->DynamicCast<Item>();
      if (!pItem) {
        continue;
      }

      if (pItem->m_iItem == iItem) {
        return pItem;
      }
    }

    return nullptr;
  }

  // findItemFromPoint
  private: Item* findItemFromPoint(POINT pt) const {
    foreach (Elements::Enum, oEnum, &m_oElements) {
      auto const pItem = oEnum.Get()->DynamicCast<Item>();
      if (!pItem) {
        continue;
      }

      if (pt.x < pItem->GetRect()->left) {
        break;
      }

      if (pItem->HitTest(pt) == pItem) {
        return pItem;
      }
    }
    return nullptr;
  }

  // [H]

  // handleTabListMenu
  private: void handleTabListMenu(POINT) {
    POINT ptMouse;
    ptMouse.x = m_oListButton.GetRect()->left;
    ptMouse.y = m_oListButton.GetRect()->bottom;

    ::ClientToScreen(m_hwnd, &ptMouse);

    if (!m_hTabListMenu) {
      m_hTabListMenu = ::CreatePopupMenu();
    }

    // Make Tab List Menu empty
    while (::GetMenuItemCount(m_hTabListMenu) > 0) {
      ::DeleteMenu(m_hTabListMenu, 0, MF_BYPOSITION);
    }

    // Add Tab name to menu.
    Item* pPrevItem = nullptr;
    foreach (Elements::Enum, oEnum, &m_oElements) {
      auto const pItem = oEnum.Get()->DynamicCast<Item>();
      if (!pItem) {
        continue;
      }

      auto const rgfFlag =
        pItem->IsSelected()
            ? MF_STRING | MF_CHECKED
            : MF_STRING;

      if (pPrevItem && pPrevItem->IsShow() != pItem->IsShow()) {
        ::AppendMenu(
            m_hTabListMenu,
            MF_SEPARATOR,
            0,
            nullptr);
      }

      pPrevItem = pItem;

      ::AppendMenu(
          m_hTabListMenu,
          rgfFlag,
          pItem->m_iItem,
          pItem->m_pwsz);
    }

    ::TrackPopupMenuEx(
        m_hTabListMenu,
        TPM_LEFTALIGN | TPM_TOPALIGN,
        ptMouse.x, ptMouse.y,
        m_hwnd,
        nullptr);
  }

  // hitTest
  private: Element* hitTest(POINT pt) const {
    if (auto const pHit = m_oListButton.HitTest(pt)) {
      return pHit;
    }

    foreach (Elements::Enum, oEnum, &m_oElements) {
      auto const pItem = oEnum.Get()->DynamicCast<Item>();
      if (!pItem) {
        continue;
      }

      if (pt.x < pItem->GetRect()->left) {
        break;
      }

      if (auto const pElement = pItem->HitTest(pt)) {
        return pElement;
      }
    }

    return nullptr;
  }

  // [I]
  // Init
  public: static BOOL Init(HINSTANCE hInstance) {
    WNDCLASSEXW oWC;
    oWC.cbSize = sizeof(oWC);
    oWC.style = CS_DBLCLKS | CS_BYTEALIGNCLIENT;
    oWC.lpfnWndProc = windowProc;
    oWC.cbClsExtra = 0;
    oWC.cbWndExtra = 0;
    oWC.hInstance = hInstance;
    oWC.hIcon = nullptr;
    oWC.hCursor = ::LoadCursor(nullptr, MAKEINTRESOURCE(IDC_ARROW));
    oWC.hbrBackground = nullptr;
    oWC.lpszMenuName = nullptr;
    oWC.lpszClassName = WC_TABBANDCLASS;
    oWC.hIconSm = nullptr;

    g_hInstance = hInstance;

    return ::RegisterClassExW(&oWC);
  }

  // [O]
  private: LRESULT onCreate(CREATESTRUCT* pcs) {
    s_cInstances += 1;
    if (s_cInstances == 1) {
      Gdiplus::GdiplusStartupInput oInput;
      Gdiplus::GdiplusStartup(&s_GdiplusToken, &oInput, nullptr);
    }

    ScopedGetDC dc(m_hwnd);
    changeFont(dc);

    if (pcs->style & TCS_TOOLTIPS) {
      m_hwndToolTips = ::CreateWindowEx(
          WS_EX_TOPMOST,
          TOOLTIPS_CLASS,
          nullptr,
          WS_POPUP | TTS_NOPREFIX, // | TTS_ALWAYSTIP,
          CW_USEDEFAULT,
          CW_USEDEFAULT,
          CW_USEDEFAULT,
          CW_USEDEFAULT,
          m_hwnd,
          nullptr,
          g_hInstance,
          nullptr);

      if (m_hwndToolTips) {
        m_nStyle |= TCS_TOOLTIPS;

        TOOLINFO ti;
        ti.cbSize = sizeof(ti);
        ti.uFlags = TTF_IDISHWND | TTF_SUBCLASS;
        ti.hwnd = m_hwnd;
        ti.uId = reinterpret_cast<UINT_PTR>(m_hwnd);
        ti.lpszText = nullptr;
        ::SendMessage(
            m_hwndToolTips,
            TTM_ADDTOOL,
            0,
            reinterpret_cast<LPARAM>(&ti));
      }
    }

    return TRUE;
  }

  // onDeleteItem
  private: bool OnDeleteItem(int iDeleteItem) {
    auto const pItem = findItem(iDeleteItem);
    if (!pItem) {
      return FALSE;
    }

    bool fSelChanged = m_pSelected == pItem;

    if (fSelChanged) {
      if (pItem->GetPrev()) {
        m_pSelected = pItem->GetPrev()->DynamicCast<Item>();
      }

      if (m_pSelected == nullptr && pItem->GetNext()) {
        m_pSelected = pItem->GetNext()->DynamicCast<Item>();
      }
    }

    if (m_pHover == pItem) {
      m_pHover = nullptr;
    }

    m_oElements.Delete(pItem);
    m_cItems -= 1;

    // Renumber tab index
    {
      int iItem = 0;
      foreach (Elements::Enum, oEnum, &m_oElements) {
        Item* pItem = oEnum.Get()->DynamicCast<Item>();
        if (!pItem) {
          continue;
        }

        pItem->m_iItem = iItem;
        iItem += 1;
      }
    }

    if (m_hwndToolTips) {
      TOOLINFO ti;
      ti.cbSize = sizeof(ti);
      ti.hwnd = m_hwnd;
      ti.uId = m_cItems;
      ::SendMessage(
          m_hwndToolTips,
          TTM_DELTOOL,
          0,
          reinterpret_cast<LPARAM>(&ti));
    }

    UpdateLayout();

    if (fSelChanged) {
      if (m_pSelected) {
        m_pSelected->SetState(Element::State_Selected);
      }

      sendNotify(TCN_SELCHANGE);
    }

    return TRUE;
  }

  // onInsertItem
  private: int onInsertItem(int iItem, const TCITEM* pTcItem) {
    auto const pNewItem = new Item(this, iItem, pTcItem);
    if (!pNewItem) {
      return -1;
    }

    pNewItem->m_iItem = iItem;

    if (m_iFocus >= iItem) {
      m_iFocus += 1;
    }

    Item* pRefItem = nullptr;
    foreach (Elements::Enum, oEnum, &m_oElements) {
      auto const pItem = oEnum.Get()->DynamicCast<Item>();
      if (!pItem) {
        continue;
      }

      if (pItem->m_iItem < iItem) {
        continue;
      }

      if (pItem->m_iItem == iItem) {
        ASSERT(!pRefItem);
        pRefItem = pItem;
      }

      pItem->m_iItem += 1;
    }

    m_cItems += 1;

    m_oElements.InsertBefore(pNewItem, pRefItem);

    if (m_hwndToolTips) {
      TOOLINFO ti;
      ti.cbSize = sizeof(ti);
      ti.hwnd = m_hwnd;
      ti.lpszText = LPSTR_TEXTCALLBACK;
      ti.uFlags = 0;
      ti.uId = m_cItems - 1;
      ::SendMessage(
          m_hwndToolTips,
          TTM_ADDTOOL,
          0,
          reinterpret_cast<LPARAM>(&ti));
    }

    UpdateLayout();

    return iItem;
  }

  // onLButtonDown
  private: void onLButtonDown(POINT pt) {
    auto const pElement = hitTest(pt);
    if (!pElement) {
      return;
    }

    auto const pItem = pElement->DynamicCast<Item>();
    if (!pItem) {
      // Not a tab.
      return;
    }

    if (!pItem->IsSelected()) {
      SelectItem(pItem);
      // Note: We should start tab dragging, otherwise if
      // mouse pointer is in close box, onButtonUp close
      // the tab.
    }

    #if DEBUG_DRAG
      DEBUG_PRINTF("%p drag=%p\n", this, m_pDragItem);
    #endif

    loadDragTabCursor();

    m_pDragItem = pItem;
    m_eDrag = Drag_Start;
    m_ptDragStart = pt;

    ::SetCapture(m_hwnd);
  }

  // onLButtonUp
  private: void onLButtonUp(POINT pt) {
    if (!m_pDragItem) {
      auto const pElement = hitTest(pt);
      if (!pElement) {
        return;
      }

      if (pElement->Is<CloseBox>()) {
        if (sendNotify(TABBAND_NOTIFY_QUERY_CLOSE)) {
          sendNotify(TABBAND_NOTIFY_CLOSE);
        }
        return;
      }

      if (pElement->Is<ListButton>()) {
        handleTabListMenu(pt);
        return;
      }
    } else {
      Item* pDragItem = m_pDragItem;
      Item* pInsertBefore = m_pInsertBefore;

      stopDrag();

      if (!pInsertBefore) {
        handleTabDragAndDrop(m_hwnd, pt, kDrop);

      } else {
        if (pDragItem != pInsertBefore) {
          m_oElements.Delete(pDragItem);
          m_oElements.InsertBefore(pDragItem, pInsertBefore);
          int iItem = 0;
          foreach (Elements::Enum, oEnum, &m_oElements) {
            Item* pItem = oEnum.Get()->DynamicCast<Item>();
            if (!pItem) continue;
            pItem->m_iItem = iItem;
            iItem += 1;
          }

          computeLayout();
        }

        // Hide insertion position mark
        ::InvalidateRect(m_hwnd, nullptr, false);
      }
    }
  }

  // onMessage
  private: LRESULT OnMessage(UINT uMsg, WPARAM wParam, LPARAM lParam) {
    #if DEBUG_MESSAGE
      DEBUG_PRINTF("%p msg=0x%x\n", this, uMsg);
    #endif // DEBUG_MESSAGE

    switch (uMsg) {
      case WM_COMMAND: {
        int const iItem = LOWORD(wParam);
        if (iItem >= 0) {
          SelectItem(iItem);
        }
        return 0;
      }

      case WM_CREATE:
        return onCreate(reinterpret_cast<CREATESTRUCT*>(lParam));

      case WM_DESTROY:
        s_cInstances -= 1;
        if (s_cInstances == 0) {
          Gdiplus::GdiplusShutdown(s_GdiplusToken);
        }
        return 0;

      case WM_LBUTTONDOWN: {
        POINT pt;
        pt.x = GET_X_LPARAM(lParam);
        pt.y = GET_Y_LPARAM(lParam);
        onLButtonDown(pt);
        return 0;
      }

      case WM_LBUTTONUP: {
        POINT pt;
        pt.x = GET_X_LPARAM(lParam);
        pt.y = GET_Y_LPARAM(lParam);
        onLButtonUp(pt);
        return 0;
      }

      case WM_MOUSELEAVE: {
        #if DEBUG_HOVER
          DEBUG_PRINTF("WM_MOUSELEAVE %p hover=%ls.%p\n",
            this,
            nullptr == m_pHover ? L"null" : m_pHover->GetClass(),
            m_pHover);
        #endif // DEBUG_HOVER

        m_fMouseTracking = false;

        if (m_pHover) {
          m_pHover->SetHover(false);
          m_pHover->Invalidate(m_hwnd);
          m_pHover = nullptr;
        }

        return 0;
      }

      case WM_MOUSEMOVE: {
        POINT const pt = { GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam) };
        OnMouseMove(pt);
        return 0;
      }

      case WM_NCDESTROY:
        delete this;
        break;

      case WM_NCHITTEST: {
        POINT pt = { GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam) };
        if (::ScreenToClient(m_hwnd, &pt)) {
          if (!hitTest(pt)) {
            return ::SendMessage(::GetParent(m_hwnd), uMsg, wParam, lParam);
          }
        }
        break;
      }

      case WM_NCLBUTTONDOWN:
      case WM_NCLBUTTONUP:
      case WM_NCMOUSEMOVE:
      case WM_NCRBUTTONDOWN:
      case WM_NCRBUTTONUP:
        return ::SendMessage(::GetParent(m_hwnd), uMsg, wParam, lParam);

      case WM_NOTIFY: {
        #if DEBUG_TOOLTIP
        {
          auto const p = reinterpret_cast<NMHDR*>(lParam);
          DEBUG_PRINTF("WM_NOTIFY %p ctrl=%d code=%d\n",
            this, wParam, p->code);
        }
        #endif

        return ::SendMessage(
            ::GetParent(m_hwnd),
            WM_NOTIFY,
            wParam,
            lParam);
      }

      case WM_PAINT: {
        PAINTSTRUCT ps;
        ::BeginPaint(m_hwnd, &ps);

        if (!m_hTheme) {
          Draw(ps.hdc);
        } else {
          // To avoid flicker, we draw into bitmap then copy to window.
          ScopedGetDC dc(m_hwnd);
          Gdiplus::Graphics g0(dc);
          Gdiplus::Bitmap bitmap(m_rc.right, m_rc.bottom, &g0);
          {
            Gdiplus::Graphics g(&bitmap);
            DrawTheme(g, m_hTheme);
          }
          Gdiplus::Graphics psG(ps.hdc);
          psG.SetCompositingMode(Gdiplus::CompositingModeSourceCopy);
          //psG.Clear(Gdiplus::Color(0xFF000000));
          psG.DrawImage(&bitmap, 0, 0);
        }

        if (!!m_pInsertBefore) {
          drawInsertMarker(ps.hdc, m_pInsertBefore->GetRect());
        }

        ::EndPaint(m_hwnd, &ps);
        return 0;
      }

      case WM_SIZE: {
        // Handle WM_SIZE at window creation. We won't receive WM_SIZE
        // since we handle WM_WINDOWPOSCHANGED.
        auto const hwndParent = ::GetParent(m_hwnd);
        if (!hwndParent) {
          return 0;
        }

        ::GetClientRect(hwndParent, &m_rc);
        auto const iFontHeight = 16;  // must be >= 16 (Small Icon Height)
        if (!m_hTheme) {
          m_rc.bottom = m_rc.top + 6 + iFontHeight + 10;
        } else {
          m_rc.bottom = m_rc.top + 2 + 7 + iFontHeight + 5 + 2;
        }

        ::SetWindowPos(
            m_hwnd,
            nullptr,
            m_rc.left,
            m_rc.top,
            m_rc.right - m_rc.left,
            m_rc.bottom - m_rc.top,
            SWP_NOZORDER);

        return 0;
      }

      case WM_SETTINGCHANGE:
        switch (wParam) {
          case SPI_SETICONTITLELOGFONT:
          case SPI_SETNONCLIENTMETRICS: {
            ScopedGetDC dc(m_hwnd);
            changeFont(dc);
            break;
          }
        }
        break;

      case WM_THEMECHANGED:
        if (::IsThemeActive()) {
          if (!m_hTheme) {
            m_hTheme = ::OpenThemeData(m_hwnd, L"Tab");
          }
        } else {
          if (m_hTheme) {
            ::CloseThemeData(m_hTheme);
            m_hTheme = nullptr;
          }
        }
        return 0;

      case WM_USER:
        return m_hTheme == nullptr
          ? ::GetSysColor(COLOR_3DFACE)
          : RGB(183, 200, 246);

      case WM_WINDOWPOSCHANGED: {
        auto const wp = reinterpret_cast<WINDOWPOS*>(lParam);
        if (wp->flags & SWP_NOSIZE) {
          return 0;
        }

        m_rc.left = wp->x;
        m_rc.top = wp->y;
        m_rc.right = wp->x + wp->cx;
        m_rc.bottom = wp->y + wp->cy;

        UpdateLayout();
        return 0;
      }

      ////////////////////////////////////////////////////////////
      //
      // Tab Control Messages
      //
      case TCM_DELETEITEM:
        return OnDeleteItem(static_cast<int>(wParam));

      case TCM_GETCURFOCUS:
        return m_iFocus;

      case TCM_GETCURSEL:
        return m_pSelected ? m_pSelected->m_iItem : -1;

      case TCM_GETIMAGELIST:
        return reinterpret_cast<LRESULT>(m_hImageList);

      case TCM_GETITEM: {
        auto const pItem = findItem(static_cast<int>(wParam));
        if (!pItem) {
          return FALSE;
        }

        auto const pTcItem = reinterpret_cast<TCITEM*>(lParam);

        if (pTcItem->mask & TCIF_IMAGE) {
          pTcItem->iImage = pItem->m_iImage;
        }

        if (pTcItem->mask & TCIF_PARAM) {
          pTcItem->lParam = pItem->m_lParam;
        }

        if (pTcItem->mask & TCIF_STATE) {
          pTcItem->dwState = pItem->m_rgfState & pTcItem->dwStateMask;
        }

        if (pTcItem->mask & TCIF_TEXT) {
          auto const cwch = min(
            ::lstrlen(pItem->m_pwsz),
            pTcItem->cchTextMax - 1);

          ::CopyMemory(
              pTcItem->pszText,
              pItem->m_pwsz,
              sizeof(char16) * cwch);

          pTcItem->pszText[cwch] = 0;
        }
        return TRUE;
      }

      case TCM_GETITEMCOUNT:
        return m_cItems;

      case TCM_GETTOOLTIPS:
        return reinterpret_cast<LRESULT>(m_hwndToolTips);

      case TCM_INSERTITEM:
        return onInsertItem(
            static_cast<int>(wParam),
            reinterpret_cast<TCITEM*>(lParam));

      case TCM_SETCURFOCUS:
        m_iFocus = static_cast<int>(wParam);
        return 0;

      case TCM_SETCURSEL:
        return SelectItem(static_cast<int>(wParam));

      case TCM_SETIMAGELIST: {
        auto const hOldImageList = m_hImageList;
        SetImageList(reinterpret_cast<HIMAGELIST>(lParam));
        UpdateLayout();
        return reinterpret_cast<LRESULT>(hOldImageList);
      }

      case TCM_SETMINTABWIDTH: {
        auto const iPrev = m_cxMinTab;
        m_cxMinTab = max(static_cast<int>(lParam), k_cxMinTab);
        return iPrev;
      }

      case TCM_SETTOOLTIPS:
        m_hwndToolTips = reinterpret_cast<HWND>(wParam);
        return 0;
    }

    return ::DefWindowProc(m_hwnd, uMsg, wParam, lParam);
  }

  private: void OnMouseMove(POINT pt) {
    auto const pHover = hitTest(pt);

    if (!m_pDragItem) {
      // Hover
      if (!m_fMouseTracking) {
        TRACKMOUSEEVENT oTrack;
        oTrack.cbSize = sizeof(oTrack);
        oTrack.dwFlags = TME_LEAVE;
        oTrack.hwndTrack = m_hwnd;

        if (!::TrackMouseEvent(&oTrack)) {
          return;
        }

        m_fMouseTracking = true;
      }

      if (m_pHover == pHover) {
        return;
      }

      if (m_pHover) {
        m_pHover->SetHover(false);
        m_pHover->Invalidate(m_hwnd);
      }

      if (pHover) {
        pHover->SetHover(true);
        pHover->Invalidate(m_hwnd);
      }

      m_pHover = pHover;
    } else {
      if (::GetCapture() != m_hwnd) {
        // Someone takes capture. So, we stop dragging.
        stopDrag();
        return;
      }

      if (Drag_Start == m_eDrag) {
        if (pt.x - m_ptDragStart.x >= -5 &&
            pt.x - m_ptDragStart.x <= 5) {
          return;
        }

        m_eDrag = Drag_Tab;
      }

      // Tab dragging
      auto const pInsertBefore = pHover == nullptr ?
        nullptr :
        pHover->DynamicCast<Item>();

      auto hCursor = s_hDragTabCursor;

      if (!pInsertBefore) {
        #if 0
        if (handleTabDragAndDrop(m_hwnd, pt, kHover) == nullptr) {
          hCursor = ::LoadCursor(nullptr, MAKEINTRESOURCE(IDC_NO));
        }
        #endif
      }

      ::SetCursor(hCursor);

      if (pInsertBefore != m_pInsertBefore) {
        ::InvalidateRect(m_hwnd, nullptr, false);
      }

      m_pInsertBefore = pInsertBefore;
    }
  }

  // [S]
  // selectItem
  private: int SelectItem(int const iItem) {
    return SelectItem(findItem(iItem));
  }

  private: int SelectItem(Item* const pItem) {
    if (m_pSelected != pItem) {
      if (m_pSelected) {
        m_pSelected->SetState(Element::State_Normal);
        m_pSelected->Invalidate(m_hwnd);
      }

      m_pSelected = pItem;

      if (pItem) {
        pItem->SetState(Element::State_Selected);
        if (!pItem->IsShow()) {
          UpdateLayout();
        }
        pItem->Invalidate(m_hwnd);
      }

      sendNotify(TCN_SELCHANGE);
    }

    return m_pSelected ? m_pSelected->m_iItem : -1;
  }

  // sendNotify
  private: LRESULT sendNotify(uint const nCode) {
    auto const hwndParent = ::GetParent(m_hwnd);

    if (!hwndParent) {
      return TRUE;
    }

    NMHDR oNotify;
    oNotify.code = nCode;
    oNotify.hwndFrom = m_hwnd;
    oNotify.idFrom = ::GetDlgCtrlID(m_hwnd);

    return ::SendMessage(
      hwndParent,
      WM_NOTIFY,
      oNotify.idFrom,
      reinterpret_cast<LPARAM>(&oNotify));
  }

  private: void stopDrag() {
    m_eDrag = Drag_None;
    m_pDragItem = nullptr;
    m_pInsertBefore = nullptr;

    ::ReleaseCapture();
    ::SetCursor(::LoadCursor(nullptr, MAKEINTRESOURCE(IDC_ARROW)));
  }

  // [U]
  private: void UpdateLayout() {
    computeLayout();
    ::InvalidateRect(m_hwnd, nullptr, false);
  }

  // [W]
  private: static LRESULT CALLBACK windowProc(
      HWND const hwnd,
      UINT const uMsg,
      WPARAM const wParam,
      LPARAM const lParam) {
    auto tabBand = reinterpret_cast<TabBand*>(
      ::GetWindowLongPtr(hwnd, GWLP_USERDATA));

    if (!tabBand) {
      tabBand = new TabBand(hwnd);

      ::SetWindowLongPtr(
        hwnd,
        GWLP_USERDATA,
        reinterpret_cast<LONG_PTR>(tabBand));
    }

    return tabBand->OnMessage(uMsg, wParam, lParam);
  }
};

void TabBand__Init(HINSTANCE const hInstance) {
  TabBand::Init(hInstance);
}
