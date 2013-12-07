#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - Page
// listener/winapp/vi_Page.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_Page.cpp#3 $
//
#define DEBUG_DIRTY  0
#define DEBUG_FORMAT 0
#define DEBUG_HEAP   0
#define DEBUG_RENDER 0
#include "./vi_Page.h"

#include "./ed_interval.h"
#include "./ed_util.h"
#include "./gfx_base.h"
#include "./vi_Buffer.h"
#include "./vi_Selection.h"
#include "./vi_util.h"

#include <vector>

namespace PageInternal {

const float cxLeftMargin = 7.0f;
const int k_nTabWidth = 4;
const float k_cyMinScroll = 100.0f;

inline char16 toxdigit(int k) {
  if (k <= 9)
    return static_cast<char16>(k + '0');
 return static_cast<char16>(k - 10 + 'A');
} // toxdigit

inline gfx::ColorF ColorToColorF(Color color) {
  COLORREF const cr = color;
  return gfx::ColorF(
      static_cast<float>(GetRValue(cr)) / 255,
      static_cast<float>(GetGValue(cr)) / 255,
      static_cast<float>(GetBValue(cr)) / 255);
}

inline void drawLine(const gfx::Graphics& gfx, const gfx::Brush& brush,
                     float sx, float sy, float ex, float ey) {
  gfx.DrawLine(brush, sx, sy, ex, ey);
}

inline void drawHLine(const gfx::Graphics& gfx, const gfx::Brush& brush,
                      float sx, float ex, float y) {
  drawLine(gfx, brush, sx, y, ex, y);
}

inline void drawVLine(const gfx::Graphics& gfx, const gfx::Brush& brush,
                      float x, float sy, float ey) {
  drawLine(gfx, brush, x, sy, x, ey);
}

inline void fillRect(const gfx::Graphics& gfx, const gfx::RectF& rect,
                     gfx::ColorF color) {
  gfx::Brush fill_brush(gfx, color);
  gfx.FillRectangle(fill_brush, rect);
}

static void DrawText(const gfx::Graphics& gfx, const Font& font,
              const gfx::Brush& text_brush, const gfx::RectF& rect,
              const char16* chars, uint num_chars) {
  ASSERT(num_chars);
  std::vector<uint32> code_points(num_chars);
  auto it = code_points.begin();
  for (auto s = chars; s < chars + num_chars; ++s) {
    *it = *s;
    ++it;
  }
  std::vector<uint16> glyph_indexs(num_chars);
  COM_VERIFY(font.font_face()->GetGlyphIndices(&code_points[0], num_chars,
                                               &glyph_indexs[0]));
  DWRITE_GLYPH_RUN glyph_run;
  glyph_run.fontFace = font.font_face();
  glyph_run.fontEmSize = gfx::FactorySet::ScaleY(font.font_size());
  glyph_run.glyphCount = glyph_indexs.size();
  glyph_run.glyphIndices = &glyph_indexs[0];
  glyph_run.glyphAdvances = nullptr;
  glyph_run.glyphOffsets = nullptr;
  glyph_run.isSideways = false;
  glyph_run.bidiLevel = 0;

  auto left_top = rect.left_top() + gfx::SizeF(0.0f, font.ascent());
  ASSERT(gfx.drawing());
  gfx->DrawGlyphRun(left_top, &glyph_run, text_brush);
  gfx.Flush();
}

enum CellKind {
  CellKind_Filler,
  CellKind_Marker,
  CellKind_Text,
  CellKind_Unicode,
}; // CellKind

//////////////////////////////////////////////////////////////////////
//
// Cell
//
class Cell : public ObjectInHeap {
  public: Cell* m_pNext;
  public: float m_cx;
  public: float m_cy;

  protected: Color m_crBackground;

  public: Cell(Color cr, float cx, float cy)
      : m_pNext(nullptr),
        m_crBackground(cr),
        m_cx(cx),
        m_cy(cy) {
    ASSERT(cx >= 1);
    ASSERT(cy >= 1);
  }

  public: virtual Cell* Copy(HANDLE, char16*) const = 0;

  public: virtual bool Equal(const Cell* pCell) const {
    if (pCell->GetKind() != GetKind()) return false;
    if (pCell->m_cx != m_cx) return false;
    if (pCell->m_cy != m_cy) return false;
    if (!pCell->m_crBackground.Equal(m_crBackground)) return false;
    return true;
  } // Equal

  public: virtual Posn Fix(const char16*, float iHeight, float) {
    m_cy = iHeight;
    return -1;
  }

  public: virtual float GetDescent() const { return 0; }
  public: virtual float GetHeight()  const = 0;
  public: virtual CellKind GetKind()    const = 0;
  public: float GetWidth()   const { return m_cx; }

  public: virtual uint Hash() const {
    uint nHash = static_cast<uint>(m_cx);
    nHash ^= static_cast<uint>(m_cy);
    nHash ^= m_crBackground.Hash();
    return nHash;
  }

  public: virtual float MapPosnToX(const gfx::Graphics&, Posn) const {
    return -1.0f;
  }

    // MapXToPosn - x is cell relative.
  public: virtual Posn MapXToPosn(const gfx::Graphics&, float) const {
    return -1;
  }

  public: virtual bool Merge(Font*, Color, Color, TextDecoration, float) {
    return false;
  }

  public: virtual void Render(const gfx::Graphics& gfx,
                              const gfx::RectF& rect) const {
    fillRect(gfx, rect, ColorToColorF(m_crBackground));
  }
};

class EnumCell {
  private: Cell*   m_pRunner;

  public: EnumCell(const Page::Line* p)
    : m_pRunner(p->GetCell()) {
  }

  public: bool AtEnd() const { return nullptr == m_pRunner; }
  public: Cell* Get() const { ASSERT(!AtEnd()); return m_pRunner; }

  public: void Next() {
    ASSERT(!AtEnd()); m_pRunner = m_pRunner->m_pNext;
  }
};

class FillerCell final : public Cell {
  public: FillerCell(Color cr, float cx, float cy) : Cell(cr, cx, cy) {}

  public: virtual Cell* Copy(HANDLE hHeap, char16*) const override {
    return new(hHeap) FillerCell(*this);
  }

  public: virtual float GetHeight() const override { return 0.0f; }

  public: virtual CellKind GetKind() const override { 
    return CellKind_Filler;
  }
};

//////////////////////////////////////////////////////////////////////
//
// MarkerCell
//
class MarkerCell final : public Cell {
  public: enum Kind {
    Kind_Eob,
    Kind_Eol,
    Kind_Tab,
    Kind_Wrap,
  };

  private: Posn m_lStart;
  private: Posn m_lEnd;
  private: Color m_crColor;
  private: float m_iAscent;
  private: float m_iDescent;
  private: Kind  m_eKind;

  public: MarkerCell(Color crColor,
                     Color crBackground,
                     float cx,
                     float iHeight,
                     float iDescent,
                     Posn lPosn,
                     Kind eKind)
      : m_crColor(crColor),
        m_iAscent(iHeight - iDescent),
        m_iDescent(iDescent),
        m_eKind(eKind),
        m_lStart(lPosn),
        m_lEnd(eKind == Kind_Wrap ? lPosn : lPosn + 1),
        Cell(crBackground, cx, iHeight) {
  }

  public: virtual Cell* Copy(HANDLE hHeap, char16*) const {
    return new(hHeap) MarkerCell(*this);
  }

  public: virtual bool Equal(const Cell* pCell) const {
    if (!Cell::Equal(pCell))
      return false;
    auto const pMarker = reinterpret_cast<const MarkerCell*>(pCell);
    if (!m_crColor.Equal(pMarker->m_crColor))
      return false;
    if (m_eKind != pMarker->m_eKind)
      return false;
    return true;
  }

  public: virtual Posn Fix(const char16*, float iHeight,
                           float iDescent) override {
    m_cy = iHeight;
    m_iDescent = iDescent;
    return m_lEnd;
  }

  public: virtual float GetDescent() const override { return m_iDescent; }
  public: virtual float GetHeight()  const override { return m_cy; }
  public: virtual CellKind GetKind() const override {
    return CellKind_Marker;
  }

  public: virtual uint Hash() const override {
    auto nHash = Cell::Hash();
    nHash <<= 8;
    nHash ^= m_crColor.Hash();
    nHash <<= 8;
    nHash ^= m_eKind;
    return nHash;
  }

  public: virtual float MapPosnToX(const gfx::Graphics&,
                                   Posn lPosn) const override {
    if (lPosn <  m_lStart)
      return -1.0f;
    if (lPosn >= m_lEnd)
      return -1.0f;
    return 0.0f;
  }

  public: virtual Posn MapXToPosn(const gfx::Graphics&,
                                  float) const override {
    return m_lStart;
  }

  public: virtual void Render(const gfx::Graphics& gfx,
                              const gfx::RectF& rect) const override {
    Cell::Render(gfx, rect);

    auto const yBottom = rect.bottom - m_iDescent;
    auto const yTop    = yBottom - m_iAscent;
    auto const xLeft   = rect.left;
    auto const xRight  = rect.right;

    gfx::Brush stroke_brush(gfx, ColorToColorF(m_crColor));

    switch (m_eKind) {
      case Kind_Eob: { // Draw <-
        // FIXME 2007-06-13 We should get internal leading from font.
        auto const iInternalLeading = 3;
        auto const w = max(m_iAscent / 6, 2);
        auto const y = yBottom - (m_iAscent - iInternalLeading) / 2;
        drawHLine(gfx, stroke_brush, xLeft, xRight, y);
        drawLine(gfx, stroke_brush, xLeft + w, y - w, xLeft, y);
        drawLine(gfx, stroke_brush, xLeft + w, y + w, xLeft, y);
        break;
      } // Kind_Eob

      case Kind_Eol: { // Draw V
        auto const y = yBottom - m_iAscent * 3 / 5;
        auto const w = max(m_cx / 6, 2);
        auto const x = xLeft + m_cx / 2;
        drawVLine(gfx, stroke_brush, x, y, yBottom);
        drawLine(gfx, stroke_brush, x - w, yBottom - w, x, yBottom);
        drawLine(gfx, stroke_brush, x + w, yBottom - w, x, yBottom);
        break;
      } // Kind_Eol

      case Kind_Tab: { // Draw |_|
        auto const w = max(m_iAscent / 6, 2);
        drawHLine(gfx, stroke_brush, xLeft + 2, xRight - 3, yBottom);
        drawVLine(gfx, stroke_brush, xLeft + 2, yBottom, yBottom - w * 2);
        drawVLine(gfx, stroke_brush, xRight - 3, yBottom, yBottom - w * 2);
        break;
      } // Kind_Tab

      case Kind_Wrap: { // Draw ->
        auto const ex = xRight - 1;
        auto const w = max(m_iAscent / 6, 2);
        auto const y = yTop + m_iAscent / 2;
        drawHLine(gfx, stroke_brush, xLeft, ex, y);
        drawLine(gfx, stroke_brush, ex - w, y - w, xRight, y);
        drawLine(gfx, stroke_brush, ex - w, y + w, xRight, y);
        break;
      } // Kind_Wrap

        default:
        CAN_NOT_HAPPEN();
    }
  }
};

//////////////////////////////////////////////////////////////////////
//
// TextCell
//
class TextCell : public Cell {
  protected: Color            m_crColor;
  protected: TextDecoration   m_eDecoration;
  protected: float m_iDescent;

  protected: Posn             m_lStart;
  protected: Posn             m_lEnd;

  protected: Font*            m_pFont;
  protected: const char16*    m_pwch;
  protected: uint             m_cwch;
  protected: uint             m_ofs;

  public: TextCell(
      const StyleValues*    pStyle,
      Color           crColor,
      Color           crBackground,
      Font*           pFont,
      float           cx,
      Posn            lPosn,
      uint            ofs,
      uint            cwch = 1)
        : m_cwch(cwch),
          m_crColor(crColor),
          m_eDecoration(pStyle->GetDecoration()),
          m_lStart(lPosn),
          m_lEnd(lPosn + 1),
          m_ofs(ofs),
          m_pFont(pFont),
          Cell(crBackground, cx, pFont->GetHeight()) {
  }

  public: virtual Cell* Copy(HANDLE hHeap, char16* pwch) const {
    auto const pCell = new(hHeap) TextCell(*this);
    pCell->m_pwch = pwch + m_ofs;
    return pCell;
  }

  // Equal - Returns true if specified cell is equal to this cell.
  public: virtual bool Equal(const Cell* pCell) const {
    if (!Cell::Equal(pCell))
      return false;
    auto const pText = reinterpret_cast<const TextCell*>(pCell);
    if (!m_crColor.Equal(pText->m_crColor))
      return false;
    if (m_eDecoration != pText->m_eDecoration)
      return false;
    if (m_cwch != pText->m_cwch)
      return false;
    auto const cb = sizeof(char16) * m_cwch;
    return !::memcmp(m_pwch, pText->m_pwch, cb);
  }

  public: virtual Posn Fix(const char16* pwch, float iHeight,
                           float iDescent) override {
      ASSERT(m_lStart <= m_lEnd);
      m_pwch     = pwch + m_ofs;
      m_cy       = iHeight;
      m_iDescent = iDescent;
      return m_lEnd;
  }

  public: virtual float GetDescent() const override {
    return m_pFont->GetDescent();
  }

  public: virtual float GetHeight() const override {
   return m_pFont->GetHeight();
  }

  public: virtual CellKind GetKind() const override {
    return CellKind_Text;
  }

  public: virtual uint Hash() const override final {
    uint nHash = Cell::Hash();
    nHash ^= m_crColor.Hash();
    nHash ^= m_pFont->Hash();
    nHash ^= m_eDecoration;
    nHash ^= m_cwch;

    auto const pEnd = m_pwch + m_cwch;
    for (const char16* p = m_pwch; p < pEnd; p++) {
      nHash <<= 5; nHash ^= *p; nHash >>= 3;
    }
    return nHash;
  }

  public: virtual float MapPosnToX(const gfx::Graphics&,
                                   Posn lPosn) const override final {
    if (lPosn <  m_lStart)
      return -1;
    if (lPosn >= m_lEnd)
      return -1;
    auto const cwch = lPosn - m_lStart;
    if (!cwch)
      return 0;
    return ::ceilf(m_pFont->GetTextWidth(m_pwch, cwch));
  }

    public: virtual Posn MapXToPosn(const gfx::Graphics&,
                                  float x) const override final {
    if (x >= m_cx)
      return m_lEnd;
    for (uint k = 1; k <= m_cwch; ++k) {
      auto const cx = ::floorf(m_pFont->GetTextWidth(m_pwch, k));
      if (x < cx)
        return m_lStart + k - 1;
    }
    return m_lEnd;
  }

  // Merge - Returns true if specified cell is mergable to this cell.
  public: virtual bool Merge(
      Font*           pFont,
      Color           crColor,
      Color           crBackground,
      TextDecoration  eDecoration,
      float cx) override {
    if (m_pFont        != pFont) return false;
    if (m_crColor      != crColor) return false;
    if (m_crBackground != crBackground) return false;
    if (m_eDecoration  != eDecoration)  return false;
    m_cx   += cx;
    m_cwch += 1;
    m_lEnd += 1;
    return true;
  } // Merge

  // Render - Render text of this cell
  public: virtual void Render(const gfx::Graphics& gfx,
                              const gfx::RectF& rect) const override {
    auto const y = rect.bottom - m_iDescent -
                   (m_eDecoration != TextDecoration_None ? 1 : 0);

      gfx::Brush fill_brush(gfx, ColorToColorF(m_crBackground));
    gfx.FillRectangle(fill_brush, rect);

      gfx::Brush text_brush(gfx, ColorToColorF(m_crColor));
    DrawText(gfx, *m_pFont, text_brush, rect, m_pwch, m_cwch);

      #if SUPPORT_IME
    switch (m_eDecoration) {
      case TextDecoration_ImeInput:
        // TODO: We should use dotted line. It was PS_DOT.
        drawHLine(gfx, text_brush, rect.left, rect.right - 4, y + 3);
        break;

        case TextDecoration_ImeInactiveA:
        drawHLine(gfx, text_brush, rect.left, rect.right - 4, y + 3);
        break;

        case TextDecoration_ImeInactiveB:
        drawHLine(gfx, text_brush, rect.left, rect.right - 4, y + 3);
        break;

        case TextDecoration_ImeActive:
        drawHLine(gfx, text_brush, rect.left, rect.right - 4, y + 3);
        drawHLine(gfx, text_brush, rect.left, rect.right - 4, y + 2);
        break;
    }
    #endif
  }
};

//////////////////////////////////////////////////////////////////////
//
// UnicodeCell
//
class UnicodeCell final : public TextCell {
  public: UnicodeCell(
      const StyleValues*    pStyle,
      Color           crColor,
      Color           crBackground,
      Font*           pFont,
      float           cx,
      Posn            lPosn,
      uint            ofs,
      uint            cwch) :
          TextCell(
              pStyle,
              crColor,
              crBackground,
              pFont,
              cx,
              lPosn,
              ofs,
              cwch) {
      m_cy += 4;
  }

  public: virtual Cell* Copy(HANDLE hHeap, char16* pwch) const {
    auto const pCell = new(hHeap) UnicodeCell(*this);
    pCell->m_pwch = pwch + m_ofs;
    return pCell;
  }

  public: virtual CellKind GetKind() const {
    return CellKind_Unicode;
  }

  public: virtual void Render(const gfx::Graphics& gfx,
                              const gfx::RectF& rect) const override {
    gfx::Brush fill_brush(gfx, ColorToColorF(m_crBackground));
    gfx.FillRectangle(fill_brush, rect);

    gfx::Brush text_brush(gfx, ColorToColorF(m_crColor));
    DrawText(gfx, *m_pFont, text_brush, rect, m_pwch, m_cwch);

    gfx.DrawRectangle(text_brush,
                      gfx::RectF(rect.left, rect.top,
                                 rect.right - 1, rect.bottom - 1));
  }
};

//////////////////////////////////////////////////////////////////////
//
// EnumCI
//  Enumerator for characters and interval
//
class EnumCI {
  private: Posn m_lBufEnd;
  private: Posn m_lBufStart;
  private: Posn m_lPosn;
  private: Edit::Buffer* m_pBuffer;
  private: Edit::Interval* m_pInterval;
  private: char16 m_rgwch[80];

  public: EnumCI(Edit::Buffer *pBuffer, Posn lPosn)
      : m_pBuffer(pBuffer),
        m_lPosn(lPosn) {
    m_pInterval = m_pBuffer->GetIntervalAt(m_lPosn);
    ASSERT(nullptr != m_pInterval);
    fill();
  }

  public: bool AtEnd() const {
    return m_lBufStart == m_lBufEnd;
  }

  private: void fill() {
    auto const cwch = m_pBuffer->GetText(m_rgwch, m_lPosn,
                                         m_lPosn + lengthof(m_rgwch));

    m_lBufStart = m_lPosn;
    m_lBufEnd   = m_lPosn + cwch;
  }

  public: char16 GetChar() const {
    if (AtEnd())
      return 0;
    ASSERT(m_lPosn >= m_lBufStart);
    ASSERT(m_lPosn < m_lBufEnd);
    return m_rgwch[m_lPosn - m_lBufStart];
  }

  public: Posn GetPosn() const { return m_lPosn; }

  public: const StyleValues* GetStyle() const {
    if (AtEnd())
      return m_pBuffer->GetDefaultStyle();
    ASSERT(nullptr != m_pInterval);
    return m_pInterval->GetStyle();
  }

  public: void Next() {
    if (AtEnd())
      return;
    m_lPosn += 1;
    if (m_lPosn >= m_lBufEnd)
      fill();

    if (m_lPosn >= m_pInterval->GetEnd()) {
      Edit::Interval* pNext = m_pInterval->GetNext();
      if (pNext)
        m_pInterval = pNext;
    }
  }
};

//////////////////////////////////////////////////////////////////////
//
// Formatter
//
class Formatter {
  private: const gfx::Graphics& m_gfx;
  private: HANDLE const m_hObjHeap;
  private: Page*  const m_pPage;
  private: LocalCharSink_<> m_oCharSink;
  private: EnumCI m_oEnumCI;

  public: Formatter(const gfx::Graphics& gfx,
                    HANDLE hHeap,
                    Page* pPage,
                    Posn lStart)
      : m_gfx(gfx),
        m_hObjHeap(hHeap),
        m_pPage(pPage),
        m_oCharSink(hHeap),
        m_oEnumCI(pPage->GetBuffer(), lStart) {
  }

  public: void Format();
  public: bool FormatLine(Page::Line*);

  private: Cell* formatChar(Cell*, float x, char16);
  private: Cell* formatMarker(MarkerCell::Kind);
  private: Cell* formatTab(int);

  DISALLOW_COPY_AND_ASSIGN(Formatter);
}; // Formatter

//////////////////////////////////////////////////////////////////////
//
// Formatter::Format
//
void Formatter::Format() {
  #if DEBUG_FORMAT
    DEBUG_PRINTF("%p: lStart=%d\n", m_pPage, m_pPage->GetStart());
  #endif

  auto const cyPage = static_cast<float>(
      m_pPage->m_rc.bottom - m_pPage->m_rc.top);
  for (;;) {
    Page::Line* pLine = m_pPage->m_oFormatBuf.NewLine();

    bool fMore = FormatLine(pLine);
    ASSERT(pLine->m_iHeight >= 1);

    m_pPage->m_oFormatBuf.Append(pLine);

    // Line must have at least one cell other than filler.
    ASSERT(pLine->GetEnd() >= pLine->GetStart());

    if (m_pPage->m_oFormatBuf.GetHeight() >= cyPage) {
      // Page is filled up with lines.
      break;
    }

    if (!fMore) {
      // We have no more contents. Add a filler line.
      break;
    }
  }
}

//////////////////////////////////////////////////////////////////////
//
// Formatter::FormatLine
//
// Description:
//  Returns true if more contents is avaialble, otherwise returns false.
//
bool Formatter::FormatLine(Page::Line* pLine) {
  auto fMoreContents = true;
  pLine->m_lStart = m_oEnumCI.GetPosn();
  m_oCharSink.Reset();
  auto ppPrevCell = &pLine->m_pCell;
  *ppPrevCell = nullptr;

  auto x = static_cast<float>(m_pPage->m_rc.left);
  auto iDescent = 0.0f;
  auto iAscent  = 0.0f;

  Cell* pCell;

  // Left margin
  {
    auto const cyMinHeight = 1.0f;

    pCell = new(m_hObjHeap) FillerCell(m_pPage->m_crBackground,
                                       cxLeftMargin, cyMinHeight);

    *ppPrevCell = pCell;
    ppPrevCell = &pCell->m_pNext;
    x += cxLeftMargin;
  }

  for (;;) {
    if (m_oEnumCI.AtEnd()) {
        pCell = formatMarker(MarkerCell::Kind_Eob);
        fMoreContents = false;
        break;
    }

    auto const wch = m_oEnumCI.GetChar();

    if (wch == 0x0A) {
      pCell = formatMarker(MarkerCell::Kind_Eol);
      m_oEnumCI.Next();
      break;
    }

    auto const cx = pCell->m_cx;

    pCell = formatChar(pCell, x, wch);
    if (!pCell) {
      pCell = formatMarker(MarkerCell::Kind_Wrap);
      break;
    }

    m_oEnumCI.Next();

    if (ppPrevCell == &pCell->m_pNext) {
      x -= cx;
    } else {
      *ppPrevCell = pCell;
      ppPrevCell = &pCell->m_pNext;
    }

    x += pCell->m_cx;
    iDescent = max(pCell->GetDescent(), iDescent);
    iAscent  = max(pCell->GetHeight() - pCell->GetDescent(),  iAscent);
  }

  // We have at least one cell.
  //   o end of buffer: End-Of-Buffer MarkerCell
  //   o end of line:   End-Of-Line MarkerCell
  //   o wrapped line:  Warp MarkerCEll
  ASSERT(pCell);
  ASSERT(!*ppPrevCell);
  ASSERT(ppPrevCell != &pCell->m_pNext);

  *ppPrevCell = pCell;
  ppPrevCell = &pCell->m_pNext;

  x += pCell->m_cx;
  iDescent = max(pCell->GetDescent(), iDescent);
  iAscent  = max(pCell->GetHeight() - pCell->GetDescent(),  iAscent);

  pLine->m_iHeight = iAscent + iDescent;

  pLine->m_cwch = m_oCharSink.GetLength();
  pLine->m_pwch = m_oCharSink.Fix();
  pLine->Fix(iDescent);

  return fMoreContents;
}

//////////////////////////////////////////////////////////////////////
//
// Formatter::formatChar
//
Cell* Formatter::formatChar(
    Cell* pPrev,
    float x,
    char16 wch) {
    Color crColor;
    Color crBackground;
    TextDecoration  eDecoration;

    Posn lPosn = m_oEnumCI.GetPosn();
    const StyleValues* pStyle = m_oEnumCI.GetStyle();

    if (lPosn >= m_pPage->m_lSelStart &&
        lPosn <  m_pPage->m_lSelEnd)
    {
        crColor      = m_pPage->m_crSelFg;
        crBackground = m_pPage->m_crSelBg;
        eDecoration  = TextDecoration_None;
    }
    else
    {
        crColor      = pStyle->GetColor();
        crBackground = pStyle->GetBackground();
        eDecoration  = pStyle->GetDecoration();
    } // if

    if (0x09 == wch) {
      Font* pFont = FontSet::Get(m_gfx, pStyle)->FindFont(m_gfx, 'x');
      auto const cxTab = pFont->GetTextWidth(L" ", 1) * k_nTabWidth;
      auto const x2 = (x + cxTab - cxLeftMargin) / cxTab * cxTab;
      auto const cx = (x2 + cxLeftMargin) - x;
      if (pPrev && x2 + pFont->GetCharWidth('M') > m_pPage->m_rc.right)
        return nullptr;

      return new(m_hObjHeap) MarkerCell(
          pStyle->GetMarker(),
          crBackground,
          cx,
          pFont->GetHeight(),
          pFont->GetDescent(),
          lPosn,
          MarkerCell::Kind_Tab);
    }

    Font* pFont = wch < 0x20 ? 
        nullptr : 
        FontSet::Get(m_gfx, pStyle)->FindFont(m_gfx, wch);

    if (!pFont) {
        Font* pFont = FontSet::Get(m_gfx, pStyle)->FindFont(m_gfx, 'u');
        char16 rgwch[5];
        int cwch;

        if (wch < 0x20) {
            rgwch[0] = '^';
            rgwch[1] = static_cast<char16>(wch + 0x40);
            cwch = 2;
        } else {
            rgwch[0] = 'u';
            rgwch[1] = toxdigit((wch >> 12) & 15);
            rgwch[2] = toxdigit((wch >>  8) & 15);
            rgwch[3] = toxdigit((wch >>  4) & 15);
            rgwch[4] = toxdigit((wch >>  0) & 15);
            cwch = 5;
        } // if

        auto const cxUni = pFont->GetTextWidth(rgwch, cwch) + 6.0f;
        auto const cxM = pFont->GetCharWidth('M');
        if (pPrev && x + cxUni + cxM > m_pPage->m_rc.right)
          return nullptr;

        UnicodeCell* pCell = new(m_hObjHeap) UnicodeCell(
            pStyle,
            pStyle->GetMarker(),
            crBackground,
            pFont,
            cxUni,
            lPosn,
            m_oCharSink.GetLength(),
            cwch);

        m_oCharSink.Add(rgwch, cwch);
        return pCell;
    } // if

    auto const cx = pFont->GetCharWidth(wch);

    if (nullptr == pPrev)
    {
        // We must draw a char at start of window line.
        TextCell* pCell = new(m_hObjHeap) TextCell(
            pStyle,
            crColor,
            crBackground,
            pFont,
            cx,
            lPosn,
            m_oCharSink.GetLength());

        m_oCharSink.Add(wch);
        return pCell;
    } // if

    if (x + cx + pFont->GetCharWidth('M') > m_pPage->m_rc.right) {
      // We doesn't have enough room for a char in the line.
      return nullptr;
    }

    if (pPrev->Merge(pFont, crColor, crBackground, eDecoration, cx)) {
      m_oCharSink.Add(wch);
      return pPrev;
    }

    {
        TextCell* pCell = new(m_hObjHeap) TextCell(
            pStyle,
            crColor,
            crBackground,
            pFont,
            cx,
            lPosn,
            m_oCharSink.GetLength());

        m_oCharSink.Add(wch);
        return pCell;
    }
}

Cell* Formatter::formatMarker(MarkerCell::Kind  eKind) {
    Color crColor;
    Color crBackground;

    Posn lPosn = m_oEnumCI.GetPosn();
    const StyleValues* pStyle = m_oEnumCI.GetStyle();

    if (lPosn >= m_pPage->m_lSelStart &&
        lPosn <  m_pPage->m_lSelEnd)
    {
        crColor      = m_pPage->m_crSelFg;
        crBackground = m_pPage->m_crSelBg;
    }
    else
    {
        crColor      = pStyle->GetMarker();
        crBackground = pStyle->GetBackground();
    }

    Font* pFont = FontSet::Get(m_gfx, pStyle)->FindFont(m_gfx, 'x');
    MarkerCell* pCell = new(m_hObjHeap) MarkerCell(
        crColor,
        crBackground,
        pFont->GetTextWidth(L"x", 1),
        pFont->GetHeight(),
        pFont->GetDescent(),
        m_oEnumCI.GetPosn(),
        eKind);
    return pCell;
} // Formatter::formatMarker

} // PageInternal

using namespace PageInternal;

//////////////////////////////////////////////////////////////////////
//
// Page::fillBottom - Fill page bottom
//
void Page::fillBottom(const gfx::Graphics& gfx, float y) const
{
    if (y < m_rc.bottom) {
        gfx::RectF rc(static_cast<float>(m_rc.left), 
                      y, 
                      static_cast<float>(m_rc.right), 
                      static_cast<float>(m_rc.bottom));
        fillRect(gfx, rc, ColorToColorF(m_crBackground));
    }

    // FIXME 2007-08-05 yosi@msn.com We should expose show/hide
    // ruler settings to both script and UI.

    // Ruler
    Font* pFont = FontSet::Get(gfx, m_pBuffer->GetDefaultStyle())->
        FindFont(gfx, 'x');

    // FIXME 2007-08-05 yosi@msn.com We should expose rule position to
    // user.
    auto const num_columns = 80;
    
    drawVLine(gfx, gfx::Brush(gfx, gfx::ColorF::LightGray),
              m_rc.left + pFont->GetCharWidth('M') * num_columns,
              m_rc.top,
              m_rc.bottom);
}

//////////////////////////////////////////////////////////////////////
//
// Page::fillRight
//
void Page::fillRight(const gfx::Graphics& gfx, const Line* pLine, 
                     float y) const {
  gfx::RectF rc;
  rc.left  = pLine->GetWidth();
  rc.right = static_cast<float>(m_rc.right);
  if (rc.left < rc.right) {
    rc.top = y;
    rc.bottom = y + pLine->GetHeight();
    fillRect(gfx, rc, ColorToColorF(m_crBackground));
  }
}

//////////////////////////////////////////////////////////////////////
//
// Page::FindLine
//
Page::Line* Page::FindLine(Posn lPosn) const
{
    if (lPosn < m_lStart) return nullptr;
    if (lPosn > m_lEnd)   return nullptr;

    foreach (EnumLine, oEnum, m_oFormatBuf)
    {
        Line* pLine = oEnum.Get();
        if (lPosn < pLine->m_lEnd) return pLine;
    } // for each line

    // We must not here.
    return nullptr;
} // Page::FindLine

//////////////////////////////////////////////////////////////////////
//
// Page::Format
//
void Page::Format(const gfx::Graphics& gfx, gfx::RectF rect, 
                  const Selection& selection, Posn lStart) {
  Prepare(selection);
  m_rc = rect;
  formatAux(gfx, lStart);
}

//////////////////////////////////////////////////////////////////////
//
// Page::formatAux
//
void Page::formatAux(const gfx::Graphics& gfx, Posn lStart)
{
    m_oFormatBuf.Reset();
    m_lStart = lStart;

    Formatter oFormatter(gfx, m_oFormatBuf.GetHeap(), this, lStart);
    oFormatter.Format();

    m_lEnd = GetLastLine()->GetEnd();
} // Page::formatAux

//////////////////////////////////////////////////////////////////////
//
// Page::FormatLine
//
Page::Line* Page::FormatLine(
    const gfx::Graphics&     gfx,
    const Selection&    selection,
    Posn                lStart) {
    Prepare(selection);
    m_oFormatBuf.Reset();

    HANDLE hHeap = m_oFormatBuf.GetHeap();
    Formatter oFormatter(gfx, hHeap, this, lStart);

    Line* pLine = m_oFormatBuf.NewLine();
    oFormatter.FormatLine(pLine);
    return pLine;
} // Page::FormatLine

//////////////////////////////////////////////////////////////////////
//
// Page::IsDirty
//
//  fSelection
//    True if caller wants to show selection.
//
bool Page::IsDirty(RECT rc, const Selection& selection, bool fSelection) const
{
    if (nullptr == m_pBuffer)
    {
        #if DEBUG_DIRTY
            DEBUG_PRINTF("%p: No buffer.\n", this);
        #endif // DEBUG_DIRTY
        return true;
    }

    if (m_rc.right - m_rc.left != rc.right - m_rc.left)
    {
        #if DEBUG_DIRTY
            DEBUG_PRINTF("%p: Width is changed.\n", this);
        #endif // DEBUG_DIRTY
        return true;
    }

    if (m_rc.bottom - m_rc.top != rc.bottom - rc.top)
    {
        #if DEBUG_DIRTY
            DEBUG_PRINTF("%p: Height is changed.\n", this);
        #endif // DEBUG_DIRTY
        return true;
    }

    // Buffer
    Edit::Buffer* pBuffer = selection.GetBuffer();
    if (m_pBuffer != pBuffer)
    {
        #if DEBUG_DIRTY
            DEBUG_PRINTF("%p: Buffer is changed.\n", this);
        #endif // DEBUG_DIRTY
        return true;
    }

    if (m_nModfTick != pBuffer->GetModfTick())
    {
        #if DEBUG_DIRTY
            DEBUG_PRINTF("%p: ModfTick is changed.\n", this);
        #endif // DEBUG_DIRTY
        return true;
    }

    Posn lSelStart = selection.GetStart();
    Posn lSelEnd   = selection.GetEnd();

    // Page shows caret instead of seleciton.
    if (m_lSelStart == m_lSelEnd)
    {
        if (lSelStart == lSelEnd) 
        {
            #if DEBUG_DIRTY
                DEBUG_PRINTF("%p: clean with caret.\n", this);
            #endif // DEBUG_DIRTY
            return false;
        }

        if (!fSelection)
        {
            if (lSelEnd < m_lStart || lSelStart > m_lEnd)
            {
                #if DEBUG_DIRTY
                    DEBUG_PRINTF("%p: clean with selection in outside.\n", this);
                #endif // DEBUG_DIRTY
                return false;
            }
        }

        #if DEBUG_DIRTY
            DEBUG_PRINTF("%p: Need to show selection.\n", this);
        #endif // DEBUG_DIRTY
        return true;
    } // if

    if (!fSelection)
    {
        // Page doesn't contain selection.
        if (m_lSelEnd < m_lStart || m_lSelStart > m_lEnd)
        {
            if (lSelStart == lSelEnd) 
            {
                #if DEBUG_DIRTY
                    DEBUG_PRINTF("%p: clean with selection.\n", this);
                #endif // DEBUG_DIRTY
                return false;
            }

            if (lSelEnd < m_lStart || lSelStart > m_lEnd)
            {
                return false;
            }
            #if DEBUG_DIRTY
                DEBUG_PRINTF("%p: Need to show selection.\n", this);
            #endif // DEBUG_DIRTY
            return true;
        }
    }

    // Page shows selection.
    if (m_lSelStart != lSelStart)
    {
        #if DEBUG_DIRTY
            DEBUG_PRINTF("%p: Selection start is changed.\n", this);
        #endif // DEBUG_DIRTY
        return true;
    }

    if (m_lSelEnd != lSelEnd)
    {
        #if DEBUG_DIRTY
            DEBUG_PRINTF("%p: Selection end is changed.\n", this);
        #endif // DEBUG_DIRTY
        return true;
    }

    if (m_crSelFg != selection.GetColor())
    {
        #if DEBUG_DIRTY
            DEBUG_PRINTF("%p: SelColor is changed.\n", this);
        #endif // DEBUG_DIRTY
        return true;
    }

    if (m_crSelBg  != selection.GetBackground())
    {
        #if DEBUG_DIRTY
            DEBUG_PRINTF("%p: SelBackground is changed.\n", this);
        #endif // DEBUG_DIRTY
        return true;
    }

    #if DEBUG_DIRTY
        DEBUG_PRINTF("%p is clean.\n", this);
    #endif // DEBUG_DIRTY

    return false;
} // Page::IsDirty

//////////////////////////////////////////////////////////////////////
//
// Page::isPosnVisible
//
bool Page::isPosnVisible(Posn lPosn) const {
  if (lPosn <  m_lStart) 
    return false;
  if (lPosn >= m_lEnd) 
    return false;

  auto y = static_cast<float>(m_rc.top);
  foreach (EnumLine, oEnum, m_oFormatBuf) {
    auto const pLine = oEnum.Get();
    if (lPosn >= pLine->GetStart() && lPosn <  pLine->GetEnd())
      return y + pLine->GetHeight() <= m_rc.bottom;
    y += pLine->GetHeight();
  }
  return false;
}

//////////////////////////////////////////////////////////////////////
//
// Page::MapPointToPosn
//
Posn Page::MapPointToPosn(const gfx::Graphics& gfx, gfx::PointF pt) const
{
    if (pt.y < m_rc.top)     return GetStart();
    if (pt.y >= m_rc.bottom) return GetEnd();

    float yLine = m_rc.left;
    foreach (EnumLine, oEnum, m_oFormatBuf)
    {
        const Line* pLine = oEnum.Get();
        float y = pt.y - yLine;
        yLine += pLine->GetHeight();

        if (y >= pLine->GetHeight()) continue;

        float xCell = m_rc.left;
        if (pt.x < xCell) return pLine->GetStart();

        Posn lPosn = pLine->GetEnd() - 1;
        foreach (EnumCell, oEnum, pLine)
        {
            const Cell* pCell = oEnum.Get();
            float x = pt.x - xCell;
            xCell += pCell->m_cx;
            Posn lMap = pCell->MapXToPosn(gfx, x);
            if (lMap >= 0) lPosn = lMap;
            if (x >= 0 && x < pCell->m_cx) break;
        } // for each cell
        return lPosn;
    } // for each line
    return GetEnd() - 1;
} // Page::MapPointToPosn

//////////////////////////////////////////////////////////////////////
//
// Page::MapPosnToPoint
//
// Description:
//  Maps specified buffer position to window point and returns true. If
//  specified buffer point isn't in window, this function returns false.
//
//  A Page object must be formatted with the latest buffer.
//
gfx::RectF Page::MapPosnToPoint(const gfx::Graphics& gfx, Posn lPosn) const {
  if (lPosn <  m_lStart || lPosn > m_lEnd)
    return gfx::RectF();

  auto y = m_rc.top;
  foreach (EnumLine, oEnum, m_oFormatBuf) {
    auto const pLine = oEnum.Get();
    if (lPosn >= pLine->m_lStart && lPosn <  pLine->m_lEnd) {
        auto x = m_rc.left;
        foreach (EnumCell, oEnum, pLine) {
          auto const pCell = oEnum.Get();
          float cx = pCell->MapPosnToX(gfx, lPosn);
          if (cx >= 0) {
            return gfx::RectF(gfx::PointF(x + cx, y),
                              gfx::SizeF(pCell->m_cx, pCell->m_cy));
          }
          x += pCell->m_cx;
        }
    }
    y += pLine->GetHeight();
  }
  return gfx::RectF();
}

//////////////////////////////////////////////////////////////////////
//
// Page::pageLines
//
// Description:
//  Returns number of lines to be displayed in this page when using
//  buffer's default style.
//
int Page::pageLines(const gfx::Graphics& gfx) const
{
    Font* pFont = FontSet::Get(gfx, m_pBuffer->GetDefaultStyle())->
        FindFont(gfx, 'x');

    return static_cast<int>(m_rc.height() / pFont->GetHeight());
} // Page::pageLines

//////////////////////////////////////////////////////////////////////
//
// Page::prepare
//
void Page::Prepare(const Selection& selection) {
  auto& buffer = *selection.GetBuffer();
  m_pBuffer = &buffer;
  m_nModfTick = buffer.GetModfTick();

  // Selection
  m_lSelStart = selection.GetStart();
  m_lSelEnd = selection.GetEnd();
  m_crSelFg = selection.GetColor();
  m_crSelBg = selection.GetBackground();

  #if DEBUG_FORMAT
    DEBUG_PRINTF("selection: %d...%d 0x%x/0x%x\n",
        m_lSelStart, m_lSelEnd,
        m_crSelFg, m_crSelBg);
  #endif // DEBUG_FORMAT

  // Page
  m_crBackground = buffer.GetDefaultStyle()->GetBackground();
}

//////////////////////////////////////////////////////////////////////
//
// Page::Render
//
void Page::Render(const gfx::Graphics& gfx, const gfx::RectF& rcClip) const {
    #if DEBUG_RENDER
    {
        DEBUG_PRINTF("%p"
            " range:(%d, %d) sel=(%d, %d)"
            " rc=(%d,%d)-(%d,%d) clip=(%d,%d)-(%d,%d)\r\n",
            this,
            m_lStart, m_lEnd,
            m_lSelStart, m_lSelEnd,
            m_rc.left, m_rc.top, m_rc.right, m_rc.bottom,
            rcClip.left, rcClip.top, rcClip.right, rcClip.bottom);
    }
    #endif // DEBUG_RENDER

    //::SetTextAlign(gfx, TA_BASELINE | TA_NOUPDATECP);

    auto y = m_rc.top;
    foreach (EnumLine, oEnum, m_oFormatBuf) {
        auto const pLine = oEnum.Get();

        ASSERT(y < m_rc.bottom);

        if (y < rcClip.bottom &&
            y + pLine->GetHeight() >= rcClip.top)
        {
            auto x = m_rc.left;
            foreach (EnumCell, oEnum, pLine)
            {
                auto const pCell = oEnum.Get();
                if (x < rcClip.right &&
                    x + pCell->m_cx >= rcClip.left)
                {
                    gfx::RectF rc(x, y, x + pCell->m_cx, y + pCell->m_cy);
                    pCell->Render(gfx, rc);
                }
                x += pCell->m_cx;
            } // for each cell

            // Fill right
            {
                gfx::RectF rc;
                rc.left  = pLine->GetWidth();
                rc.right = m_rc.right;

                rc.left  = max(rc.left,  rcClip.left);
                rc.right = min(rc.right, rcClip.right);

                if (rc.left < rc.right) {
                  rc.top = y;
                  rc.bottom = y + pLine->GetHeight();

                  rc.top    = max(rc.top,    rcClip.top);
                  rc.bottom = min(rc.bottom, rcClip.bottom);

                  if (rc.top < rc.bottom)
                    fillRect(gfx, rc, ColorToColorF(m_crBackground));
                }
            }
        }
        y += pLine->m_iHeight;
    } // for each line

    fillBottom(gfx, y);
} // Page::Render

//////////////////////////////////////////////////////////////////////
//
// Page::Render
//
// Note:
//  We need hwnd for ScrollWindowEx in TryScroll.
//
bool Page::Render(const gfx::Graphics& gfx, HWND hwnd) {
    ASSERT(hwnd);
    uint cRedraws = 0;

    auto yNew = m_rc.top;

    // Compute common Start -- pNewStart is end of common Start.
    Line* pCurStart = m_oScreenBuf.GetFirst();
    Line* pNewStart = m_oFormatBuf.GetFirst();
    {
        while (pNewStart && pCurStart) {
            if (!pNewStart->Equal(pCurStart)) 
                break;
            yNew += pNewStart->GetHeight();
            pNewStart = pNewStart->GetNext();
            pCurStart = pCurStart->GetNext();
        } // while

        if (!pNewStart && !pCurStart) {
            // m_oFormatBuf and m_oScreenBuf are same.
            #if DEBUG_RENDER
                DEBUG_PRINTF("nothing to do\n");
            #endif // DEBUG_RENDER
            return false;
        }
    }

    // Compute common End -- pNewEnd is start of common End.
    Line* pCurEnd = m_oScreenBuf.GetLast();
    Line* pNewEnd = m_oFormatBuf.GetLast();

    if (m_oFormatBuf.GetHeight() == m_oScreenBuf.GetHeight()) {
      while (pNewEnd->Equal(pCurEnd)) {
        pNewEnd = pNewEnd->GetPrev();
        pCurEnd = pCurEnd->GetPrev();
      }
    }

    if (pCurEnd)
        pCurEnd = pCurEnd->GetNext();
    pNewEnd = pNewEnd->GetNext();

    // We need to redraw pNewStart (inclusive) to pNewEnd (exclsuive).
    //::SetTextAlign(gfx, TA_BASELINE);

    Line* pScrollEnd;
    Line* pScrollStart = TryScroll(
        hwnd,
        pNewStart, pNewEnd, yNew,
        pCurStart, pCurEnd, yNew,
        &pScrollEnd);
    if (pScrollStart) {
        for (Line* pNewLine = pNewStart; pNewLine != pNewEnd;
             pNewLine = pNewLine->GetNext()) {
          ASSERT(pNewLine);

          bool fRedraw = true;
          if (pNewLine == pScrollStart) {
            fRedraw = false;
          } else if (pNewLine == pScrollEnd) {
            fRedraw = true;
          }

          if (fRedraw) {
            cRedraws += 1;
            pNewLine->Render(gfx, gfx::PointF(m_rc.left, yNew));
            fillRight(gfx, pNewLine, yNew);
          }

          yNew += pNewLine->GetHeight();
        }
    } else {
        auto yCur = m_rc.top;
        auto pCurLine = pCurStart;

        for (
            auto pNewLine = pNewStart;
            pNewLine != pNewEnd;
            pNewLine = pNewLine->GetNext())
        {
            bool fRedraw;

            while (nullptr != pCurLine)
            {
                if (yCur >= yNew) break;
                pCurLine = pCurLine->GetNext();
            } // while

            ASSERT(nullptr != pNewLine);
            
            if (!pCurLine) {
                fRedraw = true;
            } else {
                fRedraw = yNew != yCur || !pNewLine->Equal(pCurLine);
                yCur += pCurLine->GetHeight();
                pCurLine = pCurLine->GetNext();
            } // if

            if (fRedraw) {
                cRedraws += 1;
                pNewLine->Render(gfx, gfx::PointF(m_rc.left, yNew));
                fillRight(gfx, pNewLine, yNew);
            } // if

            yNew += pNewLine->GetHeight();
        } // for each line
    } // if

    while (pNewEnd) {
        yNew += pNewEnd->GetHeight();
        pNewEnd = pNewEnd->GetNext();
    } // while

    fillBottom(gfx, yNew);

    // Update m_oScreenBuf for next rendering.
    {
        auto const hHeap = m_oScreenBuf.Reset();
        foreach (EnumLine, oEnum, m_oFormatBuf) {
            auto const pLine = oEnum.Get();
            m_oScreenBuf.Append(pLine->Copy(hHeap));
        } // for each line
    }

    if (cRedraws >= 1) {
        #if DEBUG_RENDER
        {
            DEBUG_PRINTF("%p"
                " redraw=%d"
                " r[%d, %d] s[%d, %d]"
                " rc=(%d,%d)-(%d,%d)\r\n",
                this,
                cRedraws,
                m_lStart, m_lEnd,
                m_lSelStart, m_lSelEnd,
                m_rc.left, m_rc.top, m_rc.right, m_rc.bottom);
        }
        #endif // DEBUG_RENDER
    } // if

    return cRedraws > 0;
}

void Page::Reset() {
  m_oScreenBuf.Reset();
}

#if 0
Page::Line* Page::TryScroll(
    HWND    hwnd,
    Line*   pNewStart,
    Line*   pNewEnd,
    float   yNewStart,
    Line*   pCurStart,
    Line*   pCurEnd,
    float   yCurStart,
    Line**  out_pScrollEnd) {
  auto yCurScroll   = 0.0f;
  auto yNewScroll   = 0.0f;
  auto cyScroll     = k_cyMinScroll;
  Line* pScrollStart = nullptr;
  Line* pScrollEnd   = nullptr;

  auto yNew = yNewStart;
  for (auto pNewLine = pNewStart; pNewLine != pNewEnd;
    pNewLine = pNewLine->GetNext())  {
    auto yCur = yCurStart;
    for (auto pCurLine = pCurStart; pCurLine != pCurEnd;
         pCurLine = pCurLine->GetNext()) {
      if (yCur != yNew) {
          if (pNewLine->Equal(pCurLine)) {
            auto cy = pNewLine->GetHeight();
            auto pNewRunner = pNewLine->GetNext();
            auto pCurRunner = pCurLine->GetNext();
            while (pNewRunner && pCurRunner) {
              if (!pNewRunner->Equal(pCurRunner))
                break;
              cy += pNewRunner->GetHeight();
              pNewRunner = pNewRunner->GetNext();
              pCurRunner = pCurRunner->GetNext();
            }

            if (cyScroll < cy) {
              cyScroll     = cy;
              yNewScroll   = yNew;
              yCurScroll   = yCur;
              pScrollStart = pNewLine;
              pScrollEnd   = pNewRunner;
            }
         }
      }
      yCur += pCurLine->GetHeight();
    }
    yNew += pNewLine->GetHeight();
  }

  if (cyScroll <= k_cyMinScroll)
    return nullptr;

  // Note:
  //  SW_INVALIDATE is needed for when source area is not
  //  visible, e.g. out of screen, other windows hides
  //  this window.
  RECT rc = gfx::RectF(m_rc.left, yCurScroll, m_rc.right, rc.top + cyScroll);
  RECT rc_clip = m_rc;
  auto const iRgn = ::ScrollWindowEx(hwnd, 0, 
                                     static_cast<int>(yNewScroll - yCurScroll),
                                     &rc, // prcScroll
                                     &rc_clip, // prcClip
                                     nullptr, // prgnUpdate
                                     nullptr, // prcUpdate
                                     SW_INVALIDATE);

  #if DEBUG_RENDER
    DEBUG_PRINTF("scroll %d->%d+%d %d\n", yCurScroll, yNewScroll, cyScroll,
                 iRgn);
  #endif // DEBUG_RENDER

  if (iRgn == ERROR) {
    DEBUG_PRINTF("ScorllWindowEx failed!\n");
    return nullptr;
  }

  *out_pScrollEnd = pScrollEnd;
  return pScrollStart;
}
#else
Page::Line* Page::TryScroll(
    HWND    /* hwnd */,
    Line*   /* pNewStart */,
    Line*   /* pNewEnd */,
    float   /* yNewStart */,
    Line*   /* pCurStart */,
    Line*   /* pCurEnd */,
    float   /*yCurStart */,
    Line**  /*out_pScrollEnd*/) {
  return nullptr;
}
#endif

//////////////////////////////////////////////////////////////////////
//
// Page::ScrollDown
//
bool Page::ScrollDown(const gfx::Graphics& gfx) {
  if (!m_lStart) {
    // This page shows start of buffer.
    return false;
  }

  auto const pLine = m_oFormatBuf.GetHeight() < m_rc.height() ?
      m_oFormatBuf.NewLine() : m_oFormatBuf.ScrollDown();
  if (!pLine) {
    // This page shows only one line.
    return false;
  }

  auto const lGoal  = m_lStart - 1;
  auto const lStart = m_pBuffer->ComputeStartOf(Unit_Paragraph, lGoal);
  Formatter formatter(gfx, m_oFormatBuf.GetHeap(), this, lStart);

  do {
    formatter.FormatLine(pLine);
  } while (lGoal >= pLine->GetEnd());

  m_oFormatBuf.Prepend(pLine);

  while (m_oFormatBuf.GetHeight() > m_rc.height()) {
    auto const pLast = m_oFormatBuf.ScrollDown();
    if (!pLast)
      break;
    pLast->Discard();
  }

  m_lStart = GetFirstLine()->GetStart();
  m_lEnd   = GetLastLine()->GetEnd();
  return true;
}

bool Page::ScrollToPosn(const gfx::Graphics& gfx, Posn lPosn) {
  if (isPosnVisible(lPosn))
    return false;

  auto const cLines = pageLines(gfx);
  auto const cLines2 = max(cLines / 2, 1);

  if (lPosn > m_lStart) {
    for (auto k = 0; k < cLines2; k++) {
        if (!ScrollUp(gfx))
          return k;
        if (isPosnVisible(lPosn))
          return true;
    }
  } else {
    for (int k = 0; k < cLines2; k++) {
      if (!ScrollDown(gfx))
        return k;
      if (isPosnVisible(lPosn))
        return true;
    }
  } // if

  auto lStart = lPosn;
  for (int k = 0; k < cLines2; k++) {
    if (!lStart)
      break;
    lStart = m_pBuffer->ComputeStartOf(Unit_Paragraph, lStart - 1);
  }

  #if DEBUG_FORMAT
    DEBUG_PRINTF("%p\n", this);
  #endif // DEBUG_FORMAT

  formatAux(gfx, lStart);
  for (;;) {
    if (isPosnVisible(lPosn))
      break;
    if (!ScrollUp(gfx))
      break;
  }

  // If this page shows end of buffer, we shows lines as much as 
  // posibble to fit in page.
  if (GetEnd() >= m_pBuffer->GetEnd()) {
    while (isPosnVisible(lPosn)) {
      if (!ScrollDown(gfx))
        return true;
    }
    ScrollUp(gfx);
  }
  return true;
}

//////////////////////////////////////////////////////////////////////
//
// Page::ScrollUp
//
bool Page::ScrollUp(const gfx::Graphics& gfx)
{
    // Note: We should scroll up if page shows end of buffer. Since,
    // the last line may not be fully visible.

    // Recycle the first line.
    Line* pLine = m_oFormatBuf.ScrollUp();
    if (nullptr == pLine)
    {
        // This page shows only one line.
        return false;
    }

    pLine->Reset();

    Formatter oFormatter(
        gfx,
        m_oFormatBuf.GetHeap(),
        this,
        GetLastLine()->GetEnd());

    bool fMore = oFormatter.FormatLine(pLine);

    m_oFormatBuf.Append(pLine);

    auto const cyPage = m_rc.height();
    while (m_oFormatBuf.GetHeight() > cyPage) {
        auto const pFirst = m_oFormatBuf.ScrollUp();
        if (!pFirst) 
          break;
        pFirst->Discard();
    }

    m_lStart = GetFirstLine()->GetStart();
    m_lEnd   = GetLastLine()->GetEnd();

    return fMore;
}

Page::DisplayBuffer::DisplayBuffer()
    : m_cy(0),
      m_hObjHeap(nullptr),
      m_pFirst(nullptr),
      m_pLast(nullptr) {
}

Page::DisplayBuffer::~DisplayBuffer() {
  #if DEBUG_HEAP
    DEBUG_PRINTF("%p: heap=%p\n", this, m_hObjHeap);
  #endif // DEBUG_HEAP

  if (m_hObjHeap) 
    ::HeapDestroy(m_hObjHeap);
}

// Page::DisplayBuffer::Append
void Page::DisplayBuffer::Append(Line* pLine) {
  if (!m_pFirst)
    m_pFirst = pLine;
  if (m_pLast)
    m_pLast->m_pNext = pLine;
  pLine->m_pPrev = m_pLast;
  m_pLast = pLine;
  m_cy += pLine->GetHeight();
}

void* Page::DisplayBuffer::Alloc(size_t cb) {
  return ::HeapAlloc(m_hObjHeap, 0, cb);
}

Page::Line* Page::DisplayBuffer::NewLine() {
  return new(m_hObjHeap) Line(m_hObjHeap);
}

void Page::DisplayBuffer::Prepend(Line* pLine) {
  if (!m_pLast)
    m_pLast = pLine;
  if (m_pFirst)
    m_pFirst->m_pPrev = pLine;
  pLine->m_pNext = m_pFirst;
  m_pFirst = pLine;
  m_cy += pLine->GetHeight();
}

// Page::DisplayBuffer::Reset
HANDLE Page::DisplayBuffer::Reset() {
  #if DEBUG_HEAP
    DEBUG_PRINTF("%p: heap=%p\n", this, m_hObjHeap);
  #endif // DEBUG_HEAP

  if (nullptr != m_hObjHeap)
    ::HeapDestroy(m_hObjHeap);

  m_hObjHeap = ::HeapCreate(HEAP_NO_SERIALIZE, 0, 0);
  m_pFirst = nullptr;
  m_pLast  = nullptr;
  m_cy = 0;

  #if DEBUG_HEAP
    DEBUG_PRINTF("%p: new heap=%p\n", this, m_hObjHeap);
  #endif // DEBUG_HEAP

  return m_hObjHeap;
}

Page::Line* Page::DisplayBuffer::ScrollDown() {
  if (m_pLast == m_pFirst)
    return nullptr;

  Line* pLine = m_pLast;
  m_pLast = m_pLast->GetPrev();
  m_pLast->m_pNext = nullptr;
  m_cy -= pLine->GetHeight();
  return pLine;
}

Page::Line* Page::DisplayBuffer::ScrollUp() {
  if (m_pLast == m_pFirst)
    return nullptr;

  auto const pLine = m_pFirst;
  m_pFirst = m_pFirst->GetNext();
  m_pFirst->m_pPrev = nullptr;
  m_cy -= pLine->GetHeight();
  return pLine;
}

Page::Line* Page::Line::Copy(HANDLE hHeap) const {
  auto const pLine = new(hHeap) Line(*this);

  pLine->m_pwch = reinterpret_cast<char16*>(
      ::HeapAlloc(hHeap, 0, sizeof(char16) * m_cwch));

  myCopyMemory(pLine->m_pwch, m_pwch, sizeof(char16) * m_cwch);

  auto ppPrev = &pLine->m_pCell;
  auto const pwch = pLine->m_pwch;
  foreach (EnumCell, oEnum, this) {
    const auto* const pCell = oEnum.Get();
    auto const pCopy = pCell->Copy(hHeap, pwch);
    *ppPrev = pCopy;
    ppPrev = &pCopy->m_pNext;
  }
  return pLine;
}

void Page::Line::Discard() {
  if (m_pwch)
    ::HeapFree(m_hObjHeap, 0, m_pwch);
  ::HeapFree(m_hObjHeap, 0, this);
}

bool Page::Line::Equal(const Line* pLine2) const
{
    if (Hash() != pLine2->Hash()) return false;
    EnumCell oEnum2(pLine2);
    foreach (EnumCell, oEnum, this)
    {
        Cell* pCell = oEnum.Get();
        if (oEnum2.AtEnd()) return false;
        Cell* pCell2 = oEnum2.Get();
        oEnum2.Next();
        if (!pCell->Equal(pCell2)) return false;
    } // for each cell
    return oEnum2.AtEnd();
} // Page::Line::Equal

//////////////////////////////////////////////////////////////////////
//
// Page::Line::Fix
//
// o Assign real pointer to m_pwch.
// o Adjust line cell height.
//
void Page::Line::Fix(float iDescent) {
  auto const pwch = m_pwch;
  auto cx = 0.0f;
  foreach (EnumCell, oEnum, this) {
    auto const pCell = oEnum.Get();
    auto const lEnd = pCell->Fix(pwch, m_iHeight, iDescent);
    if (lEnd >= 0) 
      m_lEnd = lEnd;
    cx += pCell->GetWidth();
  }
  m_iWidth  = cx;
}

uint Page::Line::Hash() const {
  if (m_nHash)
    return m_nHash;
  foreach (EnumCell, oEnum, this) {
    auto const pCell = oEnum.Get();
    m_nHash <<= 5;
    m_nHash ^= pCell->Hash();
    m_nHash >>= 3;
  }
  return m_nHash;
}

Posn Page::Line::MapXToPosn(const gfx::Graphics& gfx, float xGoal) const {
  auto xCell = 0.0f;
  auto lPosn = GetEnd() - 1;
  foreach (EnumCell, oEnum, this) {
    auto const pCell = oEnum.Get();
    auto const x = xGoal - xCell;
    xCell += pCell->m_cx;
    auto const lMap = pCell->MapXToPosn(gfx, x);
    if (lMap >= 0) 
      lPosn = lMap;
    if (x >= 0 && x < pCell->m_cx)
      break;
  }
  return lPosn;
}

void Page::Line::Render(const gfx::Graphics& gfx,
                        const gfx::PointF& left_top) const {
  auto x = left_top.x;
  foreach (EnumCell, oEnum, this) {
    auto const pCell = oEnum.Get();
    gfx::RectF rect(gfx::PointF(x, left_top.y),
                    gfx::SizeF(pCell->m_cx, pCell->m_cy));
    pCell->Render(gfx, rect);
    x = rect.right;
  }
  gfx.Flush();
}

// Page::Line::Reset
void Page::Line::Reset() {
  m_iHeight = 0;
  m_iWidth  = 0;
  m_nHash   = 0;
  m_lStart  = -1;
  m_lEnd    = -1;
  m_pCell   = nullptr;
  m_pNext   = nullptr;
  m_pPrev   = nullptr;
  m_cwch    = 0;
  if (m_pwch)
    ::HeapFree(m_hObjHeap, 0, m_pwch);
  m_pwch = nullptr;
}
