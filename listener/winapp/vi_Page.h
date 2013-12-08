//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - Page object
// listener/winapp/vi_page.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_Page.h#1 $
//
#if !defined(INCLUDE_listener_winapp_visual_formatter_h)
#define INCLUDE_listener_winapp_visual_formatter_h

#include "./vi_defs.h"
#include "./vi_style.h"
#include "gfx_base.h"

class Buffer;
class Font;
class Selection;
class Style;


namespace PageInternal {
  class Cell;
  class DisplayBuffer;
  class Formatter;
}; // PageInternal

//////////////////////////////////////////////////////////////////////
//
// Page
//
class Page {
  friend class PageInternal::Formatter;

  private: typedef PageInternal::Cell  Cell;
  private: class DisplayBuffer;
  public: class Line;

  private: class DisplayBuffer {
      private: float   m_cy;
      private: HANDLE  m_hObjHeap;
      private: Line*   m_pFirst;
      private: Line*   m_pLast;

      public: DisplayBuffer();
      public: DisplayBuffer(const DisplayBuffer&);
      public: ~DisplayBuffer();

      public: void   Append(Line*);
      public: void*  Alloc(size_t);
      public: Line*  GetFirst()  const { return m_pFirst; }
      public: HANDLE GetHeap()   const { return m_hObjHeap; }
      public: float  GetHeight() const { return m_cy; }
      public: Line*  GetLast()   const { return m_pLast; }
      public: Line*  NewLine();
      public: void   Prepend(Line*);
      public: HANDLE Reset();
      public: Line*  ScrollDown();
      public: Line*  ScrollUp();
  };

  // Line
  public: class Line : public ObjectInHeap {
    friend class Page;
    friend class DisplayBuffer;
    friend class PageInternal::Formatter;

    private: ~Line() {}

    private: uint           m_cwch;
    private: mutable uint   m_nHash;
    private: HANDLE         m_hObjHeap;
    private: float          m_iHeight;
    private: float          m_iWidth;
    private: Posn           m_lStart;
    private: Posn           m_lEnd;
    private: Cell*          m_pCell;
    private: Line*          m_pNext;
    private: Line*          m_pPrev;
    private: char16*        m_pwch;

    public: Line(HANDLE hHeap) :
        m_iHeight(0),
        m_iWidth(0),
        m_hObjHeap(hHeap),
        m_lEnd(0),
        m_lStart(0),
        m_nHash(0),
        m_pCell(NULL),
        m_pNext(NULL),
        m_pPrev(NULL) {}

        public: void  Discard();
    public: Line* Copy(HANDLE hHeap) const;
    public: bool  Equal(const Line*) const;
    public: void  Fix(float dscent);
    public: Cell* GetCell()   const { return m_pCell; }
    public: Posn  GetEnd()    const { return m_lEnd; }
    public: float GetHeight() const { return m_iHeight; }
    public: Line* GetNext()   const { return m_pNext; }
    public: Line* GetPrev()   const { return m_pPrev; }
    public: Posn  GetStart()  const { return m_lStart; }
    public: float GetWidth()  const { return m_iWidth; }
    public: uint  Hash() const;
    public: Posn  MapXToPosn(const gfx::Graphics&, float) const;
    public: void  Render(const gfx::Graphics&, const gfx::PointF& left_top) const;
    public: void  Reset();
  }; // Line

    // EnumLine
  private: class EnumLine {
      Line*   m_pRunner;

        public: EnumLine(const DisplayBuffer& r) :
          m_pRunner(r.GetFirst()) {}

        public: bool AtEnd() const { return NULL == m_pRunner; }
      public: Line* Get() const { ASSERT(! AtEnd()); return m_pRunner; }

        public: void Next()
          { ASSERT(! AtEnd()); m_pRunner = m_pRunner->m_pNext; }
  }; // EnumLine

    // Buffer
  public: Edit::Buffer*   m_pBuffer;
  private: Count          m_nModfTick;


  // Selection
  public: Posn    m_lSelStart;;
  public: Posn    m_lSelEnd;
  public: Color   m_crSelFg;
  public: Color   m_crSelBg;

    // Page dimension
  public: gfx::RectF m_rc;
  public: Color   m_crBackground;

  private: Posn    m_lStart;
  private: Posn    m_lEnd;

  private: DisplayBuffer  m_oFormatBuf;
  private: DisplayBuffer  m_oScreenBuf;

    // Page ctor
  public: Page(RECT rc) :
      m_pBuffer(NULL),
      m_rc(rc),
      m_lStart(0),
      m_lEnd(0),
      m_nModfTick(0),
      m_lSelStart(0),
      m_lSelEnd(0),
      m_crSelFg(0),
      m_crSelBg(0),
      m_crBackground(0) {}

    // [F]
  public: Line* FindLine(Posn) const;
  public: void Format(const gfx::Graphics&, gfx::RectF, const Selection&,
                      Posn);
  public: Line* FormatLine(const gfx::Graphics&, const Selection&, Posn);

    // [G]
  public: Edit::Buffer* GetBuffer()    const { return m_pBuffer; }
  public: Line*   GetFirstLine() const { return m_oFormatBuf.GetFirst(); }
  public: Line*   GetLastLine()  const { return m_oFormatBuf.GetLast(); }
  public: Posn    GetStart()     const { return m_lStart; }
  public: Posn    GetEnd()       const { return m_lEnd; }

    // [I]
  public:  bool IsDirty(RECT, const Selection&, bool = false) const;

    // [M]
  public: void  MakePosnVisible(Posn);
  public: Posn  MapPointToPosn(const gfx::Graphics&, gfx::PointF) const;
  public: gfx::RectF MapPosnToPoint(const gfx::Graphics&, Posn) const;

    // [R]
  public: void Render(const gfx::Graphics&, const gfx::RectF&) const;
  public: bool Render(const gfx::Graphics&, HWND);

    // [S]
  public: bool ScrollDown(const gfx::Graphics&);
  public: bool ScrollToPosn(const gfx::Graphics&, Posn);
  public: bool ScrollUp(const gfx::Graphics&);

    ////////////////////////////////////////////////////////////
  //
  // Private methods
  //

    // [A]
  private: void allocHeap();

    // [F]
  private: void fillBottom(const gfx::Graphics&, float top) const;
  private: void fillRight(const gfx::Graphics&, const Line*, float) const;
  private: void formatAux(const gfx::Graphics&, Posn);

    // [I]
  private: bool isPosnVisible(Posn) const;

    // [P]
  private: int  pageLines(const gfx::Graphics&) const;
  private: void Prepare(const Selection&);

  // [R]
  public: void Reset();

    // [T]
  private: Line* TryScroll(const gfx::Graphics& gfx,
                           Line* new_start, Line* new_end, float new_top,
                           Line* cur_start, Line* cur_end, float cur_top,
                           Line** out_scroll_end);
};

#endif //!defined(INCLUDE_listener_winapp_visual_formatter_h)
