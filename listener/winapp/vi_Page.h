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

class Buffer;
class Font;
class Selection;
class Style;


namespace PageInternal
{
    class Cell;
    class DisplayBuffer;
    class Formatter;
}; // PageInternal


//////////////////////////////////////////////////////////////////////
//
// Page
//
class Page
{
    friend class PageInternal::Formatter;

    private: typedef PageInternal::Cell  Cell;
    public: class DisplayBuffer;
    public: class Line;

    // DisplayBuffer
    public: class DisplayBuffer
    {
        private: int     m_cy;
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
        public: int    GetHeight() const { return m_cy; }
        public: Line*  GetLast()   const { return m_pLast; }
        public: Line*  NewLine();
        public: void   Prepend(Line*);
        public: HANDLE Reset();
        public: Line*  ScrollDown();
        public: Line*  ScrollUp();
    }; // DisplayBuffer

    // Line
    public: class Line : public ObjectInHeap
    {
        friend class Page;
        friend class DisplayBuffer;
        friend class PageInternal::Formatter;

        private: ~Line() {}

        private: uint           m_cwch;
        private: mutable uint   m_nHash;
        private: HANDLE         m_hObjHeap;
        private: int            m_iHeight;
        private: int            m_iWidth;
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
        public: void  Fix(int);
        public: Cell* GetCell()   const { return m_pCell; }
        public: Posn  GetEnd()    const { return m_lEnd; }
        public: int   GetHeight() const { return m_iHeight; }
        public: Line* GetNext()   const { return m_pNext; }
        public: Line* GetPrev()   const { return m_pPrev; }
        public: Posn  GetStart()  const { return m_lStart; }
        public: int   GetWidth()  const { return m_iWidth; }
        public: uint  Hash() const;
        public: Posn  MapXToPosn(HDC, int) const;
        public: void  Render(HDC, int, int) const;
        public: void  Reset();
    }; // Line

    // EnumLine
    public: class EnumLine
    {
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
    public: Rect    m_rc;
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
    public: void  Format(HDC, RECT, const Selection*, Posn);
    public: Line* FormatLine(HDC, const Selection*, Posn);

    // [G]
    public: Edit::Buffer* GetBuffer()    const { return m_pBuffer; }
    public: Line*   GetFirstLine() const { return m_oFormatBuf.GetFirst(); }
    public: Line*   GetLastLine()  const { return m_oFormatBuf.GetLast(); }
    public: Posn    GetStart()     const { return m_lStart; }
    public: Posn    GetEnd()       const { return m_lEnd; }

    // [I]
    public:  bool IsDirty(RECT, const Selection*, bool = false) const;

    // [M]
    public: void  MakePosnVisible(Posn);
    public: Posn  MapPointToPosn(HDC, POINT) const;
    public: int   MapPosnToPoint(HDC, Posn, POINT* = NULL) const;

    // [R]
    public: void Render(HDC, RECT) const;
    public: bool Render(HDC, HWND);

    // [S]
    public: bool ScrollDown(HDC);
    public: bool ScrollToPosn(HDC, Posn);
    public: bool ScrollUp(HDC);

    ////////////////////////////////////////////////////////////
    //
    // Private methods
    //

    // [A]
    private: void allocHeap();

    // [F]
    private: void fillBottom(HDC, int) const;
    private: void fillRight(HDC, const Line*, int) const;
    private: void formatAux(HDC, Posn);

    // [I]
    private: bool isPosnVisible(Posn) const;

    // [P]
    private: int  pageLines(HDC) const;
    private: void prepare(const Selection*);

    // [R]
    private: Line* renderAux(
        HWND, Line*, Line*, int, Line*, Line*, int, Line** );
}; // Page

#endif //!defined(INCLUDE_listener_winapp_visual_formatter_h)
