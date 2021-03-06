//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - visual - text pane
// listener/winapp/vi_text_pane.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_TextEditWindow.h#2 $
//
#if !defined(INCLUDE_listener_winapp_visual_text_pane_h)
#define INCLUDE_listener_winapp_visual_text_pane_h

#include "./li_util.h"
#include "./vi_CommandWindow.h"
#include "./vi_Page.h"

class Buffer;
class Page;
class Selection;

enum DragMode
{
    DragMode_None,
    DragMode_Selection,
}; // DragMode


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow
//
//  Member Variables:
//    m_pBlink
//     A range contains the restore position after blinking matched
//     parenthesis.
//    m_pViewRange
//     A range contains the start position of window.
//
class TextEditWindow :
    public CommandWindow_<TextEditWindow>,
    public DoubleLinkedNode_<TextEditWindow>,
    public DoubleLinkedNode_<TextEditWindow, Buffer>
{
    private: typedef DoubleLinkedNode_<TextEditWindow> WindowItem;

    public: enum WindowMessage
    {
        WN_QueryClose = WM_USER,
    }; // Message

    protected: typedef Edit::Range  Range;

    protected: struct AutoScroll
    {
        int     m_iDirection;
        uint    m_nStartTick;
        uint    m_nTimerId;

        AutoScroll() :
            m_iDirection(0),
            m_nStartTick(0),
            m_nTimerId(0) {}

        void Continue(HWND);
        void Start(HWND, int);
        void Stop(HWND);
    }; // AutoScroll

    protected: struct ScrollBar
    {
        HWND    m_hwnd;
        int     m_nBar;

        ScrollBar() :
            m_hwnd(NULL),
            m_nBar(SB_CTL) {}

        bool GetInfo(SCROLLINFO* pInfo)
        {
            if (NULL == m_hwnd) return false;
            return 0 != ::GetScrollInfo(m_hwnd, m_nBar, pInfo);
        } // GetInfo

        HWND GetHwnd() const { return m_hwnd; }

        void Set(HWND hwnd, int nBar)
        {
            m_hwnd = hwnd;
            m_nBar = nBar;
        } // Set

        void SetInfo(SCROLLINFO* pInfo, bool fRedraw)
        {
            if (NULL == m_hwnd) return;
            ::SetScrollInfo(m_hwnd, m_nBar, pInfo, fRedraw);
        } // SetInfo
    }; // ScrollBar

    private: static int sm_nActiveTick;

    protected: DragMode     m_eDragMode;
    protected: bool         m_fBlink;
    protected: bool         m_fHasFocus;
    protected: Posn         m_lCaretPosn;
    protected: uint         m_nActiveTick;
    protected: uint         m_nBlinkTimerId;
    protected: int          m_nCharTick;
    protected: AutoScroll   m_oAutoScroll;
    protected: ScrollBar    m_oHoriScrollBar;
    protected: ScrollBar    m_oVertScrollBar;
    protected: Range*       m_pBlink;
    protected: Page*        m_pPage;
    protected: Selection*   m_pSelection;
    protected: Range*       m_pViewRange;
    protected: void*        m_pvHost;
    protected: RECT         m_rc;

    // ctor/dtor
    public: TextEditWindow(void* pvHost, Buffer*, Posn = 0);
    public: ~TextEditWindow();

    // [A]
    public: void Activate() { ::SetFocus(m_hwnd); }

    // [B]
    public: void Blink(Posn, uint);

    // [C]
    public:    Count ComputeMotion(Unit, Count, POINT, Posn*);
    protected: Posn  computeGoalX(int, Posn);

    // [E]
    public:    Posn EndOfLine(Posn);
    protected: Posn endOfLineAux(HDC, Posn);

    // [F]
    protected: void format(HDC, Posn);

    // [G]
    public: uint       GetActiveTick() const { return m_nActiveTick; }
    public: Buffer*    GetBuffer() const;

    public: static   const char16* GetClass_()
        { return L"TextEditWindow"; }

    public: Count        GetColumn(Posn);
    public: Posn         GetEnd();

    public: template<class T> T* GetHost() const
        { return reinterpret_cast<T*>(m_pvHost); }
        
    public: TextEditWindow* GetNext() const
        { return static_cast<const WindowItem*>(this)->GetNext(); }

    public: TextEditWindow* GetPrev() const
        { return static_cast<const WindowItem*>(this)->GetPrev(); }

    public: HWND       GetScrollBarHwnd(int) const;
    public: Selection* GetSelection() const { return m_pSelection; }
    public: Posn       GetStart();
    public: size_t     GetUndoSize() const;

    // [H]
    public: bool HasFocus() const { return m_fHasFocus; }

    // [L]
    public:  int LargeScroll(int, int, bool = true);

    // [M]
    public: virtual Command::KeyBindEntry* MapKey(uint) override;
    public: void MakeSelectionVisible();
    public: Posn MapPointToPosn(POINT);
    public: int  MapPosnToPoint(Posn, POINT* = NULL);

    // [O]
    public:    virtual bool    OnIdle(uint);
    protected: virtual LRESULT onMessage(UINT, WPARAM, LPARAM);
    protected:   void          onVScroll(uint);

    // [R]
    protected: void redraw();
    protected: void redraw(bool);
    protected: void render(HDC);

    // [S]
    protected: void selectWord(Posn);
    public:    void SetScrollBar(HWND, int);
    public:    int  SmallScroll(int, int);
    public:    Posn StartOfLine(Posn);
    protected: Posn startOfLineAux(HDC, Posn);
    private:   void stopDrag();

    // [U]
    protected: void updateScreen(HDC);
    protected: void updateScrollBar();

    #if SUPPORT_IME
    private: bool m_fImeTarget;
    private: Posn m_lImeStart;
    private: Posn m_lImeEnd;
    private: void onImeComposition(LPARAM);
    public:  void Reconvert(Posn, Posn);
    private: uint setReconvert(RECONVERTSTRING*, Posn, Posn);
    private: BOOL showImeCaret(SIZE, POINT);
    #endif SUPPORT_IME
}; // TextEditWindow

typedef DoubleLinkedList_<TextEditWindow, Buffer> WindowList;

#endif //!defined(INCLUDE_listener_winapp_visual_text_pane_h)
