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
#include "./gfx_base.h"
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
// Member Variables:
// m_pBlink
// A range contains the restore position after blinking matched
// parenthesis.
// m_pViewRange
// A range contains the start position of window.
//
class TextEditWindow
    : public CommandWindow_<TextEditWindow>,
      public DoubleLinkedNode_<TextEditWindow>,
      public DoubleLinkedNode_<TextEditWindow, Buffer> {
  private: typedef DoubleLinkedNode_<TextEditWindow> WindowItem;

  protected: typedef Edit::Range Range;

  protected: struct AutoScroll {
    int m_iDirection;
    uint m_nStartTick;
    uint m_nTimerId;

    AutoScroll()
        : m_iDirection(0),
          m_nStartTick(0),
          m_nTimerId(0) {
    }

    void Continue(HWND);
    void Start(HWND, int);
    void Stop(HWND);
  };

  protected: struct ScrollBar {
    HWND m_hwnd;
    int m_nBar;

    ScrollBar()
        : m_hwnd(nullptr),
          m_nBar(SB_CTL) {
    }

    bool GetInfo(SCROLLINFO* pInfo) {
      return m_hwnd && ::GetScrollInfo(m_hwnd, m_nBar, pInfo);
    }

    HWND GetHwnd() const { return m_hwnd; }

    void Set(HWND hwnd, int nBar) {
      m_hwnd = hwnd;
      m_nBar = nBar;
    }

    void SetInfo(SCROLLINFO* pInfo, bool fRedraw) {
      if (!m_hwnd)
        return;
      ::SetScrollInfo(m_hwnd, m_nBar, pInfo, fRedraw);
    }
  };

  protected: DragMode m_eDragMode;
  protected: bool m_fBlink;
  protected: bool m_fHasFocus;
  private: const gfx::Graphics* m_gfx;
  protected: Posn m_lCaretPosn;
  protected: uint m_nActiveTick;
  protected: uint m_nBlinkTimerId;
  protected: int m_nCharTick;
  protected: AutoScroll m_oAutoScroll;
  protected: ScrollBar m_oHoriScrollBar;
  protected: ScrollBar m_oVertScrollBar;
  protected: Range* m_pBlink;
  protected: Page* m_pPage;
  // TODO(yosi): Manage life time of selection.
  protected: Selection* selection_;
  protected: Range* m_pViewRange;
  #if SUPPORT_IME
  private: bool m_fImeTarget;
  private: Posn m_lImeStart;
  private: Posn m_lImeEnd;
  #endif // SUPPORT_IME
  protected: void* m_pvHost;
  protected: RECT m_rc;
  private: int show_count_;

  // ctor/dtor
  public: TextEditWindow(void* pvHost, Buffer*, Posn = 0);
  public: ~TextEditWindow();

  // [A]
  public: void Activate();

  // [B]
  public: void Blink(Posn, uint);

  // [C]
  public: Count ComputeMotion(Unit, Count, const gfx::PointF&, Posn*);
  protected: Posn computeGoalX(float, Posn);

  // [E]
  public: Posn EndOfLine(Posn);
  protected: Posn endOfLineAux(const gfx::Graphics&, Posn);

  // [F]
  protected: void format(const gfx::Graphics&, Posn);
  public: MessageResult ForwardMessage(uint message, WPARAM wParam, 
                                       LPARAM lParam);

  // [G]
  public: uint GetActiveTick() const { return m_nActiveTick; }
  public: Buffer* GetBuffer() const;
  public: HCURSOR GetCursorAt(const Point&) const;

  public: static const char16* GetClass_() { return L"TextEditWindow"; }

  public: Count GetColumn(Posn);
  public: Posn GetEnd();

  public: template<class T> T* GetHost() const {
    return reinterpret_cast<T*>(m_pvHost);
  }

  public: TextEditWindow* GetNext() const {
    return static_cast<const WindowItem*>(this)->GetNext();
  }

  public: TextEditWindow* GetPrev() const {
    return static_cast<const WindowItem*>(this)->GetPrev();
  }

  public: HWND GetScrollBarHwnd(int) const;
  public: Selection* GetSelection() const { return &*selection_; }
  public: Posn GetStart();
  public: size_t GetUndoSize() const;

  // [H]
  public: bool HasFocus() const { return m_fHasFocus; }
  public: void Hide();

  // [L]
  public: int LargeScroll(int, int, bool = true);

  // [M]
  public: virtual Command::KeyBindEntry* MapKey(uint) override;
  public: void MakeSelectionVisible();
  public: Posn MapPointToPosn(const gfx::PointF point);
  public: gfx::RectF MapPosnToPoint(Posn);

  // [O]
  public: virtual bool OnIdle(uint);
  public: void OnLeftButtonDown(uint flags, const Point&);
  public: void OnLeftButtonUp(uint flags, const Point&);
  protected: virtual LRESULT onMessage(UINT, WPARAM, LPARAM);
  public: void OnMouseMove(uint flags, const Point&);
  protected: void onVScroll(uint);

  // [R]
  public: void Realize(HWND hwnd, const gfx::Graphics& gfx, const Rect& rect);
  public: void Redraw();
  protected: void redraw(bool);
  protected: void render(const gfx::Graphics&);
  private: void Render();
  public: void Resize(const Rect& rect);

  // [S]
  protected: void selectWord(Posn);
  public: void SetScrollBar(HWND, int);
  public: void Show();
  public: int SmallScroll(int, int);
  public: Posn StartOfLine(Posn);
  protected: Posn startOfLineAux(const gfx::Graphics&, Posn);
  private: void stopDrag();

  // [U]
  protected: void updateScreen();
  protected: void updateScrollBar();

  #if SUPPORT_IME
  private: void onImeComposition(LPARAM);
  public: void Reconvert(Posn, Posn);
  private: uint setReconvert(RECONVERTSTRING*, Posn, Posn);
  private: BOOL showImeCaret(SIZE, POINT);
  #endif // SUPPORT_IME
};

typedef DoubleLinkedList_<TextEditWindow, Buffer> WindowList;

#endif //!defined(INCLUDE_listener_winapp_visual_text_pane_h)
