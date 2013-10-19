//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - Edit Pane
// listener/winapp/vi_EditPane.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_EditPane.h#2 $
//
#if !defined(INCLUDE_listener_winapp_visual_EditPane_h)
#define INCLUDE_listener_winapp_visual_EditPane_h

#include "./vi_defs.h"
#include "./vi_Pane.h"

class Buffer;
class TextEditWindow;

// EditPane is a container of multiple TextEditWindow windows and layouts
// them vertically with draggable splitter.
class EditPane : public CommandWindow_<EditPane, Pane> {
  private: enum Limits {
    k_cxSplitter = 8,
    k_cxSplitterBig = 11,
    k_cxMinBox = 50,
    k_cySplitter = 8,
    k_cySplitterBig = 11,
    k_cyMinBox = k_cySplitter,
  };

  private: enum StatusBarPart {
    StatusBarPart_Message,
    StatusBarPart_Mode,
    StatusBarPart_CodePage,
    StatusBarPart_Newline,
    StatusBarPart_LineNumber,
    StatusBarPart_Column,
    StatusBarPart_Posn,
    StatusBarPart_Insert,
  };

  private: class Box;
  private: class LayoutBox;
  private: class LeafBox;

  private: struct HitTestResult;

  private: struct HitTestResult {
    enum Type {
      None,
      HScrollBar,
      HSplitter,
      HSplitterBig,
      VScrollBar,
      VSplitter,
      VSplitterBig,
      Window,
    };

    Box* box;
    Type type;

    HitTestResult();
    HitTestResult(Type type, const Box& box);
  };

  private: typedef TextEditWindow Window;
  private: typedef DoubleLinkedList_<Window> Windows;

  private: class Box
        : public DoubleLinkedNode_<Box>,
          public RefCounted_<Box> {
    private: bool is_removed_;
    private: LayoutBox* outer_;
    private: Rect rect_;
    protected: Box(LayoutBox*);
    public: virtual ~Box();
    public: bool is_removed() const { return is_removed_; }
    public: LayoutBox* outer() const { return outer_; }
    public: const Rect& rect() const { return rect_; }
    public: Rect& rect() { return rect_; }
    public: void set_outer(LayoutBox& outer) { outer_ = &outer; }
    public: virtual void CloseAllBut(Window*) = 0;
    public: virtual uint CountLeafBox() const = 0;
    public: virtual void Destroy() = 0;
    public: virtual void DrawSplitters(HDC) { }
    public: virtual LeafBox* GetActiveLeafBox() const = 0;
    public: virtual LeafBox* GetFirstLeafBox() const = 0;
    public: virtual LeafBox* GetLeafBox(HWND) const = 0;
    public: virtual HitTestResult HitTest(Point) const = 0;
    public: virtual bool IsLeafBox() const = 0;
    public: virtual bool OnIdle(uint) = 0;
    public: virtual void Realize(HWND, const Rect&);
    public: void Removed();
    public: virtual void SetRect(const Rect&);
    DISALLOW_COPY_AND_ASSIGN(Box);
  };

  private: class LayoutBox : public Box {
    protected: typedef DoubleLinkedList_<Box> BoxList;
    protected: BoxList boxes_;
    protected: HWND hwndParent_;
    protected: LayoutBox(LayoutBox*);
    public: virtual ~LayoutBox();
    public: void Add(Box& box);
    public: virtual void CloseAllBut(Window*) override final;
    public: virtual uint CountLeafBox() const override final;
    public: virtual void Destroy() override final;
    protected: virtual void DidRemoveBox(Box*, Box*, const Rect&) = 0;
    public: virtual void MoveSplitter(const Point&, Box&) = 0;
    public: virtual LeafBox* GetActiveLeafBox() const override final;
    public: virtual LeafBox* GetFirstLeafBox() const override final;
    public: virtual LeafBox* GetLeafBox(HWND) const override final;
    public: virtual bool IsLeafBox() const override final { return false; }
    public: bool IsSingle() const;
    public: virtual bool IsVerticalLayoutBox() const = 0;
    public: virtual bool OnIdle(uint) override final;
    public: virtual void Realize(HWND, const Rect&) override;
    public: void RemoveBox(Box&);
    public: void Replace(Box&, Box&);
    public: virtual LeafBox& Split(Box&, int) = 0;
    public: virtual void StopSplitter(const Point&, Box&) = 0;
    protected: void UpdateSplitters();
  };

  private: class LeafBox : public Box {
    private: HWND m_hwndVScrollBar;
    private: Window* m_pWindow;

    public: LeafBox(LayoutBox* outer, Window* pWindow)
        : Box(outer),
          m_hwndVScrollBar(nullptr),
          m_pWindow(pWindow) {}

    public: ~LeafBox();

    // [C]
    public: virtual void CloseAllBut(Window*) override final;
    public: virtual uint CountLeafBox() const  override final { return 1; }

    // [D]
    public: virtual void Destroy() override final;
    public: void DetachWindow();

    public: void EnsureInHorizontalLayoutBox();
    public: void EnsureInVerticalLayoutBox();

    // [G]
    public: virtual LeafBox* GetActiveLeafBox() const override final;
    public: virtual LeafBox* GetFirstLeafBox() const override final;
    public: virtual LeafBox* GetLeafBox(HWND) const override final;
    public: Window* GetWindow() const { return m_pWindow; }

    // [H]
    private: bool HasSibling() const { return GetNext() || GetPrev(); }
    public: virtual HitTestResult HitTest(Point) const override final;

    // [I]
    public: virtual bool IsLeafBox() const override final { return true; }

    // [O]
    public: virtual bool OnIdle(uint) override final;

    // [R]
    public: virtual void Realize(HWND, const Rect&) override final;

    // [S]
    public: virtual void SetRect(const Rect&) override final;
  };

  private: class HorizontalLayoutBox : public LayoutBox {
    public: HorizontalLayoutBox(LayoutBox*);
    public: virtual ~HorizontalLayoutBox();
    protected: virtual void DidRemoveBox(
        Box*, Box*, const Rect&) override final;
    public: virtual HitTestResult HitTest(Point) const override final;
    public: virtual void DrawSplitters(HDC) override final;
    public: virtual bool IsVerticalLayoutBox() const override final;
    public: virtual void MoveSplitter(const Point&, Box&) override final;
    public: virtual void Realize(HWND, const Rect&) override final;
    public: virtual void SetRect(const Rect&) override final;
    public: virtual LeafBox& Split(Box&, int) override final;
    public: virtual void StopSplitter(const Point&, Box&) override final;
  };

  private: class VerticalLayoutBox : public LayoutBox {
    public: VerticalLayoutBox(LayoutBox*);
    public: virtual ~VerticalLayoutBox();
    protected: virtual void DidRemoveBox(
        Box*, Box*, const Rect&) override final;
    public: virtual HitTestResult HitTest(Point) const override final;
    public: virtual void DrawSplitters(HDC) override final;
    public: virtual bool IsVerticalLayoutBox() const override final;
    public: virtual void MoveSplitter(const Point&, Box&) override final;
    public: virtual void Realize(HWND, const Rect&) override final;
    public: virtual void SetRect(const Rect&) override final;
    public: virtual LeafBox& Split(Box&, int) override final;
    public: virtual void StopSplitter(const Point&, Box&) override final;
  };

  private: class SplitterDrag {
    public: enum State {
      State_None,
      State_Drag,
      State_DragSingle,
    };

    private: Box* m_pBox;
    private: State m_eState;

    public: SplitterDrag();
    public: ~SplitterDrag();
    public: void End(const Point&);
    public: void Move(const Point&);
    public: void Start(HWND, State, Box&);
    public: void Stop();
  };

  private: enum State {
    State_NotRealized,
    State_Destroyed,
    State_Realized,
  };

  private: State m_eState;
  private: ScopedRefCount_<LayoutBox> root_box_;
  private: SplitterDrag m_oSplitterDrag;
  private: Windows m_oWindows;
  private: RECT m_rc;

  // ctro/dtor
  public: EditPane(Buffer*, Posn = 0);
  public: virtual ~EditPane();

  // [C]
  public: void CloseAllBut(Window*);

  // [G]
  private: LeafBox* GetActiveLeafBox() const;
  public: Window* GetActiveWindow() const;
  public: Buffer* GetBuffer() const;

  public: static const char16* GetClass_() { return L"EditPane"; }
  public: Window* GetFirstWindow() const { return m_oWindows.GetFirst(); }
  public: Window* GetLastWindow() const { return m_oWindows.GetLast(); }

  public: virtual int GetTitle(char16*, int) override final;

  // [H]
  public: virtual bool HasFocus() const override final;

  // [M]
  public: virtual Command::KeyBindEntry* MapKey(uint) override final;

  // [O]
  public:  virtual bool OnIdle(uint) override final;
  private: virtual LRESULT onMessage(UINT, WPARAM, LPARAM) override final;

  // [R]
  private: void Resize();

  // [S]
  private: void setupStatusBar();
  public: Window* SplitHorizontally();
  public: Window* SplitVertically();

  // [U]
  public: virtual void UpdateStatusBar() override final;
};

#endif //!defined(INCLUDE_listener_winapp_visual_EditPane_h)
