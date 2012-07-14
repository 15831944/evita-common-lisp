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

  private: struct HitTestResult {
    enum Type {
      None,
      VScrollBar,
      VSplitter,
      VSplitterBig,
      Window,
    };

    Box* box;
    Type type;

    HitTestResult(Type type, const Box* box = nullptr)
        : box(const_cast<Box*>(box)), type(type) {}
  };

  private: typedef TextEditWindow Window;
  private: typedef DoubleLinkedList_<Window> Windows;

  private: class Box : public DoubleLinkedNode_<Box> {
    private: Rect rect_;
    private: LayoutBox* const outer_;
    protected: Box(LayoutBox* outer) : outer_(outer) {}
    public: virtual ~Box() {}
    public: LayoutBox* outer() const { return outer_; }
    public: const Rect& rect() const { return rect_; }
    public: Rect& rect() { return rect_; }
    public: virtual uint CountLeafBox() const = 0;
    public: virtual void Destroy() = 0;
    public: virtual void DrawSplitters(HDC) { }
    public: virtual LeafBox* GetActiveLeafBox() const = 0;
    public: virtual LeafBox* GetLeafBox(HWND) const = 0;
    public: virtual HitTestResult HitTest(Point) const = 0;
    public: virtual bool OnIdle(uint) = 0;
    public: virtual void Realize(HWND, const Rect&);
    public: virtual void SetRect(const Rect&);
    DISALLOW_COPY_AND_ASSIGN(Box);
  };

  private: class LayoutBox : public Box {
    private: typedef DoubleLinkedList_<Box> BoxList;
    protected: BoxList boxes_;
    protected: HWND hwndParent_;
    protected: LayoutBox(LayoutBox*);
    public: virtual ~LayoutBox();
    public: void Add(Box& box) { boxes_.Append(&box); }
    public: virtual uint CountLeafBox() const override final;
    public: virtual void Destroy() override final;
    public: virtual void MoveSplitter(const Point&, Box&) = 0;
    public: virtual LeafBox* GetActiveLeafBox() const override final;
    public: virtual LeafBox* GetLeafBox(HWND) const override final;
    public: virtual bool OnIdle(uint) override final;
    public: virtual void Realize(HWND, const Rect&) override;
    public: virtual void Remove(LeafBox&) = 0;
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
    public: virtual uint CountLeafBox() const { return 1; }

    // [D]
    public: virtual void Destroy() override final;
    public: void DetachWindow() { m_pWindow = nullptr; }

    // [G]
    public: virtual LeafBox* GetActiveLeafBox() const {

      return const_cast<LeafBox*>(this);
    }
    public: virtual LeafBox* GetLeafBox(HWND) const override final;
    public: Window* GetWindow() const { return m_pWindow; }

    // [H]
    private: bool HasSibling() const { return GetNext() || GetPrev(); }
    public: virtual HitTestResult HitTest(Point) const override final;

    // [O]
    public: virtual bool OnIdle(uint) override final;

    // [R]
    public: virtual void Realize(HWND, const Rect&) override final;

    // [S]
    public: virtual void SetRect(const Rect&) override final;
  };

  private: class VirticalLayoutBox : public LayoutBox {
    public: VirticalLayoutBox(LayoutBox*);
    public: virtual HitTestResult HitTest(Point) const override final;
    public: virtual void DrawSplitters(HDC) override final;
    public: virtual void MoveSplitter(const Point&, Box&) override final;
    public: virtual void Realize(HWND, const Rect&) override final;
    public: virtual void Remove(LeafBox&) override final;
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

    public: Box* m_pBox;
    public: State m_eState;

    public: SplitterDrag()
        : m_eState(State_None),
          m_pBox(nullptr) {}

    public: void Start(HWND, State);
    public: void Stop();
  };

  private: enum State {
    State_NotRealized,
    State_Destroyed,
    State_Realized,
  };

  private: State m_eState;
  private: LayoutBox* root_box_;
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
  public: Window* SplitVertically();
  public: LeafBox* splitVertically(LeafBox*, int);

  // [U]
  public: virtual void UpdateStatusBar() override final;
};

#endif //!defined(INCLUDE_listener_winapp_visual_EditPane_h)
