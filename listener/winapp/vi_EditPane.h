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
class Frame;
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

  private: class Box;
  private: class LayoutBox;
  private: class LeafBox;
  private: class HorizontalLayoutBox;
  private: class VerticalLayoutBox;

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
  private: const gfx::Graphics* gfx_;

  // ctro/dtor
  public: EditPane(Buffer*, Posn = 0);
  public: virtual ~EditPane();

  public: const gfx::Graphics& gfx() const { return *gfx_; }

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
  public: void Realize(Frame& frame, const gfx::Graphics& gfx) override final;
  private: void Resize();

  // [S]
  private: void setupStatusBar();
  public: Window* SplitHorizontally();
  public: Window* SplitVertically();

  // [U]
  public: virtual void UpdateStatusBar() override final;
};

#endif //!defined(INCLUDE_listener_winapp_visual_EditPane_h)
