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

#include "./vi_Pane.h"

class Buffer;
class TextEditWindow;

// EditPane is a container of multiple TextEditWindow windows and layouts
// them vertically with draggable splitter.
class EditPane : public CommandWindow_<EditPane, Pane> {
  private: enum Element {
    Element_None,
    Element_Splitter,
    Element_SplitterBig,
    Element_VScrollBar,
    Element_Window,
  };

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

  private: typedef TextEditWindow Window;
  private: typedef DoubleLinkedList_<Window> Windows;

  // Used for layout boxes
  public: class LeafBox : public DoubleLinkedNode_<LeafBox> {
    private: HWND m_hwndVScrollBar;
    private: Window* m_pWindow;
    private: RECT m_rc;

    public: LeafBox(Window* pWindow)
        : m_hwndVScrollBar(NULL),
          m_pWindow(pWindow) {}

    public: ~LeafBox();

    // [D]
    public: void DetachWindow() { m_pWindow = NULL; }

    // [G]
    public: RECT* GetRect() { return &m_rc; }
    public: Window* GetWindow() const { return m_pWindow; }

    // [R]
    public: void Realize(HWND);
  };

  private: typedef DoubleLinkedList_<LeafBox> Boxes;

  private: class SplitterDrag {
    public: enum State {
      State_None,
      State_Drag,
      State_DragSingle,
    };

    public: LeafBox* m_pBox;
    public: State m_eState;

    public: SplitterDrag()
        : m_eState(State_None),
          m_pBox(NULL) {}

    public: void Start(HWND, State);
    public: void Stop();
  };

  private: enum State {
    State_NotRealized,
    State_Destroyed,
    State_Realized,
  };

  private: State m_eState;
  private: Boxes m_oBoxes;
  private: SplitterDrag m_oSplitterDrag;
  private: Windows m_oWindows;
  private: RECT m_rc;

  // ctro/dtor
  public: EditPane(Buffer*, Posn = 0);
  public: ~EditPane();

  // [C]
  public: void CloseAllBut(Window*);

  // [D]
  private: void drawSplitters(HDC);
  private: void drawSplitters();

  // [E]
  private: class EnumBox : public Boxes::Enum {
    public: EnumBox(const EditPane* pPane) : Boxes::Enum(&pPane->m_oBoxes) {}
  };

  // [G]
  private: LeafBox* GetActiveLeafBox() const;
  public: Window* GetActiveWindow() const;
  public: Buffer* GetBuffer() const;

  public: static const char16* GetClass_() { return L"EditPane"; }
  public: Window* GetFirstWindow() const { return m_oWindows.GetFirst(); }
  public: Window* GetLastWindow() const { return m_oWindows.GetLast(); }

  public: virtual int GetTitle(char16*, int) override;

  // [H]
  public: virtual bool HasFocus() const override;

  private: bool HasMultipleWindows() const {
    return m_oWindows.GetFirst() != m_oWindows.GetLast();
  }

  private: Element hitTest(POINT, LeafBox**) const;

  // [M]
  public: virtual Command::KeyBindEntry* MapKey(uint) override;

  // [O]
  public:  virtual bool OnIdle(uint) override;
  private: virtual LRESULT onMessage(UINT, WPARAM, LPARAM) override;

  // [S]
  private: void setupStatusBar();
  private: void setBoxPos(LeafBox*) const;
  public: Window* SplitVertically();
  public: LeafBox* splitVertically(LeafBox*, int);

  // [U]
  public: virtual void UpdateStatusBar() override;
};

#endif //!defined(INCLUDE_listener_winapp_visual_EditPane_h)
