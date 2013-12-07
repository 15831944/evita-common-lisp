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
class EditPane final : public CommandWindow_<EditPane, Pane> {
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

  private: typedef TextEditWindow Window;
  private: typedef DoubleLinkedList_<Window> Windows;

  private: class Box;
  private: class LayoutBox;
  private: class LeafBox;
  private: class HorizontalLayoutBox;
  private: class VerticalLayoutBox;
  private: struct HitTestResult;
  private: class SplitterController;

  private: enum State {
    State_NotRealized,
    State_Destroyed,
    State_Realized,
  };

  private: State m_eState;
  private: Frame* frame_;
  private: ScopedRefCount_<LayoutBox> root_box_;
  private: const base::OwnPtr<SplitterController> splitter_controller_;
  private: Windows m_oWindows;
  private: RECT m_rc;
  private: bool showed_;

  // ctro/dtor
  public: EditPane(Frame*, Buffer*, Posn = 0);
  public: virtual ~EditPane();

  public: Frame& frame() const { return *frame_; }
  public: bool is_showed() const { return showed_; }

  // [A]
  public: virtual void Activate() override;

  // [C]
  public: void CloseAllBut(Window*);

  // [D]
  public: virtual void Destroy() override;
  public: virtual bool DidCreateHwnd(HWND hwnd) override;
  public: virtual bool DidDestroyHwnd(HWND hwnd) override;

  // [G]
  private: LeafBox* GetActiveLeafBox() const;
  public: virtual HCURSOR GetCursorAt(const Point&) const override;
  public: Window* GetActiveWindow() const;
  public: Buffer* GetBuffer() const;

  public: static const char16* GetClass_() { return L"EditPane"; }
  public: Window* GetFirstWindow() const { return m_oWindows.GetFirst(); }
  public: Window* GetLastWindow() const { return m_oWindows.GetLast(); }

  public: virtual int GetTitle(char16*, int) override;

  // [H]
  public: virtual bool HasFocus() const override;
  public: virtual void Hide() override;

  // [M]
  public: virtual Command::KeyBindEntry* MapKey(uint) override;

  // [O]
  public: virtual void OnDeprecatedVScroll(uint code, HWND hwnd) override;
  public: virtual bool OnIdle(uint) override;
  public: virtual void OnLeftButtonDown(uint flags, const Point&) override;
  public: virtual void OnLeftButtonUp(uint flags, const Point&) override;
  public: virtual void OnMouseMove(uint flags, const Point&) override;

  // [R]
  public: virtual void Realize() override;
  public: virtual void Resize(const RECT& rc) override;

  // [S]
  public: virtual void SetFocus() override;
  private: void setupStatusBar();
  public: virtual void Show() override;
  public: Window* SplitHorizontally();
  public: Window* SplitVertically();

  // [U]
  public: virtual void UpdateStatusBar() override;
};

#endif //!defined(INCLUDE_listener_winapp_visual_EditPane_h)
