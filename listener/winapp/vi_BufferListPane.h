//////////////////////////////////////////////////////////////////////////////
//
// Editor - Buffer List Pane
// listener/winapp/vi_Buffer.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_BufferListPane.h#1 $
//
#if !defined(INCLUDE_visual_BufferListPane_h)
#define INCLUDE_visual_BufferListPane_h

#include "./vi_Pane.h"

#include "./cm_CmdProc.h"

class Buffer;

class NativePane : public CommandWindow_<NativePane, Pane> {
  // [G]
  public: static const char16* GetClass_() { return L"NativePane"; }

  // [H]
  public: virtual void Hide() override;

  // [O]
  protected: virtual LRESULT onMessage(UINT uMsg, WPARAM wParam,
                                       LPARAM lParam) override;

  // [R]
  public: virtual void Realize() override;
  public: virtual void Resize(const RECT&) override;

  // [S]
  public: virtual void SetFocus() override;
  public: virtual void Show() override;
};

class BufferListPane : public CommandWindow_<BufferListPane, NativePane> {
  private: enum Constant {
      ListViewId = 1234,
  };

  private: static HCURSOR sm_hDragCursor;
  private: static Command::KeyBinds* sm_pKeyBinds;

  private: Buffer* m_pDragItem;
  private: HWND m_hwndListView;

  // ctor/dtor
  public: BufferListPane();

  // [A]
  private: void ActivateBuffers(bool);

  // [C]
  private: static int CALLBACK compareItems(LPARAM, LPARAM, LPARAM);

  // [D]
  private: void dragFinish(POINT);
  private: void dragMove(POINT);
  private: void dragStart(int);
  private: void dragStop();

  // [G]
  public: static const char16* GetClass_() { return L"BufferListPane"; }

  public: HWND GetListWindow() const { return m_hwndListView; }

  public: virtual int GetTitle(char16*, int) override;

  // [L]
  public: void Refresh();

  // [M]
  public: virtual Command::KeyBindEntry* MapKey(uint) override;

  // [O]
  private: void onCreate(CREATESTRUCT*);
  private: void onKeyDown(uint);
  private: virtual LRESULT onMessage(uint, WPARAM, LPARAM) override;
};

#endif //!defined(INCLUDE_visual_BufferListPane_h)
