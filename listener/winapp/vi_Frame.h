//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - Frame Window
// listener/winapp/vi_Frame.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_Frame.h#2 $
//
#if !defined(INCLUDE_visual_Frame_h)
#define INCLUDE_visual_Frame_h

#include "./vi_BaseWindow.h"

#include "./ctrl_StatusBar.h"
#include "./ctrl_TabBand.h"
#include "./ctrl_TitleBar.h"

namespace gfx {
class Graphics;
}

struct Rect;

/// <summary>
///   Sevirity of message.
/// </summary>
enum MessageLevel
{
    MessageLevel_Min,

    MessageLevel_Idle = MessageLevel_Min,
    MessageLevel_Information,
    MessageLevel_Warning,
    MessageLevel_Error,

    MessageLevel_Limit,
}; // MessageLevel

class Buffer;

/// <summary>
///   Represents a frame window aka toplevel window. This window communicates
///   with window manager.
/// </summary>
class Frame final : public BaseWindow, public DoubleLinkedNode_<Frame> {
  private: enum CtrlId {
    CtrlId_TabBand  = 1,
    CtrlId_StatusBar,
  };

  private: typedef ChildList_<Frame, Pane> Panes;

  private: base::OwnPtr<gfx::Graphics> gfx_;
  private: int m_cyTabBand;
  private: HWND m_hwndTabBand;
  private: Panes m_oPanes;
  private: StatusBar m_oStatusBar;
  private: TitleBar m_oTitleBar;
  private: Pane* m_pActivePane;
  private: RECT m_rc;   // client rect
  private: char16* m_rgpwszMessage[MessageLevel_Limit];
  private: mutable char16 m_wszToolTip[1024];

  public: Frame();
  private: virtual ~Frame();

  public: gfx::Graphics& gfx() const { return *gfx_; }
  public: const Panes& panes() const { return m_oPanes; }
  public: Panes& panes() { return m_oPanes; }

  // [A]
  public: bool  Activate();
  public: void AddPane(Pane*);
  private: void AddTab(Pane*);
  private: void AdoptPane(Pane*);

  // [C]
  private: bool canClose();

  // [D]
  public: void DidActivePane(Pane*);
  private: virtual void DidKillFocus() override;
  private: virtual void DidSetFocus() override;

  // [G]
  public: Pane* GetActivePane();
  public: int GetCxStatusBar() const;

  public: Pane* GetFirstPane() const { return m_oPanes.GetFirst(); }
  public: Pane* GetLastPane() const { return m_oPanes.GetLast(); }

  private: Pane* getPaneFromTab(int) const;
  public: Rect GetPaneRect() const;
  private: int getTabFromPane(Pane*) const;
  private: const char16* getToolTip(NMTTDISPINFO*) const;

  // [H]
  private: bool hasFocus() const;

  public: bool HasMultiplePanes() const {
    return GetFirstPane() != GetLastPane();
  }

  // [O]
  private: void onDropFiles(HDROP);
  public: virtual bool OnIdle(uint) override;
  private: virtual LRESULT onMessage(uint, WPARAM, LPARAM) override;
  private: bool onTabDrag(TabBandDragAndDrop, HWND);

  // [P]
  private: void Paint();

  // [R]
  public: void Realize();
  public: void ResetMessages();

  // [S]
  public: void SetStatusBar(int, const char16*);
  public: void SetStatusBarf(int, const char16*, ...);
  public: void SetStatusBarParts(const int*, int);
  public: bool ShowBuffer(Buffer*);
  public: void ShowMessage(MessageLevel, uint = 0, ...);

  // [U]
  public: void updateTitleBar();

  // [W]
  public: void WillDestroyPane(Pane*);

  DISALLOW_COPY_AND_ASSIGN(Frame);
};

#endif //!defined(INCLUDE_visual_Frame_h)
