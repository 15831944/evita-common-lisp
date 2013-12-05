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
class Frame :
    public BaseWindow,
    public DoubleLinkedNode_<Frame>
{
    protected: enum CtrlId
    {
        CtrlId_TabBand  = 1,
        CtrlId_StatusBar,
    }; // CtrlId

    protected: typedef ChildList_<Frame, Pane> Panes;

    private: base::OwnPtr<gfx::Graphics> gfx_;
    protected: int              m_cyTabBand;
    protected: HWND             m_hwndTabBand;
    protected: Panes            m_oPanes;
    protected: StatusBar        m_oStatusBar;
    protected: TitleBar         m_oTitleBar;
    protected: Pane*            m_pActivePane;
    protected: RECT             m_rc;   // client rect
    protected: char16*          m_rgpwszMessage[MessageLevel_Limit];
    protected: mutable char16   m_wszToolTip[1024];

    public:     Frame();
    protected: ~Frame();

    public: const Panes& panes() const { return m_oPanes; }
    public: Panes& panes() { return m_oPanes; }

    // [A]
    public: bool  Activate();
    public: Pane* AddPane(Pane*);
    private: void addTab(Pane*);

    // [C]
    private: bool canClose();

    // [D]
    private: void detachPane(Pane*);

    // [G]
    public:  Pane* GetActivePane();
    public:  int   GetCxStatusBar() const;

    public:  Pane* GetFirstPane() const
        { return m_oPanes.GetFirst(); }

    public:  Pane* GetLastPane() const
        { return m_oPanes.GetLast(); }

    private: Pane*         getPaneFromTab(int) const;
    public:  void          GetPaneRect(RECT* out_rc);
    private: int           getTabFromPane(Pane*) const;
    private: const char16* getToolTip(NMTTDISPINFO*) const;

    // [H]
    private: bool hasFocus() const;

    public: bool HasMultiplePanes() const
        { return GetFirstPane() != GetLastPane(); }

    // [O]
    private:         void    onDropFiles(HDROP);
    public:  virtual bool    OnIdle(uint);
    private: virtual LRESULT onMessage(uint, WPARAM, LPARAM);
    private: virtual bool    onTabDrag(TabBandDragAndDrop, HWND);

    // [P]
    private: void Paint();

    // [R]
    public: virtual void Realize();
    public: void ResetMessages();
    private: void xRemovePane(Pane*);

    // [S]
    public: void SetActivePane(Pane*);
    public: void SetStatusBar(int, const char16*);
    public: void SetStatusBarf(int, const char16*, ...);
    public: void SetStatusBarParts(const int*, int);
    public: bool ShowBuffer(Buffer*);
    public: void ShowMessage(MessageLevel, uint = 0, ...);

    // [U]
    public: void updateTitleBar();

    DISALLOW_COPY_AND_ASSIGN(Frame);
}; // Frame

#endif //!defined(INCLUDE_visual_Frame_h)
