#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - Buffer List Pane
// listener/winapp/vi_BufferListPane.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_BufferListPane.cpp#2 $
//
#include "./vi_BufferListPane.h"

#include "./cm_CmdProc.h"
#include "./ed_Mode.h"

#include "./vi_defs.h"
#include "./vi_Application.h"
#include "./vi_Buffer.h"
#include "./vi_EditPane.h"
#include "./vi_Frame.h"

namespace Command
{
    uint TranslateKey(uint);

    DEFCOMMAND(KillBuffers);
} // Command

class EnumItem
{
    private: HWND   m_hwnd;
    private: int    m_iItem;
    private: uint   m_rgf;

    public: EnumItem(HWND hwnd, uint rgf = LVNI_SELECTED) :
        m_hwnd(hwnd),
        m_iItem(-1),
        m_rgf(rgf)
    {
        next();
    } // EnumItem

    public: bool AtEnd() const { return m_iItem < 0; }
    public: int Get() const { ASSERT(!AtEnd()); return m_iItem; }
    public: void Next() { ASSERT(!AtEnd()); next(); }

    private: void next()
        {  m_iItem = ListView_GetNextItem(m_hwnd, m_iItem, m_rgf); }
}; // EnumItem


HCURSOR BufferListPane::sm_hDragCursor;
Command::KeyBinds* BufferListPane::sm_pKeyBinds;

static HCURSOR loadCursor(
    HCURSOR*        inout_hCursor,
    const char16*   pwszDll,
    uint            nId )
{
    HCURSOR hCursor = *inout_hCursor;
    if (NULL == hCursor)
    {
        HMODULE hDll = ::LoadLibraryEx(
            pwszDll,
            NULL,
            LOAD_LIBRARY_AS_DATAFILE );
        when (NULL == hDll) return NULL;

        hCursor = ::LoadCursor(hDll, MAKEINTRESOURCE(nId));
        ::FreeLibrary(hDll);
        *inout_hCursor = hCursor;
    }

    return hCursor;
} // loadCursor

//////////////////////////////////////////////////////////////////////
//
// BufferListPane ctor
//
BufferListPane::BufferListPane() :
    m_pDragItem(NULL),
    m_hwndListView(NULL)
{
    m_pwszName = L"Buffer List";
} // BufferListPane::BufferListPane

// Activates the first selected buffer in-place.
void BufferListPane::ActivateBuffers(bool is_new_frame) {
  LVITEM oItem;
  oItem.iSubItem = 0;
  oItem.mask = LVIF_PARAM;
  foreach (EnumItem, oEnum, m_hwndListView) {
    oItem.iItem = oEnum.Get();
    unless (ListView_GetItem(m_hwndListView, &oItem)) continue;
    auto const buffer = reinterpret_cast<Buffer*>(oItem.lParam);

    // Make sure buffer is alive.
    for (auto& runner: Application::Get()->buffers()) {
      if (runner == buffer) {
        if (is_new_frame) {
          auto const frame = Application::Get()->CreateFrame();
          frame->AddPane(new EditPane(buffer));
          frame->Realize();
        } else {
          GetFrame()->ShowBuffer(buffer);
        }
        return;
      }
    }
  }
}

//////////////////////////////////////////////////////////////////////
//
// BufferListPane::compareItems
//
int CALLBACK BufferListPane::compareItems(LPARAM a, LPARAM b, LPARAM)
{
    Buffer* pa = reinterpret_cast<Buffer*>(a);
    Buffer* pb = reinterpret_cast<Buffer*>(b);
    return ::lstrcmpW(pa->GetName(), pb->GetName());
} // BufferListPane::compareItems

//////////////////////////////////////////////////////////////////////
//
// BufferListPane::dragFinish
//
void BufferListPane::dragFinish(POINT pt) {
  if (!m_pDragItem) return;
  auto const buffer = m_pDragItem;
  dragStop();
  if (auto const pane = Application::Get()->FindPane(m_hwnd, pt)) {
    // Drop to pane contains specified point.
    pane->GetFrame()->ShowBuffer(buffer);
  } else {
    // Create new fame
    auto const frame = Application::Get()->CreateFrame();
    frame->AddPane(new EditPane(buffer));
    frame->Realize();
  }
}

// BufferListPane::dragMove
void BufferListPane::dragMove(POINT pt)
{
    if (NULL == m_pDragItem) return;

    if (::GetCapture() != m_hwnd)
    {
        dragStop();
        return;
    }

    Pane* pPane = Application::Get()->FindPane(m_hwnd, pt);

    HCURSOR hCursor;
    if (NULL != pPane && pPane->Is<EditPane>())
    {
        hCursor = loadCursor(&sm_hDragCursor, L"ole32.dll", 3);
    }
    else
    {
        hCursor = ::LoadCursor(NULL, MAKEINTRESOURCE(IDC_NO));
    }

    ::SetCursor(hCursor);
} // BufferListPane::dragMove

void BufferListPane::dragStart(int iItem)
{
    if (NULL != m_pDragItem) return;

    LVITEM oItem;
    oItem.iItem    = iItem;
    oItem.iSubItem = 0;
    oItem.mask     = LVIF_PARAM;
    unless (ListView_GetItem(m_hwndListView, &oItem)) return;

    Buffer* pBuffer = reinterpret_cast<Buffer*>(oItem.lParam);
    for (auto& runner: Application::Get()->buffers()) {
        if (runner == pBuffer) {
            m_pDragItem = pBuffer;
            ::SetCursor(loadCursor(&sm_hDragCursor, L"ole32.dll", 3));
            ::SetCapture(m_hwnd);
            return;
        }
    } // for each buffer
} // BufferListPane::dragStart

// BufferListPane::dragStop
void BufferListPane::dragStop()
{
    ::ReleaseCapture();
    m_pDragItem = NULL;
} // BufferListPane::dragStop


//////////////////////////////////////////////////////////////////////
//
// BufferListPane::GetTitle
//
int BufferListPane::GetTitle(char16* out_wszTitle, int)
{
    ::lstrcpy(out_wszTitle, GetName());
    return lstrlen(out_wszTitle);
} // // BufferListPane::GetTitle

//////////////////////////////////////////////////////////////////////
//
// BufferListPane::MapKey
//
Command::KeyBindEntry* BufferListPane::MapKey(uint nKey)
{
    if (NULL == sm_pKeyBinds)
    {
        sm_pKeyBinds = new Command::KeyBinds;
        sm_pKeyBinds->Bind(VK_DELETE | 0x100, Command::KillBuffers);
    } // if

    Command::KeyBindEntry* pEntry = sm_pKeyBinds->MapKey(nKey);
    when (NULL != pEntry) return pEntry;
    return Command::g_pGlobalBinds->MapKey(nKey);
} // BufferListPane::MapKey


//////////////////////////////////////////////////////////////////////
//
// BufferListPane::onCreate
//
void BufferListPane::onCreate(CREATESTRUCT* p)
{
    uint dwExStyle = 0;
        dwExStyle |= LVS_EX_DOUBLEBUFFER;
        dwExStyle |= LVS_EX_FULLROWSELECT;
        //dwExStyle |= LVS_EX_GRIDLINES;
        dwExStyle |= LVS_EX_HEADERDRAGDROP;
        dwExStyle |= LVS_EX_LABELTIP;
        dwExStyle |= LVS_EX_UNDERLINEHOT;
        //dwExStyle |= LVS_EX_TRACKSELECT;

    uint dwStyle = WS_CHILD | WS_VISIBLE | LVS_REPORT;
        dwStyle |= LVS_SHAREIMAGELISTS;
        dwStyle |= LVS_SHOWSELALWAYS;

    m_hwndListView = ::CreateWindowEx(
        0,//dwExStyle,      // dwExStyle
        WC_LISTVIEW,
        NULL,           // title
        dwStyle,
        0,
        0,
        p->cx,
        p->cy,
        *this,  // parent
        reinterpret_cast<HMENU>(ListViewId),
        g_hInstance,
        NULL );

    ListView_SetExtendedListViewStyleEx(
        m_hwndListView,
        dwExStyle,
        dwExStyle );

    ListView_SetImageList(
        m_hwndListView,
        Application::Get()->GetIconList(),
        LVSIL_SMALL );

    struct ColumnDef
    {
        const char16*   m_pwsz;
        int             m_cx;
        int             m_fmt;
    }; // ColumnDef

    static ColumnDef k_rgoColumn[] =
    {
        { L"Name",      150, LVCFMT_LEFT },
        { L"Size",       60, LVCFMT_RIGHT },
        { L"State",      60, LVCFMT_LEFT },
        { L"Saved At",  100, LVCFMT_LEFT },
        { L"File",      300, LVCFMT_LEFT },
    }; // k_rgoColumn

    LVCOLUMN oColumn;
    oColumn.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH;
    //oColumn.iSubItem = 0;
    for (
        const ColumnDef* p = k_rgoColumn;
        p < k_rgoColumn + lengthof(k_rgoColumn);
        p++ )
    {
        oColumn.cx  = p->m_cx;
        oColumn.fmt = p->m_fmt;
        oColumn.pszText = const_cast<char16*>(p->m_pwsz);
        ListView_InsertColumn(m_hwndListView, p - k_rgoColumn, &oColumn);
        oColumn.iSubItem += 1;
    } // for each ColumnDef

    Refresh();
} // BufferListPane::onCreate


//////////////////////////////////////////////////////////////////////
//
// BufferListPane::onKeyDown
//
void BufferListPane::onKeyDown(uint nVKey)
{
    uint nKey = Command::TranslateKey(nVKey);
    when (0 == nKey) return;
    Application::Get()->Execute(this, nKey, 0);
} // BufferListPane::onKeyDown


//////////////////////////////////////////////////////////////////////
//
// BufferListPane::onMessage
//
LRESULT BufferListPane::onMessage(
    UINT    uMsg,
    WPARAM  wParam,
    LPARAM  lParam )
{
    switch (uMsg)
    {
    case WM_CREATE:
        onCreate(reinterpret_cast<CREATESTRUCT*>(lParam));
        break;

    case WM_LBUTTONUP:
    {
        POINT pt;
        pt.x = GET_X_LPARAM(lParam);
        pt.y = GET_Y_LPARAM(lParam);
        dragFinish(pt);
        return 0;
    } // WM_LBUTTONUP

    case WM_MOUSEMOVE:
    {
        POINT pt;
        pt.x = GET_X_LPARAM(lParam);
        pt.y = GET_Y_LPARAM(lParam);
        dragMove(pt);
        return 0;
    } // WM_MOUSEMOVE

    case WM_NOTIFY: {
      auto const hdr = reinterpret_cast<NMHDR*>(lParam);
      switch (hdr->code) {
        case LVN_BEGINDRAG:
          dragStart(reinterpret_cast<NMLISTVIEW*>(hdr)->iItem);
          break;

        case LVN_ITEMACTIVATE: {
          auto const active = reinterpret_cast<NMITEMACTIVATE*>(hdr);
          ActivateBuffers((active->uKeyFlags & LVKF_CONTROL) != 0);
          break;
        }

        case LVN_KEYDOWN:
          onKeyDown(reinterpret_cast<NMLVKEYDOWN*>(lParam)->wVKey);
          break;
      } // switch code
      return 0;
    } // WM_NOTIFY

    case WM_SETFOCUS:
        ::SetFocus(m_hwndListView);
        break;

    case WM_WINDOWPOSCHANGED:
    {
        WINDOWPOS* p = reinterpret_cast<WINDOWPOS*>(lParam);
        if (p->flags & SWP_HIDEWINDOW)
        {
            ::ShowWindow(m_hwndListView, SW_HIDE);
        }
        else
        {
            ::SetWindowPos(
                m_hwndListView,
                NULL,
                0,
                0,
                p->cx,
                p->cy,
                SWP_NOZORDER | SWP_SHOWWINDOW );
        }
        return 0;
    } // WM_WINDOWPOSCHANGED
    } // swtich message

    return Pane::onMessage(uMsg, wParam, lParam);
} // BufferListPane::onMessage

//////////////////////////////////////////////////////////////////////
//
// BufferListPane::refresh
//
void BufferListPane::Refresh()
{
    ListView_DeleteAllItems(m_hwndListView);

    for (auto& buffer: Application::Get()->buffers()) {
        LVITEM oItem;
        oItem.mask = LVIF_IMAGE | LVIF_PARAM | LVIF_TEXT;

        oItem.iItem = 0;

        oItem.iImage   = buffer.GetMode()->GetIcon();
        oItem.iSubItem = 0;
        oItem.lParam   = reinterpret_cast<LPARAM>(&buffer);
        oItem.pszText  = const_cast<char16*>(buffer.GetName());
        ListView_InsertItem(m_hwndListView, &oItem);

        oItem.mask = LVIF_TEXT;
        char16 wsz[100];

        // Size
        oItem.iSubItem = 1;
        ::wsprintf(wsz, L"%d", buffer.GetEnd());
        oItem.pszText = wsz;
        ListView_SetItem(m_hwndListView, &oItem);

        // State
        {
            Buffer::EnumWindow oEnum(&buffer);

            char16* pwsz = wsz;
            *pwsz++ = buffer.IsModified() ? '*' : '-';
            *pwsz++ = buffer.IsReadOnly() ? '%' : '-';
            *pwsz++ = buffer.IsNotReady() ? '!' : '-';
            *pwsz++ = oEnum.AtEnd()         ? '-' : 'w';
            *pwsz = 0;

            oItem.iSubItem = 2;
            oItem.pszText = wsz;
            ListView_SetItem(m_hwndListView, &oItem);
        }

        // Last Saved
        {
            if (0 == *buffer.GetFileName())
            {
                 wsz[0] = 0;
            }
            else
            {
                // FIXME 2007-08-05 We should use localized date time format.
                FILETIME ft;
                ::FileTimeToLocalFileTime(buffer.GetLastWriteTime(), &ft);
                SYSTEMTIME st;
                ::FileTimeToSystemTime(&ft, &st);
                ::wsprintf(wsz, L"%d/%d/%d %02d:%02d:%02d",
                    st.wMonth,
                    st.wDay,
                    st.wYear,
                    st.wHour,
                    st.wMinute,
                    st.wSecond );
            }

            oItem.iSubItem = 3;
            oItem.pszText = wsz;
            ListView_SetItem(m_hwndListView, &oItem);
        }

        // File
        {
            oItem.iSubItem = 4;
            oItem.pszText = const_cast<char16*>(buffer.GetFileName());
            ListView_SetItem(m_hwndListView, &oItem);
        }
    } // for each bufer

    ListView_SortItems(m_hwndListView, compareItems, NULL);
} // BufferListPane::Refresh

namespace Command
{

//////////////////////////////////////////////////////////////////////
//
// KillBuffers
//
DEFCOMMAND(KillBuffers)
{
    BufferListPane* pBufferList = pCtx->GetWindow()->
        DynamicCast<BufferListPane>();
    if (NULL == pBufferList) return;

    bool fKilled = false;

    HWND hwndList = pBufferList->GetListWindow();

    LVITEM oItem;
    oItem.iSubItem = 0;
    oItem.mask = LVIF_PARAM;
    foreach (EnumItem, oEnum, hwndList)
    {
        oItem.iItem = oEnum.Get();
        unless (ListView_GetItem(hwndList, &oItem)) continue;
        Buffer* pBuffer = reinterpret_cast<Buffer*>(oItem.lParam);
        if (Application::Get()->KillBuffer(pBuffer))
        {
            fKilled = true;
        }
    } // for each item

    if (fKilled) pBufferList->Refresh();
} // BufferListPane::killBuffers

//////////////////////////////////////////////////////////////////////
//
// ListBuffer
//
DEFCOMMAND(ListBuffer)
{
    foreach (Application::EnumFrame, oEnum, Application::Get())
    {
        Frame* pFrame = oEnum.Get();
        for (auto& pane: pFrame->panes()) {
            BufferListPane* pPane = pane.DynamicCast<BufferListPane>();

            if (NULL != pPane)
            {
                if (pPane->GetFrame() != pCtx->GetFrame())
                {
                    pCtx->GetFrame()->AddPane(pPane);
                }

                pPane->Activate();
                ::InvalidateRect(*pPane, NULL, 0);
                pPane->Refresh();
                return;
            }
        } // for each frame
    } // for each frame

    BufferListPane* pPane = new BufferListPane;
    pCtx->GetFrame()->AddPane(pPane);
} // ListBuffer

} // Command
