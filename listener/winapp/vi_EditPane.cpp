#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - Edit Pane
// listener/winapp/vi_EditPane.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_EditPane.cpp#3 $
//
#define DEBUG_CARET     0
#define DEBUG_FOCUS     0
#define DEBUG_KEY       0
#define DEBUG_REDRAW    0
#define DEBUG_SPLIT     0
#include "./vi_EditPane.h"

#include "./ed_Mode.h"

#include "./vi_Application.h"
#include "./vi_Buffer.h"
#include "./vi_Selection.h"
#include "./vi_TextEditWindow.h"
#include "./vi_util.h"

const char16* const
k_rgwszNewline[4] =
{
    L"--",
    L"LF",
    L"CR",
    L"CRLF",
}; // k_rgwszNewline

static HCURSOR s_hVSplitCursor;

EditPane::Box::~Box()
{
    #if DEBUG_DESTORY
        DEBUG_PRINTF(L"%p\n", this);
    #endif

    ASSERT(NULL == m_pWindow);

    if (NULL != m_hwndVScrollBar)
    {
        ::DestroyWindow(m_hwndVScrollBar);
        m_hwndVScrollBar = NULL;
    }
} // EditPane::Box::~Box

void EditPane::SplitterDrag::Start(HWND hwnd, State eState)
{
    ::SetCapture(hwnd);
    m_eState = eState;
} // EditPane::SplitterDrag::Start

void EditPane::SplitterDrag::Stop()
{
    ::ReleaseCapture();
    m_eState = State_None;
} // EditPane::SplitterDrag::Stop

EditPane::EditPane(Buffer* pBuffer, Posn lStart) :
    m_eState(State_NotRealized)
{
    Window* pWindow = new TextEditWindow(this, pBuffer, lStart);
    Box* pBox = new Box(pWindow);
    m_oBoxes.Append(pBox);
    m_oWindows.Append(pWindow);
    m_pwszName = pBuffer->GetName();
} // EditPane::EditPane

EditPane::~EditPane()
{
    while (NULL != m_oBoxes.GetFirst())
    {
        delete m_oBoxes.GetFirst();
    } // for
} // EditPane::~EditPane

void EditPane::CloseAllBut(Window* pWindow)
{
    Box* pRunner = m_oBoxes.GetFirst();

    while (pRunner->GetWindow() != pWindow)
    {
        pRunner->GetWindow()->Destroy();

        pRunner = m_oBoxes.GetFirst();
    } // while
    
    Box* pThisBox = pRunner;

    for (;;)
    {
        pRunner = pThisBox->GetNext();
        if (NULL == pRunner)
        {
            break;
        }

        pRunner->GetWindow()->Destroy();
    } // for
} // EditPane::CloseAllBut

static void drawSplitter(HDC hdc, RECT* prc, uint grfFlag)
{
    RECT rc = *prc;
    ::FillRect(hdc, &rc, reinterpret_cast<HBRUSH>(COLOR_3DFACE + 1));
    ::DrawEdge(hdc, &rc, EDGE_RAISED, grfFlag);
} // drawSplitter

//////////////////////////////////////////////////////////////////////
//
// EditPane::drawSplitters
//  Draw edges between panes.
//
void EditPane::drawSplitters(HDC hdc)
{
    RECT rc = m_rc;

    if (! HasMultipleWindows())
    {
        int cxVScroll = ::GetSystemMetrics(SM_CXVSCROLL);
        rc.left   = rc.right - cxVScroll;
        rc.bottom = rc.top + k_cySplitterBig;
        drawSplitter(hdc, &rc, BF_RECT);
    }
    else
    {
        Box* pAbove = NULL;
        foreach (EnumBox, oEnum, this)
        {
            Box* pBox = oEnum.Get();
            if (NULL != pAbove)
            {
                rc.top    = pAbove->GetRect()->bottom;
                rc.bottom = pBox->GetRect()->top;
                drawSplitter(hdc, &rc, BF_TOP | BF_BOTTOM);
            }
            pAbove = pBox;
        } // for each window
    } // if
} // EditPane::drawSplitters

void EditPane::drawSplitters()
{
    Dc dc(m_hwnd, ::GetDC(m_hwnd));
    drawSplitters(dc);
} // EditPane::drawSplitters

//////////////////////////////////////////////////////////////////////
//
// EditPane::GetActiveBox
//  Returns the last active Box.
EditPane::Box* EditPane::getActiveBox() const
{
    Box* pActive = m_oBoxes.GetFirst();
    foreach (EnumBox, oEnum, this)
    {
        Box* pBox = oEnum.Get();
        if (pActive->GetWindow()->GetActiveTick() < 
            pBox->GetWindow()->GetActiveTick() )
        {
            pActive = pBox;
        }
    } // for each Box
    return pActive;
} // EditPane::GetActiveBox

//////////////////////////////////////////////////////////////////////
//
// EditPane::GetActiveWindow
//  Returns the last active Box.
EditPane::Window* EditPane::GetActiveWindow() const
{ 
    Box* pBox = getActiveBox();
    if (NULL == pBox)
    {
        return NULL;
    }
    
    return pBox->GetWindow();
} // EditPane::GetActiveWindow

Buffer* EditPane::GetBuffer() const
    { return GetActiveWindow()->GetBuffer(); }

int EditPane::GetTitle(char16* out_wszTitle, int cchTitle)
{
    Buffer* pBuffer = GetActiveWindow()->GetBuffer();

    const char16* pwszName = pBuffer->GetName();

    int cwchName = ::lstrlenW(pwszName);

    int cwchExtra = 1;
    if (pBuffer->IsModified())
    {
        cwchExtra += 2;
    }

    int cwch = cwchName;

    if (cchTitle < cwchName + cwchExtra)
    {
        cwch = cchTitle - 2;
    }

    myCopyMemory(
        out_wszTitle,
        pwszName,
        sizeof(char16) * cwch );

    char16* pwch = out_wszTitle + cwch;

    if (cwch != cwchName)
    {
        *pwch++ = '.';
        *pwch++ = '.';
    }

    if (pBuffer->IsModified())
    {
        *pwch++ = ' ';
        *pwch++ = '*';
    }

    *pwch = 0;

    return static_cast<int>(pwch - out_wszTitle);
} // EditPane::GetTitle

bool EditPane::HasFocus() const
{
    return ::GetFocus() == *GetActiveWindow();
} // EditPane::HasFocus

EditPane::Element EditPane::hitTest(POINT pt, Box** out_pBox) const
{
    *out_pBox = NULL;

    int cxVScroll = ::GetSystemMetrics(SM_CXVSCROLL);

    if (HasMultipleWindows())
    {
        foreach (EnumBox, oEnum, this)
        {
            Box* pBox = oEnum.Get();
            *out_pBox = pBox;

            if (::PtInRect(pBox->GetRect(), pt))
            {
                if (pt.x > m_rc.right - cxVScroll)
                {
                    return Element_VScrollBar;
                }

                return Element_Window;
            }

            if (pt.y < pBox->GetRect()->top)
            {
                return Element_Splitter;
            }
        } // for each box
    }
    else if (::PtInRect(m_oBoxes.GetFirst()->GetRect(), pt))
    {
        Box* pBox = m_oBoxes.GetFirst();
        *out_pBox = pBox;

        if (pt.x < pBox->GetRect()->right - cxVScroll)
        {
            return Element_Window;
        }

        if (pt.y < pBox->GetRect()->top + k_cySplitterBig)
        {
            return Element_SplitterBig;
        }

        return Element_VScrollBar;
    } // if

    return Element_None;
} // EditPane::hitTest


Command::KeyBindEntry* EditPane::MapKey(uint nKey)
{
    return GetActiveWindow()->MapKey(nKey);
} // EditPane::MapKey


//////////////////////////////////////////////////////////////////////
//
// EditPane::OnIdle
//
bool EditPane::OnIdle(uint nCount)
{
    bool fMore = GetActiveWindow()->OnIdle(nCount);
    foreach (EnumBox, oEnum, this)
    {
        if (oEnum.Get()->GetWindow()->OnIdle(nCount))
        {
            fMore = true;
        }
    } // for each window
    return fMore;
} // EditPane::OnIdle

LRESULT EditPane::onMessage(
    UINT    uMsg,
    WPARAM  wParam,
    LPARAM  lParam )
{
    switch (uMsg)
    {
    case WM_CREATE:
    {
        CREATESTRUCT* pCreate = reinterpret_cast<CREATESTRUCT*>(lParam);

        m_eState = State_Realized;

        ::SetWindowText(m_hwnd, m_pwszName);

        m_rc.left = 0;
        m_rc.top  = 0;
        m_rc.right = pCreate->cx;
        m_rc.bottom = pCreate->cy;

        foreach (EnumBox, oEnum, this)
        {
            Box* pBox = oEnum.Get();
            *pBox->GetRect() = m_rc;
            realizeBox(pBox);
        } // for each window
        break;
    } // WM_CREATE

    case WM_DESTROY:
        #if DEBUG_DESTROY
            DEBUG_PRINTF("WM_DESTROY %p\n", this);
        #endif

        m_eState = State_Destroyed;
        while (NULL != m_oBoxes.GetFirst())
        {
            m_oBoxes.GetFirst()->GetWindow()->Destroy();
        } // while
        break;

    case WM_LBUTTONDOWN:
    {
        Point pt(MAKEPOINTS(lParam));
        switch (hitTest(pt, &m_oSplitterDrag.m_pBox))
        {
        case Element_Splitter:
            m_oSplitterDrag.Start(m_hwnd, SplitterDrag::State_Drag);
            break;

        case Element_SplitterBig:
            m_oSplitterDrag.Start(m_hwnd, SplitterDrag::State_DragSingle);
            break;
        } // switch element
        return 0;
    } // WM_LBUTTONWODN

    case WM_LBUTTONUP:
        switch (m_oSplitterDrag.m_eState)
        {
        case SplitterDrag::State_Drag:
        case SplitterDrag::State_DragSingle:
        {
            Box* pBelow = m_oSplitterDrag.m_pBox;
            Box* pAbove = pBelow->GetPrev();

            if (NULL == pAbove)
            {
                break;
            }

            Point pt(MAKEPOINTS(lParam));

            int cyMin = k_cyMinBox;

            if (pt.y - pAbove->GetRect()->top < cyMin)
            {
                pBelow->GetRect()->top = pAbove->GetRect()->top;
                pAbove->GetWindow()->Destroy();
                setBoxPos(pBelow);
                drawSplitters();
            }
            else if (pBelow->GetRect()->bottom - pt.y < k_cyMinBox)
            {
                pAbove->GetRect()->bottom = pBelow->GetRect()->bottom;
                pBelow->GetWindow()->Destroy();
                setBoxPos(pAbove);
                drawSplitters();
            }

            m_oSplitterDrag.Stop();
            break;
        } // SplitterDrag::State_Drag
        } // switch state
        return 0;

    case WM_MOUSEMOVE:
    {
        switch (m_oSplitterDrag.m_eState)
        {
        case SplitterDrag::State_Drag:
        case SplitterDrag::State_DragSingle:
        {
            Box* pBelow = m_oSplitterDrag.m_pBox;
            Box* pAbove = pBelow->GetPrev();

            if (NULL == pAbove)
            {
                pAbove = splitVertically(pBelow, k_cySplitter);
            }

            Point pt(MAKEPOINTS(lParam));

            if (pt.y - pAbove->GetRect()->top <= 0)
            {
                // Above box is too small.
            }
            else if (pBelow->GetRect()->bottom - (pt.y + k_cySplitter) <= 0)
            {
                // Below box is too small.
            }
            else
            {
                pAbove->GetRect()->bottom = pt.y;
                pBelow->GetRect()->top    = pt.y + k_cySplitter;
                setBoxPos(pAbove);
                setBoxPos(pBelow);
            }

            drawSplitters();
            break;
        } // SplitterDrag::State_Drag
        } // switch state

        return 0;
    } // WM_MOUSEMOVE

#if 0
    case WM_NCDESTROY:
        delete this;
        break;
#endif
    case WM_PAINT:
    {
        PAINTSTRUCT ps;
        HDC hdc = ::BeginPaint(m_hwnd, &ps);
        #if DEBUG_REDRAW
            DEBUG_PRINTF(L"WM_PAINT %p (%d,%d)-(%d,%d)\n",
                this,
                ps.rcPaint.left, ps.rcPaint.top,
                ps.rcPaint.right, ps.rcPaint.bottom );
        #endif
        drawSplitters(hdc);
        ::EndPaint(m_hwnd, &ps);
        return 0;
    } // WM_PAINT

    case WM_PARENTNOTIFY:
        switch (wParam)
        {
        case WM_DESTROY:
        {
            HWND hwnd = reinterpret_cast<HWND>(lParam);
            foreach (EnumBox, oEnum, this)
            {
                Box* pBox = oEnum.Get();
                if (*pBox->GetWindow() != hwnd)
                {
                    continue;
                }

                #if DEBUG_DESTROY
                    DEBUG_PRINTF("WM_PARENTNOTIFY %p WM_DESTROY box=%p\n",
                        this, pBox );
                #endif

                Box* pAbove = pBox->GetPrev();
                Box* pBelow = pBox->GetNext();

                m_oWindows.Delete(pBox->GetWindow());
                m_oBoxes.Delete(pBox);

                pBox->DetachWindow();

                RECT rc = *pBox->GetRect();

                delete pBox;

                if (NULL != pAbove)
                {
                    // Extend pane above.
                    pAbove->GetRect()->bottom = rc.bottom;
                    setBoxPos(pAbove);
                    break;
                }

                if (NULL != pBelow)
                {
                    // Extend pane below.
                    pBelow->GetRect()->top = rc.top;
                    setBoxPos(pBelow);
                    break;
                }

                if (State_Realized == m_eState)
                {
                    // There is no window in this pane. So, we delete
                    // this pane.
                    ::DestroyWindow(*this);
                }
                break;
            } // for each box
            break;
        } // WM_DESTROY
        } // switch wParam
        return 0;

    case WM_SETCURSOR:
    {
        POINT pt;
        if (! ::GetCursorPos(&pt))
        {
            return FALSE;
        }

        if (! ::ScreenToClient(m_hwnd, &pt))
        {
            return FALSE;
        }

        HCURSOR hCursor;
        Box* pBox;
        switch (hitTest(pt, &pBox))
        {
        case Element_Splitter:
        case Element_SplitterBig:
            if (NULL == s_hVSplitCursor)
            {

                s_hVSplitCursor = ::LoadCursor(
                    g_hInstance,
                    MAKEINTRESOURCE(IDC_VSPLIT) );
            }

            hCursor = s_hVSplitCursor;
            break;

        default:
            hCursor = ::LoadCursor(NULL, IDC_ARROW);
            break;
        } // switch element

        ::SetCursor(hCursor);
        return TRUE;
    } // WM_SETCURSOR

    case WM_SETFOCUS:
    {
        #if DEBUG_FOCUS
            DEBUG_PRINTF(L"WM_SETFOCUS %p\n", this);
        #endif

        Pane::onMessage(uMsg, wParam, lParam);

        Window* pWindow = GetActiveWindow();

        if (NULL != pWindow)
        {
            pWindow->Activate();
            GetBuffer()->UpdateFileStatus(true);
        }

        return 0;
    } // WM_SETFOCUS

    case WM_SIZE:
        goto resize;

    case WM_VSCROLL:
    {
        Box* pBox = reinterpret_cast<Box*>(
            ::GetWindowLongPtr(
                reinterpret_cast<HWND>(lParam),
                GWLP_USERDATA ) );

        Window* pWindow = pBox->GetWindow();
        if (NULL != pWindow)
        {
            pWindow->SendMessage(WM_VSCROLL, wParam, lParam);
        }
        return 0;
    } // WM_VSCROLL

    case WM_WINDOWPOSCHANGED:
    {
        const WINDOWPOS* wp = reinterpret_cast<WINDOWPOS*>(lParam);
        if (wp->flags & SWP_NOSIZE)
        {
            return 0;
        }

        goto resize;
    } // WM_WINDOWPOSCHANGED

    resize:
    {
        RECT rcOld = m_rc;
        ::GetClientRect(*this, &m_rc);

        int cyNewPane = m_rc.bottom  - m_rc.top;
        int cyOldPane = rcOld.bottom - rcOld.top;

        if (0 == cyOldPane)
        {
            int cBoxes = 0;
            foreach (EnumBox, oEnum, this)
            {
                cBoxes += 1;
            } // for each Box

            if (0 == cBoxes)
            {
                return 0;
            }

            int cyNewWin= cyNewPane / cBoxes;
            int yBox = m_rc.top;
            int cySplitter = 0;
            Box* pBox = NULL;
            foreach (EnumBox, oEnum, this)
            {
                pBox = oEnum.Get();

                RECT* prc = pBox->GetRect();

                yBox += cySplitter;
                prc->top    = yBox;
                yBox += cyNewWin;
                prc->bottom = yBox;

                cySplitter = k_cySplitter;
            } // for each Box

            if (NULL != pBox)
            {
                pBox->GetRect()->bottom = m_rc.bottom;
            }
        }
        else
        {
            tryAgain:

            int yBox = m_rc.top;
            int cySplitter = 0;
            Box* pBox = NULL;
            foreach (EnumBox, oEnum, this)
            {
                pBox = oEnum.Get();

                RECT* prc = pBox->GetRect();

                int cyOldWin = prc->bottom - prc->top;
                int cyNewWin = cyNewPane * cyOldWin / cyOldPane;

                if (cyNewWin < k_cyMinBox)
                {
                    pBox->GetWindow()->Destroy();
                    goto tryAgain;
                }

                yBox += cySplitter;
                prc->top = yBox;
                yBox += cyNewWin;
                prc->bottom = yBox;

                cySplitter = k_cySplitter;
            } // for each Box

            if (NULL == pBox)
            {
                return 0;
            }

            pBox->GetRect()->bottom = m_rc.bottom;
        } // if

        foreach (EnumBox, oEnum, this)
        {
            Box* pBox = oEnum.Get();
            pBox->GetRect()->left  = m_rc.left;
            pBox->GetRect()->right = m_rc.right;
            setBoxPos(pBox);
        } // for each window

        drawSplitters();
        return 0;

    } // WM_WINDOWPOSCHANGED

    case TextEditWindow::WN_QueryClose:
        // Do we have multiple frame?
        if (Application::Get()->HasMultipleFrames())
        {
            // We have mutliple frame. So, we have at least one frame even if
            // we destroy frame contains this pane.
            return TRUE;
        }

        if (HasMultipleWindows())
        {
            // This pane won't be closed when close specified window.
            return TRUE;
        }

        return Application::Get()->CanExit();
    } // switch message

    return Pane::onMessage(uMsg, wParam, lParam);
} // EditPane::onMessage

void EditPane::realizeBox(Box* pBox)
{
    Window* pWindow = pBox->GetWindow();

    HWND hwndVertScrollBar = ::CreateWindowEx(
        0,
        L"SCROLLBAR",
        NULL,   // title
        WS_CHILD | WS_VISIBLE | SBS_VERT,
        0,      // x
        0,      // y
        0,      // width
        0,      // height
        *this,  // parent
        NULL,   // menu
        g_hInstance,
        NULL );

    ::SetWindowLongPtr(
        hwndVertScrollBar,
        GWLP_USERDATA,
        reinterpret_cast<LONG_PTR>(pBox) );

    pBox->SetVScrollBar(hwndVertScrollBar);
    pWindow->CreateWindowEx(0, NULL, WS_CHILD | WS_VISIBLE, *this);
    pWindow->SetScrollBar(hwndVertScrollBar, SB_VERT);
} // EditPane::realizeWindow

void EditPane::setupStatusBar()
{
    int cx = GetFrame()->GetCxStatusBar();

    static const int rgiWidth[] =
    {
        25, // ins/ovf
        70, // posn
        40, // column
        50, // line
        32, // newline
        50, // code page
        70, // mode
        0,
    };

    int rgiRight[lengthof(rgiWidth)];
    int iRight = cx;
    for (int i = 0; i < lengthof(rgiRight); i++)
    {
        rgiRight[lengthof(rgiRight) - i - 1] = iRight;
        iRight -= rgiWidth[i];
    }

    GetFrame()->SetStatusBarParts(rgiRight, lengthof(rgiRight));
} // EditPane::setupStatusBar

void EditPane::setBoxPos(Box* pBox) const
{
    Window* pWindow = pBox->GetWindow();

    HWND hwndVScrollBar = pWindow->GetScrollBarHwnd(SB_VERT);

    const RECT* prc = pBox->GetRect();

    #if DEBUG_SPLIT
        DEBUG_PRINTF(L"%p %p %d+%d-%d+%d\n",
            this,
            pWindow,
            prc->left, prc->top, prc->right, prc->bottom );
    #endif

    int cxVScroll = ::GetSystemMetrics(SM_CXVSCROLL);

    if (NULL == hwndVScrollBar)
    {
        cxVScroll = 0;
    }
    else
    {
        int cySplitter = HasMultipleWindows() ? 0 : k_cySplitterBig;

        ::SetWindowPos(
            hwndVScrollBar,
            NULL,
            prc->right - cxVScroll,
            prc->top + cySplitter,
            cxVScroll,
            prc->bottom - prc->top - cySplitter,
            SWP_NOZORDER );
    } // if

    ::SetWindowPos(
        *pWindow,
        NULL,
        prc->left,
        prc->top,
        prc->right - prc->left - cxVScroll,
        prc->bottom - prc->top,
        SWP_NOZORDER );
} // EditPane::setBoxPos

EditPane::Window* EditPane::SplitVertically()
{
    Box*  pBelow = getActiveBox();
    RECT* prcBelow = pBelow->GetRect();

    // Active Box is too small to split.
    int cyBox = prcBelow->bottom - prcBelow->top;
    if (cyBox < k_cyMinBox * 2 + k_cySplitter)
    {
        return NULL;
    }

    splitVertically(pBelow, cyBox / 2);

    pBelow->GetWindow()->MakeSelectionVisible();

    return pBelow->GetWindow();
} // EditPane::SplitVertically

EditPane::Box* EditPane::splitVertically(Box* pBelow, int cyBox)
{
    RECT* prcBelow = pBelow->GetRect();
    ASSERT(prcBelow->bottom - prcBelow->top > cyBox);

    Window* pWindow = new Window(
        this,
        pBelow->GetWindow()->GetBuffer(),
        pBelow->GetWindow()->GetStart() );

    Selection* pSelection = pBelow->GetWindow()->GetSelection();

    pWindow->GetSelection()->SetRange(
        pSelection->GetStart(),
        pSelection->GetEnd() );

    pWindow->GetSelection()->SetStartIsActive(
        pSelection->IsStartActive() );

    Box* pAbove = new Box(pWindow);

    RECT* prcAbove = pAbove->GetRect();

    m_oBoxes.InsertBefore(pAbove, pBelow);
    m_oWindows.InsertBefore(pWindow, pBelow->GetWindow());
    realizeBox(pAbove);

    prcAbove->left   = prcBelow->left;
    prcAbove->right  = prcBelow->right;
    prcAbove->top    = prcBelow->top;
    prcAbove->bottom = prcBelow->top + cyBox;

    prcBelow->top = prcAbove->bottom + k_cySplitter;

    setBoxPos(pBelow);
    setBoxPos(pAbove);

    drawSplitters();

    return pAbove;
} // EditPane::splitVertically

void EditPane::UpdateStatusBar()
{
    setupStatusBar();

    Buffer* pBuffer = GetActiveWindow()->GetBuffer();

    GetFrame()->ShowMessage(
        MessageLevel_Idle,
        (
            pBuffer->IsNotReady()         ? IDS_STATUS_BUSY :
            GetActiveWindow()->HasFocus() ? IDS_STATUS_READY :
                                            0
        ) );

    GetFrame()->SetStatusBarf(
        StatusBarPart_Mode,
        pBuffer->GetMode()->GetName() );

    GetFrame()->SetStatusBarf(
        StatusBarPart_CodePage,
        L"CP%u",
        pBuffer->GetCodePage() );

    GetFrame()->SetStatusBarf(
        StatusBarPart_Newline,
        k_rgwszNewline[pBuffer->GetNewline()] );

    Selection* pSelection = GetActiveWindow()->GetSelection();

    // FIXME 2007-07-18 yosi We should use lazy evaluation object for
    // computing line number of column or cache.
    Selection::Information oInfo;
    pSelection->GetInformation(&oInfo);

    GetFrame()->SetStatusBarf(
        StatusBarPart_LineNumber,
        L"Ln %d%s",
        oInfo.m_lLineNum,
        oInfo.m_fLineNum ? L"" : L"+" );

    GetFrame()->SetStatusBarf(
        StatusBarPart_Column,
        L"Cn %d%s",
        oInfo.m_lColumn,
        oInfo.m_fColumn ? L"" : L"+" );

    GetFrame()->SetStatusBarf(
        StatusBarPart_Posn,
        L"Ch %d",
        pSelection->IsStartActive() ?
            pSelection->GetStart() : pSelection->GetEnd() );

    // FIXME 2007-07-25 yosi@msn.com We need to show "OVR" if
    // we are in overwrite mode.
    GetFrame()->SetStatusBarf(
        StatusBarPart_Insert,
        pBuffer->IsReadOnly() ? L"R/O" : L"INS",
        pBuffer->GetStart() );
} // EditPane::UpdateStatusBar
