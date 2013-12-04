#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - winapp - Text Edit Window
// listener/winapp/vi_TextEditWindow.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_TextEditWindow.cpp#3 $
//
#define DEBUG_AUTOSCROLL 0
#define DEBUG_CARET      1
#define DEBUG_FOCUS      0
#define DEBUG_IDLE       1
#define DEBUG_KEY        0
#define DEBUG_PAINT      1
#define DEBUG_REDRAW     1
#define DEBUG_RESIZE     0
#include "./vi_TextEditWindow.h"

#include "./ed_Style.h"
#include "./gfx_base.h"
#include "./vi_Application.h"
#include "./vi_Buffer.h"
#include "./vi_EditPane.h"
#include "./vi_Frame.h"
#include "./vi_Selection.h"
#include "./vi_util.h"

extern HWND g_hwndActiveDialog;

namespace Command
{
    uint TranslateKey(uint);
} // Command

namespace {
//////////////////////////////////////////////////////////////////////
//
// Caret
//
// Description:
//  Represents caret in per-thread queue. To blink caret, we must track
//  caret size. If we call CreateCaret, caret doesn't blink.
//
class Caret
{
    SIZE    m_size;
    POINT   m_pt;
    HWND    m_hwnd;
    bool    m_fShow;

    public: Caret() :
        m_hwnd(NULL),
        m_fShow(false) {}

    public: void Destroy()
    {
        m_hwnd = NULL;
        ::DestroyCaret();
        #if DEBUG_CARET
            DEBUG_PRINTF("%p\n", this);
        #endif // DEBUG_CARET
        m_size.cx = -1;
        m_pt.x = -1;
    } // Destroy

    // Hide - Hide caret
    public: void Hide()
    {
        if (NULL == m_hwnd) return;
        if (! m_fShow) return;
        #if DEBUG_CARET
            DEBUG_PRINTF("%p\n", this);
        #endif // DEBUG_CARET

        ::HideCaret(m_hwnd);
        m_fShow = false;
    } // Hide

    // IsDirty - Returns true if position or size of caret is changed
    // since last show.
    bool isDirty(HWND hwnd, SIZE size) const
    {
        if (m_hwnd != hwnd)
        {
            #if DEBUG_CARET
                DEBUG_PRINTF("m_hwnd\n");
            #endif
            return true;
        }

        if (m_size.cx != size.cx)
        {
            #if DEBUG_CARET
                DEBUG_PRINTF("m_size.cx %d %d\n", m_size.cx, size.cx);
            #endif
            return true;
        }

        if (m_size.cy != size.cy)
        {
            #if DEBUG_CARET
                DEBUG_PRINTF("m_size.cy\n");
            #endif
            return true;
        }

        return false;
    } // isDirty

    // Show - Show caret at pt with size.
    public: void Show(HWND hwnd, SIZE size, POINT pt)
    {
        if (isDirty(hwnd, size))
        {
            m_hwnd = hwnd;
            m_size = size;
            ::CreateCaret(m_hwnd, NULL, size.cx, size.cy);
            #if DEBUG_CARET
            {
                DEBUG_PRINTF("Create %dx%d\r\n", 
                    size.cx, size.cy);
            }
            #endif // DEBUG_CARET
        } // if

        if (m_pt.x != pt.x || m_pt.y != pt.y)
        {
            m_pt = pt;
            ::SetCaretPos(m_pt.x, m_pt.y);
            #if DEBUG_CARET
            {
                DEBUG_PRINTF("SetCaretPos (%d, %d)\r\n", 
                    pt.x, pt.y );
            }
            #endif // DEBUG_CARET
        } // if

        #if DEBUG_CARET
            DEBUG_PRINTF("ShowCaret: (%d, %d) %dx%d\n", 
                m_pt.x, m_pt.y, m_size.cx, m_size.cy );
        #endif // DEBUG_CARET
        ::ShowCaret(m_hwnd);
        m_fShow = true;
    } // Show
}; // Caret

Caret g_oCaret;

enum MyTimerId
{
    MyTimerId_AutoScroll = 1,
    MyTimerId_Blink      = 2,
}; // MyTimerId

static int s_active_tick;

} // namespace

//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::AutoScroll::Continue
//
void TextEditWindow::AutoScroll::Continue(HWND hwnd)
{
    #if DEBUG_AUTOSCROLL
        DEBUG_PRINTF("\n");
    #endif // DEBUG_AUTOSCROLL

    m_nTimerId = MyTimerId_AutoScroll;
    ::SetTimer(hwnd, m_nTimerId, 50, NULL);
} // TextEditWindow::AutoScroll::Continue


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::AutoScroll::Start
//
void TextEditWindow::AutoScroll::Start(HWND hwnd, int iDir)
{
    if (0 == m_nTimerId)
    {
        m_nStartTick = ::GetTickCount();
        Continue(hwnd);
    }

    m_iDirection = iDir;
} // TextEditWindow::AutoScroll::Start


// TextEditWindow::AutoScroll::Stop
void TextEditWindow::AutoScroll::Stop(HWND hwnd)
{
    if (0 != m_nTimerId)
    {
        #if DEBUG_AUTOSCROLL
            DEBUG_PRINTF("id=%d\n", m_nTimterid);
        #endif // DEBUG_AUTOSCROLL

        ::KillTimer(hwnd, m_nTimerId);
        m_nTimerId = 0;
    }
} // TextEditWindow::AutoScroll::Stop


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow ctor
//
TextEditWindow::TextEditWindow(void* pvHost, Buffer* pBuffer, Posn lStart)
    : m_eDragMode(DragMode_None),
      m_fBlink(false),
      m_fHasFocus(false),
      m_gfx(new gfx::Graphics()),
      m_lCaretPosn(-1),
      m_nActiveTick(0),
      m_nBlinkTimerId(0),
      m_pBlink(pBuffer->CreateRange(0,0)),
      m_pPage(new Page(m_rc)),
      ALLOW_THIS_IN_INITIALIZER_LIST(selection_(
          new(pBuffer->GetHeap()) Selection(this, pBuffer))),
      m_pViewRange(pBuffer->CreateRange(lStart)),
      #if SUPPORT_IME
        m_fImeTarget(false),
        m_lImeEnd(0),
        m_lImeStart(0),
      #endif // SUPPORT_IME
      m_pvHost(pvHost) {
  pBuffer->AddWindow(this);
}

TextEditWindow::~TextEditWindow() {
    GetSelection()->GetBuffer()->RemoveWindow(this);
}

void TextEditWindow::Blink(Posn lPosn, uint nMillisecond) {
    m_pBlink->SetRange(lPosn, lPosn);
    m_fBlink = true;
    redraw();
    if (0 == m_nBlinkTimerId)
    {
        m_nBlinkTimerId = MyTimerId_Blink; 
        ::SetTimer(m_hwnd, m_nBlinkTimerId, nMillisecond, NULL);
    }
} // TextEditWindow::Blink


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::computeGoalX
//
Posn TextEditWindow::computeGoalX(int xGoal, Posn lGoal)
{
    when (xGoal < 0) return lGoal;

    Page::Line* pLine = NULL;

    if (! m_pPage->IsDirty(m_rc, *selection_))
    {
        pLine = m_pPage->FindLine(lGoal);
    }

    if (NULL != pLine) 
    {
        return pLine->MapXToPosn(*m_gfx, static_cast<float>(xGoal));
    }
    else
    {
        Posn lStart = GetBuffer()->ComputeStartOf(Unit_Paragraph, lGoal);

        Page oPage(m_rc);
        for (;;)
        {
            Page::Line* pLine = oPage.FormatLine(*m_gfx, *selection_, lStart);

            Posn lEnd = pLine->GetEnd();
            if (lGoal < lEnd)
              return pLine->MapXToPosn(*m_gfx, static_cast<float>(xGoal));
            lStart = lEnd;
        } // for
    } // if
} // TextEditWindow::computeGoalX


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::ComputeMotion
//
Count TextEditWindow::ComputeMotion(
    Unit  eUnit,
    Count n,
    POINT pt,
    Posn* inout_lPosn )
{
    switch (eUnit)
    {
    case Unit_Line:
        if (n > 0)
        {
            const Posn lBufEnd = GetBuffer()->GetEnd();
            Posn lGoal = *inout_lPosn;
            Count k;
            for (k = 0; k < n; k++)
            {
                lGoal = EndOfLine(lGoal);
                if (lGoal >= lBufEnd) break;
                lGoal += 1;
            } // for k

            *inout_lPosn = computeGoalX(pt.x, min(lGoal, lBufEnd));
            return k;
        }
        else if (n < 0)
        {
            n = -n;

            const Posn lBufStart = GetBuffer()->GetStart();
            Posn lStart = *inout_lPosn;
            Count k;
            for (k = 0; k < n; k++)
            {
                lStart = StartOfLine(lStart);
                if (lStart <= lBufStart) break;
                lStart -= 1;
            } // for k

            *inout_lPosn = computeGoalX(pt.x, max(lStart, lBufStart));
            return k;
        } // if
        return 0;

    case Unit_Screen:
    {
        Count k = LargeScroll(0, n, false);
        if (k > 0) 
        {
            Posn lStart = m_pPage->GetStart();
            m_pViewRange->SetRange(lStart, lStart);
            *inout_lPosn = MapPointToPosn(pt);
        }
        else if (n > 0)
        {
            *inout_lPosn = GetEnd();
            k = 1;
        }
        else if (n < 0)
        {
            *inout_lPosn = GetStart();
            k = 1;
        }
        return k;
    } // Screen

    case Unit_Window:
        if (n > 0)
        {
            *inout_lPosn = GetEnd();
            return 1;
        }
        else if (n < 0)
        {
            *inout_lPosn = GetStart();
            return 1;
        } // if
        return 0;

    default:
        return GetBuffer()->ComputeMotion(eUnit, n, inout_lPosn);
    } // switch unit
} // TextEditWindow::ComputeMotion


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::EndOfLine
//
Posn TextEditWindow::EndOfLine(Posn lPosn)
{
    if (! m_pPage->IsDirty(m_rc, *selection_))
    {
        Page::Line* pLine = m_pPage->FindLine(lPosn);
        if (NULL != pLine) return pLine->GetEnd() - 1;
    } // if

    const Posn lBufEnd = selection_->GetBuffer()->GetEnd();
    if (lPosn >= lBufEnd) return lBufEnd;

    return endOfLineAux(*m_gfx, lPosn);
} // TextEditWindow::EndOfLine


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::endOfLineAux
//
Posn TextEditWindow::endOfLineAux(const gfx::Graphics& gfx, Posn lPosn)
{
    const Posn lBufEnd = selection_->GetBuffer()->GetEnd();
    if (lPosn >= lBufEnd) return lBufEnd;

    Page oPage(m_rc);
    Posn lStart = selection_->GetBuffer()->ComputeStartOf(
        Unit_Paragraph,
        lPosn );
    for (;;)
    {
        Page::Line* pLine = oPage.FormatLine(gfx, *selection_, lStart);
        lStart = pLine->GetEnd();
        if (lPosn < lStart) return lStart - 1;
    } // for
} // TextEditWindow::endOfLineAux


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::format
//
void TextEditWindow::format(const gfx::Graphics& gfx, Posn lStart) {
  m_pPage->Format(gfx, gfx::RectF(m_rc), *selection_, lStart);
} // TextEditWindow::format


// TextEditWindow::GetBuffer()
Buffer* TextEditWindow::GetBuffer() const
{
    return selection_->GetBuffer();
} // TextEditWindow::GetBuffer


// TextEditWindow::GetColumn
Count TextEditWindow::GetColumn(Posn lPosn)
{
    Posn lStart = StartOfLine(lPosn);
    return lPosn - lStart;
} // TextEditWindow::GetColumn


// TextEditWindow::GetEnd()
//  For Selection.MoveDown Screen
Posn TextEditWindow::GetEnd()
{
    updateScreen();
    return m_pPage->GetEnd();
} // TextEditWindow::GetEnd


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::GetScrollBarHwnd
//
HWND TextEditWindow::GetScrollBarHwnd(int nBar) const
{
    switch (nBar)
    {
    case SB_VERT:
        return m_oVertScrollBar.GetHwnd();
    } // switch

    return NULL;
} // TextEditWindow::GetScrollBarHwnd


// TextEditWindow::GetStart()
//  For Selection.MoveUp Screen
Posn TextEditWindow::GetStart()
{
    updateScreen();
    return m_pPage->GetStart();
} // TextEditWindow::GetStart


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::LargeScroll
//
int TextEditWindow::LargeScroll(int, int iDy, bool fRender)
{
    updateScreen();

    if (iDy < 0)
    {
        // Scroll Down -- place top line out of window.
        iDy = -iDy;

        const Posn lBufStart = selection_->GetBuffer()->GetStart();
        int k;
        for (k = 0; k < iDy; k++)
        {
            Posn lStart = m_pPage->GetStart();
            if (lStart == lBufStart) break;

            // Scroll down until page start goes out to page.
            do
            {

                DEBUG_PRINTF("scroll down lStart=%d\n", lStart);
                if (! m_pPage->ScrollDown(*m_gfx)) break;
            } while (m_pPage->GetEnd() != lStart);
        } // for k

        if (fRender && k > 0) render(*m_gfx);
        return k;
    }    else if (iDy > 0)
    {
        // Scroll Up -- format page from page end.
        const Posn lBufEnd = selection_->GetBuffer()->GetEnd();
        int k;
        for (k = 0; k < iDy; k++)
        {
            Posn lStart = m_pPage->GetEnd();
            if (lStart >= lBufEnd) break;
            DEBUG_PRINTF("scroll up lStart=%d\n", lStart);
            format(*m_gfx, lStart);
        } // for k

        if (fRender && k > 0) render(*m_gfx);
        return k;
    }
    else
    {
        return 0;
    } // if
} // TextEditWindow::LargeScroll


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::MapKey
//
Command::KeyBindEntry* TextEditWindow::MapKey(uint nKey)
{
    return GetBuffer()->MapKey(nKey);
} // TextEditWindow::MapKey


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::MakeSelectionVisible
//
void TextEditWindow::MakeSelectionVisible()
{
    DEBUG_PRINTF("%p [%d,%d]\n",
        this, selection_->GetStart(), selection_->GetEnd() );

    m_lCaretPosn = -1;
    redraw(true);
} // TextEditWindow::MakePosnVisible


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::MapPointToPosn
//
// Description:
//  Maps window position to buffer position.
Posn TextEditWindow::MapPointToPosn(POINT pt)
{
    updateScreen();
    return m_pPage->MapPointToPosn(*m_gfx, pt);
} // TextEditWindow::MapPointToPosn


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::MapPosnToPoint
//
// Description:
//  Maps position specified buffer position and returns height
//  of caret, If specified buffer position isn't in window, this function
//  returns 0.
// TODO(yosi): We should return gfx::RectF for TextEditWindow::MapPosnToPoint()
int TextEditWindow::MapPosnToPoint(Posn lPosn, POINT* out_pt) {
  updateScreen();
  for (;;) {
    auto rect = m_pPage->MapPosnToPoint(*m_gfx, lPosn);
    if (rect.height() > 0) {
      if (out_pt) {
        out_pt->x = static_cast<int>(rect.left);
        out_pt->y = static_cast<int>(rect.top);
      }
      return static_cast<int>(rect.height());
    }
    m_pPage->ScrollToPosn(*m_gfx, lPosn);
  }
}

//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::OnIdle
//
bool TextEditWindow::OnIdle(uint count) {
  auto const more = GetBuffer()->OnIdle(count);

  #if DEBUG_IDLE
    DEBUG_PRINTF("%p count=%d more=%d\n", this, count, more);
  #endif

  if (more || !count)
    redraw();
  return more;
}

//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::onMessage
//
LRESULT TextEditWindow::onMessage(
    UINT    uMsg,
    WPARAM  wParam,
    LPARAM  lParam )
{
    if (WM_SYSTIMER == uMsg)  // WM_SYSTIMER for blinking caret
    {
        DEBUG_PRINTF("WM_SYSTIMER %p\r\n", this);
    } // if

    switch (uMsg)
    {
    case WM_CHAR:
    {
        // Ctrl+<key> is handled by WM_KEYDOWN.
        char16 wch = static_cast<char16>(wParam);
        if (wch >= 0x20)
        {
            m_fBlink = false;
            Application::Get()->Execute(
                this, 
                wch,
                HIWORD(lParam) & KF_REPEAT );
        }
        break;
    } // WM_CAHR

    case WM_CLOSE:
    {
        LRESULT lContinue = ::SendMessage(
            ::GetParent(m_hwnd),
            WN_QueryClose,
            0,
            0 );
        if (! lContinue) return 0;
        break;
    } // WM_CLOSE

    case WM_DESTROY:
        #if DEBUG_DESTROY
            DEBUG_PRINTF("WM_DESTROY %p\n", this);
        #endif
        m_nActiveTick = 0;
        break;

    case WM_CREATE:
      #if DEBUG_RESIZE
      {
        CREATESTRUCT* p = reinterpret_cast<CREATESTRUCT*>(lParam);
        DEBUG_PRINTF("WM_CREATE %p %d+%d+%dx%d\n",
            this,
            p->x, p->y, p->cx, p->cy );
      }
      #endif // DEBUG_RESIZE
      m_gfx->Init(m_hwnd);
      break;

    case WM_APPCOMMAND:
        #if DEBUG_KEY
            DEBUG_PRINTF("WM_APPCOMMAND %p %x\n", this, lParam);
        #endif
        return TRUE;

    case WM_ERASEBKGND:
        return TRUE;

    case WM_KEYDOWN:
    {
        #if DEBUG_KEY
        {
            DEBUG_PRINTF("WM_KEYDOWN VKey=0x%0X 0x%04X 0x%04X\r\n",
                wParam,
                HIWORD(lParam), LOWORD(lParam) );
        }
        #endif

        uint nVKey = static_cast<uint>(wParam);

        uint nKey = Command::TranslateKey(nVKey);
        if (0 != nKey)
        {
            m_fBlink = false;
            Application::Get()->Execute(
                this,
                nKey,
                HIWORD(lParam) & KF_REPEAT );
            return 0;
        } // if
        break;
    } // WM_KEYDOWN

    case WM_KILLFOCUS:
        #if DEBUG_FOCUS
        {
            DEBUG_PRINTF("WM_KILLFOCUS\r\n");
        }
        #endif // DEBUG_FOCUS

        m_fHasFocus = false;
        g_oCaret.Destroy();
        break;

    case WM_LBUTTONDBLCLK:
    {
        Point pt(MAKEPOINTS(lParam));
        Posn lPosn = MapPointToPosn(pt);
        if (lPosn >= 0) selectWord(lPosn);
        return 0;
    } // WM_LBUTTONDBLCLK

    ////////////////////////////////////////////////////////////
    //
    // WM_LBUTTONDOWN
    //  o Start selection drag
    //  o Activate window
    case WM_LBUTTONDOWN:
    {
        Point pt(MAKEPOINTS(lParam));
        Posn lPosn = MapPointToPosn(pt);
        if (lPosn < 0)
        {
            // Click outside window. We do nothing.
            break;
        } // if

        #if DEBUG_FOCUS
            DEBUG_PRINTF("WM_LBUTTONDOWN: p=%d\r\n", lPosn);
        #endif

        if (! m_fHasFocus)
        {
            ::SetFocus(m_hwnd);

            if (lPosn >= GetSelection()->GetStart() &&
                lPosn <  GetSelection()->GetEnd() )
            {
                return 0;
            }
        }

        GetSelection()->MoveTo(lPosn, 0 != (wParam & MK_SHIFT));

        if (wParam & MK_CONTROL)
        {
            selectWord(lPosn);
        }
        else
        {
            m_eDragMode = DragMode_Selection;
            ::SetCapture(m_hwnd);
        }

        return 0;
    } // WM_LBUTTONDOWN

    case WM_LBUTTONUP:
    {
        if (m_eDragMode != DragMode_None)
        {
            ::ReleaseCapture();
            stopDrag();
        }
        return 0;
    } // WM_LBUTTONUP

    case WM_MOUSEMOVE:
    {
        if (m_eDragMode == DragMode_None)
        {
            // We have nothing to do if mouse isn't dragged.
            return 0;
        } // if

        if (::GetCapture() != m_hwnd)
        {
            // Someone takes mouse capture.
            stopDrag();
            return 0;
        }

        Point pt(MAKEPOINTS(lParam));
        Posn lPosn = MapPointToPosn(pt);
        if (lPosn >= 0)
        {
            selection_->MoveTo(lPosn, true);
        } // if

        #if DEBUG_FORMAT
        {
            DEBUG_PRINTF("WM_MOUSEMOVE: %d@%d\n", pt.x, pt.y);
        }
        #endif // DEBUG_FORMAT

        if (pt.y < m_rc.top)
        {
            m_oAutoScroll.Start(m_hwnd, -1);
        }
        else if (pt.y > m_rc.bottom)
        {
            m_oAutoScroll.Start(m_hwnd, 1);
        }
        else
        {
            m_oAutoScroll.Stop(m_hwnd);
        }
        return 0;
    } // WM_MOUSEMOVE

    case WM_MOUSEWHEEL:
      SmallScroll(0, GET_WHEEL_DELTA_WPARAM(wParam) > 0 ? -2 : 2);
      return 0;

    case WM_PAINT: {
      #if DEBUG_PAINT
        DEBUG_PRINTF("~~~~~~~~~~ WM_PAINT Start\n");
      #endif
      // Note: We don't need to show/hide caret. See MSDN/Carets/Using
      // Carets/Hiding a Caret
      {
        gfx::Graphics::DrawingScope drawing_scope(*m_gfx);
        //(*m_gfx)->Clear(gfx::ColorF(gfx::ColorF::Red));
        m_pPage->Render(*m_gfx, m_hwnd);
      }
      ::ValidateRect(*this, nullptr);
      #if DEBUG_PAINT
        DEBUG_PRINTF("~~~~~~~~~~ WM_PAINT End\n");
      #endif
      return 0;
    }

    case WM_SETCURSOR:
        if (HTCLIENT == LOWORD(lParam))
        {
            ::SetCursor(::LoadCursor(NULL, MAKEINTRESOURCE(IDC_IBEAM)));
            return 1;
        }
        break;

    case WM_SETFOCUS:
        #if DEBUG_FOCUS
        {
            DEBUG_PRINTF("WM_SETFOCUS %p\n", this);
        }
        #endif // DEBUG_FOCUS
        s_active_tick += 1;
        m_nActiveTick = s_active_tick;
        m_fHasFocus = true;
        return 0;

    case WM_SIZE:
        #if DEBUG_RESIZE
            DEBUG_PRINTF("WM_SIZE %p %dx%d\n",
                this,
                LOWORD(lParam), HIWORD(lParam) );
        #endif
        return 0;

    case WM_TIMER:
    {
        #if DEBUG_AUTOSCROLL
        DEBUG_PRINTF("WM_TIMER: %d dir=%d\n",
            wParam, m_oAutoScroll.m_iDirection );
        #endif // DEBUG_AUTOSCROLL

        switch (wParam)
        {
        case MyTimerId_AutoScroll:
            if (0 != m_oAutoScroll.m_nTimerId)
            {
                int iDuration = ::GetTickCount() - m_oAutoScroll.m_nStartTick;
                Count lCount = iDuration / 500;
                lCount = max(lCount, 1);
                lCount = min(lCount, 20);

                Count k;
                if (m_oAutoScroll.m_iDirection > 0)
                {
                    k = selection_->MoveDown(Unit_Line, lCount, true);
                }
                else
                {
                    k = selection_->MoveUp(Unit_Line, lCount, true);
                }

                if (0 == k)
                {
                    m_oAutoScroll.Stop(m_hwnd);
                }
                else
                {
                    m_oAutoScroll.Continue(m_hwnd);
                }
            } // if
            break;

        case MyTimerId_Blink:
            ::KillTimer(m_hwnd, wParam);
            m_nBlinkTimerId = 0;
            m_fBlink = false;
            break;
        } // switch id
        break;
    } // WM_TIMER

    case WM_VSCROLL:
     onVScroll(LOWORD(wParam));
     return 0;

    case WM_WINDOWPOSCHANGED:
    {
      // DefWindowProc sents WM_SIZE and WM_MOVE, so handling
      // WM_WINDPOSCHANGED is faster than DefWindowProc.
      auto const wp = reinterpret_cast<const WINDOWPOS*>(lParam);
      if (wp->flags & SWP_NOSIZE)
        return 0;
      #if DEBUG_REDRAW || DEBUG_RESIZE
        DEBUG_PRINTF("WM_WINDOWPOSCHANGED %p 0x%X %dx%d+%d+%d\n",
                     this, wp->flags,
                     wp->cx, wp->cy, wp->x, wp->y );
      #endif
      ::GetClientRect(m_hwnd, &m_rc);
      m_gfx->Resize(m_rc);
      m_pPage->Reset();
      {
        gfx::Graphics::DrawingScope drawing_scope(*m_gfx);
        (*m_gfx)->Clear(gfx::ColorF(gfx::ColorF::Red));
      }
      redraw();
      return 0;
    } // WM_WINDOWPOSCHANGED

    #if SUPPORT_IME
    case WM_IME_COMPOSITION:
        onImeComposition(lParam);
        return 0;

    case WM_IME_ENDCOMPOSITION:
        m_fImeTarget = false;
        return 0;

    case WM_IME_REQUEST:
        if (IMR_RECONVERTSTRING == wParam)
        {
            uint cb = setReconvert(
                reinterpret_cast<RECONVERTSTRING*>(lParam),
                GetSelection()->GetStart(),
                GetSelection()->GetEnd() );
            return cb;
        }
        break;

    case WM_IME_SETCONTEXT:
        // We draw composition string instead of IME. So, we don't
        // need default composition window.
        lParam &= ~ISC_SHOWUICOMPOSITIONWINDOW;
        break;

    case WM_IME_STARTCOMPOSITION:
        if (! m_fImeTarget)
        {
            m_lImeStart  = GetSelection()->GetStart();
            m_lImeEnd    = m_lImeStart;
            m_fImeTarget = false;
        }
        return 0;
    #endif // SUPPORT_IME
    } // switch uMsg

    return BaseWindow::onMessage(uMsg, wParam, lParam);
} // TextEditWindow::onMessage


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::onVScroll
//
void TextEditWindow::onVScroll(uint nCode)
{
    switch (nCode)
    {
    case SB_ENDSCROLL:  // 8
        break;

    case SB_LINEDOWN:   // 1
        SmallScroll(0, 1);
        break;

    case SB_LINEUP:     // 0
        SmallScroll(0, -1);
        break;

    case SB_PAGEDOWN:   // 3
        LargeScroll(0, 1);
        break;

    case SB_PAGEUP:   // 2
        LargeScroll(0, -1);
        break;

    case SB_THUMBPOSITION:  // 4
        return;

    case SB_THUMBTRACK:     // 5
    {
        SCROLLINFO oInfo;
            oInfo.cbSize = sizeof(oInfo);
            oInfo.fMask  = SIF_ALL;
        if (m_oVertScrollBar.GetInfo(&oInfo)) {
            Posn lStart = startOfLineAux(*m_gfx, oInfo.nTrackPos);
            format(*m_gfx, lStart);
            render(*m_gfx);
        }
        break;
    } // SB_THUMBTRACK

    default:
        return;
    } // switch nCode
} // TextEditWindow::onVScroll

//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::redraw
//
// Called by:

//  TextEditWindow::OnIdle
//  TextEditWindow::onMessage WM_WINDOWPOSCHANGED
//
void TextEditWindow::redraw() {
  bool fSelectionActive = m_fHasFocus;

  if (g_hwndActiveDialog) {
    auto const edit_pane = Application::Get()->GetActiveFrame()->
        GetActivePane()->DynamicCast<EditPane>();
    if (edit_pane)
      fSelectionActive = edit_pane->GetActiveWindow() == this;
  }
  redraw(fSelectionActive);
}

void TextEditWindow::redraw(bool fSelectionIsActive) {
  #if DEBUG_REDRAW
    DEBUG_PRINTF("~~~~~~~~~~ Start selection_is_active=%d\n",
                 fSelectionIsActive);
  #endif
  Posn lCaretPosn;
  Posn lSelStart;
  Posn lSelEnd;

  if (m_fBlink) {
    lSelStart = m_pBlink->GetStart();
    lSelEnd = m_pBlink->GetEnd();
  } else {
    lSelStart = selection_->GetStart();
    lSelEnd = selection_->GetEnd();
  }

  if (fSelectionIsActive) {
    lCaretPosn = selection_->IsStartActive() ? lSelStart : lSelEnd;

    // EvEdit 1.0's highlight color
    //selection_->SetColor(Color(0, 0, 0));
    //selection_->SetBackground(Color(0xCC, 0xCC, 0xFF));

    // We should not use GetSysColor. If we want to use here
    // default background must be obtained from GetSysColor.
    //selection_->SetColor(::GetSysColor(COLOR_HIGHLIGHTTEXT));
    //selection_->SetBackground(::GetSysColor(COLOR_HIGHLIGHT));

    // We use Vista's highlight color.
    selection_->SetColor(Color(255, 255, 255));
    selection_->SetBackground(Color(51, 153, 255));
  } else {
    lCaretPosn = m_lCaretPosn;

    Posn lEnd = GetBuffer()->GetEnd();
    if (lSelStart == lEnd && lSelEnd == lEnd)
      lCaretPosn = lEnd;

     selection_->SetColor(Color(67, 78, 84));
     selection_->SetBackground(Color(191, 205, 219));
  }

  {
    auto const lStart = m_pViewRange->GetStart();
    if (m_pPage->IsDirty(m_rc, *selection_, fSelectionIsActive)) {
      #if DEBUG_REDRAW
        DEBUG_PRINTF("Page %p is dirty. lStart=%d\n", m_pPage, lStart);
      #endif // DEBUG_REDRAW
      format(*m_gfx, startOfLineAux(*m_gfx, lStart));

      if (m_lCaretPosn != lCaretPosn) {
        // FIXME 2007-05-12 Fill the page with lines.
        #if DEBUG_REDRAW
          DEBUG_PRINTF("ScrollToPosn %d\n", lCaretPosn);
        #endif
        m_pPage->ScrollToPosn(*m_gfx, lCaretPosn);
        m_lCaretPosn = lCaretPosn;
      }
    } else if (m_lCaretPosn != lCaretPosn) {
        m_pPage->ScrollToPosn(*m_gfx, lCaretPosn);
        m_lCaretPosn = lCaretPosn;
    } else if (m_pPage->GetStart() != lStart) {
        #if DEBUG_REDRAW
        DEBUG_PRINTF("Page %p change start %d to %d\n",
            m_pPage, m_pPage->GetStart(), lStart );
        #endif // DEBUG_REDRAW
        format(*m_gfx, startOfLineAux(*m_gfx, lStart));
    }
  }

  render(*m_gfx);

  #if DEBUG_REDRAW
    DEBUG_PRINTF("~~~~~~~~~~ End\n");
  #endif
}

void TextEditWindow::render(const gfx::Graphics& gfx) {
  if (m_fHasFocus)
    g_oCaret.Hide();

  {
    gfx::Graphics::DrawingScope drawing_scope(*m_gfx);
    m_pPage->Render(gfx, m_hwnd);
  }

  {
    auto const lStart = m_pPage->GetStart();
    m_pViewRange->SetRange(lStart, lStart);
    updateScrollBar();
  }

  if (!m_fHasFocus)
    return;

  const auto rect  = m_pPage->MapPosnToPoint(gfx, m_lCaretPosn);
  if (!rect)
    return;
  ASSERT(rect.height() > 0);
  SIZE size = {
    max(::GetSystemMetrics(SM_CXBORDER), 2),
    static_cast<int>(rect.height())
  };

  POINT pt = {
    static_cast<int>(rect.left),
    static_cast<int>(rect.top)
  };

  #if SUPPORT_IME
    if (m_fImeTarget) {
        if (showImeCaret(size, pt))
          return;
        DEBUG_PRINTF("showImeCaret failed.\r\n");
    }
  #endif // SUPPORT_IME
  g_oCaret.Show(m_hwnd, size, pt);
}

//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::selectWord
//
// Description:
//  Selects word at lPosn and set end point is active.
//
void TextEditWindow::selectWord(Posn lPosn) {
    Selection* pSelection = GetSelection();
    pSelection->SetStart(lPosn);
    pSelection->StartOf(Unit_Word);
    pSelection->EndOf(Unit_Word, true);
    pSelection->SetStartIsActive(false);
} // TextEditWindow::selectWord


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::SetScrollBar
//
void TextEditWindow::SetScrollBar(HWND hwnd, int nBar)
{
    switch (nBar)
    {
    case SB_VERT:
        m_oVertScrollBar.Set(hwnd, hwnd == *this ? SB_VERT : SB_CTL);
        break;
    } // switch
} // TextEditWindow::SetScrollBar


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::SmallScroll
//
int TextEditWindow::SmallScroll(int, int iDy)
{
    updateScreen();

    if (iDy < 0)
    {
        iDy = -iDy;

        const Posn lBufStart = selection_->GetBuffer()->GetStart();
        Posn lStart = m_pPage->GetStart();
        int k;
        for (k = 0; k < iDy; k++)
        {
            if (lStart == lBufStart) break;
            lStart = startOfLineAux(*m_gfx, lStart - 1);
        } // for k

        if (k > 0)
        {
            #if DEBUG_FORMAT
                DEBUG_PRINTF("down lStart=%d\n", lStart);
            #endif // DEBUG_FORMAT
            format(*m_gfx, lStart);
            render(*m_gfx);
        }
        return k;
    }
    else if (iDy > 0)
    {
        const Posn lBufEnd = selection_->GetBuffer()->GetEnd();
        int k;
        for (k = 0; k < iDy; k++)
        {
            if (m_pPage->GetEnd() >= lBufEnd)
            {
                // Make sure whole line of buffer end is visible.
                m_pPage->ScrollToPosn(*m_gfx, lBufEnd);
                k += 1;
                break;
            } // if

            if (! m_pPage->ScrollUp(*m_gfx))
            {
                break;
            } // if
        } // for k

        if (k > 0)
        {
            render(*m_gfx);
        }

        return k;
    }
    else
    {
        return 0;
    } // if
} // TextEditWindow::SmallScroll


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::StartOfLine
//
Posn TextEditWindow::StartOfLine(Posn lPosn) {
    if (lPosn <= 0)
      return 0;
    return startOfLineAux(*m_gfx, lPosn);
} // TextEditWindow::StartOfLine


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::startOfLineAux
//
// See Also:
//  EditPange::endOfLineAux
// Description:
//  Returns start position of window line of specified position.
Posn TextEditWindow::startOfLineAux(const gfx::Graphics& gfx, Posn lPosn)
{
    if (! m_pPage->IsDirty(m_rc, *selection_))
    {
        Page::Line* pLine = m_pPage->FindLine(lPosn);
        if (NULL != pLine) return pLine->GetStart();
    } // if

    Posn lStart = selection_->GetBuffer()->ComputeStartOf(
        Unit_Paragraph,
        lPosn );
    if (0 == lStart)
    {
        return 0;
    }

    Page oPage(m_rc);
    for (;;)
    {
        Page::Line* pLine = oPage.FormatLine(gfx, *selection_, lStart );

        Posn lEnd = pLine->GetEnd();
        if (lPosn < lEnd) return pLine->GetStart();
        lStart = lEnd;
    } // for
} // TextEditWindow::startOfLineAux


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::stopDrag
//
void TextEditWindow::stopDrag()
{
    m_oAutoScroll.Stop(m_hwnd);
    m_eDragMode = DragMode_None;
} // TextEditWindow::stopDrag


//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::updateScreen
//
void TextEditWindow::updateScreen() {
  if (!m_pPage->IsDirty(m_rc, *selection_))
    return;

  Posn lStart = m_pViewRange->GetStart();
  lStart = startOfLineAux(*m_gfx, lStart);
  #if DEBUG_REDRAW
    DEBUG_PRINTF("dirty page lStart=%d\n", lStart);
  #endif // DEBUG_REDRAW
  format(*m_gfx, lStart);
}

//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::updateScrollBar
//
void TextEditWindow::updateScrollBar()
{
    if (NULL == m_pPage->GetBuffer()) return;

    Posn lBufEnd = m_pPage->GetBuffer()->GetEnd() + 1;

    SCROLLINFO oInfo;

    oInfo.cbSize = sizeof(oInfo);
    oInfo.fMask = SIF_POS | SIF_RANGE | SIF_PAGE | SIF_DISABLENOSCROLL;
    oInfo.nPage = m_pPage->GetEnd() - m_pPage->GetStart();
    oInfo.nMin  = 0;
    oInfo.nMax  = lBufEnd;

    oInfo.nPos  = m_pPage->GetStart();

    if (static_cast<Count>(oInfo.nPage) >= lBufEnd)
    {
        // Current screen shows entire buffer. We disable scroll bar.
        oInfo.nMax  = 0;
    }

    m_oVertScrollBar.SetInfo(&oInfo, true);
} // TextEditWindow::updateScrollBar


#if SUPPORT_IME

// See Also Caret::Show for moving candidate window.

#include <imm.h>
#pragma comment(lib, "imm32.lib")

extern StyleValues g_DefaultStyle;
StyleValues g_rgpImeStyleConverted[2];
StyleValues g_pImeStyleInput;
StyleValues g_pImeStyleTargetConverted;
StyleValues g_pImeStyleTargetNotConverted;

#define GCS_COMPSTRATTR (GCS_COMPSTR | GCS_COMPATTR | GCS_CURSORPOS)

class Imc
{
    private: HWND m_hwnd;
    private: HIMC m_himc;

    public: Imc(HWND hwnd) :
        m_hwnd(hwnd),
        m_himc(::ImmGetContext(hwnd)) {}

    public: ~Imc()
    {
        if (NULL != m_himc) ::ImmReleaseContext(m_hwnd, m_himc);
    } // ~Imc

    public: operator HIMC() const { return m_himc; }
}; // Imc

// TextEditWindow::onImeComposition
void TextEditWindow::onImeComposition(LPARAM lParam)
{
    Imc imc(m_hwnd);
    if (imc == NULL) return;

    Edit::UndoBlock oUndo(GetSelection(), L"IME");

    char16 rgwch[1024];
    // If IME has result string, we can insert it into buffer.
    if (lParam & GCS_RESULTSTR)
    {
        // Remove previous composition string. If user inputs "Space",
        // IME set GCS_RESULTSTR without composition.
        if (m_lImeStart != m_lImeEnd)
        {
            GetSelection()->SetRange(m_lImeStart, m_lImeEnd);
            GetSelection()->SetText(L"", 0);
        } // if

        // Get result string
        long cwch = ::ImmGetCompositionString(
            imc,
            GCS_RESULTSTR,
            rgwch,
            sizeof(rgwch) ) / sizeof(char16);

        // Insert result string into buffer
        if (cwch >= 1)
        {
            GetSelection()->SetText(rgwch, cwch);
            GetSelection()->Collapse(Collapse_End);
            m_lImeEnd = GetSelection()->GetEnd();

            m_lImeStart = m_lImeEnd;
        }
    } // if GC_RESULTSTR

    // IME has composition string
    if ((lParam & GCS_COMPSTRATTR) == GCS_COMPSTRATTR)
    {
        // Remove previous composition string
        if (m_lImeStart != m_lImeEnd)
        {
            GetSelection()->SetRange(m_lImeStart, m_lImeEnd);
            GetSelection()->SetText(L"", 0);
            m_lImeEnd = m_lImeStart;
        } // if

        // Get composition string
        long cwch = ::ImmGetCompositionString(
            imc,
            GCS_COMPSTR,
            rgwch,
            sizeof(rgwch) ) / sizeof(char16);

        // Get composition attributes
        char rgbAttr[lengthof(rgwch)];
        long cbAttr = ::ImmGetCompositionString(
            imc,
            GCS_COMPATTR,
            rgbAttr,
            sizeof(rgbAttr) );
        if (cbAttr != cwch)
        {
            DEBUG_PRINTF("GCCS_COMPATTR\n");
            return;
        } // if

        long lCursor = ::ImmGetCompositionString(
            imc,
            GCS_CURSORPOS,
            NULL,
            0 );
        if (lCursor < 0)
        {
            DEBUG_PRINTF("GCCS_CURSORPOS\n");
            return;
        } // if

        uint32 rgnClause[100];
        ::ImmGetCompositionString(
            imc,
            GCS_COMPCLAUSE,
            rgnClause,
            sizeof(rgnClause) );

        GetSelection()->SetText(rgwch, cwch);
        GetSelection()->Collapse(Collapse_End);
        m_lImeEnd = GetSelection()->GetEnd();
        GetSelection()->SetRange(
            m_lImeStart + lCursor,
            m_lImeStart + lCursor );

        if (0 == g_pImeStyleInput.m_rgfMask)
        {
            // Converted[0]
            g_rgpImeStyleConverted[0].m_rgfMask =
                StyleValues::Mask_Decoration;

            g_rgpImeStyleConverted[0].m_eDecoration =
                TextDecoration_ImeInactiveA;

            // Converted[1]
            g_rgpImeStyleConverted[1].m_rgfMask =
                StyleValues::Mask_Decoration;

            g_rgpImeStyleConverted[1].m_eDecoration =
                TextDecoration_ImeInactiveB;

            // Input
            g_pImeStyleInput.m_rgfMask =
                StyleValues::Mask_Decoration;

            g_pImeStyleInput.m_eDecoration =
                TextDecoration_ImeInput;

            // Target Converted
            g_pImeStyleTargetConverted.m_rgfMask =
                StyleValues::Mask_Decoration;

            g_pImeStyleTargetConverted.m_eDecoration =
                TextDecoration_ImeActive;

            // Target Not Converted
            g_pImeStyleTargetNotConverted.m_rgfMask =
                StyleValues::Mask_Background |
                StyleValues::Mask_Color |
                StyleValues::Mask_Decoration;

            #if 0
                g_pImeStyleTargetNotConverted.m_crBackground =
                    g_DefaultStyle.GetColor();

                g_pImeStyleTargetNotConverted.m_crColor =
                    g_DefaultStyle.GetBackground();
            #else
                g_pImeStyleTargetNotConverted.m_crBackground =
                    GetSelection()->GetBackground();

                g_pImeStyleTargetNotConverted.m_crColor =
                    GetSelection()->GetColor();
            #endif

            g_pImeStyleTargetNotConverted.m_eDecoration =
                TextDecoration_None;
        } // if

        m_fImeTarget = false;
        Posn lEnd = m_lImeStart + cwch;
        Posn lPosn = m_lImeStart;
        int iClause = 0;
        int iConverted = 0;
        while (lPosn < lEnd)
        {
            StyleValues* pStyle;
            switch (rgbAttr[lPosn - m_lImeStart])
            {
            case ATTR_INPUT:
            case ATTR_INPUT_ERROR:
                pStyle = &g_pImeStyleInput;
                iConverted = 0;
                break;

            case ATTR_TARGET_CONVERTED:
                pStyle = &g_pImeStyleTargetConverted;
                m_fImeTarget = true;
                iConverted = 0;
                break;

            case ATTR_TARGET_NOTCONVERTED:
                pStyle = &g_pImeStyleTargetNotConverted;
                m_fImeTarget = true;
                iConverted = 0;
                break;

            case ATTR_CONVERTED:
                pStyle = &g_rgpImeStyleConverted[iConverted];
                iConverted = 1 - iConverted;
                break;

            default:
                pStyle = &g_pImeStyleInput;
                break;
            } // switch attr

            iClause += 1;
            Posn lNext = m_lImeStart + rgnClause[iClause];
            GetBuffer()->SetStyle(lPosn, lNext, pStyle);
            lPosn = lNext;
        } // for posn
    } // if GCS_COMPSTRATTR

    ////////////////////////////////////////////////////////////
    //
    // We have already insert composed string. So, we don't
    // need WM_IME_CHAR and WM_CHAR messages to insert
    // composed string.
    if (lParam & GCS_RESULTSTR)
    {
        m_fImeTarget = false;
        return;
    } // if

    // Composition was canceled.
    if (0 == lParam)
    {
        m_fImeTarget = false;

        // Remove previous composition string
        GetSelection()->SetRange(m_lImeStart, m_lImeEnd);
        GetSelection()->SetText(L"", 0);

        // if (m_fCancelButLeave)
        {
            long cwch = ::ImmGetCompositionString(
                imc,
                GCS_COMPSTR,
                rgwch,
                sizeof(rgwch) ) / sizeof(char16);
            if (cwch >= 1)
            {
                GetSelection()->SetText(rgwch, cwch);
            }
        }

        m_lImeEnd = m_lImeStart;
    } // if
} // TextEditWindow::onImeComposition

//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::Reconvert
//
// Note: 
//  o IME2000 ignores string after newline.
//  o We should limit number of characters to be reconverted.
//
void TextEditWindow::Reconvert(Posn lStart, Posn lEnd)
{
    BOOL fSucceeded;

    uint cb = setReconvert(NULL, lStart, lEnd);
    if (0 == cb) return;

    char* pb = new char[cb];
    if (NULL == pb) return;

    RECONVERTSTRING* p = reinterpret_cast<RECONVERTSTRING*>(pb);

    setReconvert(p, lStart, lEnd);

    Imc imc(m_hwnd);

    fSucceeded = ::ImmSetCompositionString(
        imc,
        SCS_QUERYRECONVERTSTRING,
        p,
        cb,
        NULL,
        0 );
    unless (fSucceeded)
    {
        DEBUG_PRINTF("SCS_QUERYRECONVERTSTRING\n");
        goto exit;
    }

    m_lImeStart  = lStart + p->dwCompStrOffset / 2;
    m_lImeEnd    = m_lImeStart + p->dwCompStrLen;
    m_fImeTarget = true;

    fSucceeded = ::ImmSetCompositionString(
        imc,
        SCS_SETRECONVERTSTRING,
        p,
        cb,
        NULL,
        0 );
    unless (fSucceeded)
    {
        DEBUG_PRINTF("SCS_SETRECONVERTSTRING\n");
        goto exit;
    }

  exit:
    delete[] pb;
} // TextEditWindow::Reconvert

//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::setReconvert
//
uint TextEditWindow::setReconvert(RECONVERTSTRING* p, Posn lStart, Posn lEnd)
{
    Count cwch = lEnd - lStart;
    when (0 == cwch) return 0;

    uint cb = sizeof(RECONVERTSTRING) + sizeof(char16) * (cwch + 1);

    if (NULL == p) return cb;

    p->dwSize            = cb;
    p->dwVersion         = 0;
    p->dwStrLen          = cwch;
    p->dwStrOffset       = sizeof(RECONVERTSTRING);
    p->dwCompStrLen      = cwch;    // # of characters
    p->dwCompStrOffset   = 0;       // byte offset
    p->dwTargetStrLen    = p->dwCompStrLen;
    p->dwTargetStrOffset = p->dwCompStrOffset;

    char16* pwch = reinterpret_cast<char16*>(
        reinterpret_cast<char*>(p) + p->dwStrOffset );

    GetBuffer()->GetText(pwch, lStart, lEnd);

    pwch[cwch] = 0;

    return cb;
} // TextEditWindow::Reconvert

//////////////////////////////////////////////////////////////////////
//
// TextEditWindow::showImeCaret
//  Set left top coordinate of IME candiate window.
//
BOOL TextEditWindow::showImeCaret(SIZE sz, POINT pt)
{
    Imc imc(m_hwnd);
    if (imc == NULL) return FALSE;

    CANDIDATEFORM oCF;
    oCF.dwIndex         = 0;
    oCF.dwStyle         = CFS_EXCLUDE;
    oCF.ptCurrentPos.x  = pt.x;
    oCF.ptCurrentPos.y  = pt.y + sz.cy;

    oCF.rcArea.left     = pt.x;
    oCF.rcArea.top      = pt.y;
    oCF.rcArea.right    = pt.x;
    oCF.rcArea.bottom   = pt.y + sz.cy;

    return ::ImmSetCandidateWindow(imc, &oCF);
} // TextEditWindow::showImeCaret

#endif // SUPPORT_IME
