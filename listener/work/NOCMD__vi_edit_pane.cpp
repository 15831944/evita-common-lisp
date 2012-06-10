#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - winapp - winmain
// listener/winapp/winmain.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/console/console.cpp 12 2006-01-26 01:14:00 yosi $
//
//  Key Combination and Method
//
//      Key Combination | Method
//      ----------------+--------------------
//      Ctrl+C          | Selection.Copy
//      Ctrl+V          | Selection.Paste
//      Ctrl+X          | Selection.Cut
//      Ctrl+Y          | Buffer.Redo
//      Ctrl+Z          | Buffer.Undo
//      Ctrl+Down       | Selection.MoveDown  Paragraph
//      Ctrl+Left       | Selection.MoveLeft  Word
//      Ctrl+PageDn     | Selection.MoveDown  Window
//      Ctrl+PageUp     | Selection.MoveUp    Window
//      Ctrl+Right      | Selection.MoveRight Word
//      Ctrl+Up         | Selection.MoveUp    Paragraph
//      Backspace       | Selection.Delete    -1
//      Delete          | Selection.Delete    1
//      Down            | Selection.MoveDown  WindowLine
//      End             | Selection.EndKey
//      Enter           | Selection.TypeText
//      Esc             | - not assigned -
//      Home            | Selection.HomeKey
//      Insert          | - not assigned -
//      Left            | Selection.MoveLeft  Char
//      PageUp          | Selection.MoveUp    Screen
//      Right           | Selection.MoveRight Char
//      Tab             | Selection.TabKey (NYI)
//      Up              | Selection.MoveUp    WindowLine
//      <Graphic>       | Selection.TypeText
//      Shift+Delete    | Selection.Cut
//      Shift+Insert    | Selection.Paste
//
#define DEBUG_CARET     0
#define DEBUG_FOCUS     0
#define DEBUG_KEY       0
#define DEBUG_REDRAW    0
#include "./vi_edit_pane.h"

#include "./ed_buffer.h"

#include "./vi_selection.h"
#include "./vi_util.h"

//////////////////////////////////////////////////////////////////////
//
// EditPane ctor
//
EditPane::EditPane(Edit::Buffer* pBuffer, Posn lStart) :
    TextPane(pBuffer, lStart) {}


//////////////////////////////////////////////////////////////////////
//
// EditPane::OnIdle
//
bool EditPane::OnIdle()
{
    {
        Buffer* pBuffer = GetBuffer();
        int nCharTick = GetBuffer()->IncCharTick(0);
        //if (m_nCharTick != nCharTick)
        {
            char16 wsz[100];
            ::wsprintfW(wsz, L"Untitled%s, %d,%d/%d",
                pBuffer->IsModified() ? L"*" : L"",
                GetSelection()->GetStart(),
                GetSelection()->GetEnd(),
                pBuffer->GetEnd() );

            int cwch = ::lstrlenW(wsz);
            if (0 != ::memcmp(m_rgwszTitle, wsz, cwch))
            {
                m_cwchTitle = cwch;
                myCopyMemory(m_rgwszTitle, wsz, sizeof(char16) * cwch);
                ::SetWindowText(::GetParent(m_hwnd), wsz);
            }
            m_nCharTick = nCharTick;
        } // if
    }

    return TextPane::OnIdle();
} // EditPane::OnIdle


//////////////////////////////////////////////////////////////////////
//
// EditPane::onKeyDown
//
void EditPane::onKeyDown(uint nVKey, BOOL fRepeat)
{
    bool fControl = 0 != (nVKey & MY_VK_CONTROL);
    bool fExtend  = 0 != (nVKey & MY_VK_SHIFT);
    Count lCount = fRepeat ? 2 : 1;

    switch (nVKey & 0xFF)
    {
    //case 0x08:  m_pSelection->Delete(Unit_Char, -1); break;
    case VK_DELETE:
        if (fExtend)
        {
            if (fControl)
            {
                // Ctrl+Shift+Del = Copy
                m_pSelection->Copy();
            }
            else
            {
                // Shift+Del = Cut
                m_pSelection->Cut();
            }
        }
        else
        {
            m_pSelection->Delete(
                fControl ? Unit_Word : Unit_Char,
                lCount );
        }
        break;

    case VK_DOWN:
        if (fControl)
        {
            SmallScroll(0, lCount);
        }
        else
        {
            m_pSelection->MoveDown(Unit_WindowLine, lCount, fExtend);
        }
        break;

    case VK_END:
        m_pSelection->EndKey(
            fControl ? Unit_Buffer : Unit_WindowLine,
            fExtend );
        break;

    case VK_HOME:
        m_pSelection->HomeKey(
            fControl ? Unit_Buffer : Unit_WindowLine,
            fExtend );
        break;

    case VK_INSERT:
        if (fExtend)
        {
            // Shift+Ins
            m_pSelection->Paste();
        }
        break;

    case VK_LEFT:
        m_pSelection->MoveLeft(
            fControl ? Unit_Word : Unit_Char,
            lCount,
            fExtend );
        break;

    case VK_NEXT: // [PageDn]
        m_pSelection->MoveDown(
            fControl ? Unit_Window : Unit_Screen,
            lCount,
            fExtend );
        break;

    case VK_PRIOR:  // [PageUp]
        m_pSelection->MoveUp(
            fControl ? Unit_Window : Unit_Screen,
            lCount,
            fExtend );
        break;

    case VK_RIGHT:
        m_pSelection->MoveRight(
            fControl ? Unit_Word : Unit_Char,
            lCount,
            fExtend );
        break;

    case VK_UP:
        if (fControl)
        {
            SmallScroll(0, -lCount);
        }
        else
        {
            m_pSelection->MoveUp(Unit_WindowLine, lCount, fExtend);
        }
        break;
    } // switch vkey
} // EditPane::onKeyDown


//////////////////////////////////////////////////////////////////////
//
// EditPane::onMessage
//
LRESULT EditPane::onMessage(
    UINT    uMsg,
    WPARAM  wParam,
    LPARAM  lParam )
{
    switch (uMsg)
    {
    case WM_CHAR:
    {
        char16 wch = static_cast<WCHAR>(wParam);
        #if DEBUG_KEY
        {
            DEBUG_PRINTF(L"WM_CHAR %04X\r\n", wch);
        }
        #endif // DEBUG_KEY

        switch (wch)
        {
        case 'A' - '@':
            m_pSelection->SetRange(0, GetBuffer()->GetEnd());
            m_pSelection->SetStartIsActive(false);
            break;

        case 'C' - '@':     // Ctrl+C
            m_pSelection->Copy();
            break;

        case 'H' - '@': // Ctrl+H
            m_pSelection->Delete(Unit_Char, -1);
            break;

        case 'M' - '@': // Ctrl+M
            wch = 0x0A;
            goto graph_key;

        case 'V' - '@':     // Ctrl+V
            m_pSelection->Paste();
            break;

        case 'X' - '@':     // Ctrl+X
            m_pSelection->Cut();
            break;

        case 'Y' - '@':     // Ctrl+Y
        {
            Posn lPosn = m_pSelection->GetBuffer()->Redo(
                m_pSelection->GetActivePosn() );
            if (lPosn >= 0) m_pSelection->MoveTo(lPosn);
            break;
        } // Ctrl+Y

        case 'Z' - '@':     // Ctrl+Z
        {
            Posn lPosn = m_pSelection->GetBuffer()->Undo(
                m_pSelection->GetActivePosn() );
            if (lPosn >= 0) m_pSelection->MoveTo(lPosn);
            break;
        } // Ctrl+Z

        case 0x7F:  // Ctrl+Backspace
            m_pSelection->Delete(Unit_Word, -1);
            break;

        default:
        graph_key:
            m_pSelection->TypeText(&wch, 1);
            switch (wch)
            {
            case 0x29:
                blinkOpenParen(0x28, 0x29);
                break;
            case 0x5D:
                blinkOpenParen(0x5B, 0x5D);
                break;
            case 0x7D:
                blinkOpenParen(0x7B, 0x7D);
                break;
            } // switch wch
            break;
        } // switch wch
        break;
    } // WM_CAHR

    case WM_KEYDOWN:
    {
        #if DEBUG_KEY
        {
            DEBUG_PRINTF(L"WM_KEYDOWN vkey=0x%04X lParam=%08X\r\n",
                wParam, lParam );
        }
        #endif // DEBUG_KEY
        uint nKey = static_cast<uint>(wParam);
        if (::GetKeyState(VK_CONTROL) < 0) nKey |= MY_VK_CONTROL;
        if (::GetKeyState(VK_SHIFT)   < 0) nKey |= MY_VK_SHIFT;
        onKeyDown(nKey, HIWORD(lParam) & KF_REPEAT);
        break;
    } // WM_KEYDOWN

    } // switch uMsg
    return TextPane::onMessage(uMsg, wParam, lParam);
} // EditPane::onMessage
