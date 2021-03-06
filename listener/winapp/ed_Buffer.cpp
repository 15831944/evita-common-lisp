#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - winapp - Edit Buffer
// listener/winapp/ed_buffer.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/ed_Buffer.cpp#5 $
//
#define DEBUG_STYLE 0
#include "./ed_Buffer.h"

#include "./ed_Interval.h"
#include "./ed_Range.h"
#include "./ed_Undo.h"

#include "./mode_PlainText.h"

namespace Edit
{

/// <summary>
///   Construct a buffer object.
/// </summary>
/// <param name="pwszName">A buffername in C-string.</param>
/// <param name="pMode">A editing mode.</param>
Buffer::Buffer(const char16* pwszName, Mode* pMode) :
    m_eState(State_Ready),
    m_fReadOnly(false),
    m_fUndo(true),
    m_nCharTick(1),
    m_nModfTick(1),
    m_nSaveTick(1),
    m_pFirstRange(NULL),
    m_pMode(pMode)
{
    m_hObjHeap = ::HeapCreate(HEAP_NO_SERIALIZE, 0, 0);

    m_pUndo = new(m_hObjHeap) UndoManager(this);

    {
        Interval* pIntv = newInterval(0, 1);
        pIntv->SetStyle(&g_DefaultStyle);
        m_oIntervalTree.Insert(pIntv);
        m_oIntervals.Append(pIntv);
    }

    ::lstrcpy(m_wszName, pwszName);

    if (NULL == m_pMode)
    {
        m_pMode = ModeFactory::Get(this)->Create(this);
    }
} // Buffer::Buffer

/// <summary>
///   Destruct this buffer object.
/// </summary>
Buffer::~Buffer()
{
    foreach (EnumRange, oEnum, this)
    {
        oEnum.Get()->m_pBuffer = NULL;
    } // for each range

    if (NULL != m_hObjHeap)
    {
        ::HeapDestroy(m_hObjHeap);
    }
} // Buffer::~Buffer

/// <summary>
///   Returns true if this buffer redo-able.
/// </summary>
bool Buffer::CanRedo() const
{
    if (! m_fUndo) return false;
    if (NULL == m_pUndo) return false;
    return m_pUndo->CanRedo();
} // Buffer::CanRedo

/// <summary>
///   Returns true if this buffer undo-able.
/// </summary>
bool Buffer::CanUndo() const
{
    if (! m_fUndo) return false;
    if (NULL == m_pUndo) return false;
    return m_pUndo->CanUndo();
} // Buffer::CanUndo


//////////////////////////////////////////////////////////////////////
//
// getCharClass
//  Returns ANSIC C/POSIX(LC_TYPE)
//
// See WinNls.h for C1_xxx
//  C1_UPPER    0x001
//  C1_LOWER    0x002
//  C1_DIGIT    0x004
//  C1_SPACE    0x008
//  C1_PUNCT    0x010
//  C1_CTRL     0x020
//  C1_BLANK    0x040
//  C1_XDIGIT   0x080
//  C1_ALPHA    0x100
//  C1_DEFINED  0x200
//
//    Code    Name      Type
//  +-------+---------+-------------------------------
//  | 0x09  | TAB     | C1_SPACE + C1_CTRL + C1_BLANK
//  | 0x0A  | LF      | C1_SPACE + C1_CTRL
//  | 0x0D  | CR      | C1_SPACE + C1_CTRL
//  | 0x20  | SPACE   | C1_SPACE + C1_BLANK
//  +-------+---------+-------------------------------
//
static int getCharClass(char16 wch)
{
    WORD wType;
    if (! ::GetStringTypeW(CT_CTYPE1, &wch, 1, &wType)) return 0;
    int iMask = 0;
    //iMask |= C1_ALPHA;
    iMask |= C1_BLANK;
    iMask |= C1_CNTRL;
    //iMask |= C1_DEFINED;
    iMask |= C1_PUNCT;
    iMask |= C1_SPACE;
    return wType & iMask;
} // getCharClass

/// <summary>
///   Computes end position of specified unit.
/// </summary>
/// <param name="eUnit">An unit for computing end position.</param>
/// <param name="lPosn">A position starting computation.</param>
/// <returns>An end position of specified unit.</returns>
Posn Buffer::ComputeEndOf(Unit eUnit, Posn lPosn) const
{
    ASSERT(IsValidPosn(lPosn));

    switch (eUnit)
    {
    case Unit_Buffer:
        lPosn = GetEnd();
        break;

    case Unit_Paragraph:
        while (lPosn < GetEnd())
        {
            if (0x0A == GetCharAt(lPosn))
            {
                break;
            } // if
            lPosn += 1;
        } // while
        break;

    case Unit_Word:
        if (lPosn >= GetEnd())
        {
            lPosn = GetEnd();
        }
        else
        {
            int iClass1 = getCharClass(GetCharAt(lPosn));
            if (iClass1 & C1_BLANK)
            {
                // We are on whiespace.
                return lPosn;
            } // if

            // Move to end of word.
            while (lPosn < GetEnd())
            {
                int iClass2 = getCharClass(GetCharAt(lPosn));
                if (iClass1 != iClass2)
                {
                    break;
                } // if
                lPosn += 1;
            } // while
        } // if
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch unit

    return lPosn;
} // Buffer::ComputeEndOf


//////////////////////////////////////////////////////////////////////
//
// Buffer::ComputeMotion
//
// Word Motion:
//                      forward             backward
//  th|is is a word.    this |is a word.    |this is a word.
//  this |is a word.    this is| a word.    |this is a word.
//
Count Buffer::ComputeMotion(Unit eUnit, Count n, Posn* inout_lPosn) const
{
    Posn lPosn = *inout_lPosn;
    ASSERT(IsValidPosn(lPosn));

    switch (eUnit)
    {
    case Unit_Char:
    {
        if (n > 0)
        {
            *inout_lPosn = min(GetEnd(), lPosn + n);
            return *inout_lPosn - lPosn;
        }
        else if (n < 0)
        {
            *inout_lPosn = max(GetStart(), lPosn + n);
            return lPosn - *inout_lPosn;
        }

        return 0;
    } // char

    case Unit_Paragraph:
    {
        if (n > 0)
        {
            Count k;
            for (k = 0; k < n; k++)
            {
                lPosn = ComputeEndOf(Unit_Paragraph, lPosn);
                if (lPosn == GetEnd()) break;
                lPosn += 1;
            } // for k
            *inout_lPosn = lPosn;
            return k;
        }
        else if (n < 0)
        {
            n = -n;
            Count k;
            for (k = 0; k < n; k++)
            {
                lPosn = ComputeStartOf(Unit_Paragraph, lPosn);
                if (lPosn == GetStart()) break;
                lPosn -= 1;
            } // for k
            *inout_lPosn = lPosn;
            return k;
        } // if

        return 0;
    } // line

    case Unit_Word:
    {
        Count k = 0;
        if (n > 0)
        {
            if (lPosn == GetEnd()) return 0;
            do
            {
                int iClass1 = getCharClass(GetCharAt(lPosn));
                // Skip current word
                for (;;)
                {
                    lPosn += 1;
                    if (lPosn == GetEnd()) 
                    {
                        k += 1;
                        goto exit;
                    }

                    int iClass2 = getCharClass(GetCharAt(lPosn));
                    if (iClass1 == iClass2) continue;

                    while (iClass2 & C1_BLANK)
                    {
                        lPosn += 1;
                        if (lPosn == GetEnd()) break;
                        iClass2 = getCharClass(GetCharAt(lPosn));
                    } // while
                    break;
                } // for
                k += 1;
            } while (k < n);
        }
        else if (n < 0)
        {
            if (lPosn == GetStart()) return 0;
            do
            {
                lPosn -= 1;
                int iClass1 = getCharClass(GetCharAt(lPosn));
                // Skip current work
                for (;;)
                {
                    if (lPosn == GetStart()) 
                    {
                        k += 1;
                        goto exit;
                    }

                    lPosn -= 1;
                    int iClass2 = getCharClass(GetCharAt(lPosn));
                    if (iClass1 == iClass2) continue;
                    if (iClass1 & C1_BLANK)
                    {
                        while (iClass2 & C1_BLANK)
                        {
                            if (lPosn == GetStart()) goto exit;
                            lPosn -= 1;
                            iClass2 = getCharClass(GetCharAt(lPosn));
                        } // while
                    } // if
                    lPosn += 1;
                    break;
                } // for
                k += 1;
            } while (k < n);
        } // if
      exit:
        *inout_lPosn = lPosn;
        return k;
    } // word

    default:
        // FIXME 2007-05-09 Should we signal unknown unit for
        // Buffer::ComputeMotion.
        CAN_NOT_HAPPEN();
    } // switch unit
} // Buffer::ComputeMotion


//////////////////////////////////////////////////////////////////////
//
// Buffer::ComputeStartOf
//
Posn Buffer::ComputeStartOf(Unit eUnit, Posn lPosn) const
{
    ASSERT(IsValidPosn(lPosn));

    switch (eUnit)
    {
    case Unit_Buffer:
        lPosn = GetStart();
        break;

    case Unit_Paragraph:
        while (lPosn > 0)
        {
            lPosn -= 1;

            if (0x0A == GetCharAt(lPosn))
            {
                lPosn += 1;
                break;
            } // if
        } // while
        break;

    case Unit_Word:
        if (lPosn <= 0)
        {
            lPosn = 0;
        }
        else
        {
            int iClass1 = getCharClass(GetCharAt(lPosn));
            if (iClass1 & C1_BLANK)
            {
                // We are on whiespace.
                return lPosn;
            } // if

            // Move to start of word.
            while (lPosn > 0)
            {
                lPosn -= 1;
                int iClass2 = getCharClass(GetCharAt(lPosn));
                if (iClass1 != iClass2)
                {
                    lPosn += 1; // back to previous character.
                    break;
                } // if
            } // while
        } // if
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch eUnit
    return lPosn;
} // Buffer::ComputeStartOf


//////////////////////////////////////////////////////////////////////
//
// Buffer::ComputeWhile
//
Count Buffer::ComputeWhile(
    const char16*   pwszSet,
    Count           n,
    Posn*           inout_lPosn ) const
{
    Posn lStart = *inout_lPosn;
    if (n > 0)
    {
        Posn lPosn;
        for (lPosn = lStart; lPosn < GetEnd(); lPosn += 1)
        {
            char16 wch = GetCharAt(lPosn);
            if (NULL == ::lstrchrW(pwszSet, wch))
            {
                break;
            } // if

            n -= 1;
            if (0 == n)
            {
                break;
            }
        } // while

        *inout_lPosn = lPosn;
        return lPosn - lStart;
    } // if

    if (n < 0)
    {
        Posn lPosn = lStart;
        while (lPosn > 0)
        {
            lPosn -= 1;
            char16 wch = GetCharAt(lPosn);
            if (NULL == ::lstrchrW(pwszSet, wch))
            {
                lPosn += 1;
                break;
            } // if

            n += 1;
            if (0 == n)
            {
                break;
            }
        } // while

        *inout_lPosn = lPosn;
        return lStart - lPosn;
    } // if

    return 0;
} // Buffer::ComputeWhile


//////////////////////////////////////////////////////////////////////
//
// Buffer::ComputeWhile
//
Count Buffer::ComputeWhile(uint nMask, Count n, Posn* inout_lPosn) const
{
    Posn lStart = *inout_lPosn;
    if (n > 0)
    {
        Posn lPosn;
        for (lPosn = lStart; lPosn < GetEnd(); lPosn += 1)
        {
            int iClass = getCharClass(GetCharAt(lPosn));
            if (0 == (iClass & nMask))
            {
                break;
            } // if
        } // while

        *inout_lPosn = lPosn;
        return lPosn - lStart;
    } // if

    if (n < 0)
    {
        Posn lPosn = lStart;
        while (lPosn > 0)
        {
            lPosn -= 1;
            int iClass = getCharClass(GetCharAt(lPosn));
            if (0 == (iClass & nMask))
            {
                lPosn += 1;
                break;
            } // if
        } // while

        *inout_lPosn = lPosn;
        return lStart - lPosn;
    } // if

    return 0;
} // Buffer::ComputeWhile


#if 0
//////////////////////////////////////////////////////////////////////
//
// Buffer::CreateInterval
//
Interval* Buffer::CreateInterval(
    Posn    lStart,
    Posn    lEnd,
    int     nZ)
{
    return addInterval(new(m_hObjHeap) Interval(lStart, max(lStart, lEnd), nZ));
} // Buffer::CreateInterval
#endif


//////////////////////////////////////////////////////////////////////
//
// Buffer::CreateRange
//
Range* Buffer::CreateRange(Posn lStart, Posn lEnd)
{
    Range* pRange = new(m_hObjHeap) Range(
        this,
        Range::Kind_Range,
        lStart,
        max(lStart, lEnd) );

    return InternalAddRange(pRange);
} // Buffer::CreateRange


//////////////////////////////////////////////////////////////////////
//
// Buffer::CreateRange
//
Range* Buffer::CreateRange(Range* pRange)
{
    return CreateRange(pRange->GetStart(), pRange->GetEnd());
} // Buffer::CreateRange


//////////////////////////////////////////////////////////////////////
//
// Buffer::Delete
//
// Description:
//  Deletes characters between lStart and lEnd and returns number of
//  characters deleted.
//
Count Buffer::Delete(Posn lStart, Posn lEnd)
{
    if (IsReadOnly()) return 0;
    if (IsNotReady()) return 0;

    lStart = max(lStart, 0);
    lEnd   = min(lEnd, GetEnd());

    if (lEnd <= lStart) return 0;

    if (m_fUndo && NULL != m_pUndo)
    {
        m_pUndo->CheckPoint();
        m_pUndo->RecordDelete(lStart, lEnd);
    } // if

    InternalDelete(lStart, lEnd);

    onChange();

    return lEnd - lStart;
} // Buffer::Delete

// Buffer::GetDefaultStyle
const StyleValues* Buffer::GetDefaultStyle() const
{
    return m_oIntervals.GetLast()->GetStyle();
} // Buffer::GetDefaultStyle


// Buffer::GetStyleAt
const StyleValues* Buffer::GetStyleAt(Posn lPosn) const
{
    return GetIntervalAt(lPosn)->GetStyle();
} // Buffer::GetStyleAt


//////////////////////////////////////////////////////////////////////
//
// Buffer::GetUndoSize
//
size_t Buffer::GetUndoSize() const
{
    if (NULL == m_pUndo) return 0;
    return m_pUndo->GetSize();
} // Buffer::GetUndoSize


//////////////////////////////////////////////////////////////////////
//
// Buffer::Insert
//
Count Buffer::Insert(Posn lPosn, char16 wch, Count n)
{
    ASSERT(IsValidPosn(lPosn));

    if (IsReadOnly()) return 0;
    if (IsNotReady()) return 0;

    if (n <= 0) return 0;
    lPosn = min(lPosn, GetEnd());

    InternalInsert(lPosn, wch, n);

    onChange();

    if (m_fUndo && NULL != m_pUndo)
    {
        m_pUndo->CheckPoint();
        m_pUndo->RecordInsert(lPosn, lPosn + n);
    } // if

    return n;
} // Buffer::Insert


//////////////////////////////////////////////////////////////////////
//
// Buffer::Insert
//
Count Buffer::Insert(Posn lPosn, const char16* pwch, Count n)
{
    ASSERT(IsValidPosn(lPosn));

    if (IsReadOnly()) return 0;
    if (IsNotReady()) return 0;

    if (n <= 0) return 0;
    lPosn = min(lPosn, GetEnd());

    InternalInsert(lPosn, pwch, n);

    onChange();

    if (m_fUndo && NULL != m_pUndo)
    {
        m_pUndo->CheckPoint();
        m_pUndo->RecordInsert(lPosn, lPosn + n);
    } // if

    return n;
} // Buffer::Insert


//////////////////////////////////////////////////////////////////////
//
// Buffer::InternalAddRange
//
Range* Buffer::InternalAddRange(Range* pRange)
{
    pRange->m_pNext = m_pFirstRange;
    pRange->m_pPrev = NULL;

    if (NULL != m_pFirstRange)
    {
        m_pFirstRange->m_pPrev = pRange;
    }

    m_pFirstRange = pRange;
    return pRange;
} // Buffer::InternalAddRange


//////////////////////////////////////////////////////////////////////
//
// Buffer::InternalDelete
//
void Buffer::InternalDelete(Posn lStart, Posn lEnd)
{
    Count n = deleteChars(lStart, lEnd);
    m_nModfTick += 1;
    relocate(lStart, -n);
} // Buffer::InternalDelete


//////////////////////////////////////////////////////////////////////
//
// Buffer::InternalInsert
//
void Buffer::InternalInsert(Posn lPosn, char16 wch, Count n)
{
    insert(lPosn, wch, n);
    m_nModfTick += 1;
    relocate(lPosn, n);
} // Buffer::InternalInsert


//////////////////////////////////////////////////////////////////////
//
// Buffer::InternalInsert
//
void Buffer::InternalInsert(Posn lPosn, const char16* pwch, Count n)
{
    insert(lPosn, pwch, n);
    m_nModfTick += 1;
    relocate(lPosn, n);
} // Buffer::InternalInsert


// Buffer::InternalRemoveRange
void Buffer::InternalRemoveRange(Range* pRange)
{
    Range* pNext = pRange->m_pNext;
    Range* pPrev = pRange->m_pPrev;

    if (NULL != pNext)
    {
        pNext->m_pPrev = pPrev;
    } // if

    if (NULL == pPrev)
    {
        m_pFirstRange = pNext;
    }
    else
    {
        pPrev->m_pNext = pNext;
    } // if
} // Buffer::InternalRemoveRange


//////////////////////////////////////////////////////////////////////
//
// Buffer::IsNotReady
//
bool Buffer::IsNotReady() const
{
    return m_eState != State_Ready;
} // Buffer::IsNotReady


//////////////////////////////////////////////////////////////////////
//
// Buffer::onChange
//
void Buffer::onChange()
{
    if (m_nCharTick < m_nSaveTick)
    {
        m_nCharTick = m_nSaveTick;
    }

    m_nCharTick += 1;
} // Buffer::onChange

/// <summary>
///   Put proeprty.
/// </summary>
void Buffer::PutProperty(Buffer::Property* pProp)
{
    m_oProperties.Append(pProp);
} // Buffer::PutProperty

/// <summary>
///   Redo at specified position.
/// </summary>
/// <param name="p">A position doing redo.</param>
/// <param name="n">Number of times doing redo.</param>
/// <returns>A position after redo.</returns>
/// <seealso cref="Buffer::Undo"/>
Posn Buffer::Redo(Posn lPosn, Count n)
{
    when (IsReadOnly()) return -1;
    when (! m_fUndo) return -1;
    when (NULL == m_pUndo) return -1;
    return m_pUndo->Redo(lPosn, n);
} // Buffer::Redo


//////////////////////////////////////////////////////////////////////
//
// Buffer::relocate
//
void Buffer::relocate(Posn lPosn, Count iDelta)
{
    // Note: We should scan range backward to terminate faster.
    foreach (EnumRange, oEnum, this)
    {
        Range* pRunner = oEnum.Get();

        if (pRunner->m_lStart > lPosn)
        {
            pRunner->m_lStart += iDelta;

            if (pRunner->m_lStart < lPosn)
            {
                pRunner->m_lStart = lPosn;
            }
            else if (pRunner->m_lStart > GetEnd())
            {
                pRunner->m_lStart = GetEnd();
            } // if
        } // if

        if (pRunner->m_lEnd > lPosn)
        {
            pRunner->m_lEnd += iDelta;

            if (pRunner->m_lEnd < lPosn)
            {
                pRunner->m_lEnd = lPosn;
            }
            else if (pRunner->m_lEnd > GetEnd())
            {
                pRunner->m_lEnd = GetEnd();
            } // if
        } // if
    } // for each range

    // Remove empty interval
    if (iDelta < 0)
    {
        Posn lBufEnd1 = GetEnd() + 1;

        // Note: We should scan interval backward to terminate faster.
        EnumInterval oEnum(this);
        while (! oEnum.AtEnd())
        {
            Interval* pRunner = oEnum.Get();
            oEnum.Next();

            Posn lStart = pRunner->m_lStart;
            Posn lEnd   = pRunner->m_lEnd;

            when (lEnd <= lPosn) continue;

            if (lStart > lPosn)
            {
                lStart += iDelta;

                if (lStart < lPosn)
                {
                    lStart = lPosn;
                }
                else if (lStart > lBufEnd1)
                {
                    lStart = lBufEnd1;
                } // if
            } // if

            if (lEnd > lPosn)
            {
                lEnd += iDelta;

                if (lEnd < lPosn)
                {
                    lEnd = lPosn;
                }
                else if (lEnd > lBufEnd1)
                {
                    lEnd = lBufEnd1;
                } // if
            } // if

            if (lStart == lEnd)
            {
                m_oIntervals.Delete(pRunner);
                m_oIntervalTree.Delete(pRunner);

                DEBUG_PRINTF("destroyObject: interval [%d,%d] @ posn=%d%d\n",
                    pRunner->m_lStart, pRunner->m_lEnd, lPosn, iDelta );

                destroyObject(pRunner);
            }
        } // for each interval
    } // if

    // Relocate intervals
    {
        Posn lBufEnd1 = GetEnd() + 1;
        foreach (EnumInterval, oEnum, this)
        {
            Interval* pRunner = oEnum.Get();

            Posn lStart = pRunner->m_lStart;
            Posn lEnd   = pRunner->m_lEnd;

            if (lStart > lPosn)
            {
                lStart += iDelta;

                if (lStart < lPosn)
                {
                    lStart = lPosn;
                }
                else if (lStart > lBufEnd1)
                {
                    lStart = lBufEnd1;
                } // if
            } // if

            if (lEnd > lPosn)
            {
                lEnd += iDelta;

                if (lEnd < lPosn)
                {
                    lEnd = lPosn;
                }
                else if (lEnd > lBufEnd1)
                {
                    lEnd = lBufEnd1;
                } // if
            } // if

            ASSERT(lStart < lEnd);
            pRunner->m_lStart = lStart;
            pRunner->m_lEnd   = lEnd;
        } // for each interval
    }

    // Update change trackers
    {
        Posn lChangeStart = lPosn;
        Posn lChangeEnd   = lPosn;
        
        if (iDelta > 0)
        {
            lChangeEnd += iDelta;
        }

        foreach (ChangeTrackers::Enum, oEnum, &m_oChangeTrackers)
        {
            ChangeTracker* pRunner = oEnum.Get();

            pRunner->m_lStart = min(pRunner->m_lStart, lChangeStart);
            pRunner->m_lEnd   = max(pRunner->m_lEnd, lChangeEnd),

            pRunner->m_lEnd = min(pRunner->m_lEnd, GetEnd());
        } // for each tracker
    }
} // Buffer::relocate

/// <summary>
///  Replace character at specified position.
/// </summary>
/// <param name="lPosn">A position to replace.</param>
/// <param name="wch">A character to be replaced.</param>
/// <returns>True if successfully replaced.</returns>
bool Buffer::SetCharAt(Posn lPosn, char16 wch)
{
    if (1 != Insert(lPosn, wch, 1))
    {
        return false;
    }
    
    return 1 == Delete(lPosn + 1, lPosn + 2);
} // Buffer::SetCharAt

/// <summary>
///  Set editting mode.
/// </summary>
/// <param name="pMode">A Mode object.</param>
void Buffer::SetMode(Mode* pMode)
{
    delete m_pMode;
    m_pMode = pMode;
} // Buffer::SetMode

/// <summary>
///   Undo at specified position.
/// </summary>
/// <param name="lPosn">A position doing undo.</param>
/// <param name="n">Number of times doing undo.</param>
/// <returns>A position after undo.</returns>
/// <seealso cref="Buffer::Redo"/>
Posn Buffer::Undo(Posn lPosn, Count n)
{
    if (IsReadOnly()) return -1;
    if (! m_fUndo) return -1;
    if (NULL == m_pUndo) return -1;
    return m_pUndo->Undo(lPosn, n);
} // Buffer::Undo

} // Edit
