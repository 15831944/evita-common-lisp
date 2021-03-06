//////////////////////////////////////////////////////////////////////////////
//
// Editor - Buffer
// listener/winapp/ed_Buffer.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/ed_Buffer.h#9 $
//
#if !defined(INCLUDE_edit_Buffer_h)
#define INCLUDE_edit_Buffer_h

#include "./ed_BinTree.h"
#include "./li_util.h"

#include "./IStringCursor.h"
#include "./ed_BufferCore.h"
#include "./ed_Style.h"

namespace Command
{
    class KeyBindEntry;
} // Command

namespace Edit
{

class Buffer;
class Interval;
class Mode;
class Range;
class UndoManager;

/// <summary>
//   Represents file related properties.
/// </summary>
class FileFeatures
{
    public: enum Obsolete
    {
        Obsolete_No,

        Obsolete_Checking,
        Obsolete_Ignore,
        Obsolete_Unknown,
        Obsolete_Yes,
    }; // Obsolete

    enum Limits
    {
        k_tickFileCheck = 1000 * 60,    // one minute
    }; // Limits

    protected: NewlineMode  m_eNewline;
    protected: Obsolete     m_eObsolete;
    protected: bool         m_fNoSave;
    protected: FileTime     m_ftLastWrite;
    protected: uint         m_nCodePage;
    protected: uint         m_tickLastCheck;
    protected: char16       m_wszFileName[MAX_PATH + 1];

    // ctor
    protected: FileFeatures() :
        m_eNewline(NewlineMode_Detect),
        m_eObsolete(Obsolete_No),
        m_fNoSave(false),
        m_nCodePage(CP_UTF8),
        m_tickLastCheck(0)
    {
        m_wszFileName[0] = 0;
    } // FileFeatures

    // [G]
    public: uint          GetCodePage() const { return m_nCodePage; }
    public: const char16* GetFileName() const { return m_wszFileName; }

    public: const FileTime* GetLastWriteTime() const
        { return &m_ftLastWrite; }

    public: NewlineMode   GetNewline()  const { return m_eNewline; }
    public: bool          GetNoSave()   const { return m_fNoSave; }

    /// <summary>
    ///  Retreives buffer obsolete state.
    /// </summary>
    /// <seealso>::Buffer::UpdateFileStatus</seealso>
    public: Obsolete GetObsolete() const
        { return m_eObsolete; }

    // [S]
    public: uint SetCodePage(uint const nCodePage)
        { return m_nCodePage = nCodePage; }

    public: NewlineMode SetNewline(NewlineMode eNewline)
        { return m_eNewline = eNewline; }

    public: bool SetNoSave(bool f)
        { return m_fNoSave = f; }

    public: Obsolete SetObsolete(Obsolete e)
        { return m_eObsolete = e; }
}; // FileFeatures

/// <summary>
///   Represents text buffer.
/// </summary>
class Buffer :
    public BufferCore,
    public FileFeatures
{
    public: class Property :
        public DoubleLinkedNode_<Property>
    {
        private: const char* const m_pszName;

        public: Property(
            const char* const pszName) :
                m_pszName(pszName)
            {}

        private: Property& operator=(Property&) {}
        private: void operator delete(void*) {}

        public: void* operator new(size_t cb, HANDLE h)
            { return ::HeapAlloc(h, 0, cb); }

        public: const char* GetName() const
        {
            return m_pszName;
        } //  getName

        public: template<class T> T* StaticCast()
        {
            return reinterpret_cast<T*>(this);
        } // Cast
    }; // Property

    protected: typedef DoubleLinkedList_<Interval> Intervals;
    protected: typedef BinaryTree<Interval>        IntervalTree;

    public: virtual Command::KeyBindEntry* MapKey(uint) const;

    private: HANDLE         m_hObjHeap;
    private: Range*         m_pFirstRange;
    private: Mode*          m_pMode;

    protected: bool         m_fUndo;
    protected: UndoManager* m_pUndo;

    private: bool           m_fReadOnly;

    // Redisplay
    private: int            m_nModfTick;

    // Modified?
    private: int            m_nCharTick;
    private: int            m_nSaveTick;

    // Interval
    private: Intervals    m_oIntervals;
    private: IntervalTree m_oIntervalTree;

    private: DoubleLinkedList_<Property> m_oProperties;

    // Asynchronous Operation Support
    public: enum State
    {
        State_Save,
        State_Ready,
        State_Load,
    }; // State

    protected: State      m_eState;
    protected: char16     m_wszName[MAX_PATH + 1];

    // ctor/dtor
    public: Buffer(const char16*, Mode* = NULL);
    public: ~Buffer();

    // [C]
    public: bool      CanRedo() const;
    public: bool      CanUndo() const;
    public: Posn      ComputeEndOf(Unit, Posn) const;
    public: Count     ComputeMotion(Unit, Count, Posn*) const;
    public: Posn      ComputeStartOf(Unit, Posn) const;
    public: Count     ComputeWhile(const char16*, Count, Posn*) const;
    public: Count     ComputeWhile(uint, Count, Posn*) const;
    //public: Interval* CreateInterval(Posn = 0, Posn = 0, int = 0);
    public: Range*    CreateRange(Posn lStart = 0, Posn lEnd = 0);
    public: Range*    CreateRange(Range*);

    // [D]
    private: void destroyObject(void* pv)
        { ::HeapFree(m_hObjHeap, 0, pv); }

    public: Count Delete(Posn, Posn);

    // [E]
    public: bool EnableUndo(bool f)
        { bool f0 = f; m_fUndo = f; return f0; }

    // [F]
    // <summary>
    //   Find property.
    // </summary>
    public: template<class T> T* FindProperty(
            const char* const pszName) const
    {
        foreach (DoubleLinkedList_<Property>::Enum, oEnum, &m_oProperties)
        {
            if (oEnum.Get()->GetName() == pszName)
            {
                return oEnum.Get()->StaticCast<T>();
            }
        } // for

        return NULL;
    } // FindProperty

    // [G]
    public: const StyleValues* GetDefaultStyle() const;
    public: HANDLE             GetHeap()     const { return m_hObjHeap; }
    public: Interval*          GetIntervalAt(Posn) const;
    public: Mode*              GetMode() const { return m_pMode; }
    public: Count              GetModfTick() const { return m_nModfTick; }
    public: const char16*      GetName()     const { return m_wszName; }
    public: Posn               GetStart() const { return 0; }
    public: const StyleValues* GetStyleAt(Posn) const;
    public: UndoManager*       GetUndo() const { return m_pUndo; }
    public: size_t             GetUndoSize() const;

    // [I]
    public: Count   IncCharTick(int n) { return m_nCharTick += n; }
    public: Count   Insert(Posn, char16, Count);
    public: Count   Insert(Posn, const char16*, Count);
    public: Range*  InternalAddRange(Range*);
    public: void    InternalDelete(Posn, Posn);
    public: void    InternalInsert(Posn, char16, Count);
    public: void    InternalInsert(Posn, const char16*, Count);
    public: void    InternalRemoveRange(Range*);
    public: bool    IsModified() const { return m_nCharTick != m_nSaveTick; }
    public: bool    IsNotReady() const;
    public: bool    IsReadOnly() const { return m_fReadOnly; }

    public: void Insert(Posn lPosn, const char16* pwsz)
        { Insert(lPosn, pwsz, ::lstrlenW(pwsz)); }

    // [N]
    public: bool NeedSave () const
    {
        return ! m_fNoSave && IsModified();
    } // NeedSave

    private: Interval* newInterval(Posn, Posn);

    // [P]
    public: void PutProperty(Property*);

    // [R]
    public: Posn Redo(Posn, Count = 1);

    // [S]
    public: bool   SetCharAt(Posn, char16);
    public: void   SetFile(const char16*, const FILETIME*);

    public: void SetMode(Mode*);

    public: const char16* SetName(const char16* pwszName)
        { return ::lstrcpyW(m_wszName, pwszName); }

    public: bool SetReadOnly(bool f)
        { return m_fReadOnly = f; }

    public: void SetStyle(Posn, Posn, const StyleValues*);

    // [T]
    private: Interval* tryMergeInterval(Interval*);

    // [U]
    public: Posn Undo(Posn, Count = 1);


    // [V]
    public: bool ValidateIntervals(Buffer*) const;

    ////////////////////////////////////////////////////////////
    //
    // Buffer management methods
    //
    private: void       onChange();
    private: void       relocate(Posn, Count);

    // ChangeTracker
    public: class ChangeTracker : 
        public DoubleLinkedNode_<ChangeTracker>
    {
        friend class Buffer;

        private: Posn m_lStart;
        private: Posn m_lEnd;

        public: ChangeTracker() { Reset(); }

        public: Posn GetStart() const { return m_lStart; }
        public: Posn GetEnd()   const { return m_lEnd; }

        public: void Reset()
        {
            m_lStart = Posn_Max;
            m_lEnd   = 0;
        } // Reset
    }; // ChangeTracker

    private: typedef DoubleLinkedList_<ChangeTracker> ChangeTrackers;
    private: ChangeTrackers m_oChangeTrackers;

    public: void RegisterChangeTracker(ChangeTracker* p)
        { m_oChangeTrackers.Append(p); }

    public: void UnregisterChangeTracker(ChangeTracker* p)
        { m_oChangeTrackers.Delete(p); }

    /// <summary>
    ///  Buffer character enumerator
    /// </summary>
    public: class EnumChar
    {
        private: Posn       m_lEnd;
        private: Posn       m_lPosn;
        private: Buffer*    m_pBuffer;

        public: struct Arg
        {
            Posn        m_lEnd;
            Posn        m_lPosn;
            Buffer*     m_pBuffer;

            Arg(Buffer* pBuffer, Posn lPosn, Posn lEnd) :
                m_lEnd(lEnd),
                m_lPosn(lPosn),
                m_pBuffer(pBuffer) {}

            Arg(Buffer* pBuffer, Posn lPosn) :
                m_lEnd(pBuffer->GetEnd()),
                m_lPosn(lPosn),
                m_pBuffer(pBuffer) {}
        }; // Arg

        public: EnumChar(Buffer* pBuffer) :
            m_lEnd(pBuffer->GetEnd()),
            m_lPosn(0),
            m_pBuffer(pBuffer)
        {
            ASSERT(m_pBuffer->IsValidRange(m_lPosn, m_lEnd));
        } // EnumChar

        public: EnumChar(Arg oArg) :
            m_lEnd(oArg.m_lEnd),
            m_lPosn(oArg.m_lPosn),
            m_pBuffer(oArg.m_pBuffer)
        {
            ASSERT(m_pBuffer->IsValidRange(m_lPosn, m_lEnd));
        } // EnumChar

        public: EnumChar(const Range*);

        public: bool AtEnd() const { return m_lPosn >= m_lEnd; }

        public: char16 Get() const
        {
            ASSERT(! AtEnd());
            return m_pBuffer->GetCharAt(m_lPosn);
        } // Get

        public: Posn GetPosn() const
            { return m_lPosn; }

        public: const StyleValues* GetStyle() const
        {
            ASSERT(! AtEnd());
            return m_pBuffer->GetStyleAt(m_lPosn);
        } // GetStyle

        public: Posn GoTo(Posn lPosn)
            { return m_lPosn = lPosn; }

        public: void Next()
            { ASSERT(! AtEnd()); m_lPosn += 1; }

        public: void Prev()
            { m_lPosn -= 1; }

        public: char16 Set(char16 wch)
            { return m_pBuffer->SetCharAt(m_lPosn, wch); }

        public: void SetRange(Posn lStart, Posn lEnd)
        {
            m_lPosn = lStart;
            m_lEnd  = lEnd;
        } // SetRange

        public: void SyncEnd()
        {
            m_lEnd = m_pBuffer->GetEnd();
        } // Sync
    }; // EnumChar

    /// <summary>
    ///   Reverse character enumerator
    /// </summary>
    public: class EnumCharRev
    {
        private: Posn           m_lPosn;
        private: Posn           m_lStart;
        private: const Buffer*  m_pBuffer;

        public: struct Arg
        {
            Posn    m_lPosn;
            Posn    m_lStart;
            Buffer* m_pBuffer;
            Arg(Buffer* pBuffer, Posn lPosn, Posn lStart = 0) :
                m_lPosn(lPosn), m_lStart(lStart), m_pBuffer(pBuffer) {}
        }; // Arg

        public: EnumCharRev(Arg oArg) :
            m_lPosn(oArg.m_lPosn), 
            m_lStart(oArg.m_lStart), 
            m_pBuffer(oArg.m_pBuffer) {}

        public: EnumCharRev(const Range*);

        public: bool AtEnd() const { return m_lPosn <= m_lStart; }

        public: char16 Get() const
        {
            ASSERT(! AtEnd());
            return m_pBuffer->GetCharAt(m_lPosn - 1);
        } // Get

        public: Posn GetPosn() const
            { ASSERT(! AtEnd()); return m_lPosn; }

        public: const StyleValues* GetStyle() const
        {
            ASSERT(! AtEnd());
            return m_pBuffer->GetStyleAt(m_lPosn - 1);
        } // GetStyle

        public: void Next()
            { ASSERT(! AtEnd()); m_lPosn -= 1; }

        public: void Prev()
            { m_lPosn += 1; }
    }; // EnumCharRev

    // EnumInterval
    public: class EnumInterval : public Intervals::Enum
    {
        public: EnumInterval(const Buffer* pBuffer) :
            Intervals::Enum(&pBuffer->m_oIntervals) {}
    }; // EnumInterval

    // EnumRange
    public: class EnumRange
    {
        Range* m_pRunner;

        public: EnumRange(const Buffer* pBuffer) :
            m_pRunner(pBuffer->m_pFirstRange) {}

        public: bool AtEnd() const { return NULL == m_pRunner; }
        public: Range* Get() const { ASSERT(! AtEnd()); return m_pRunner; }
        public: void Next();
    }; // EnumRange

    // Cursor
    public: class Cursor : public IStringCursor
    {
        protected: Posn     m_lCurStart;
        protected: Posn     m_lCurEnd;
        protected: Posn     m_lEnd;
        protected: Posn     m_lStart;
        protected: Buffer*  m_pBuffer;

        public: Cursor(Buffer* pBuffer, Posn lStart, Posn lEnd, Posn lPosn) :
            m_pBuffer(pBuffer)
        {
            m_lStart = m_pBuffer->EnsurePosn(lStart);
            m_lEnd   = m_pBuffer->EnsurePosn(lEnd);
            if (m_lStart > m_lEnd) swap(m_lStart, m_lEnd);

            m_lCurStart = m_pBuffer->EnsurePosn(lPosn);
            m_lCurEnd   = m_lCurStart;

        } // Cursor

        // [C]
        public: virtual bool CanMove(int iDelta) override
            { return isValidPosn(m_lCurStart + iDelta); }

        public: void Collapse(CollapseWhich eCollapse)
        {
            if (Collapse_Start == eCollapse)
            {
                m_lCurEnd = m_lCurStart;
            }
            else
            {
                m_lCurStart = m_lCurEnd;
            }
        } // Collapse

        // [E]
        private: Posn ensurePosn(Posn lPosn) const
        {
            if (lPosn < m_lStart) return m_lStart;
            if (lPosn > m_lEnd) return m_lEnd;
            return lPosn;
        } // ensurePosn

        // [F]
        public: bool FindBackward(char16 wchWhat, uint rgf)
        {
            if (rgf & SearchFlag_IgnoreCase)
            {
                wchWhat = ::CharUpcase(wchWhat);
                while (m_lCurStart > m_lStart)
                {
                    char16 wch = m_pBuffer->GetCharAt(m_lCurStart - 1);
                    if (wchWhat == ::CharUpcase(wch)) return true;
                    m_lCurStart -= 1;
                }
            }
            else
            {
                while (m_lCurStart > m_lStart)
                {
                    char16 wch = m_pBuffer->GetCharAt(m_lCurStart - 1);
                    if (wchWhat == wch) return true;
                    m_lCurStart -= 1;
                }
            }
            return false;
        } // FindBackward

        public: bool FindForward(char16 wchWhat, uint rgf)
        {
            if (rgf & SearchFlag_IgnoreCase)
            {
                wchWhat = ::CharUpcase(wchWhat);
                while (m_lCurStart < m_lEnd)
                {
                    char16 wch = m_pBuffer->GetCharAt(m_lCurStart);
                    if (wchWhat == ::CharUpcase(wch)) return true;
                    m_lCurStart += 1;
                }
            }
            else
            {
                while (m_lCurStart < m_lEnd)
                {
                    char16 wch = m_pBuffer->GetCharAt(m_lCurStart);
                    if (wchWhat == wch) return true;
                    m_lCurStart += 1;
                }
            }
            return false;
        } // FindForward

        // [G]
        public: virtual char16 GetChar(int iDelta) override
        {
            Posn lPosn = m_lCurStart + iDelta;
            return isValidPosn(lPosn) ? m_pBuffer->GetCharAt(lPosn) : 0;
        } // GetChar

        public:          Posn GetEnd()   const { return m_lCurEnd; }
        public: virtual Posn GetPosition() override { return m_lCurStart; }
        public:          Posn GetStart() const { return m_lCurStart; }

        // [I]
        private: bool isValidPosn(Posn lPosn) const
            { return ensurePosn(lPosn) == lPosn; }

        // [M]
        public: virtual bool Match(const char16* pwch, int cwch, uint rgf) override
            { return m_pBuffer->Match(m_lCurStart, pwch, cwch, rgf); }

        public: virtual long Move(int iDelta) override
            { return m_lCurStart = ensurePosn(m_lCurStart + iDelta); }

        public: virtual long MoveToEnd() override
            { return m_lCurStart = m_lEnd; }

        public: virtual long MoveToStart() override
            { return m_lCurStart = m_lStart; }

        // [S]
        public: Posn SetEnd(Posn lEnd)
            { return m_lCurEnd   = lEnd; }

        public: void SetRange(Posn lStart, Posn lEnd)
        {
            m_lCurStart = lStart;
            m_lCurEnd   = lEnd;
        } // SetRange

        public: void SetText(const char16* pwch, int cwch)
        {
            int iDelta = cwch - (m_lCurEnd - m_lCurStart);
            m_pBuffer->Delete(m_lCurStart, m_lCurEnd);
            m_pBuffer->Insert(m_lCurStart, pwch, cwch);

            m_lEnd += iDelta;
            m_lCurEnd += iDelta;
        } // SetText

        public: Posn SetStart(Posn lStart) { return m_lCurStart = lStart; }
    }; // Cursor
}; // Buffer

/// <summary>
///   Utility class for disabling undo.
/// </summary>
class DisableUndo
{
    private: Buffer*    m_pBuffer;
    private: bool       m_fUndo;

    public: DisableUndo(Buffer* p) :
        m_pBuffer(p),
        m_fUndo(p->EnableUndo(false)) {}

    public: ~DisableUndo()
    {
        m_pBuffer->EnableUndo(m_fUndo);
    } // ~DisableUndo
}; // DisableUndo

/// <summary>
///   Utility class for inserting begin/end undo operation. Instances are
///   created on stack instead of heap.
/// </summary>
class UndoBlock
{
    private: UndoManager*   m_pUndo;
    private: const char16*  m_pwszName;
    public: UndoBlock(Range* p, const char16* s);
    public: UndoBlock(Buffer* p, const char16* s) { init(p, s); }
    public: ~UndoBlock();
    private: void init(Buffer*, const char16*);
}; // UndoBlock

} // Edit

#endif //!defined(INCLUDE_edit_Buffer_h)
