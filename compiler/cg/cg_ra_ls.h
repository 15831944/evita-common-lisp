//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x86 - ra-ls
// cg/x86/x86_ra_ls.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_ra_ls.h#10 $
//
#if !defined(INCLUDE_compiler_cg_ra_ls_h)
#define INCLUDE_compiler_cg_ra_ls_h

#include "./cg_ra.h"

namespace Compiler
{

namespace LinearScanRA
{
enum Posn
{
    Posn_LiveIn         = 0,

    Posn_Split          = 3,
    Posn_SplitEnd       = 4,

    Posn_Reload         = 4,        // reserved for future use
    Posn_ReloadEnd      = 5,        // reserved for future use

    Posn_Input          = 5,
    Posn_InputEnd       = 6,

    Posn_Clobber        = 6,
    Posn_ClobberEnd     = 7,

    Posn_Output         = 7,
    Posn_OutputEnd      = 8,

    Posn_Spill          = 8,        // reserved for future use
    Posn_SpillEnd       = 9,        // reserved for future use

    Posn_LiveOut        = 9,
    Posn_LiveOutEnd     = 10,

    Posn_Step = 10,

    Posn_Max  = 1234567 * Posn_Step,
}; // Posn

//////////////////////////////////////////////////////////////////////
//
// Use Posn
//
class UsePosn :
    public DLinkSite_<UsePosn>,
    public Object
{
    public: virtual void HtmlPrint(Val stream, bool) const
    {
        if (m_fPhysical)
        {
            html_format(stream, L"<b>~D</b>", m_nPosn);
        }
        else
        {
            html_format(stream, L"~D", m_nPosn);
        }
    } // HtmlPrint

    protected: uint m_nPosn;
    protected: bool m_fPhysical;

    public: UsePosn(uint nPosn = Posn_Max, bool fPhysical = false) :
        m_nPosn(nPosn),
        m_fPhysical(fPhysical) {}

    public: uint GetPosn() const { return m_nPosn; }
    public: uint SetPosn(uint n) { return m_nPosn = n; }

    public: bool GetPhysical() const { return m_fPhysical; }
    public: bool SetPhysical(bool f) { return m_fPhysical = f; }
}; // UsePosn


typedef DLinkAnchor_<UsePosn> UsePosnList;


////////////////////////////////////////////////////////////
//
// Live Range
//
class LiveRange :
    public DLinkSite_<LiveRange>,
    public Object
{
    protected: uint m_nStart;   // inclusive
    protected: uint m_nEnd;     // exclusive

    public: virtual void HtmlPrint(Val stream, bool) const
        { html_format(stream, L"[~D, ~D]", m_nStart, m_nEnd); }

    public: LiveRange(uint nStart = Posn_Max, uint nEnd = Posn_Max) :
            m_nStart(nStart), m_nEnd(nEnd) {}

    public: bool IsEmpty()
        { return m_nStart == m_nEnd; }

    public: uint GetEnd() const { return m_nEnd; }
    public: uint SetEnd(uint n) { return m_nEnd = n; }

    public: uint GetStart() const { return m_nStart; }
    public: uint SetStart(uint n) { return m_nStart = n; }

    public: bool IsPointIn(uint nPosn) const
    {
        return nPosn >= GetStart() && nPosn < GetEnd();
    } // IsPointIn

    public: bool IsOverlapped(const LiveRange* pRange2) const
    {
        uint nS1 = GetStart();
        uint nE1 = GetEnd();

        uint nS2 = pRange2->GetStart();
        uint nE2 = pRange2->GetEnd();

        if (nE1 <= nS2)
        {
            // S1----E1 S2----E2
            return false;
        }

        if (nS1 >= nE2)
        {
            // S2---E2 S1----E2
            return false;
        }

        if (pRange2->IsPointIn(nS1))
        {
            // S2---S1---E2
            return true;
        }

        if (pRange2->IsPointIn(nE1))
        {
            // S1--S2---E1---E2
            return true;
        }

        if (IsPointIn(nS2))
        {
            // S1---S2---E1
            return true;
        }

        if (IsPointIn(nE2))
        {
            // S2---S1---E2---E1
            return true;
        }

        return false;
    } // IsOverlapped
}; // LiveRange

typedef DLinkAnchor_<LiveRange> LiveRangeList;


////////////////////////////////////////////////////////////
//
// Live Interval
//
class LiveInterval :
    public DLinkSite_<LiveInterval>,
    public LiveRangeList,
    public UsePosnList,
    public Object
{
    public: virtual void HtmlPrint(Val, bool) const;

    public: typedef Register::Storage Storage;

    protected: LiveInterval*    m_pParent;
    protected: LiveInterval*    m_pSibling;

    protected: Register*    m_pReg;
    public: Register*       m_pRspill;
    public: Storage         m_eStorage;
    public: int             m_iStorage;
    public: uint            m_nFlags;

    protected: bool     m_fFixed;

    protected: const char16* m_pwsz;

    public: LiveInterval() :
        m_nFlags(0),
        m_pSibling(NULL),
        m_pReg(NULL),
        m_pRspill(NULL),
        m_fFixed(false),
        m_eStorage(Register::Storage_Virtual),
        m_iStorage(0)
    {
        m_pParent = this;
        m_pwsz = L"Anchor";
    } // LiveInterval

    public: LiveInterval(int iStorage) :
        m_nFlags(0),
        m_pSibling(NULL),
        m_pReg(NULL),
        m_pRspill(NULL),
        m_fFixed(true),
        m_eStorage(Register::Storage_Physical),
        m_iStorage(iStorage)
    {
        m_pParent = this;
        m_pwsz = L"Fixed";
    } // LiveInterval


    public: LiveInterval(Register* pReg) :
        m_nFlags(0),
        m_pSibling(NULL),
        m_pReg(pReg),
        m_pRspill(NULL),
        m_fFixed(false),
        m_eStorage(Register::Storage_Virtual),
        m_iStorage(0)
    {
        m_pParent = this;
        pReg->SetExtension(this);
        m_pwsz = L"Parent";
    } // LiveInterval

    public: LiveInterval(LiveInterval* pParent) :
        m_nFlags(0),
        m_pParent(pParent),
        m_pRspill(NULL),
        m_pSibling(NULL),
        m_fFixed(false),
        m_eStorage(Register::Storage_Virtual),
        m_iStorage(0)
    {
        ASSERT(pParent->GetParent() == pParent);
        ASSERT(! pParent->IsFixed());
        m_pwsz = L"Child";

        m_nFlags = pParent->m_nFlags;

        {
            Register* pRep = pParent->GetReg();
            m_pReg = new Register(pRep->GetVar(), pRep->GetClass());
            m_pReg->SetRep(pRep);
            m_pReg->SetIndex(pRep->GetIndex());
            m_pReg->SetExtension<LiveInterval>(this);
        }

        LiveInterval** pp = &pParent->m_pSibling;
        for (;;)
        {
            LiveInterval* p = *pp;
            if (NULL == p)
            {
                *pp = this;
                break;
            }
            pp = &p->m_pSibling;
        } // for
    } // LiveInterval

    public: LiveInterval* GetParent() const { return m_pParent; }
    public: Register* GetReg() const        { return m_pReg; }
    public: Register* SetReg(Register* pRx) { return m_pReg = pRx; }

    // IsFixed
    public: bool IsFixed() const
        { return m_fFixed; }

    // IsPhysical
    public: bool IsPhysical() const
        { return Register::Storage_Physical == m_eStorage; }

    ////////////////////////////////////////////////////////////
    //
    // Spill related
    //
    public: bool IsSpilled() const
        { return NULL != GetParent()->m_pRspill; }

    public: Register* GetSpill() const
        { return GetParent()->m_pRspill; }

    public: void SetSpill(Register* pSpill)
        { GetParent()->m_pRspill = pSpill; }

    ////////////////////////////////////////////////////////////
    //
    // Use Posn related
    //
    public: void AddUsePosn(uint, bool);
    public: uint NextUseAfter(uint) const;

    ////////////////////////////////////////////////////////////
    //
    // Range related
    //

    // AddRange
    public: void AddRange(LiveRange* pRange)
        { LiveRangeList::Prepend_(pRange); }
    
    public: void AddRange(uint nStart, uint nEnd)
    {
        ASSERT(nStart < nEnd);

        if (! HasRange())
        {
            AddRange(new LiveRange(nStart, nEnd));
        }
        else
        {
            LiveRange* pRange = GetFirstRange();

            if (pRange->GetStart() <= nStart ||
                pRange->GetStart() == nEnd )
            {
                pRange->SetStart(nStart);
            }
            else
            {
                AddRange(new LiveRange(nStart, nEnd));
            }
        } // if
    } // AddRange

    // FreeUntil
    //  Returns start of uncovered range after nPosn (inclusive)
    public: uint FreeUntil(uint nPosn)
    {
        foreach (LiveRangeList::Enum, oEnum, this)
        {
            const LiveRange* pRange = oEnum.Get();
            if (nPosn <= pRange->GetStart())
            {
                uint nKind = pRange->GetStart() % Posn_Step;
                if (nKind == Posn_LiveIn) return pRange->GetStart();
                return pRange->GetStart() - nKind + Posn_Split;
            }
        } // for each range

        return 0;   // no free after nPosn
    } // FreeUntil

    // GetEnd
    public: uint GetEnd() const
    {
        ASSERT(HasRange());
        return GetLastRange()->GetEnd();
    } // GetEnd

    // GetFirstRange
    public: LiveRange* GetFirstRange() const
        { return LiveRangeList::GetHead(); }

    // GetLastRange
    public: LiveRange* GetLastRange() const
        { return LiveRangeList::GetTail(); }

    // GetStart
    public: uint GetStart() const
    {
        ASSERT(HasRange());
        return GetFirstRange()->GetStart();
    } // GetStart

    // HasRange
    public: bool HasRange() const
        { return ! LiveRangeList::IsEmpty(); }

    public: bool IsPointIn(uint nPosn) const
    {
        if (! HasRange())
        {
            return false;
        }

        if (nPosn < GetStart() || nPosn >= GetEnd())
        {
            return false;
        }

        foreach (LiveRangeList::Enum, oEnum, this)
        {
            LiveRange* pRange = oEnum.Get();
            if (pRange->IsPointIn(nPosn))
            {
                return true;
            }
        } // for each range

        return false;
    } // IsPointIn

    public: uint IntersectionPosn(const LiveInterval* pIntv) const
    {
        if (GetEnd() <= pIntv->GetStart())
        {
            return Posn_Max;
        }

        if (GetStart() > pIntv->GetEnd())
        {
            return Posn_Max;
        }

        foreach (LiveRangeList::Enum, oEnum, this)
        {
            const LiveRange* pRange1 = oEnum.Get();

            foreach (LiveRangeList::Enum, oEnum, pIntv)
            {
                const LiveRange* pRange2 = oEnum.Get();

                if (pRange1->IsPointIn(pRange2->GetStart()))
                {
                    // 1--2--2--1 or 1--2---1--2
                    return pRange2->GetStart();
                }

                if (pRange2->IsPointIn(pRange1->GetStart()))
                {
                    // 2--1--1--2 or 2--1--2--1
                    return pRange1->GetStart();
                }
            } // for each range
        } // for each range

        return Posn_Max;
    } // IntersectionPosn

    public: bool IsOverlapped(const LiveInterval* pIntv) const
    {
        if (GetEnd() <= pIntv->GetStart())
        {
            return false;
        }

        if (GetStart() > pIntv->GetEnd())
        {
            return false;
        }

        foreach (LiveRangeList::Enum, oEnum, this)
        {
            const LiveRange* pRange1 = oEnum.Get();


            foreach (LiveRangeList::Enum, oEnum, pIntv)
            {
                const LiveRange* pRange2 = oEnum.Get();

                if (pRange1->IsOverlapped(pRange2))
                {
                    return true;
                }
            } // for each range
        } // for each range

        return false;
    } // IsOverlapped

    ////////////////////////////////////////////////////////////
    //
    // Sibling related
    //
    public: LiveInterval* FindAt(uint);

    public: class EnumSibling
    {
        protected: LiveInterval* m_pRunner;

        public: EnumSibling(const LiveInterval* pIntv) :
            m_pRunner(pIntv->m_pSibling)
                { ASSERT(pIntv->GetParent() == pIntv); }

        public: bool AtEnd() const { return NULL == m_pRunner; }

        public: LiveInterval* Get() const
            { ASSERT(! AtEnd()); return m_pRunner; }

        public: void Next()
            { ASSERT(! AtEnd()); m_pRunner = m_pRunner->m_pSibling; }
    }; // EnumSibling
}; // LiveInterval


typedef DLinkAnchor_<LiveInterval> LiveIntervalList;

//////////////////////////////////////////////////////////////////////
//
// IRaTarget
//
class __declspec(novtable) IRaTarget
{
    public: virtual void BuildFixedInterval(Function*) = 0;

    public: virtual const RegSet*
        GetNonAllocable(const LiveInterval*) const = 0;

    public: virtual bool CanCopy(Instruction*, uint) const = 0;
    public: virtual Register* MapPseudo(Instruction*, Register*) const = 0;

    public: virtual bool NeedPhysical(Instruction*, uint) const = 0;
    public: virtual bool NeedPhysicalOutput(Instruction*) const = 0;

    public: virtual Operand* GetMap(int) const = 0;
    public: virtual Operand* SetMap(int, Operand*) = 0;
    public: virtual void     ResetMap() = 0;

    public: virtual uint GetPosn(int) const = 0;
    public: virtual uint SetPosn(int, uint) = 0;
    public: virtual void ResetPosn() = 0;

    public: virtual LiveInterval* GetFixed(int) const = 0;
    public: virtual LiveInterval* SetFixed(int, LiveInterval*) = 0;

    public: virtual uint SizeOfSpill(Ty) = 0;
}; // IRaTarget


//////////////////////////////////////////////////////////////////////
//
// RegisterAllocator
//
// Note: This class contains target independent methods. We must
// make subclass to implement target dependent methods.
//
class RegisterAllocator :
    public BaseRA,
    public IRaTarget
{
    public: static const RegSet k_oEmptyRegSet;

    public: RegisterAllocator(
            const char16*   pwsz,
            const Mach*     pMach,
            const RegSet*   pGprAlloc,
            const RegSet*   pFprAlloc ) :
        m_pMach(pMach),
        m_pGprAlloc(pGprAlloc),
        m_pFprAlloc(pFprAlloc),
        m_iSpillSlot(0),
        BaseRA(pwsz) {}

    protected: virtual void process_function(Function*);

    public: const Mach*     m_pMach;
    public: const RegSet*   m_pGprAlloc;
    public: const RegSet*   m_pFprAlloc;

    public: int             m_iSpillSlot;

    protected: void initOnFunction(Function*);

    public: void AddSpill(Ty, Register*);

    public: virtual const RegSet*
        GetNonAllocable(const LiveInterval*) const
            { return &k_oEmptyRegSet; }
}; // RegisterAllocator

} // LinearScanRA

} // Compiler

#endif //!defined(INCLUDE_compiler_cg_ra_ls_h)
