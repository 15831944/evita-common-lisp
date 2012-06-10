//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - common base
// compiler/cm/cm_base.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_base.h#3 $
//
//      Mm
//      Atom
//      Object
//
//      DataFlowData
//
//      DLinkSite_
//      DLinkAnchor_
//      SLinkSite_
//      SLinkAnchor_
//      WithExtension
//      WithIndex
//      WithWorkArea
//      WorkList_
//      WorkListItem_
//
#if !defined(INCLUDE_compiler_cm_base_h)
#define INCLUDE_compiler_cm_base_h

#include "./cm_defs.h"

namespace Compiler
{

class BitVec;
class Mm;

class Atom
{
    public: void* operator new(size_t);
    public: void* operator new[](size_t);
}; // Atom

class Object : public Atom
{
    public: virtual void HtmlPrint(Val, bool) const;
    public: virtual void Print(Val) const;
    public: virtual LPCWSTR GetHtmlClass() const { return L"z"; }
    public: void HtmlA(Val, bool = false) const;
}; // Object


// BUGBUG: We should move DataFlowData to another file.
template<class Site_, class Node_, class Anchor_> class DLinkAnchor_;
class DataFlowData;


//////////////////////////////////////////////////////////////////////
//
// DLinkSite_
//
template<class Site_, class Node_ = Site_>
class DLinkSite_
{
    //friend class DLinkAnchor_<Site_, Node_>;
    //friend class DLinkAnchor_<Site_, Node_>::Enum;
    //friend class DLinkAnchor_<Site_, Node_>::EnumRev;

    protected: Node_* m_pNext;
    protected: Node_* m_pPrev;

    public: Node_* GetNext_() const { return m_pNext; }
    public: Node_* GetPrev_() const { return m_pPrev; }

    public: Node_* SetNext_(Node_* p) { return m_pNext = p; }
    public: Node_* SetPrev_(Node_* p) { return m_pPrev = p; }

    protected: DLinkSite_()
    {
        LinkSelf_();
    } // DLinkSite_

    // Link_
    //  Inserts pNode before this
    public: void Link_(Node_* pNode)
    {
        ASSERT(NULL != pNode);
        ASSERT(pNode != this);

        static_cast<Site_*>(m_pPrev)->m_pNext = pNode;

        static_cast<Site_*>(pNode)->m_pPrev = m_pPrev;
        static_cast<Site_*>(pNode)->m_pNext = static_cast<Node_*>(this);

        m_pPrev = pNode;
    } // Link

    // LinkSelf_
    //  For DLinkAnchor_::RemoveAll
    public: void LinkSelf_()
    {
        m_pNext = m_pPrev = static_cast<Node_*>(this);
    } // LinkSelf_

    // Unlink_
    public: void Unlink_()
    {
        Node_* pNext = m_pNext;
        Node_* pPrev = m_pPrev;

        static_cast<Site_*>(pNext)->m_pPrev = pPrev;
        static_cast<Site_*>(pPrev)->m_pNext = pNext;

        m_pNext = m_pPrev = NULL;
    } // Unlink
}; // DLinkSite_


//////////////////////////////////////////////////////////////////////
//
// DLink Item Anchor
//
template<class Site_, class Item_ = Site_, class Anchor_ = Item_>
class DLinkAnchor_
{
    private: typedef DLinkAnchor_<Site_, Item_, Anchor_> DLinkAnchor;

    protected: Anchor_ m_oAnchor;

    public: Item_* getHead() const
    {
        return static_cast<const Site_*>(&m_oAnchor)->GetNext_();
    } // GetHead

    public: Item_* getTail() const
    {
        return static_cast<const Site_*>(&m_oAnchor)->GetPrev_();
    } // GetTail

    // Append_
    public: Item_* Append_(Item_* pItem)
    {
        ASSERT(NULL != pItem);
        static_cast<Site_*>(&m_oAnchor)->Link_(pItem);
        return pItem;
    } // Append

    public: Item_* GetAnchor() const
        { return const_cast<Anchor_*>(&m_oAnchor); }

    // GetHead
    public: Item_* GetHead() const
        { ASSERT(! IsEmpty()); return getHead(); }

    // GetTail
    public: Item_* GetTail() const
        { ASSERT(! IsEmpty()); return getTail(); }

    // Insert - Insert pItem before pRef
    public: Item_* Insert_(Item_* pItem, Item_* pRef)
    {
        ASSERT(NULL != pItem);

        if (NULL == pRef)
        {
            Append_(pItem);
        }
        else
        {
            static_cast<Site_*>(pRef)->Link_(pItem);
        }
        return pItem;
    } // Insert

    // IsEmpty
    public: bool IsEmpty() const
    {
        return getHead() == &m_oAnchor;
    } // IsEmpty

    // Prepend_
    public: Item_* Prepend_(Item_* pItem)
    {
        ASSERT(NULL != pItem);
        getHead()->Link_(pItem);
        return pItem;
    } // Prepend

    // Pop
    public: Item_* Pop_()
    {
        Item_* pItem = GetHead();
        Remove_(pItem);
        return pItem;
    } // Pop_

    // Remove_
    public: Item_* Remove_(Item_* pItem)
    {
        ASSERT(NULL != pItem);
        static_cast<Site_*>(pItem)->Unlink_();
        return pItem;
    } // Remove

    public: void RemoveAll()
    {
        static_cast<Site_*>(&m_oAnchor)->LinkSelf_();
    } // RemoveAll

    // Enum
    public: class Enum
    {
        protected: const Item_* m_pEnd;
        protected: Item_*       m_pRunner;

        public: Enum(const DLinkAnchor* pAnchor)
            { Reset(pAnchor); }

        public: Enum() :
            m_pRunner(NULL),
            m_pEnd(NULL) {}

        public: void Reset(const DLinkAnchor* pAnchor)
        {
            ASSERT(NULL != pAnchor);
            m_pRunner = pAnchor->getHead();
            m_pEnd    = &pAnchor->m_oAnchor;
        } // Enum

        public: bool AtEnd() const
            { return m_pRunner == m_pEnd; }

        public: Item_* Get() const
        {
            ASSERT(! AtEnd());
            return m_pRunner;
        } // Get

        public: void Next()
        {
            ASSERT(! AtEnd());
            m_pRunner = static_cast<Site_*>(m_pRunner)->GetNext_();
        } // Next
    }; // Enum

    // Enum_Reverse
    public: class Enum_Reverse
    {
        protected: const Item_* m_pEnd;
        protected: Item_*       m_pRunner;

        public: Enum_Reverse(const DLinkAnchor* pAnchor)
        {
            Reset(pAnchor);
        } // Enum_Rev

        public: Enum_Reverse()
        {
            m_pRunner = NULL;
            m_pEnd    = NULL;
        } // Enum_Revese

        public: void Reset(const DLinkAnchor* pAnchor)
        {
            ASSERT(NULL != pAnchor);
            m_pRunner = pAnchor->getTail();
            m_pEnd    = &pAnchor->m_oAnchor;
        } // Enum

        public: bool AtEnd() const
            { return m_pRunner == m_pEnd; }

        public: Item_* Get() const
        {
            ASSERT(! AtEnd());
            return m_pRunner;
        } // Get

        public: void Next()
        {
            ASSERT(! AtEnd());
            m_pRunner = static_cast<Site_*>(m_pRunner)->GetPrev_();
        } // Next
    }; // Enum_Rev
}; // DLinkAnchor_


//////////////////////////////////////////////////////////////////////
//
// Helper Macro for Defining Enumerator
//
//  This macro defines two enumerators:
//      Enum{Node}_{Order}
//      Enum{Node}_Reverse_{Order}
//
#define Class_Enum_(mp_node, mp_order, mp_anchor) \
    public: class Enum ## mp_node ## _ ## mp_order : \
        public mp_order::Enum \
    { \
        public: Enum ## mp_node ## _ ## mp_order(mp_anchor* p) \
            { p->PrepareEnum ## mp_order (); Reset(p); } \
        public: Enum ## mp_node ## _ ## mp_order() {} \
    }; \
    public: class Enum ## mp_node ## _Reverse_ ## mp_order : \
        public mp_order::Enum_Reverse \
    { \
        public: Enum ## mp_node ## _Reverse_ ## mp_order(mp_anchor* p) \
            { p->PrepareEnum ## mp_order (); Reset(p); } \
        public: Enum ## mp_node ## _Reverse_ ## mp_order() {} \
    };


//////////////////////////////////////////////////////////////////////
//
// SLinkSite_
//
template<class Item_>
class SLinkSite_
{
    public: typedef SLinkSite_<Item_> SLinkSite;

    protected: SLinkSite*   m_pNext;
    protected: Item_*       m_pItem;

    public: SLinkSite_(Item_* pItem, SLinkSite* pNext = NULL) :
        m_pItem(pItem),
        m_pNext(pNext) {}

    public: Item_* GetItem() const   { return m_pItem; }
    public: Item_* SetItem(Item_* p) { return m_pItem = p; }

    public: SLinkSite* GetNext() const       { return m_pNext; }
    public: SLinkSite* SetNext(SLinkSite* p) { return m_pNext = p; }
}; // SLinkSite_


//////////////////////////////////////////////////////////////////////
//
// SLinkAnchor_
//
template<class Item_>
class SLinkAnchor_
{
    public: typedef SLinkSite_<Item_> SLinkSite;
    public: typedef SLinkAnchor_<Item_> SLinkAnchor;

    protected: SLinkSite* m_pFirst;

    public: SLinkAnchor_() :
        m_pFirst(NULL) {}

    public: SLinkSite* GetFirst() const       { return m_pFirst; }
    public: SLinkSite* SetFirst(SLinkSite* p) { return m_pFirst = p; }

    // Find
    public: Item_* Find(const Item_* pItem) const
    {
        foreach (Enum, oEnum, this)
        {
            if (oEnum.Get() == pItem)
            {
                return const_cast<Item_*>(pItem);
            }
        } // for
        return NULL;
    } // Find

    public: bool IsEmpty() const
        { return NULL == m_pFirst; }

    public: Item_* Pop()
    {
        ASSERT(! IsEmpty());
        Item_* pItem = m_pFirst->GetItem();
        m_pFirst = m_pFirst->GetNext();
        return pItem;
    } // Pop

    // Push
    public: void Push(SLinkSite* pSite)
    {
        pSite->SetNext(m_pFirst);
        m_pFirst = pSite;
    } // Push

    // RemoveAll
    public: void RemoveAll()
    {
        m_pFirst = NULL;
    } // RemoveAll

    // Enum
    public: class Enum
    {
        protected: const SLinkSite* m_pRunner;

        public: Enum(const SLinkAnchor* p) :
            m_pRunner(p->m_pFirst) {}

        public: bool AtEnd() const { return NULL == m_pRunner; }

        public: Item_* Get() const
        {
            ASSERT(! AtEnd());
            return const_cast<SLinkSite*>(m_pRunner)->GetItem();
        } // Get

        public: void Next()
        {
            ASSERT(! AtEnd());
            m_pRunner = m_pRunner->GetNext();
        } // Next
    }; // Enum
}; // SLinkAnchor_


//////////////////////////////////////////////////////////////////////
//
// WithExtension
//
class WithExtension
{
    protected: void* m_pvExtension;

    public: WithExtension() : m_pvExtension(NULL) {}

    public: template<class T> T* GetExtension()
        { return reinterpret_cast<T*>(m_pvExtension); }

    public: template<class T> void SetExtension(T* p)
        { m_pvExtension = p; }

    public: void Reset() { m_pvExtension = NULL; }
}; // WithExtension


//////////////////////////////////////////////////////////////////////
//
// WithIndex
//
class WithIndex
{
    public: static const UINT NoIndex = static_cast<UINT>(-1);

    public: WithIndex() { Reset(); }

    protected: UINT m_nIndex;
        public: bool HasIndex() const
            { return NoIndex != m_nIndex; }

        public: UINT GetIndex() const
            { ASSERT(HasIndex()); return m_nIndex; }

        public: UINT ForceGetIndex() const
            { return m_nIndex; }

        public: UINT SetIndex(UINT i)
            { return m_nIndex = i; }

        public: void ResetIndex()
            { m_nIndex = NoIndex; }

        public: void Reset()
            { m_nIndex = NoIndex; }
}; // WithIndex


//////////////////////////////////////////////////////////////////////
//
// WithWorkArea
//
class WithWorkArea
{
    protected: void* m_pvWork;
        public: template<class T> T* GetExtension() const
            { return reinterpret_cast<T*>(m_pvWork); }

        public: template<class T> T* SetExtension(T* p)
            { m_pvWork = p; return p; }

    protected: int m_iWork;
        public: int GetFlag() const { return m_iWork; }
        public: int SetFlag(int i)  { return m_iWork = i; }
        public: int IncFlag(int n = 1) { return m_iWork += n; }
        public: int DecFlag(int n = 1) { return m_iWork -= n; }

    public: WithWorkArea() { Reset(); }

    public: void Reset()
    {
        m_pvWork = NULL;
        m_iWork  = 0;
    } // ResetWorkArea
}; // WithWorkArea


//////////////////////////////////////////////////////////////////////
//
// WorkList
//

//////////////////////////////////////////////////////////////////////
//
// Work List Anchor
//
template<class Item_>
class WorkList_
{
    protected: Item_*  m_pWorkHead;

    public: WorkList_() : m_pWorkHead(NULL) {}

    public: ~WorkList_()
        { MakeEmpty(); }

    public: void Dispose() { m_pWorkHead = NULL; }
    public: Item_* Get() { ASSERT(! IsEmpty()); return m_pWorkHead; }
    public: bool Has(Item_* pItem) const { return NULL != pItem->m_pWorkNext; }
    public: bool IsEmpty() const { return NULL == m_pWorkHead; }

    public: void MakeEmpty()
    {
        while (! IsEmpty())
        {
            Pop();
        } // until empty
    } // MakeEmpty

    public: Item_* Pop()
    {
        Item_* pItem = m_pWorkHead;
        ASSERT(NULL != pItem);

        m_pWorkHead = pItem->m_pWorkNext;
        pItem->m_pWorkNext = NULL;

        if (m_pWorkHead == pItem)
        {
            m_pWorkHead = NULL;
        }

        return pItem;
    } // Pop

    public: Item_* Push(Item_* pItem)
    {
        ASSERT(NULL != pItem);
        ASSERT(NULL == pItem->m_pWorkNext);

        if (NULL == m_pWorkHead)
        {
            pItem->m_pWorkNext = pItem;
        }
        else
        {
            pItem->m_pWorkNext = m_pWorkHead;
        }

        return m_pWorkHead = pItem;
    } // Push

    public: class Enum
    {
        protected: Item_*  m_pCurr;

        public: Enum(const WorkList_<Item_>* pAnchor)
        {
            ASSERT(NULL != pAnchor);
            m_pCurr = pAnchor->m_pWorkHead;
        } // Enum

        public: bool AtEnd() const { return NULL == m_pCurr; }
        public: Item_* Get() const { return const_cast<Item_*>(m_pCurr); }

        public: void Next()
        {
            Item_* pNext = m_pCurr->m_pWorkNext;
            if (pNext == m_pCurr)
            {
                pNext = NULL;
            }
            m_pCurr = pNext;
        } // Next
    }; // Enum
}; // WorkList_


template<class Item_>
class WorkListItem_
{
    public: typedef WorkList_<Item_> WorkList;

    friend class WorkList_<Item_>;
    friend class WorkList_<Item_>::Enum;

    protected: Item_* m_pWorkNext;
        public: bool IsPushed() const { return NULL != m_pWorkNext; }

    public: WorkListItem_() :
        m_pWorkNext(NULL) {}
}; // WorkListItem_


} // Compiler

#endif //!defined(INCLUDE_compiler_cm_base_h)
