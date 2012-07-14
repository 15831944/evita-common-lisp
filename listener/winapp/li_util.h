//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - edit buffer
// listener/winapp/ed_buffer.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/li_util.h#1 $
//
#if !defined(INCLUDE_listener_util_h)
#define INCLUDE_listener_util_h

template<class Base_>
class Castable_
{
    private: typedef Castable_<Base_> Self;

    // [D]
    public: template<class T> T* DynamicCast() const
    {
        Base_* p = static_cast<Base_*>(const_cast<Self*>(this));
        return T::Is_(p) ? reinterpret_cast<T*>(p) : NULL;
    } // DynamicCast

    // [S]
    public: template<class T> T* StaticCast() const
    {
        T* p = DynamicCast<T>();
        ASSERT(NULL != p);
        return p;
    } // StaticCast
}; // Castable_

template<class T, class Base_, typename KindType_ = const char*>
class HasKind_ : public Base_
{
    public: virtual KindType_ GetKind() const override
        { return T::Kind_(); }

    public: static bool Is_(const Base_* p)
        { return p->GetKind() == T::Kind_(); }
}; // HasKind_

template<int t_N = 100>
class CharSink_
{
    private: int     m_cwch;
    private: char16* m_pwsz;
    private: char16* m_pwszBuffer;
    private: char16  m_wsz[t_N];

    // ctor
    public: CharSink_() :
        m_cwch(t_N),
        m_pwsz(m_wsz),
        m_pwszBuffer(m_wsz) {}

    // dtor
    public: ~CharSink_()
        { releaseBuffer(); }

    // [A]
    public: void Add(char16 wch)
    {
        if (m_pwsz - m_pwszBuffer >= m_cwch)
        {
            int     cwch    = m_cwch * 13 / 10;
            char16* pwszNew = new char16[cwch];
            myCopyMemory(pwszNew, m_pwsz, sizeof(char16) * m_cwch);
            releaseBuffer();
            m_pwszBuffer = pwszNew;
            m_pwsz       = pwszNew + m_cwch;
            m_cwch       = cwch;
        }

        *m_pwsz++ = wch;
    } // Add

    public: void Add(const char16* s)
    {
        while (0 != *s) Add(*s++);
    } // Add

    // [G]
    public: const char16* GetStart() const
        { return m_pwszBuffer; }

    // [R]
    protected: void releaseBuffer()
    {
        if (m_wsz != m_pwszBuffer)
        {
            delete[] m_pwszBuffer;
        }
    } // releaseBuffer
}; // CharSink_


template<class Item_, class Parent_>
class DoubleLinkedList_;

class DummyParent {};

template<class Item_, class Parent_ = DummyParent>
class DoubleLinkedNode_
{
    friend class DoubleLinkedList_<Item_, Parent_>;

    private: Item_* m_pNext;
    private: Item_* m_pPrev;

    public: DoubleLinkedNode_() :
        m_pNext(NULL),
        m_pPrev(NULL) {}

    public: Item_* GetNext() const { return m_pNext; }
    public: Item_* GetPrev() const { return m_pPrev; }
}; // DoubleLinkedNode_


template<class Item_, class Parent_ = DummyParent>
class DoubleLinkedList_
{
    protected: typedef DoubleLinkedList_<Item_, Parent_> List_;
    private: typedef DoubleLinkedNode_<Item_, Parent_> Cons_;

    private: Item_* m_pFirst;
    private: Item_* m_pLast;

    public: DoubleLinkedList_() :
        m_pFirst(NULL),
        m_pLast(NULL) {}

    // [A]
    public: Item_* Append(Item_* pItem)
    {
        Cons_* pCons = static_cast<Cons_*>(pItem);

        pCons->m_pNext = NULL;
        pCons->m_pPrev = m_pLast;

        if (NULL == m_pFirst)
        {
            m_pFirst = pItem;
        } // if

        if (NULL != m_pLast)
        {
            static_cast<Cons_*>(m_pLast)->m_pNext = pItem;
        } // if

        return m_pLast = pItem;
    } // Append

    // [D]
    public: Item_* Delete(Item_* pItem)
    {
        #if _DEBUG
        {
            bool fFound = false;
            foreach (Enum, oEnum, this)
            {
                if (oEnum.Get() == pItem)
                {
                    fFound = true;
                    break;
                }
            } // for each item
            ASSERT(fFound);
        }
        #endif

        Cons_* pCons = static_cast<Cons_*>(pItem);

        Item_* pNext = pCons->m_pNext;
        Item_* pPrev = pCons->m_pPrev;
        if (NULL == pNext)
        {
            m_pLast = pPrev;
        }
        else
        {
            static_cast<Cons_*>(pNext)->m_pPrev = pPrev;
        } // if

        if (NULL == pPrev)
        {
            m_pFirst = pNext;
        }
        else
        {
            static_cast<Cons_*>(pPrev)->m_pNext = pNext;
        } // if

        pCons->m_pNext = NULL;
        pCons->m_pPrev = NULL;

        return pItem;
    } // Delete

    // [E]
    public: class Enum
    {
        private: Item_* m_pRunner;
        public: Enum(List_* p) : m_pRunner(p->m_pFirst) {}
        public: Enum(const List_* p) : m_pRunner(p->m_pFirst) {}
        public: bool AtEnd() const { return m_pRunner == NULL; }
        public: Item_* Get() { return m_pRunner; }
        public: void Next()
        {
            ASSERT(! AtEnd());
            m_pRunner = static_cast<Cons_*>(m_pRunner)->m_pNext;
        } // Next
    }; // Enum

    // [G]
    public: Item_* GetFirst() const { return m_pFirst; }
    public: Item_* GetLast()  const { return m_pLast; }

    // [I]
    public: Item_* InsertAfter(Item_* pItem, Item_* pRefItem)
    {
        Item_* pNext = static_cast<Cons_*>(pRefItem)->m_pNext;
        if (NULL == pNext)
        {
            m_pLast = pItem;
        }
        else
        {
            static_cast<Cons_*>(pNext)->m_pPrev = pItem;
        }

        static_cast<Cons_*>(pItem)->m_pPrev    = pRefItem;
        static_cast<Cons_*>(pItem)->m_pNext    = pNext;
        static_cast<Cons_*>(pRefItem)->m_pNext = pItem;
        return pItem;
    } // InsertAfter

    public: Item_* InsertBefore(Item_* pItem, Item_* pRefItem)
    {
        Item_* pPrev = static_cast<Cons_*>(pRefItem)->m_pPrev;
        if (NULL == pPrev)
        {
            m_pFirst = pItem;
        }
        else
        {
            static_cast<Cons_*>(pPrev)->m_pNext = pItem;
        }

        static_cast<Cons_*>(pItem)->m_pPrev    = pPrev;
        static_cast<Cons_*>(pItem)->m_pNext    = pRefItem;
        static_cast<Cons_*>(pRefItem)->m_pPrev = pItem;
        return pItem;
    } // InsertBefore

    public: bool IsEmpty() const
    {
        return NULL == m_pFirst;
    } // IsEmpty
}; // DoubleLinkedList_


template<class Parent_, class Item_>
class ChildList_;

template<class Parent_, class Item_>
class ChildNode_ : public DoubleLinkedNode_<Item_, Parent_>
{
    friend class ChildList_<Parent_, Item_>;

    protected: Parent_*   m_pParent;

    public: ChildNode_() : m_pParent(NULL) {}
}; // ChildNode_


template<class Parent_, class Item_>
class ChildList_ : public DoubleLinkedList_<Item_, Parent_>
{
    protected: typedef DoubleLinkedList_<Item_, Parent_> List;
    protected: typedef ChildList_<Parent_, Item_> ChildList;

    public: Item_* Append(Parent_* pParent, Item_* pItem)
    {
        List::Append(pItem);
        pItem->m_pParent = pParent;
        return pItem;
    } // Append
};  // ChiildList_


//////////////////////////////////////////////////////////////////////
//
// StringKey
//
class StringKey
{
    private: int            m_cwch;
    private: const char16*  m_pwch;

    public: StringKey(const char16* pwsz) :
        m_cwch(::lstrlenW(pwsz)),
        m_pwch(pwsz) {}

    public: StringKey(const char16* pwch, int cwch) :
        m_cwch(cwch),
        m_pwch(pwch) {}

    public: bool EqualKey(const StringKey* p) const
    {
        if (m_cwch != p->m_cwch) return false;
        return 0 == ::memcmp(m_pwch, p->m_pwch, sizeof(char16) * m_cwch);
    } // Equal

    // Hash - returns hash code.
    public: int Hash() const
    {
        uint nHashCode = 0;
        const char16* s = m_pwch;
        const char16* e = s + m_cwch;
        for (const char16* p = s; p < e; p++)
        {
            uint nHigh = nHashCode >> (sizeof(uint) * 8 - 5);
            nHashCode |= *p;
            nHashCode <<= 5;
            nHashCode |= nHigh;
        } // for p
        return nHashCode & ((1<<28)-1);
    } // Hash

    public: static StringKey* Removed()
        { return reinterpret_cast<StringKey*>(1); }
}; // StringKey


//////////////////////////////////////////////////////////////////////
//
// HashTable_
//
template<class Key_, typename Value_, int t_N = 31>
class HashTable_
{
    private: struct Slot
    {
        const Key_* m_pKey;
        Value_      m_Value;
    }; // Slot

    private: Slot*  m_prgSlot;
    private: int    m_cAlloc;
    private: int    m_cItems;

    public: HashTable_() :
        m_cAlloc(t_N),
        m_cItems(0),
        m_prgSlot(new Slot[t_N])
    {
        ::ZeroMemory(m_prgSlot, sizeof(Slot) * m_cAlloc);
    } // Cache_
        
    public: Value_* Get(const Key_* pKey) const
    {
        int iHashCode = pKey->Hash();
        const Slot* pTop    = &m_prgSlot[0];
        const Slot* pBottom = &m_prgSlot[m_cAlloc];
        const Slot* pStart  = &m_prgSlot[iHashCode % m_cAlloc];
        const Slot* pRunner = pStart;
        do
        {
            const Key_* pPresent = pRunner->m_pKey;
            if (NULL == pPresent)
            {
                return NULL;
            }

            if (Key_::Removed() == pPresent)
            {
                // removed
            }
            else if (pPresent->EqualKey(pKey))
            {
                return const_cast<Value_*>(&pRunner->m_Value);
            }

            pRunner++;
            if (pRunner == pBottom) pRunner = pTop;
        } while (pRunner != pStart);
        CAN_NOT_HAPPEN();
    } // Get

    public: void Put(const Key_* pKey, Value_ oValue)
    {
        if (m_cItems * 100 > m_cAlloc * 60)
        {
            rehash();
        }

        int iHashCode = pKey->Hash();
        Slot* pTop    = &m_prgSlot[0];
        Slot* pBottom = &m_prgSlot[m_cAlloc];
        Slot* pStart  = &m_prgSlot[iHashCode % m_cAlloc];
        Slot* pRunner = pStart;
        Slot* pHome = NULL;

        do
        {
            const Key_* pPresent = pRunner->m_pKey;
            if (NULL == pPresent)
            {
                if (NULL == pHome) pHome = pRunner;
                pHome->m_pKey  = pKey;
                pHome->m_Value = oValue;
                m_cItems += 1;
                return;
            }

            if (Key_::Removed() == pPresent)
            {
                if (NULL == pHome) pHome = pRunner;
            }
            else if (pPresent->EqualKey(pKey))
            {
                return;
            }

            pRunner++;
            if (pRunner == pBottom) pRunner = pTop;
        } while (pRunner != pStart);
        CAN_NOT_HAPPEN();
    } // Put

    private: void rehash()
    {
        Slot* prgStart = m_prgSlot;
        int cAllocs = m_cAlloc;
        int cItems  = m_cItems;
        
        m_cAlloc = m_cAlloc * 130 / 100;
        m_cItems  = 0;
        m_prgSlot = new Slot[m_cAlloc];
        ::ZeroMemory(m_prgSlot, sizeof(Slot) * m_cAlloc);

        Slot* prgEnd = prgStart + cAllocs;
        for (Slot* pRunner = prgStart; pRunner < prgEnd; pRunner++)
        {
            const Key_* pPresent = pRunner->m_pKey;
            if (NULL != pPresent && Key_::Removed() != pPresent)
            {
                Put(pRunner->m_pKey, pRunner->m_Value);
                cItems -= 1;
                if (0 == cItems) break;
            }
        } // for pRunner
    } // rehash
}; // HashTable_


#endif //!defined(INCLUDE_listener_util_h)
