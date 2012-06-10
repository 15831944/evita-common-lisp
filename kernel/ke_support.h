//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Support
// kernel/ke_support.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_support.h#3 $
//
#if !defined(INCLUDE_kernel_support_h)
#define INCLUDE_kernel_support_h

#include "./ke_layout.h"

namespace Kernel
{

template<class Impl_, class Slot_>
class EnumHashAll_
{
    public: typedef Slot_ Slot;

    public: struct Arg
    {
        Val m_vector;
        Int m_iHashCode;

        Arg(Val vector, Int iHashCode = 0) :
            m_vector(vector),
            m_iHashCode(iHashCode) {}

        Arg(Val vector, Val hash_code) :
            m_vector(vector),
            m_iHashCode(Fixnum::Decode_(hash_code)) {}
    }; // Arg

    Slot* m_pStart;
    Slot* m_pRunner;
    Slot* m_pTop;
    Slot* m_pBtm;
    bool  m_fEnd;

    // init
    void init(Val vector, Int iHashCode)
    {
        m_fEnd = false;

        SimpleVector* p = vector->Decode<SimpleVector>();

        m_pTop = reinterpret_cast<Slot*>(p->mv_element) + 1;

        m_pBtm = reinterpret_cast<Slot*>(
            p->mv_element + Fixnum::Decode_(p->m_length) );

        m_pStart  = m_pTop + (iHashCode % (m_pBtm - m_pTop));

        m_pRunner = m_pStart;
    } // init

    public: EnumHashAll_(Arg oArg)
        { init(oArg.m_vector, oArg.m_iHashCode); }

    public: EnumHashAll_(Val vector)
        { init(vector, 0); }

    public: Val Add()
    {
        return m_pTop[-1].m_key = Fixnum::Encode(
            Fixnum::Decode_(m_pTop[-1].m_key) + 1 );
    } // Add

    public: bool AtEnd() const
        { return m_fEnd; }

    public: Val Get() const
        { ASSERT(! AtEnd()); return m_pRunner->m_key; }

    public: Val GetKey() const
        { return Get(); }

    public: Val GetVal() const
        { ASSERT(! AtEnd()); return m_pRunner->m_val; }

    public: Slot* GetRef() const
        { return m_pRunner; }

    public: void Next()
    {
        m_pRunner++;
        if (m_pRunner == m_pBtm) m_pRunner = m_pTop;
        m_fEnd = m_pRunner == m_pStart;
    } // Next

    public: void Remove()
    {
        ASSERT(m_pRunner >= m_pTop);
        ASSERT(m_pRunner <  m_pBtm);

        Slot* pNext = m_pRunner + 1;
            if (pNext == m_pBtm) pNext = m_pTop;

        m_pRunner->Remove(
            Impl_::Free() == pNext->m_key ?
                Impl_::Free() :
                Impl_::Removed() );

        m_pTop[-1].m_key = Fixnum::Encode(
            Fixnum::Decode_(m_pTop[-1].m_key) - 1 );
    } // Remove

    public: Val SetKey(Val x)
    {
        ASSERT(! AtEnd());
        return m_pRunner->m_key = x;
    } // SetKey

    public: Val SetVal(Val x)
    {
        ASSERT(! AtEnd());
        return m_pRunner->m_val = x;
    } // SetVal
}; // EnumHashAll_


// Enum
template<class Impl_, class Slot_>
class EnumHash_ : public EnumHashAll_<Impl_, Slot_>
{
    Int m_iRest;

    public: EnumHash_(Val vector) :
        EnumHashAll_<Impl_, Slot_>(vector)
    {
        m_iRest = Fixnum::Decode_(m_pTop[-1].m_key);
        next();
    } // Enum

    public: bool AtEnd() const
        { return 0 == m_iRest; }

    public: void Next()
    {
        ASSERT(! AtEnd());
        EnumHashAll_<Impl_, Slot_>::Next();
        m_iRest -= 1;
        next();
    } // Next

    void next()
    {
        if (AtEnd()) return;
        for (;;)
        {
            if (Impl_::Removed() != GetKey() &&
                Impl_::Free()    != GetKey() )
            {
                break;
            }

            EnumHashAll_<Impl_, Slot_>::Next();
        } // for
    } // next
}; // EnumHash_


//////////////////////////////////////////////////////////////////////
//
// PackageImpl
//
class PackageImpl : public Package
{
    public: static Val Free()
        { return Fixnum::Encode(0); }

    public: static Val Removed()
        { return Fixnum::Encode(1); }

    public: struct Slot
    {
        Val m_key;

        Val GetKey() const { return m_key; }
        Val GetVal() const { return m_key; }
        void Remove(Val x) { m_key = x; }
    }; // Slot

    // EnumAll
    public: class EnumAll : public EnumHashAll_<PackageImpl, Slot>
    {
        public: EnumAll(Arg oArg) :
            EnumHashAll_<PackageImpl, Slot>(oArg) {}
    }; // EnumAll

    // Enum
    public: class Enum : public EnumHash_<PackageImpl, Slot>
    {
        public: Enum(Val vector) :
            EnumHash_<PackageImpl, Slot>(vector) {}
    }; // EnumAll
}; // PackageImpl

CASSERT(sizeof(PackageImpl::Slot) == sizeof(Val));


//////////////////////////////////////////////////////////////////////
//
// HashTableImpl
//
class HashTableImpl : public HashTable
{
    public: static Val Free()
        { return QQfree_slot_marker; }

    public: static Val Removed()
        { return QQremoved_slot_marker; }

    public: struct Slot
    {
        Val m_key;
        Val m_val;

        Val GetKey() const { return m_key; }
        Val GetVal() const { return m_val; }
        void Remove(Val x) { m_key = x; m_val = Fixnum::Encode(0); }
    }; // Slot

    // EnumAll
    public: class EnumAll : public EnumHashAll_<HashTableImpl, Slot>
    {
        public: EnumAll(Arg oArg) :
            EnumHashAll_<HashTableImpl, Slot>(oArg) {}
    }; // EnumAll

    // Enum
    public: class Enum : public EnumHash_<HashTableImpl, Slot>
    {
        public: Enum(Val vector) :
            EnumHash_<HashTableImpl, Slot>(vector) {}
    }; // EnumAll
}; // HashTableImpl

} // Kernel

#endif //!defined(INCLUDE_kernel_support_h)
