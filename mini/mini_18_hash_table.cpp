#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 18 Hash Tables
// mini/mini_18_hash_table.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_18_hash_table.cpp#13 $
//

#include "./mini_lisp.h"

namespace MiniLisp
{

namespace
{
static const int Default_Size            = 511;
static const int Default_RehashSize      = 130;
static const int Default_RehashThreshold = 67;
static const Int MaxHashCode   = HashTable::MaxHashCode;
} // namespace


static bool htb_need_rehash_p(Val);
static void htb_put(Val, Val, Val);
static void htb_rehash(Val);
static Int  htb_sxhash(Val, Val);


// htb_need_rehash_p
static bool htb_need_rehash_p(Val htb)
{
    Val vector = htb->Decode<HashTable>()->m_vector;
    Val count     = svref(vector, Fixnum::Encode(0));
    Val threshold = svref(vector, 1);
    return cmp_xx(mul_xx(count, 100), threshold) >= 0;
} // htb_need_rehash_p


// htb_put - for htb_rehash
static void htb_put(Val htb, Val key, Val val)
{
    ASSERT(HashTableImpl::Free() != key);
    ASSERT(HashTableImpl::Removed() != key);

    HashTableImpl::EnumAll::Arg oArg(
        htb->Decode<HashTable>()->m_vector,
        htb_sxhash(htb, key) );

    foreach (HashTableImpl::EnumAll, oEnum, oArg)
    {
        Val present = oEnum.Get();
        if (HashTableImpl::Free() == present)
        {
            oEnum.SetKey(key);
            oEnum.SetVal(val);
            return;
        }
        ASSERT(key != present);
    } // for each slot

    error(L"Broken hash-table ~S.", htb);
} // htb_put


// htb_rehash
static void htb_rehash(Val htb)
{
    Val oldvec = htb->Decode<HashTable>()->m_vector;

    Val count = svref(oldvec, Fixnum::Encode(0));

    Val oldthreshold = svref(oldvec, 1);
    Val oldsize      = sub_xx(truncate_xx(length(oldvec), 2), 2);

    // Note: We ignore m_rehash_size, since mini doesn't support
    // float arithmetic.
    Val rehash_size = Fixnum::Encode(130);

    Val newsize = truncate_xx(mul_xx(oldsize, rehash_size), 100);
    Val newvec  = make_vector(add_xx(mul_xx(newsize, 2), 2));

    Val newthreshold = mul_xx(newsize, truncate_xx(oldthreshold, oldsize));

    htb->Decode<HashTable>()->m_vector = newvec;

    fill(newvec, HashTableImpl::Free());

    setf_svref(count, newvec, Fixnum::Encode(0));
    setf_svref(newthreshold, newvec, 1);

    foreach (HashTableImpl::Enum, oEnum, oldvec)
    {
        htb_put(htb, oEnum.GetKey(), oEnum.GetVal());
    } // for
} // htb_rehash


//////////////////////////////////////////////////////////////////////
//
// Get hash code
//
Val sxhash_eq(Val obj)
{
    if (fixnump(obj))
    {
        return FromInt<Val_>(
            obj->ToInt() & 
            Fixnum::Encode(MaxHashCode)->ToInt() );
    }

    if (symbolp(obj))
    {
        return symbol_hash_code(obj);
    }

    if (setf_cell_p(obj))
    {
        return FromInt<Val_>(
            ~symbol_hash_code(setf_cell_name(obj))->ToInt() &
            Fixnum::Encode(MaxHashCode)->ToInt() );
    }

    if (characterp(obj))
    {
        return Fixnum::Encode(Character::ToCode(obj));
    }

    #if ! defined(EVCL_BOOT)
    {
        return MiniThread::Get()->SxHash(obj);
    }
    #else  // defined(EVCL_BOOT)
    {
        error(L"Boot lisp doesn't support sxhash for ~S.", obj);
    }
    #endif // defined(EVCL_BOOT)
} // sxhash_eq


#if defined(EVCL_BOOT)
Val sxhash_eql(Val obj)
{
    // bignum
    // single-float
    // double-float
    // rational-complex
    // double-float-complex
    // single-float-complex
    // ratio

    return sxhash_eq(obj);
} // sxhash_eql
#endif // defined(EVCL_BOOT)


//////////////////////////////////////////////////////////////////////
//
// htb_sxhash
//
static Int htb_sxhash(Val htb, Val obj)
{
    Val test = htb->Decode<HashTable>()->m_test;

    if (Qeq == test)
    {
        return Fixnum::Decode_(sxhash_eq(obj));
    }

    if (Qequal == test)
    {
        return Fixnum::Decode_(sxhash(obj));
    }

    if (Qeql == test)
    {
        return Fixnum::Decode_(sxhash_eql(obj));
    }

    if (Qequalp == test)
    {
        return Fixnum::Decode_(sxhash_equalp(obj));
    }

    error(L"Broken hash-table ~S.", htb);
} // htb_sxhash


static bool htb_test(Val htb, Val x, Val y)
{
    Val test = htb->Decode<HashTable>()->m_test;

    if (Qeq == test)
    {
        return x == y;
    }

    if (Qequal == test)
    {
        return equal(x, y);
    }

    if (Qeql == test)
    {
        return eql(x, y);
    }

    if (Qequalp == test)
    {
        return equalp(x, y);
    }

    error(L"Broken hash-table ~S.", htb);
} // htb_test


namespace
{
    // For sxhash_equal and sxhash_equalp
    template<class T>
    class Hash_
    {
        public: static Val Run(Val x)
        {
            Int iHashCode = walk(0, x, 0);
            return Fixnum::Encode(iHashCode);
        } // Hash

        // hash
        protected: static Int hash(Int x, Int y)
        {
            y = y & 0xFFFFFF;
            x = x ^ y;
            return ((x & 0xFFFF) << 8) | (x >> 16);
        } // hash


    #if SIZEOF_VAL == 4
        protected: static Int walk(Int iHashCode, Val x, uint nDepth)
        {
            // 16-way branch
            switch (x->GetTag4())
            {
            case Val_::Tag_Fixnum:
            case Val_::Tag_Fixnum1:
            case Val_::Tag_Fixnum2:
            case Val_::Tag_Fixnum3:
                return Fixnum::Decode_(x) & MaxHashCode;

            case Val_::Tag_Null:
                return 1;

            case Val_::Tag_Record:
            case Val_::Tag_Record8:
                if (x->Decode<Record>()->m_classd == CLASSD_symbol)
                {
                    return Fixnum::Decode_(x->Decode<Symbol>()->m_hash_code);
                }
                return T::HashRecord(iHashCode, x, nDepth);

            case Val_::Tag_Cons:
            case Val_::Tag_Cons8:
                return T::HashCons(iHashCode, x, nDepth);

            case Val_::Tag_Function:
                return T::HashFunction(iHashCode, x, nDepth);
            } // tag

            return 1;
        } // walk
    #elif SIZEOF_VAL == 8
        protected: static Int walk(Int iHashCode, Val x, uint nDepth)
        {
            // 16-way branch
            switch (x->GetTag4())
            {
            case Val_::Tag_Fixnum:
            case Val_::Tag_Fixnum1:
                return Fixnum::Decode_(x) & MaxHashCode;

            #if USE_SINGLE_FLOAT_TAG
                case Val_::Tag_SingleFloat:
                    return x->ToInt() >> SingleFloat::ShiftCount;
            #endif // USE_SINGLE_FLOAT_TAG

            case Val_::Tag_Null:
                return 1;

            case Val_::Tag_Record:
                if (x->Decode<Record>()->m_classd == CLASSD_symbol)
                {
                    return Fixnum::Decode_(x->Decode<Symbol>()->m_hash_code);
                }
                return T::HashRecord(iHashCode, x, nDepth);

            case Val_::Tag_Cons:
                return T::HashCons(iHashCode, x, nDepth);

            case Val_::Tag_Function:
                return T::HashFunction(iHashCode, x, nDepth);
            } // tag
            return 1;
        } // walk
    #else
        #error "Unsupported SIZEOF_VAL"
    #endif // SIZEOF_VAL == 4

        // HashCons
        protected: static Int HashCons(Int iHashCode, Val x, uint nDepth)
        {
            if (nDepth >= 10)
            {
                return hash(iHashCode, 12345);
            }

            nDepth += 1;

            for (uint k = 0; k < 10; k++)
            {
                iHashCode = walk(iHashCode, car(x), nDepth);
                x = cdr(x);
                if (! consp(x))
                {
                    return walk(iHashCode, x, nDepth);
                }
            } // for
            return iHashCode;
        } // HashCons

        protected: static Int HashFunction(Int iHashCode, Val x, uint)
        {
            return T::HashOther(iHashCode, x);
        } // HashFunction
    }; // Hash_
} // namespace


// sxhash_equal
//     number          use eql
//     character       use eql
//     cons            descends
//     bit vector      descends
//     string          descends
//     pathanme        functionally equivalent (not support in mini)
//     structure       use eq
//     other array     use eq
//     hash-table      use eq
//     other objects   use eq
Val sxhash_equal(Val x)
{
    class HashEqual : public Hash_<HashEqual>
    {
        // HashOther
        public: static Int HashOther(Int iHashCode, Val x)
        {
            return hash(iHashCode, Fixnum::Decode_(sxhash_eql(x)));
        } // HashOther

        // HashRecord
        public: static Int HashRecord(Int iHashCode, Val x, uint)
        {
            if (characterp(x))
            {
                return hashChar(iHashCode, x);
            }

            if (stringp(x))
            {
                return hashString(iHashCode, x);
            }

            return hash(iHashCode, HashOther(iHashCode, x));
        } // HashRecord

        static Int hashChar(Int iHashCode, Val x)
        {
            return hash(iHashCode, Character::ToCode(x));
        }

        // hashString
        static Int hashString(Int iHashCode, Val x)
        {
            foreach (EnumString, oEnum, x)
            {
                iHashCode = hashChar(iHashCode, oEnum.Get());
            } // for each char
            return iHashCode;
        } // HashString
    }; // HashEqual

    return HashEqual::Run(x);
} // sxhash_equal

// sxhash_equalp
Val sxhash_equalp(Val x)
{
    class HashEqualp : public Hash_<HashEqualp>
    {
        // HashOther
        public: static Int HashOther(Int iHashCode, Val x)
        {
            return hash(iHashCode, Fixnum::Decode_(sxhash(x)));
        } // HashOther

        // HashRecord
        public: static Int HashRecord(Int iHashCode, Val x, uint)
        {
            if (characterp(x))
            {
                return hashChar(iHashCode, x);
            }

            if (stringp(x))
            {
                return hashString(iHashCode, x);
            }

            // we don't support array, structure, and hash-tables.

            return HashOther(iHashCode, x);
        } // HashRecord

        // hashChar
        static Int hashChar(Int iHashCode, Val x)
        {
            Val y = char_downcase(x);
            return hash(iHashCode, Character::ToCode(y));
        } // hashChar

        // hashString
        static Int hashString(Int iHashCode, Val x)
        {
            foreach (EnumString, oEnum, x)
            {
                iHashCode = hashChar(iHashCode, oEnum.Get());
            } // for each char
            return iHashCode;
        } // HashString
    }; // HashEqualp

    return HashEqualp::Run(x);
} // sxhash_equalp

} // MiniLisp

namespace CommonLisp
{

//////////////////////////////////////////////////////////////////////
//
// clrhash
//
Val clrhash(Val htb)
{
    check_type(htb, hash_table);
    Val vector = htb->Decode<HashTable>()->m_vector;
    Val threshold = svref(vector, 1);
    fill(vector, HashTableImpl::Free());
    setf_svref(Fixnum::Encode(0), vector, Fixnum::Encode(0));
    setf_svref(threshold, vector, 1);
    return htb;
} // clrhash


//////////////////////////////////////////////////////////////////////
//
// gethash
//
Val gethash(Val key, Val htb, Val default_, Val* out_found)
{
    ASSERT(HashTableImpl::Free() != key);
    ASSERT(HashTableImpl::Removed() != key);

    HashTableImpl::EnumAll::Arg oArg(
        htb->Decode<HashTable>()->m_vector,
        htb_sxhash(htb, key) );

    foreach (HashTableImpl::EnumAll, oEnum, oArg)
    {
        Val present = oEnum.Get();
        if (HashTableImpl::Free() == present)
        {
            break;
        }

        if (key == present || htb_test(htb, key, present))
        {
            *out_found = t;
            return oEnum.GetVal();
        }
    } // for each slot

    *out_found = nil;
    return default_;
} // gethash


//////////////////////////////////////////////////////////////////////
//
// Get Count
//
Val hash_table_count(Val htb)
{
    check_type(htb, hash_table);
    return svref(htb->Decode<HashTable>()->m_vector, Fixnum::Encode(0));
} // hash_table_count


//////////////////////////////////////////////////////////////////////
//
// Make hash table
//
Val make_hash_table(Val test)
{
    ASSERT(0 != QQfree_slot_marker);

    if (functionp(test))
    {
        // Note: GF doesn't have m_name.
        test = test->Decode<NativeCodeFunction>()->m_name;
    }

    if (Qeq == test ||
        Qeql == test ||
        Qequal == test ||
        Qequalp == test )
    {
        // Ok
    }
    else
    {
        error(make_type_error(test, Qhash_table_test));
    }

    Val htb =  MiniThread::Get()->AllocRecord(CLASSD_hash_table); 

    Int iSize = Default_Size;

    Val rehash_threshold = Fixnum::Encode(Default_RehashThreshold);

    HashTable* pHashTable = htb->Decode<HashTable>();
        pHashTable->m_vector = make_vector(iSize * 2 + 2);
        pHashTable->m_test   = test;
        pHashTable->m_rehash_size = Fixnum::Encode(Default_RehashSize);

    fill(pHashTable->m_vector, HashTableImpl::Free());

    // thrshold = size * rehash_threashold * 100
    Val threshold = mul_xx(rehash_threshold, iSize);
    setf_svref(Fixnum::Encode(0), pHashTable->m_vector, Fixnum::Encode(0));
    setf_svref(threshold, pHashTable->m_vector, 1);

    return htb;
} // make_hash_table


//////////////////////////////////////////////////////////////////////
//
// remhash
//
Val remhash(Val key, Val htb)
{
    check_type(htb, hash_table);

    HashTableImpl::EnumAll::Arg oArg(
        htb->Decode<HashTable>()->m_vector,
        htb_sxhash(htb, key) );

    foreach (HashTableImpl::EnumAll, oEnum, oArg)
    {
        Val present = oEnum.Get();

        if (HashTableImpl::Free() == present)
        {
            return nil;
        }

        if (HashTableImpl::Removed() == present)
        {
            // nothing to do
        }
        else if (key == present || htb_test(htb, key, present))
        {
            oEnum.Remove();
            return t;
        }
    } // for each slot

    // Table has lot of removed keys.
    return nil;
} // remhash


//////////////////////////////////////////////////////////////////////
//
// setf gethash
//
Val setf_gethash(Val val, Val key, Val htb)
{
    check_type(htb, hash_table);

    ASSERT(HashTableImpl::Free() != key);
    ASSERT(HashTableImpl::Removed() != key);

    HashTableImpl::EnumAll::Arg oArg(
        htb->Decode<HashTable>()->m_vector,
        htb_sxhash(htb, key) );

    HashTableImpl::EnumAll oEnum(oArg);

    HashTableImpl::EnumAll::Slot* pHome = NULL;
    while (! oEnum.AtEnd())
    {
        Val present = oEnum.Get();

        if (HashTableImpl::Free() == present)
        {
            if (NULL == pHome) pHome = oEnum.GetRef();
            break;
        }

        if (HashTableImpl::Removed() == present)
        {
            if (NULL == pHome) pHome = oEnum.GetRef();
        }
        else if (key == present || htb_test(htb, key, present))
        {
            return oEnum.SetVal(val);
        }

        oEnum.Next();
    } // for each slot

    if (NULL == pHome) error(L"Broken hash-table ~S", htb);

    // Table has lot of removed keys.
    pHome->m_key = key;
    pHome->m_val = val;
    oEnum.Add();

    if (htb_need_rehash_p(htb)) htb_rehash(htb);
    return val;
} // setf_gethash

} // CommonLisp
