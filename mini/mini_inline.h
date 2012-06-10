//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - pre-compiled header
// mini_inline.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_inline.h#8 $
//
#if !defined(INCLUDE_mini_inline_h)
#define INCLUDE_mini_inline_h

namespace CommonLisp
{
    using namespace MiniLisp;

    ////////////////////////////////////////////////////////////
    //
    // 5 Data and Control Flow
    //
    inline bool eq(Val x, Val y)
        { return x == y; }

    inline bool functionp(Val x)
        { return x->Is<Funcallable>(); }

    ////////////////////////////////////////////////////////////
    //
    // 7 Objects
    //
    inline Val class_name(Val x)
        { return x->Decode<Class>()->m_name; }

    ////////////////////////////////////////////////////////////
    //
    // 9 Conditions
    //
    inline void __declspec(noreturn) error(LPCWSTR pwsz)
        { error(make_simple_error(pwsz, nil)); }

    inline void __declspec(noreturn) error(LPCWSTR pwsz, Val a)
        { error(make_simple_error(pwsz, list(a))); }

    inline void __declspec(noreturn) error(LPCWSTR pwsz, Val a, Val b)
        { error(make_simple_error(pwsz, list(a, b))); }

    inline void __declspec(noreturn) error(
        LPCWSTR pwsz, Val a, Val b, Val c )
        { error(make_simple_error(pwsz, list(a, b, c))); }

    inline void __declspec(noreturn) error(
        LPCWSTR pwsz, Val a, Val b, Val c, Val d )
        { error(make_simple_error(pwsz, list(a, b, c, d))); }

    inline void __declspec(noreturn) error(
        LPCWSTR pwsz, Val a, Val b, Val c, Val d, Val e )
        { error(make_simple_error(pwsz, list(a, b, c, d, e))); }

    inline void __declspec(noreturn) error(
        LPCWSTR pwsz, Val a, Val b, Val c, Val d, Val e, Val f )
        { error(make_simple_error(pwsz, list(a, b, c, d, e, f))); }

    ////////////////////////////////////////////////////////////
    //
    // 10 Symbols
    //
    inline bool keywordp(Val x)
        { return symbolp(x) && PACKAGE_keyword == symbol_package(x); }

    inline Val make_symbol(LPCWSTR pwsz)
        { return make_symbol(make_string(pwsz)); }

    inline bool symbolp(Val x)
        { return x->Is<Symbol>(); }

    inline Val symbol_name(Val sym)
        { return sym->Decode<Symbol>()->m_name; }

    inline Val symbol_package(Val sym)
        { return sym->Decode<Symbol>()->m_package; }

    ////////////////////////////////////////////////////////////
    //
    // 11 Packages
    //
    inline bool packagep(Val x)
        { return x->Is<Package>(); }

    inline Val intern(Val name, Val package)
        {
            Val status;
            return intern(name, package, &status);
        } // intern

    inline Val package_name(Val x)
        { return car(package_names(x)); }

    ////////////////////////////////////////////////////////////
    //
    // 12 Numbers
    //
    inline bool complexp(Val x)
    {
        return
            x->Is<RationalComplex>() ||
            x->Is<DoubleFloatComplex>() ||
            x->Is<SingleFloatComplex>();
    } // complexp

    inline bool floatp(Val x)
        { return x->Is<DoubleFloat>() || x->Is<SingleFloat>(); }

    inline bool integerp(Val x)
        { return fixnump(x) || x->Is<Bignum>(); }

    inline bool rationalp(Val x)
        { return integerp(x) || x->Is<Ratio>(); }

    inline bool realp(Val x)
        { return rationalp(x) || floatp(x); }

    inline bool numberp(Val x)
        { return realp(x) || complexp(x); }

    ////////////////////////////////////////////////////////////
    //
    // 14 Conses
    //
    inline bool consp(Val x)
        { return x->Is<Cons>(); }

    inline bool listp(Val x)
        { return x->Is<List>(); }

    inline Val car(Val x)
        { return x->Decode<Cons>()->m_car; }

    inline Val cdr(Val x)
    { return x->Decode<Cons>()->m_cdr; }

    inline Val setf_car(Val val, Val kcons)
        { return kcons->Decode<Cons>()->m_car = val; }

    inline Val setf_cdr(Val val, Val kcons)
        { return kcons->Decode<Cons>()->m_cdr = val; }

    inline Val list(Val a)
        { return cons(a, nil); }

    inline Val list(Val a, Val b)
        { return cons(a, list(b)); }

    inline Val list(Val a, Val b, Val c)
        { return cons(a, list(b, c)); }

    inline Val list(Val a, Val b, Val c, Val d)
        { return cons(a, list(b, c, d)); }

    inline Val list(Val a, Val b, Val c, Val d, Val e)
        { return cons(a, list(b, c, d, e)); }

    inline Val list(Val a, Val b, Val c, Val d, Val e, Val f)
        { return cons(a, list(b, c, d, e, f)); }

    inline Val list(Val a, Val b, Val c, Val d, Val e, Val f, Val g)
        { return cons(a, list(b, c, d, e, f, g)); }

    inline Val list(Val a, Val b, Val c, Val d, Val e, Val f, Val g, Val h)
        { return cons(a, list(b, c, d, e, f, g, h)); }

    inline Val list(
                Val a, Val b, Val c, Val d, Val e,
                Val f, Val g, Val h, Val i )
        { return cons(a, list(b, c, d, e, f, g, h, i)); }

    inline Val list(
                Val a, Val b, Val c, Val d, Val e,
                Val f, Val g, Val h, Val i, Val j )
        { return cons(a, list(b, c, d, e, f, g, h, i, j)); }

    inline Val list(
                Val a, Val b, Val c, Val d, Val e,
                Val f, Val g, Val h, Val i, Val j,
                Val k )
        { return cons(a, list(b, c, d, e, f, g, h, i, j, k)); }

    inline Val list(
                Val a, Val b, Val c, Val d, Val e,
                Val f, Val g, Val h, Val i, Val j,
                Val k, Val l )
        { return cons(a, list(b, c, d, e, f, g, h, i, j, k, l)); }

    inline Val listA(Val a)
        { return a; }

    inline Val listA(Val a, Val b)
        { return cons(a, b); }

    inline Val listA(Val a, Val b, Val c)
        { return cons(a, listA(b, c)); }

    inline Val listA(Val a, Val b, Val c, Val d)
        { return cons(a, listA(b, c, d)); }

    inline Val listA(Val a, Val b, Val c, Val d, Val e)
        { return cons(a, listA(b, c, d, e)); }

    inline Val listA(Val a, Val b, Val c, Val d, Val e, Val f)
        { return cons(a, listA(b, c, d, e, f)); }

    inline Val listA(Val a, Val b, Val c, Val d, Val e, Val f, Val g)
        { return cons(a, listA(b, c, d, e, f, g)); }

    inline Val listA(Val a, Val b, Val c, Val d, Val e, Val f, Val g, Val h)
        { return cons(a, listA(b, c, d, e, f, g, h)); }

    inline Val caar(Val x) { return car(car(x)); }
    inline Val cadr(Val x) { return car(cdr(x)); }
    inline Val cdar(Val x) { return cdr(car(x)); }
    inline Val cddr(Val x) { return cdr(cdr(x)); }

    inline Val caaar(Val x) { return car(caar(x)); }
    inline Val caadr(Val x) { return car(cadr(x)); }
    inline Val cadar(Val x) { return car(cdar(x)); }
    inline Val caddr(Val x) { return car(cddr(x)); }
    inline Val cdaar(Val x) { return cdr(caar(x)); }
    inline Val cdadr(Val x) { return cdr(cadr(x)); }
    inline Val cddar(Val x) { return cdr(cdar(x)); }
    inline Val cdddr(Val x) { return cdr(cddr(x)); }

    inline Val caaaar(Val x) { return car(car(caar(x))); }
    inline Val caaadr(Val x) { return car(car(cadr(x))); }
    inline Val caadar(Val x) { return car(car(cdar(x))); }
    inline Val caaddr(Val x) { return car(car(cddr(x))); }
    inline Val cadaar(Val x) { return car(cdr(caar(x))); }
    inline Val cadadr(Val x) { return car(cdr(cadr(x))); }
    inline Val caddar(Val x) { return car(cdr(cdar(x))); }
    inline Val cadddr(Val x) { return car(cdr(cddr(x))); }

    inline Val cdaaar(Val x) { return cdr(car(caar(x))); }
    inline Val cdaadr(Val x) { return cdr(car(cadr(x))); }
    inline Val cdadar(Val x) { return cdr(car(cdar(x))); }
    inline Val cdaddr(Val x) { return cdr(car(cddr(x))); }
    inline Val cddaar(Val x) { return cdr(cdr(caar(x))); }
    inline Val cddadr(Val x) { return cdr(cdr(cadr(x))); }
    inline Val cdddar(Val x) { return cdr(cdr(cdar(x))); }
    inline Val cddddr(Val x) { return cdr(cdr(cddr(x))); }

    inline Val rest(Val x)     { return cdr(x); }
    inline Val first(Val x)    { return car(x); }
    inline Val second(Val x)   { return cadr(x); }
    inline Val third(Val x)    { return caddr(x); }
    inline Val fourth(Val x)   { return cadddr(x); }
    inline Val fifth(Val x)    { return car(cddddr(x)); }
    inline Val sixth(Val x)    { return cadr(cddddr(x)); }
    inline Val seventh(Val x)  { return caddr(cddddr(x)); }
    inline Val eithth(Val x)   { return cadddr(cddddr(x)); }
    inline Val ninth(Val x)    { return car(cddddr(cddddr(x))); }
    inline Val tenth(Val x)    { return cadr(cddddr(cddddr(x))); }

    inline Val nconc(Val a, Val b, Val c)
        { return nconc(a, nconc(b, c)); }

    inline Val nconc(Val a, Val b, Val c, Val d)
        { return nconc(a, nconc(b, c, d)); }

    inline Val nconc(Val a, Val b, Val c, Val d, Val e)
        { return nconc(a, nconc(b, c, d, e)); }

    inline Val nconc(Val a, Val b, Val c, Val d, Val e, Val f)
        { return nconc(a, nconc(b, c, d, e, f)); }

    inline Val nconc(Val a, Val b, Val c, Val d, Val e, Val f, Val g)
        { return nconc(a, nconc(b, c, d, e, f, g)); }

    inline Val pop(Val& place)
        { Val a = car(place); place = cdr(place); return a; }

    inline Val push(Val x, Val& place)
        { return place = cons(x, place); }

    // Collector
    class Collector
    {
        Val* m_pval;

        public: Collector(Val* pval) :
            m_pval(pval) { *pval = nil; }

        public: Val Add(Val x)
        {
            *m_pval = list(x);
            m_pval = &(*m_pval)->Decode<Cons>()->m_cdr;
            return x;
        } // Add
    }; // Collector

    class EnumList
    {
        Val m_runner;
        public: EnumList(Val x) : m_runner(x) {}
        public: bool AtEnd() const { return endp(m_runner); }
        public: Val  Get() const { ASSERT(! AtEnd()); return car(m_runner); }
        public: void Next() { ASSERT(! AtEnd()); m_runner = cdr(m_runner); }

        public: Val GetList() const { return m_runner; }
    }; // EnumList

    ////////////////////////////////////////////////////////////
    //
    // 15 Arrays
    //
    inline bool bit_vector_p(Val x)
        { return simple_vector_p(x) && x->Is<BitVector>(); }

    inline bool simple_bit_vector_p(Val x)
        { return x->Is<SimpleBitVector>(); }

    inline bool simple_vector_p(Val x)
        { return x->Is<SimpleVector>(); }

    inline Val svref(Val vector, Val index)
    {
        SimpleVector* pVector = vector->Decode<SimpleVector>();
        ASSERT(cmp_xx(pVector->m_length, index) > 0);
        return pVector->mv_element[Fixnum::Decode_(index)];
    } // svref

    inline Val svref(Val vector, Int iIndex)
        { return svref(vector, Fixnum::Encode(iIndex)); }

    inline Val setf_svref(Val val, Val vector, Val index)
    {
        SimpleVector* pVector = vector->Decode<SimpleVector>();
        ASSERT(cmp_xx(pVector->m_length, index) > 0);
        return pVector->mv_element[Fixnum::Decode_(index)] = val;
    } // setf_svref

    inline Val setf_svref(Val val, Val vector, Int iIndex)
        { return setf_svref(val, vector, Fixnum::Encode(iIndex)); }

    // EnumVector
    class EnumVector
    {
        Val m_vector;
        Val m_index;
        public: EnumVector(Val vector) :
            m_vector(vector), m_index(Fixnum::Encode(0)) {}
        public: bool AtEnd() const
            { return cmp_xx(m_index, length(m_vector)) >= 0; }
        public: Val Get() const
            { ASSERT(! AtEnd()); return svref(m_vector, m_index); }
        public: void Next()
            { ASSERT(! AtEnd()); m_index = add_xx(m_index, 1); }
    }; // EnumVector

    ////////////////////////////////////////////////////////////
    //
    // 16 Strings
    //
    inline bool simple_string_p(Val x)
        { return x->Is<SimpleString>(); }

    inline bool stringp(Val x)
        { return simple_string_p(x) || x->Is<String>(); }

    inline Val make_string(Val n)
        { return allocate_string(n); }

    inline Val make_string(Int i)
        { return make_string(Fixnum::Encode(i)); }

    inline Val make_string(LPCWSTR pwsz)
        { return make_string(pwsz, ::lstrlenW(pwsz)); }

    inline Val schar(Val s, Int i)
        { return schar(s, Fixnum::Encode(i)); }

    inline Val setf_schar(Val c, Val s, Int i)
        { return setf_schar(c, s, Fixnum::Encode(i)); }

    ////////////////////////////////////////////////////////////
    //
    // 18 Hash Tables
    //
    inline bool hash_table_p(Val x)
        { return x->Is<HashTable>(); }

    inline Val hash_table_test(Val htb)
        { return htb->Decode<HashTable>()->m_test; }

    inline Val gethash(Val key, Val htb)
        { return gethash(key, htb, nil); }

    inline Val gethash(Val key, Val htb, Val def)
        { Val xFound; return gethash(key, htb, def, &xFound); }

    ////////////////////////////////////////////////////////////
    //
    // 21 Streams
    //
    inline void write_char(char16 wch, Val s)
        { write_string(&wch, 1, s); }

    inline Val write_char(Val ch, Val s)
        { write_char(Character::ToCode(ch), s); return ch; }

    inline void write_string(LPCWSTR s, Val x)
        { write_string(s, ::lstrlenW(s), x); }

    ////////////////////////////////////////////////////////////
    //
    // 23 Reader
    //
    inline bool readtablep(Val x)
        { return x->Is<Readtable>(); }

    inline Val readtable_case(Val x)
        { return x->Decode<Readtable>()->m_case; }
} // CommonLisp

namespace MiniLisp
{
    ////////////////////////////////////////////////////////////
    //
    // 03 Evaluation and Compilation
    //
    inline bool environmentp(Val x)
        { return x->Is<Environment>(); }

    inline bool runtime_environment_p(Val env)
        { return nil == env->Decode<Environment>()->m_outer; }

    inline bool toplevel_environment_p(Val env)
        { return nil != env->Decode<Environment>()->m_types; }

    ////////////////////////////////////////////////////////////
    //
    // 10 Symbols
    //
    inline Val symbol_hash_code(Val sym)
        { return sym->Decode<Symbol>()->m_hash_code; }

    ////////////////////////////////////////////////////////////
    //
    // 11 Packages
    //
    inline Val package_names(Val x)
        { return x->Decode<Package>()->m_names; }

    ////////////////////////////////////////////////////////////
    //
    // 12 Numbers
    //
    inline bool fixnump(Val x)
        { return x->Is<Fixnum>(); }

    inline Int cmp_xx(Val x, Int y)
        { return cmp_xx(x, Fixnum::Encode(y)); }

    inline Val add_xx(Val x, Int y)
        { return add_xx(x, Fixnum::Encode(y)); }

    inline Val mul_xx(Val x, Int y)
        { return mul_xx(x, Fixnum::Encode(y)); }

    inline Val sub_xx(Val x, Int y)
        { return sub_xx(x, Fixnum::Encode(y)); }

    inline Val truncate_xx(Val x, Int y)
        { return truncate_xx(x, Fixnum::Encode(y)); }

    inline bool minusp_xx(Val x)
        { return cmp_xx(x, Fixnum::Encode(0)) < 0; }

    inline bool plusp_xx(Val x)
        { return cmp_xx(x, Fixnum::Encode(0)) > 0; }

    ////////////////////////////////////////////////////////////
    //
    // 15 Arrays
    //
    inline Val make_vector(Int i)
        { return make_vector(Fixnum::Encode(i)); }

    ////////////////////////////////////////////////////////////
    //
    // 16 Stirngs
    //
    inline Val allocate_string(Int n)
        { return allocate_string(Fixnum::Encode(n)); }

    ////////////////////////////////////////////////////////////
    //
    // 49 Internals
    //
    inline bool class_description_p(Val x)
        { return x->Is<ClassD>(); }

    inline bool immediate_p(Val x)
        { return fixnump(x) || characterp(x); }

    // 49 Value Cell
    inline bool value_cell_p(Val x)
        { return x->Is<ValueCell>(); }

    inline Val value_cell_name(Val x)
        { return x->Decode<ValueCell>()->m_name; }

    inline Val value_cell_type(Val x)
        { return x->Decode<ValueCell>()->m_type; }

    inline Val value_cell_value(Val x)
        { return x->Decode<ValueCell>()->m_value; }

    inline Val setf_value_cell_value(Val val, Val cell)
        { return cell->Decode<ValueCell>()->m_value = val; }

    // 49 Setf Cell
    inline bool setf_cell_p(Val x)
        { return x->Is<SetfCell>(); }

    inline Val setf_cell_name(Val x)
        { return x->Decode<SetfCell>()->m_name; }

    inline Val setf_cell_function(Val x)
        { return x->Decode<SetfCell>()->m_function; }

    inline Val setf_setf_cell_function(Val val, Val cell)
        { return cell->Decode<SetfCell>()->m_function = val; }

    // 49 Tlv Record
    inline bool tlv_record_p(Val x)
        { return x->Is<TlvRecord>(); }

    Val make_tlv_record(Val, Val, Val);

    inline Val tlv_record_index(Val x)
    {
        return x->Decode<TlvRecord>()->m_index;
    } // tlv_record

    inline Val tlv_record_name(Val x)
    {
        return x->Decode<TlvRecord>()->m_name;
    } // tlv_record

    inline Val tlv_record_value(Val x)
    {
        return x->Decode<TlvRecord>()->m_value;
    } // tlv_record

    inline Val setf_tlv_record_value(Val y, Val x)
    {
        return x->Decode<TlvRecord>()->m_value = y;
    } // setf_tlv_record_value

    // 49 Value Cell
    inline bool closed_cell_p(Val x)
        { return x->Is<ClosedCell>(); }

    inline Val closed_cell_value(Val x)
        { return x->Decode<ClosedCell>()->m_value; }

    inline Val setf_closed_cell_value(Val y, Val x)
        { return x->Decode<ClosedCell>()->m_value = y; }

    ////////////////////////////////////////////////////////////
    //
    // 50 Extensions - Lock
    //
    inline bool latch_p(Val x)
        { return x->Is<Latch>(); }

    inline bool mutex_p(Val x)
        { return x->Is<Mutex>(); }

    inline Val latch_state(Val x)
        { return x->Decode<Latch>()->m_state; }
} // MiniLisp


namespace MiniLisp
{
    class EnumHashTable : public HashTableImpl::Enum
    {
        protected: Val m_vector;
        protected: Val m_index;
        protected: Val m_rest;

        public: EnumHashTable(Val htb) :
            Enum(htb->Decode<HashTable>()->m_vector) {}
    }; // EnumHashTable

    class EnumString
    {
        protected: LPWSTR   m_pwchRunner;
        protected: LPWSTR   m_pwchEnd;

        public: EnumString(Val x)
        {
            if (x->Is<SimpleString>())
            {
                init(
                    x,
                    Fixnum::Encode(0),
                    x->Decode<SimpleString>()->m_length );
                return;
            }

            if (x->Is<String>())
            {
                Val start = Fixnum::Encode(0);
                Val len   = x->Decode<String>()->m_fill_pointer;

                Val runner = x;
                while(x->Is<String>())
                {
                    String* p = runner->Decode<String>();
                    start = add_xx(start, p->m_offset);
                    Val s = runner->Decode<String>()->m_displaced_to;
                    if (s->Is<SimpleString>())
                    {
                        init(s, start, len);
                        return;
                    }
                    runner = s;
                } // while
            } // if string

            CAN_NOT_HAPPEN();
        } // EnumString

        protected: void init(Val x, Val start, Val len)
        {
            m_pwchRunner = x->Decode<SimpleString>()->GetElements();
            m_pwchEnd    = m_pwchRunner + Fixnum::Decode_(len);

            m_pwchRunner += Fixnum::Decode_(start);
        } // init

        public: bool AtEnd() const { return m_pwchRunner >= m_pwchEnd; }
        public: Val Get() const
            { ASSERT(! AtEnd()); return Character::Encode(*m_pwchRunner); }
        public: void Next()
            { m_pwchRunner++; }
    }; // EnumString

} // MiniLisp

#endif //!defined(INCLUDE_mini_inline_h)
