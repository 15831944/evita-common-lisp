#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - initialization
// platform/win/vanilla/vanilla_stab.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/vanilla/vanilla_stab.cpp#1 $
//
// Description:
//  Installs following C implemented lisp function for genesis.
//
#include "../../big/big_lisp.h"


class DynamicString
{
    Val                 m_string;
    StackString_<100>   m_oString;

    public: DynamicString(const char16* pwsz)
        { init(pwsz, ::lstrlenW(pwsz)); }

    public: DynamicString(const char16* pwch, size_t cwch)
        { init(pwch, cwch); }

    private: void init(const char16* pwch, size_t cwch)
    {
        if (cwch >= 100)
        {
            m_string = make_string(pwch, cwch);
        }
        else
        {
            m_string = nil;
            m_oString.Init(pwch, cwch);
        }
    } // DynamicString

    public: operator Val() const
    {
        return m_string == nil ? m_oString.Encode() : m_string;
    } // operator Val
}; // DynamicString


namespace CommonLisp
{
    Val values_list(Val);
} // CommonLisp

#if 1

// setf_sbit
Val setf_sbit(Val bit, Val bitvec, Val index)
{
    check_type(bitvec, simple_bit_vector);

    SimpleBitVector* pBitVec = bitvec->Decode<SimpleBitVector>();

    if (cmp_xx(index, pBitVec->m_length) >= 0)
    {
        error(Qvector_index_error, Kvector, bitvec, Kdatum, index);
    } // if

    Int iIndex = Fixnum::Decode_(index);

    const uint k_BITS = sizeof(Arch::BitEltT) * 8;
    Int iWordIndex = iIndex / k_BITS;
    Int iBitIndex  = iIndex % k_BITS;

    if (Fixnum::Encode(0) == bit)
    {
        pBitVec->GetElements()[iWordIndex] &=
            ~Kernel::Arch::k_rgfBit[iBitIndex];
    }
    else if (Fixnum::Encode(1) == bit)
    {
        pBitVec->GetElements()[iWordIndex] |= 
            Kernel::Arch::k_rgfBit[iBitIndex];
    }
    else
    {
        error(make_type_error(bit, Qbit));
    }

    return bit;
} // setf_sbit


// set_info
static Val set_info(Val latch, Val htb, Val name, Val kind, Val key, Val val)
{
    with_exclusive_latch(latch);

    Val entry = gethash(name, htb);

    if (nil == entry)
    {
        if (nil != val)
        {
            entry = cons(kind, list(cons(kind, val)));
            setf_gethash(entry, name, htb);
        }
    }
    else if (nil != val || Kconstant == kind)
    {
        if (nil != kind) setf_car(kind, entry);
        Val pair = assq(key, cdr(entry));
        if (nil == pair)
        {
            setf_cdr(cons(cons(key, val), cdr(entry)), entry);
        }
        else
        {
            setf_cdr(val, pair);
        }
    }
    else
    {
        if (key == kind) setf_car(nil, entry);
        Val pair = assq(key, cdr(entry));
        if (nil != pair)
        {
            setf_cdr(nil, pair);
        }
    } // if

    return val;
} // set_info


// function_information
Val function_information(Val name, Val key, Val* out_present_p, Val env)
{
    ASSERT(NULL != out_present_p);

    if (nil == env) env = TLV(AenvironmentA);

    *out_present_p = nil;

    do
    {
        check_type(env, environment);

        Environment* pEnv = env->Decode<Environment>();

        with_shared_latch(pEnv->m_latch);

        Val frob = gethash(name, pEnv->m_functions);
        if (nil != frob)
        {
            *out_present_p = t;
            return cdr(assq(key, cdr(frob)));
        } // if

        env = pEnv->m_outer;
    } while (nil != env);
    return nil;
} // function_information


// variable_information
Val variable_information(Val name, Val key, Val* out_present_p, Val env)
{
    ASSERT(NULL != out_present_p);

    if (nil == env) env = TLV(AenvironmentA);

    *out_present_p = nil;

    do
    {
        check_type(env, environment);

        Environment* pEnv = env->Decode<Environment>();

        with_shared_latch(pEnv->m_latch);

        Val frob = gethash(name, pEnv->m_variables);
        if (nil != frob)
        {
            *out_present_p = t;
            return cdr(assq(key, cdr(frob)));
        } // if

        env = pEnv->m_outer;
    } while (nil != env);

    return nil;
} // variable_information


// setf_function_information
Val setf_function_information(Val val, Val name, Val key, Val env)
{
    check_type(env, environment);
        ASSERT(toplevel_environment_p(env));

    if (symbolp(name)) ;
    else if (setf_cell_p(name)) ;
    else if (function_name_p(name)) name = intern_setf_cell(second(name));
    else error(make_type_error(name, Qfunction_name));

    Val kind;
    {
        switch (key - nil)
        {
        case Kmacro - nil:            kind = Kmacro; break;
        case Kspecial_operator - nil: kind = Kspecial_operator; break;
        case Kcompiler_macro - nil:   kind = nil; break;
        default:                         kind = Kfunction; break;
        } // key
    } // kind

    set_info(
        env->Decode<Environment>()->m_latch,
        env->Decode<Environment>()->m_functions,
        name, kind,
        key, val );

    return val;
} // setf_function_information

// setf_variable_information
Val setf_variable_information(Val val, Val name, Val key, Val env)
{
    check_type(env, environment);
        ASSERT(toplevel_environment_p(env));

    check_type(name, symbol);

    Val kind;
    {
        switch (key - nil)
        {
        case Kspecial - nil:      kind = Kspecial; break;
        case Kconstant - nil:     kind = Kconstant; break;
        case Ksymbol_macro - nil: kind = Ksymbol_macro; break;
        default:                     kind = nil; break;
        } // key
    } // kind

    return set_info(
        env->Decode<Environment>()->m_latch,
        env->Decode<Environment>()->m_variables,
        name, kind,
        key, val );
} // setf_variable_information


// decode_frame
static Val decode_frame(Frame* pFrame)
{
    Val type;
    {
        char16 wsz[4];
            wsz[0] = static_cast<char16>(pFrame->GetType() >> 24);
            wsz[1] = static_cast<char16>((pFrame->GetType() >> 16) & 0xFF);
            wsz[2] = static_cast<char16>((pFrame->GetType() >>  8) & 0xFF);
            wsz[3] = 0;

        type = intern(wsz, PACKAGE_keyword);
    } // type

    Int iFrame = pFrame->ToInt();

    if (pFrame->GetType() == Frame::Type_Function)
    {
        iFrame = reinterpret_cast<Int>(
            pFrame->StaticCast<FunctionFrame>()->m_pval );
    }

    Val frob;
    {
        switch (pFrame->GetType())
        {
        case Frame::Type_Bind:
        {
            frob = list(
                make_int(iFrame),
                type,
                make_int(pFrame->GetOuter()->ToInt()) );

            Val last = cddr(frob);
            foreach (BindFrame::Enum, oEnum, pFrame->StaticCast<BindFrame>())
            {
                Val name = oEnum.Get()->m_name;

                if (fixnump(name))
                {
                    Int idx = name->ToInt() - offsetof(Thread, mv_tlv);
                        idx /= sizeof(Val);
                    Val tlvrec = svref(VAR(Atlv_vectorA), idx);
                    name = tlv_record_name(tlvrec);
                }

                last = setf_cdr(list(name), last);
            } // for each bind
            break;
        } // bind

        case Frame::Type_Function:
            frob = list(
                make_uint(reinterpret_cast<UInt>(
                    pFrame->StaticCast<FunctionFrame>()->m_pvRA )),
                type,
                pFrame->StaticCast<FunctionFrame>()->m_fn,
                pFrame->StaticCast<FunctionFrame>()->m_ip );
            break;

        default:
            frob = list(
                make_int(iFrame),
                type,
                make_int(pFrame->GetOuter()->ToInt()) );
            break;
        } // switch frame
    } // frob

    return frob;
} // decode_frame


// backtrace
Val backtrace()
{
    Val anchor = list(0);
    Val last = anchor;

    foreach (Thread::EnumFrame, oEnum, MiniThread::Get())
    {
        Frame* pFrame = oEnum.Get();
        last = setf_cdr(list(decode_frame(pFrame)), last);
    } // for each frame

    return cdr(anchor);
} // backtrace


// (setf macro-function)
Val C_setf_macro_function(MiniThread* p)
{
    if (Fixnum::Encode(2) == p->m_n)
    {
        p->mv_value[2] = nil;
    }

    return setf_macro_function(
        p->mv_value[0], p->mv_value[1], p->mv_value[2] );
} // C_setf_macro_function

#endif

//////////////////////////////////////////////////////////////////////
//
// 5 Data and Control Flow
//

Val C_eql(Val x, Val y)
    { return eql(x, y) ? t : nil; }

Val C_equal(Val x, Val y)
    { return equal(x, y) ? t : nil; }

//////////////////////////////////////////////////////////////////////
//
// 07 Objects
//

// C_subst_in_function
Val C_subst_in_function(MiniThread* p)
{
    if (Fixnum::Encode(3) == p->m_n) p->mv_value[3] = nil;
    return subst_in_function(
        p->mv_value[0], p->mv_value[1], p->mv_value[2], p->mv_value[3] );
} // C_subst_in_function


//////////////////////////////////////////////////////////////////////
//
// 10 Symbols
//
Val C_make_symbol(Val name)
    { return make_symbol(name); }

//////////////////////////////////////////////////////////////////////
//
// 12 Numbers
//
Val C_eq(Val x, Val y) { return num_eq(x, y) ? t : nil; }
Val C_ne(Val x, Val y) { return num_eq(x, y) ? nil : t; }

Val C_lt(Val x, Val y) { return C_cmp(x, y) < 0  ? t : nil; }
Val C_le(Val x, Val y) { return C_cmp(x, y) <= 0 ? t : nil; }

Val C_gt(Val x, Val y) { return C_cmp(x, y) > 0  ? t : nil; }
Val C_ge(Val x, Val y) { return C_cmp(x, y) >= 0 ? t : nil; }

// C_logbitp
Val C_logbitp(Val k, Val n)
    { return logbitp(k, n) ? t : nil; }


//////////////////////////////////////////////////////////////////////
//
// 14 Conses
//

// Z_cons
Val Z_cons(Val a, Val b)
{
    return MiniThread::Get()->GcFence(cons(a, b));
} // Z_cons

// C_list
Val C_list(MiniThread* p)
{
    Int n = Fixnum::Decode_(p->m_n);
    Val list =nil;
    while (n >= 1)
    {
        n -= 1;
        list = cons(p->mv_value[n], list);
    } // while

    return p->GcFence(list);
} // C_list

// C_listA
Val C_listA(MiniThread* p)
{
    Int n = Fixnum::Decode_(p->m_n);

    if (1 == n)
    {
        return p->mv_value[0];
    }

    n -= 1;
    Val list = p->mv_value[n];
    while (n >= 1)
    {
        n -= 1;
        list = cons(p->mv_value[n], list);
    } // while

    return p->GcFence(list);
} // C_listA


//////////////////////////////////////////////////////////////////////
//
// 15 Arrays
//

// replace_vector
Val C_replace_vector(MiniThread* p)
{
    switch (Fixnum::Decode_(p->m_n))
    {
    case 2: p->mv_value[2] = Fixnum::Encode(0);
    case 3: p->mv_value[3] = nil;
    case 4: p->mv_value[4] = Fixnum::Encode(0);
    case 5: p->mv_value[5] = nil;
    case 6: break;
    default: CAN_NOT_HAPPEN();
    } // switch n
    return replace_vector(
        p->mv_value[0], p->mv_value[1],
        p->mv_value[2], p->mv_value[3],
        p->mv_value[4], p->mv_value[5] );
} // C_replace_vector


//////////////////////////////////////////////////////////////////////
//
// 16 Strings
//
Val Zallocate_string(Val a)
{
    return MiniThread::Get()->GcFence(allocate_string(a));
} // Zallocate_string


//////////////////////////////////////////////////////////////////////
//
// 49 Internals
//

// Zallocate_binobj
Val Zallocate_binobj(Val classd)
{
    MiniThread* p = MiniThread::Get();
    return p->GcFence(p->AllocBinObj(classd));
} // Zallocate_binobj


// Zallocate_binvec
Val Zallocate_binvec(Val classd, Val length)
{
    MiniThread* p = MiniThread::Get();
    return p->GcFence(p->AllocBinVec(classd, length));
} // Zallocate_binvec


// Zallocate_funobj
//  size    byte size of function. It must be multiple of Host::FunObj_Align.
Val Zallocate_funobj(Val classd, Val size)
{
    MiniThread* p = MiniThread::Get();
    return p->GcFence(p->AllocFunction(classd, Fixnum::Decode_(size)));
} // Zallocate_funobj


// Zallocate_instance
Val Zallocate_instance(Val classd)
{
    MiniThread* p = MiniThread::Get();
    return p->GcFence(p->AllocInstance(classd));
} // Zallocate_instance


// Zallocate_record
Val Zallocate_record(Val classd)
{
    MiniThread* p = MiniThread::Get();
    return p->GcFence(p->AllocRecord(classd));
} // Zallocate_record


// Zallocate_vector
Val Zallocate_vector(Val classd, Val length)
{
    MiniThread* p = MiniThread::Get();
    return p->GcFence(p->AllocVector(classd, length));
} // Zallocate_vector


//////////////////////////////////////////////////////////////////////
//
// 50 Extensions - TLV
//
Val C_Zdeftlv(Val name, Val init, Val docstr, Val initp)
{
    deftlv(name, init, initp, docstr);
    Val tlvrec = gethash(name, VAR(Avalue_tableA));
    setf_variable_information(t, name, Kspecial, TLV(AenvironmentA));
    setf_variable_information(tlvrec, name, Qtlv, TLV(AenvironmentA));
    return name;
} // C_Zdeftlv


#if 0

inline LONGLONG
FileTimeDiff(
    const FILETIME* pftEnd,
    const FILETIME* pftStart )
{
    ULARGE_INTEGER oX;
        oX.LowPart  = pftEnd->dwLowDateTime;
        oX.HighPart = pftEnd->dwHighDateTime;

    ULARGE_INTEGER oY;
        oY.LowPart  = pftStart->dwLowDateTime;
        oY.HighPart = pftStart->dwHighDateTime;

    return (oX.QuadPart - oY.QuadPart) / 10000;
} // FileTimeDiff

// time_it
//  FIXME 2007-05-09 We should move this function to platform/win.
Val time_it(Val fn)
{
    class Time
    {
        public: FILETIME m_ftElapsed;
        public: FILETIME m_ftCreate;
        public: FILETIME m_ftExit;
        public: FILETIME m_ftKernel;
        public: FILETIME m_ftUser;

        public: Time()
        {
            ::GetSystemTimeAsFileTime(&m_ftElapsed);

            ::GetThreadTimes(
                ::GetCurrentThread(),
                &m_ftCreate,
                &m_ftExit,
                &m_ftKernel,
                &m_ftUser );
        } // Time
    };

    MiniThread* p = MiniThread::Get();

    p->m_fn = fn;
    p->m_n  = Fixnum::Encode(0);

    Time oStart;
        CallLisp(p);
    Time oEnd;

    Val result = nil;
    Int i = Fixnum::Decode_(p->m_n);
    while (i >= 1)
    {
        i -= 1;
        result = cons(p->mv_value[i], result);
    } // while

    GcRoot oResult(result);

    LONGLONG llElapsed =
        FileTimeDiff(&oEnd.m_ftElapsed, &oStart.m_ftElapsed);

    LONGLONG llKernel =
        FileTimeDiff(&oEnd.m_ftKernel, &oStart.m_ftKernel);

    LONGLONG llUser =
        FileTimeDiff(&oEnd.m_ftUser, &oStart.m_ftUser);

    LONGLONG llGC = 0;

    format(t, L"; CPU-time (non-gc) ~D msec user, ~D msec system~%",
        Fixnum::Encode(static_cast<Int>(llUser)),
        Fixnum::Encode(static_cast<Int>(llKernel)) );
    format(t, L"; CPU-time (gc)     ~D msec user, ~D msec system~%",
        Fixnum::Encode(static_cast<Int>(llUser)),
        Fixnum::Encode(static_cast<Int>(llKernel)) );
    format(t, L"; Elapsed Time      ~D msec real, ~D msec gc~%",
        Fixnum::Encode(static_cast<Int>(llElapsed)),
        Fixnum::Encode(static_cast<Int>(llGC)) );
    format(t, L";~%");

    result = oResult.Get();
    return values_list(result);
} // time_it

#endif



// toplevel_envionment
Val toplevel_environment(Val env)
{
    if (nil == env)
    {
        env = TLV(AenvironmentA);
    }

    for (;;)
    {
        check_type(env, environment);
        if (toplevel_environment_p(env)) return env;
        env = env->Decode<Environment>()->m_outer;
    } // for
} // toplevel_environment

namespace CommonLisp
{

// setf_macro_function
Val setf_macro_function(Val fn, Val name, Val env)
{
    if (! functionp(fn) && nil != fn)
    {
        error(make_type_error(fn, list(Qor, Qfunction, Qnull)));
    }

    check_type(name, symbol);

    env = toplevel_environment(env);

    setf_function_information(
        fn,
        name,
        Kmacro,
        env );

    if (runtime_environment_p(env))
    {
        // runtime environment
        setf_symbol_function(
            make_not_function_function(name),
            name );
    }

    return fn;
} // setf_macro_function

// close
Val close(Val stream)
    { return funcall(Qclose, stream); }

// force_output
Val force_output(Val stream)
    { return funcall(Qforce_output, stream); }

// format
Val format(Val stream, const char16* pwsz)
{
    DynamicString oString(pwsz);
    return funcall(Qformat, stream, oString);
} // format


// format
Val format(Val stream, const char16* pwsz, Val a1)
{
    DynamicString oString(pwsz);
    return funcall(Qformat, stream, oString, a1);
} // format


// format
Val format(Val stream, const char16* pwsz, Val a1, Val a2)
{
    DynamicString oString(pwsz);
    return funcall(Qformat, stream, oString, a1, a2);
} // format


// format
Val format(Val stream, const char16* pwsz, Val a1, Val a2, Val a3)
{
    DynamicString oString(pwsz);
    return funcall(Qformat, stream, oString, a1, a2, a3);
} // format


// format
Val format(Val stream, const char16* pwsz, Val a1, Val a2, Val a3, Val a4)
{
    DynamicString oString(pwsz);
    return funcall(Qformat, stream, oString, a1, a2, a3, a4);
} // format


// format
Val format(Val stream, const char16* pwsz, Val a1, Val a2, Val a3, Val a4,
    Val a5 )
{
    DynamicString oString(pwsz);
    return funcall(Qformat, stream, oString, a1, a2, a3, a4, a5);
} // format


// format
Val format(Val stream, const char16* pwsz, Val a1, Val a2, Val a3, Val a4,
    Val a5, Val a6 )
{
    DynamicString oString(pwsz);
    return funcall(Qformat, stream, oString, a1, a2, a3, a4, a5, a6);
} // format


// format
Val format(Val stream, const char16* pwsz, Val a1, Val a2, Val a3, Val a4,
    Val a5, Val a6, Val a7 )
{
    DynamicString oString(pwsz);
    return funcall(Qformat, stream, oString, a1, a2, a3, a4, a5, a6, a7);
} // format


// format
Val format(Val stream, const char16* pwsz, Val a1, Val a2, Val a3, Val a4,
    Val a5, Val a6, Val a7, Val a8 )
{
    DynamicString oString(pwsz);
    return funcall(Qformat, stream, oString, a1, a2, a3, a4, a5, a6, a7, a8);
} // format


Val print_object(Val obj, Val stream)
    { return funcall(Qprint_object, obj, stream); }

bool streamp(Val obj)
    { return funcall(Qstreamp, obj) != nil; }

// write_string
void write_string(const char16* pwchString, size_t cwchString, Val stream)
{
    DynamicString oString(pwchString, cwchString);
    funcall(Qwrite_string, oString, stream);
} // write_string

Val room(Val);
} // CommonLisp

Val gs_room(MiniThread* p)
{
    return room(Fixnum::Encode(0) == p->m_n ? Kdefault : p->mv_value[0]);
} // gs_room


inline LONGLONG
FileTimeDiff(
    const FILETIME* pftEnd,
    const FILETIME* pftStart )
{
    ULARGE_INTEGER oX;
        oX.LowPart  = pftEnd->dwLowDateTime;
        oX.HighPart = pftEnd->dwHighDateTime;

    ULARGE_INTEGER oY;
        oY.LowPart  = pftStart->dwLowDateTime;
        oY.HighPart = pftStart->dwHighDateTime;

    return (oX.QuadPart - oY.QuadPart) / 10000;
} // FileTimeDiff

// time_it
//  FIXME 2007-05-09 We should move this function to platform/win.
Val time_it(Val fn)
{
    class Time
    {
        public: FILETIME m_ftElapsed;
        public: FILETIME m_ftCreate;
        public: FILETIME m_ftExit;
        public: FILETIME m_ftKernel;
        public: FILETIME m_ftUser;

        public: Time()
        {
            ::GetSystemTimeAsFileTime(&m_ftElapsed);

            ::GetThreadTimes(
                ::GetCurrentThread(),
                &m_ftCreate,
                &m_ftExit,
                &m_ftKernel,
                &m_ftUser );
        } // Time
    };

    MiniThread* p = MiniThread::Get();

    p->m_fn = fn;
    p->m_n  = Fixnum::Encode(0);

    Time oStart;
        CallLisp(p);
    Time oEnd;

    Val result = nil;
    Int i = Fixnum::Decode_(p->m_n);
    while (i >= 1)
    {
        i -= 1;
        result = cons(p->mv_value[i], result);
    } // while

    GcRoot oResult(result);

    LONGLONG llElapsed =
        FileTimeDiff(&oEnd.m_ftElapsed, &oStart.m_ftElapsed);

    LONGLONG llKernel =
        FileTimeDiff(&oEnd.m_ftKernel, &oStart.m_ftKernel);

    LONGLONG llUser =
        FileTimeDiff(&oEnd.m_ftUser, &oStart.m_ftUser);

    LONGLONG llGcElapsed = 0;
    LONGLONG llGcUser = 0;
    LONGLONG llGcKernel = 0;

    format(t, L"; CPU-time (non-gc) ~D msec user, ~D msec system~%",
        Fixnum::Encode(static_cast<Int>(llUser)),
        Fixnum::Encode(static_cast<Int>(llKernel)) );
    format(t, L"; CPU-time (gc)     ~D msec user, ~D msec system~%",
        Fixnum::Encode(static_cast<Int>(llGcUser)),
        Fixnum::Encode(static_cast<Int>(llGcKernel)) );
    format(t, L"; Elapsed Time      ~D msec real, ~D msec gc~%",
        Fixnum::Encode(static_cast<Int>(llElapsed)),
        Fixnum::Encode(static_cast<Int>(llGcElapsed)) );
    format(t, L";~%");

    result = oResult.Get();
    return values_list(result);
} // time_it
