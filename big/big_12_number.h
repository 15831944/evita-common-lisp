//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 12 Numbers
// big/big_12_number.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_12_number.h#10 $
//
#if !defined(INCLUDE_big_12_number_h)
#define INCLUDE_big_12_number_h

namespace MiniLisp
{

class BignumImpl;
class DoubleFloatImpl;
class DoubleFloatComplexImpl;
class RatioImpl;
class RationalComplexImpl;
class SingleFloatImpl;
class SingleFloatComplexImpl;


//////////////////////////////////////////////////////////////////////
//
// SmallBignum
//
// Note: To work IsOne and IsZero correctly, we should set m_nlength
// to 1.
//
class __declspec(align(Kernel_Arch_BinObj_Align)) SmallBignumBase :
    public Kernel::Bignum
{
    public: Val Pin();

    public: operator Val()
        { return Encode(); }

    public: operator BignumImpl*()
        { return reinterpret_cast<BignumImpl*>(this); }
}; // SmallBignumBase


//////////////////////////////////////////////////////////////////////
//
// BignumInt
//
// Note: To work IsOne and IsZero correctly, we should set m_nlength
// to 1.
//
class __declspec(align(Kernel_Arch_BinObj_Align)) BignumInt :
    public SmallBignumBase
{
    Bigit m_rgBigit2[1];

    public: BignumInt(Int iVal)
    {
        m_classd  = CLASSD_bignum;
        m_length  = Fixnum::Encode(1);
        m_rgBigit[0] = static_cast<Bigit>(iVal);
    } // BignumInt
}; // BignumInt


class __declspec(align(Kernel_Arch_BinObj_Align)) BignumUInt :
    public SmallBignumBase
{
    public: BignumUInt(UInt iVal)
    {
        m_classd  = CLASSD_bignum;
        m_rgBigit[0] = static_cast<Bigit>(iVal);

        if (static_cast<SignedBigit>(m_rgBigit[0]) >= 0)
        {
            m_length = Fixnum::Encode(1);
        }
        else
        {
            m_length = Fixnum::Encode(2);
            m_rgBigit[1] = 0;
        }
    } // BignumUInt
}; // BignumUInt


#if SIZEOF_VAL == 4
//////////////////////////////////////////////////////////////////////
//
// SmallBignum2
//
// Note: To work IsOne and IsZero correctly, we should set m_nlength
// to 1.
//
class __declspec(align(Kernel_Arch_BinObj_Align)) BignumInt64 :
    public SmallBignumBase
{
    Bigit m_rgBigit2[1];

    public: BignumInt64(int64 i64)
    {
        m_classd  = CLASSD_bignum;
        m_rgBigit[0] = static_cast<Bigit>(i64);
        m_rgBigit[1] = static_cast<Bigit>(i64 >> 32);

        if (i64 >= 0)
        {
            if (0 == m_rgBigit[1] &&
                static_cast<SignedBigit>(m_rgBigit[0]) >= 0 )
            {
                m_length = Fixnum::Encode(1);
            }
            else
            {
                m_length = Fixnum::Encode(2);
            }
        }
        else
        {
            if (static_cast<Bigit>(-1) == m_rgBigit[1] &&
                static_cast<SignedBigit>(m_rgBigit[0]) < 0 )
            {
                m_length = Fixnum::Encode(1);
            }
            else
            {
                m_length = Fixnum::Encode(2);
            }
        } // if
    } // BignumInt64
}; // BignumInt64


class __declspec(align(Kernel_Arch_BinObj_Align)) BignumUInt64 :
    public SmallBignumBase
{
    Bigit m_rgBigit2[1];

    public: BignumUInt64(uint64 u64)
    {
        m_classd  = CLASSD_bignum;
        m_rgBigit[0] = static_cast<Bigit>(u64);
        m_rgBigit[1] = static_cast<Bigit>(u64 >> 32);
        if (static_cast<SignedBigit>(m_rgBigit[1]) >= 0)
        {
            m_length  = Fixnum::Encode(2);
        }
        else
        {
            m_length  = Fixnum::Encode(3);
            m_rgBigit2[0] = 0;
        }
    } // SmallBignum2
}; // BignumUInt64
#elif SIZEOF_VAL == 8
    typedef BignumInt  BignumInt64;
    typedef BignumUInt BignumUInt64;
#endif
} // MiniLisp

namespace CommonLisp
{
    ////////////////////////////////////////////////////////////
    //
    // Complex
    //
    Val complex(Val, Val);  // for syntax #c(real imag)

    ////////////////////////////////////////////////////////////
    //
    // Real
    //
    #define defun_div(mp_name) \
        void mp_name(Val, Val, Val*, Val*); \
        inline Val mp_name(Val x , Val y, Val* out_r = NULL) \
            { Val q; mp_name(x, y, &q, out_r); return q; } \
        inline Val mp_name(Val x, Int i, Val* out_r = NULL) \
            { return mp_name(x, Fixnum::Encode(i), out_r); } \

    defun_div(ceiling)
    defun_div(floor)
    defun_div(round)
    defun_div(truncate)

    #undef defun_div

    Val truncate(Val, Val*);

    Val rem(Val, Val);

    bool minusp(Val);
    bool plusp(Val);
    bool zerop(Val);

    ////////////////////////////////////////////////////////////
    //
    // Float
    //
    Val rational(double);
    Val rational(float);

    ////////////////////////////////////////////////////////////
    //
    // Integer
    //
    bool logbitp(Val, Val);
    Val lognot(Val);
} // CommonLisp


namespace MiniLisp
{
    Int get_type_1(Val, Val, Val);
    Int get_type_2(Val, Val, Val, Val);

    float64 convert_to_float64(Val);
    float32 convert_to_float32(Val);

    inline bool bignump(Val x)
        { return x->Is<Bignum>(); }

    inline bool double_float_p(Val x)
        { return x->Is<DoubleFloat>(); }

    inline bool ratiop(Val x)
        { return x->Is<Ratio>(); }

    inline bool single_float_p(Val x)
        { return x->Is<SingleFloat>(); }

    ////////////////////////////////////////////////////////////
    //
    // Number
    //
    Val C_add(Val, Val);
    Val C_div(Val, Val);
    Val C_mul(Val, Val);
    Val C_sub(Val, Val);
    Val C_neg(Val);

    bool C_num_eq(Val, Val);

    ////////////////////////////////////////////////////////////
    //
    // Real
    //
    Int C_cmp(Val, Val);

    ////////////////////////////////////////////////////////////
    //
    // Float
    //
    Val decode_float32(Val, Val*, Val*);
    Val decode_float64(Val, Val*, Val*);
    Val encode_float32(Val, Val, Val);
    Val encode_float64(Val, Val, Val);

    ////////////////////////////////////////////////////////////
    //
    // Integer
    //
    Val C_ash(Val, Val);
    Val C_gcd(Val, Val);

    Val logand_2(Val, Val);
    Val logior_2(Val, Val);
    Val logxor_2(Val, Val);

    Val expt10(Int);
} // MiniLisp

////////////////////////////////////////////////////////////
//
// Inlines
//
#define define_C_wrapper(mp_name) \
    define_C_wrapper2(Val, mp_name)

#define define_C_wrapper2(mp_type, mp_cname) \
    define_wrapper(mp_type, mp_cname, C_##mp_cname)

#define define_wrapper(mp_type, mp_cname, mp_impl) \
    inline mp_type mp_cname(Val x, Val y) \
        { return mp_impl(x, y); } \
    inline mp_type mp_cname(Val x, Int i) \
        { return mp_impl(x, Fixnum::Encode(i)); }

namespace MiniLisp
{
    define_C_wrapper(add)
    define_C_wrapper(div)
    define_C_wrapper(mul)
    define_C_wrapper(sub)

    define_C_wrapper2(Int,  cmp)
    define_C_wrapper2(bool, num_eq)

    inline Val make_int(Int i)
        { BignumInt oA(i); return oA.Pin(); }

    inline Val make_uint(UInt u)
        { BignumUInt oA(u); return oA.Pin(); }

    inline Val make_int64(int64 i64)
        { BignumInt64 oA(i64); return oA.Pin(); }

    inline Val make_uint64(uint64 u64)
        { BignumUInt64 oA(u64); return oA.Pin(); }
} // MiniLisp

namespace CommonLisp
{
    inline bool minusp(Val x)
        { return cmp(x, Fixnum::Encode(0)) < 0; }

    inline bool plusp(Val x)
        { return cmp(x, Fixnum::Encode(0)) > 0; }

    inline bool zerop(Val x)
        { return num_eq(x, Fixnum::Encode(0)); }

    define_C_wrapper(ash)
    define_C_wrapper(gcd)

    define_wrapper(Val, logand, logand_2)
    define_wrapper(Val, logior, logior_2)
    define_wrapper(Val, logxor, logxor_2)

    inline Val ash(Int i, int k)
    {
        return C_ash(make_int(i), Fixnum::Encode(k));
    } // ash
} // CommonLisp

#undef define_C_wrapper
#undef define_C_wrapper2

#define CLASSD2_(mp_1, mp_2) \
    ( (CLASSD_(mp_1) << 16) | CLASSD_(mp_2) )

#endif //!defined(INCLUDE_big_12_number_h)
