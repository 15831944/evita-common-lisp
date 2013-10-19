#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 12 Numbers - Float
// big/big_12_float.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_12_float.cpp#12 $
//
#include "./big_lisp.h"

#include "./big_12_bignum.h"
#include "./big_12_float.h"
#include "./big_12_ratio.h"

extern "C"
{
    float __cdecl ceilf(float);
    double __cdecl ceil(double);

    float __cdecl floorf(float);
    double __cdecl floor(double);

    float __cdecl rintf(float);
    double __cdecl rint(double);

    float __cdecl truncatef(float);
    double __cdecl truncate(double);
} // extern "C"

#pragma function(ceil)

namespace MiniLisp
{

namespace
{

// ashl
inline Val ashl(uint64 u64, uint k)
{
    if (0 == k) return make_uint64(u64);
    BignumUInt64 oX(u64); return ash(oX, k);
} // ashl


//////////////////////////////////////////////////////////////////////
//
// FloatToDigit
//
class FloatToDigit
{
    LPWSTR  m_pwchDigit;
    WCHAR   m_wszDigit[20];

    public: FloatToDigit() :
        m_pwchDigit(m_wszDigit) {}

    public: LPCWSTR GetDigits() const
        { return m_wszDigit; }

    public: int RunNormal(uint64 ullSgfnd, int iExpt, int iPrec)
    {
        int iExpt10 = runNormal(ullSgfnd, iExpt, iPrec);
        *m_pwchDigit = 0;
        return iExpt10;
    } // Run

    // RunNormal
    public: int RunSubnormal(uint64 ullSgfnd, int iExpt)
    {
        ASSERT(iExpt < 0);

        bool fRound = 0 == ullSgfnd % 2;
        int iExpt10 = scale(
            ullSgfnd, iExpt,
            ashl(ullSgfnd, 1), ashl(2, -iExpt),
            Fixnum::Encode(1), Fixnum::Encode(1),
            fRound, fRound );
        *m_pwchDigit = 0;
        return iExpt10;
    } // runSubnormal

    // runNormal
    int runNormal(uint64 ullSgfnd, int iExpt, int iPrec)
    {
        bool fRound = 0 == ullSgfnd % 2;
        const uint64 ullBase = static_cast<uint64>(1) << iPrec;
        if (iExpt >= 0)
        {
            if (ullBase != ullSgfnd)
            {
                Val be = ashl(1, iExpt);
                return scale(
                    ullSgfnd, iExpt,
                    mul(be, ashl(ullSgfnd, 1)), Fixnum::Encode(2),
                    be, be,
                    fRound, fRound );
            }
            else
            {
                Val be  = ashl(1, iExpt);
                Val be1 = ash(be, 1);
                return scale(
                    ullSgfnd, iExpt,
                    mul(be1, ashl(ullSgfnd, 1)), Fixnum::Encode(4),
                    be1, be,
                    fRound, fRound );
            }
        }
        else if (ullBase != ullSgfnd)
        {
            return scale(
                ullSgfnd, iExpt,
                ashl(ullSgfnd, 1), ashl(2, -iExpt),
                Fixnum::Encode(1), Fixnum::Encode(1),
                fRound, fRound );
        }
        else
        {
            return scale(
                ullSgfnd, iExpt,
                ashl(ullSgfnd, 4), ashl(2, 1 - iExpt),
                Fixnum::Encode(2), Fixnum::Encode(1),
                fRound, fRound );
        }
    } // runNormal

    // scale
    int scale(
        uint64 ullSgfnd, int iExpt,    // for estimation of k
        Val r, Val s, Val mp, Val mm, bool fLow, bool fHigh )
    {
        double dblEst = (iExpt + integer_length(ullSgfnd) - 1);
            dblEst *= 0.30102999566398114e0;    // (/ (log 2) (log 10))
            dblEst -= 1e-10;
            dblEst = ::ceil(dblEst);

        int iEst = static_cast<int>(dblEst);

        if (iEst >= 0)
        {
            return fixup(
                r, mul(s, expt10(iEst)),
                mp, mm,
                iEst,
                fLow, fHigh );
        }
        else
        {
            Val scale = expt10(-iEst);
            return fixup(
                mul(r, scale), s,
                mul(mp, scale), mul(mm, scale),
                iEst,
                fLow, fHigh );
        } // if
    } // scale

    // fixup
    int fixup(Val r, Val s, Val mp, Val mm, int k, bool fLow, bool fHigh)
    {
        if (fHigh ? cmp(add(r, mp), s) >= 0 : cmp(add(r, mp), s) > 0)
        {
            generate(r, s, mp, mm, fLow, fHigh);
            k += 1;
        }
        else
        {
            generate(mul(r, 10), s, mul(mp, 10), mul(mm, 10), fLow, fHigh);
        }
        return k - 1;
    } // fixup

    // generate
    void generate(Val r, Val s, Val mp, Val mm, bool fLow, bool fHigh)
    {

      loop:
        char16 wchDigit;
        {
            Val d = truncate(r, s, &r);
            wchDigit = static_cast<char16>(Fixnum::Decode_(d) + '0');
                ASSERT(wchDigit >= '0' && wchDigit <= '9');
        } // wchDigit

        bool fTc1 = fLow  ? cmp(r, mm) <= 0: cmp(r, mm) < 0;
        bool fTc2 = fHigh ? cmp(add(r, mp), s) >= 0 : cmp(add(r, mp), s) > 0;

        if (! fTc1)
        {
            if (! fTc2)
            {
                *m_pwchDigit++ = wchDigit;
                r = mul(r, 10);
                mp = mul(mp, 10);
                mm = mul(mm, 10);
                goto loop;
            }
            else
            {
                *m_pwchDigit++ = static_cast<char>(wchDigit + 1);
            }
        }
        else if (! fTc2)
        {
            *m_pwchDigit++ = wchDigit;
        }
        else if (cmp(ash(r, 1), s) < 0)
        {
            *m_pwchDigit++ = wchDigit;
        }
        else
        {
            *m_pwchDigit++ = static_cast<char>(wchDigit + 1);
        }
    } // generate

    // integer_length
    int integer_length(uint64 k)
    {
        int n = 0;
        while (0 != k)
        {
            k >>= 1;
            n += 1;
        } // while
        return n;
    } // integer_length
}; // FloatToDigit


//////////////////////////////////////////////////////////////////////
//
// print_float_aux
//
//  Description:
//    Prints floating pointer number digits in free-format.
//
//      10^-3 <= |x| < 10^7 => dddd.dddd
//      otherwise           => d.dddd E <expt>
//
static void print_float_aux(
    Val     stream,
    UINT    nSign,
    LPCWSTR pwszDigit,
    char16  chMarker,
    int     iExpt )
{
    if (nSign)
    {
        write_char('-', stream);
    }

    if (iExpt >= -3 && iExpt <= 6)
    {
        // 10^3 <= |x| < 10^7 => free-format

        if (iExpt < 0)
        {
            write_string(L"0.", stream);
            for (int i = iExpt + 1; i < 0; i++)
            {
                write_char('0', stream);
            } // for i
        } // if

        do
        {
            if (0 == *pwszDigit)
            {
                write_char('0', stream);
            }
            else
            {
                write_char(*pwszDigit, stream);
                pwszDigit++;
            }

            if (0 == iExpt)
            {
                write_char('.', stream);
            }

            iExpt -= 1;
        } while (0 != *pwszDigit || iExpt >= -1);

        if ('E' != chMarker)
        {
            write_char(chMarker, stream);
            write_char('0', stream);
        }
    }
    else
    {
        // Free-Format with Exponential
        write_char(*pwszDigit++, stream);
        write_char('.', stream);
        if (0 == *pwszDigit)
        {
            write_char('0', stream);
        }
        else
        {
            write_string(pwszDigit, stream);
        }
        format(stream, L"~C~D",
            Character::Encode(chMarker),
            Fixnum::Encode(iExpt) );
    } // if
} // print_float_aux

} // namespace

//////////////////////////////////////////////////////////////////////
//
// print_double_float
//
template<class Float_, typename uint_> Val
print_float_(Val x, Val stream)
{
    const Float_::Layout* p =
        x->Decode<Float_>()->GetLayout();

    switch (Float_::Classify(p))
    {
    case FpClass_Normal:
    {
        uint_ ullSgfnd = Float_::GetSignificand(p);
            ullSgfnd |= static_cast<uint_>(1) << Float_::SignificandBits;

        FloatToDigit oConv;

        int iExpt10 = oConv.RunNormal(
                ullSgfnd, 
                p->m_nExponent - Float_::ExponentBias -
                    Float_::NormalPrecision + 1,
                Float_::NormalPrecision );

        print_float_aux(stream,
            p->m_nSign, oConv.GetDigits(), Float_::GetMarker(), iExpt10 );
        break;
    } // FpClassNormal

    case FpClass_Subnormal:
    {
        uint_ ullSgfnd = Float_::GetSignificand(p);

        FloatToDigit oConv;

        int iExpt10 = oConv.RunSubnormal(
            ullSgfnd,
            Float_::ExponentSubnormal );

        print_float_aux(stream,
            p->m_nSign, oConv.GetDigits(), Float_::GetMarker(), iExpt10 );
        break;
    } // FpClass_Subnormal

    case FpClass_Zero:
        if (p->m_nSign)
        {
            write_char('-', stream);
        }
        write_string(L"0.0", stream);
        write_char(Float_::GetMarker(), stream);
        write_char('0', stream);
        break;

    case FpClass_Infinity:
        write_string(L"#<", stream);
        write_string(Float_::GetTypeName(), stream);
        format(stream, L" ~CInfinity>",
            p->m_nSign ?
                Character::Encode('-') :
                Character::Encode('+') );
        break;

    case FpClass_NaN:
        write_string(L"#<", stream);
        write_string(Float_::GetTypeName(), stream);
        format(stream, L" ~CNaN>",
            p->m_nSign ?
                Character::Encode('-') :
                Character::Encode('+') );
        break;

    case FpClass_SNaN:
        write_string(L"#<", stream);
        write_string(Float_::GetTypeName(), stream);
        format(stream, L" ~CSNaN>",
            p->m_nSign ?
                Character::Encode('-') :
                Character::Encode('+') );
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch fpclass

    return x;
} // print_float_


Val print_double_float(Val x, Val stream)
    { return print_float_<DoubleFloatImpl, uint64>(x, stream); }


Val print_single_float(Val x, Val stream)
    { return print_float_<SingleFloatImpl, uint32>(x, stream); }


//////////////////////////////////////////////////////////////////////
//
// classify_double
//
template<
    class Self_, class Kernel_, class Layout_,
    typename float_, typename uint_, typename int_,
    typename Bignum_ >
FpClass FloatImpl_<Self_, Kernel_, Layout_, float_, uint_, int_, Bignum_>::
Classify(const Layout_* p)
{
    uint_ ullSignificand = Self_::GetSignificand(p);
    switch (p->m_nExponent)
    {
    case Kernel_::ExponentMin:
        if (0 == ullSignificand)
        {
            return FpClass_Zero;
        }
        else
        {
            return FpClass_Subnormal;
        }

    case Kernel_::ExponentMax:
        if (0 == ullSignificand)
        {
            return FpClass_Infinity;
        }
        else
        {
            return
                ullSignificand &
                    (static_cast<uint_>(1) << (Kernel_::SignificandBits - 1)) ?
                FpClass_NaN :
                FpClass_SNaN;
        }

    default:
        return FpClass_Normal;
    } // switch exp
} // FloatImpl_::::Classify


//////////////////////////////////////////////////////////////////////
//
// Rational
//
// Implementation of cl:rationl. We need this function for comparing
// float and rational.
//
template<
    class Self_, class Kernel_, class Layout_,
    typename float_, typename uint_, typename int_, typename Bignum_ >
Val FloatImpl_<Self_, Kernel_, Layout_, float_, uint_, int_, Bignum_>::
Rational(const Layout_* p)
{
    switch (Self_::Classify(p))
    {
    case FpClass_Normal:
    {
        int_ ll = 1;
            ll <<= Self_::SignificandBits;
            ll |= Self_::GetSignificand(p);
            ll *= (1 - p->m_nSign * 2);

        Bignum_ oNum(ll);

        int iExponent = p->m_nExponent - Self_::ExponentBias;

        if (iExponent >= Self_::NormalPrecision)
        {
            return ash(oNum, iExponent - Self_::NormalPrecision);
        }
        else
        {
            Val den = ashl(1, Self_::NormalPrecision - iExponent);
            return RatioImpl::Make(oNum, den);
        }
    } // FpClass_Normal

    case FpClass_Subnormal:
    {
        int_ ll = Self_::GetSignificand(p) * (1 - p->m_nSign * 2);

        Bignum_ oNum(ll);

        Val den = ashl(1, -Self_::ExponentSubnormal);
        return RatioImpl::Make(oNum, den);
    } // FpClass_Subnormal

    case FpClass_Zero:
        return Fixnum::Encode(0);

    case FpClass_Infinity:
    case FpClass_NaN:
    case FpClass_SNaN:
    {
        float_ flt = *reinterpret_cast<const float_*>(p);
        error(Qarithmetic_error,
            Koperation, Qrational,
            Koperands,  list(Self_::Make(flt)) );
    } // FpClass_Infinity

    default:
        CAN_NOT_HAPPEN();
    } // switch fpclass
} // FloatImpl_::Rational


template<
    class Self_, class Kernel_, class Layout_,
    typename float_, typename uint_, typename int_, typename Bignum_ >
Val FloatImpl_<Self_, Kernel_, Layout_, float_, uint_, int_, Bignum_>::
ToInt(const Layout_* p)
{
    switch (Self_::Classify(p))
    {
    case FpClass_Normal:
    {
        int_ ll = 1;
            ll <<= Self_::SignificandBits;
            ll |= Self_::GetSignificand(p);
            ll *= (1 - p->m_nSign * 2);

        Bignum_ oNum(ll);

        int iExponent = p->m_nExponent - Self_::ExponentBias;

        return ash(oNum, iExponent - Self_::NormalPrecision + 1);
    } // FpClass_Normal

    case FpClass_Subnormal:
    case FpClass_Zero:
        return Fixnum::Encode(0);

    case FpClass_Infinity:
    case FpClass_NaN:
    case FpClass_SNaN:
    {
        float_ flt = *reinterpret_cast<const float_*>(p);
        error(Qarithmetic_error,
            Koperation, Qrational,
            Koperands,  list(Self_::Make(flt)) );
    } // FpClass_Infinity

    default:
        CAN_NOT_HAPPEN();
    } // switch fpclass
} // FloatImpl_::ToInt


Val DoubleFloatImpl::Make(Val x)
    { return Make(convert_to_float64(x)); }

Val SingleFloatImpl::Make(Val x)
    { return Make(convert_to_float32(x)); }

// SingleFloatImpl::SetSignificand
void SingleFloatImpl::SetSignificand(Layout* p, Val m)
{
    if (! m->Is<Fixnum>() ||
        static_cast<UInt>(Fixnum::Decode_(m)) >= (1u << 22) )
    {
        error(make_type_error(m, list(Qunsigned_byte, Fixnum::Encode(22))));
    }

    p->m_nSignificand = Fixnum::Decode_(m);
} // SingleFloatImpl::SetSignificand


// DoubleFloatImpl::SetSignificand
void DoubleFloatImpl::SetSignificand(Layout* p, Val m)
{
    uint64 ull;
        if (m->Is<Fixnum>())
        {
            if (Fixnum::Decode_(m) < 0) goto type_error;
            ull = Fixnum::Decode_(m);
        }
        else if (m->Is<Bignum>())
        {
            BignumImpl* pA = m->Decode<BignumImpl>();
            if (pA->IsMinus()) goto type_error;
            ull = 0;
            Int n = pA->GetLength();
            for (Int i = 0; i < n; i++)
            {
                ull <<= BignumImpl::Bits;
                ull |= pA->m_rgBigit[i];
                if (ull >= (static_cast<uint64>(1) << 52)) goto type_error;
            } // for i
        }
        else
        {
            goto type_error;
        }

    p->m_nSignificandH = static_cast<uint32>(ull >> 32);
    p->m_nSignificandL = static_cast<uint32>(ull);
    return;

  type_error:
    error(make_type_error(m, list(Qunsigned_byte, Fixnum::Encode(52))));
} // DoubleFloatImpl::SetSignificand


// DoubleFloatImpl::TruncateImpl
double DoubleFloatImpl::TruncateImpl(double x)
    { return truncate(x); }

// SingleFloatImpl::TruncateImpl
float SingleFloatImpl::TruncateImpl(float x)
    { return truncatef(x); }

//////////////////////////////////////////////////////////////////////
//
// decode_float_ template
//
template<class T>
Val decode_float_(Val x, Val* out_e, Val* out_s)
{
    if (! x->Is<T>()) error(make_type_error(x, T::GetSymbol()));

    const T::Layout* p = x->Decode<T>()->GetLayout();

    *out_e = Fixnum::Encode(p->m_nExponent);
    *out_s = Fixnum::Encode(p->m_nSign);
    return make_uint64(T::GetSignificand(p));
} // decode_float_


//////////////////////////////////////////////////////////////////////
//
// encode_float_ template
//
template<class Impl_, typename float_>
Val encode_float_(Val m, Val e, Val s)
{
    float_ x;
    Impl_::Layout* p = reinterpret_cast<Impl_::Layout*>(&x);
    Impl_::SetSignificand(p, m);
    p->m_nExponent = Fixnum::Decode_(e);
    p->m_nSign     = Fixnum::Decode_(s);
    return Impl_::Make(x);
} // encode_float_


// decode_float32
Val decode_float32(Val x, Val* out_e, Val* out_s)
    { return decode_float_<SingleFloatImpl>(x, out_e, out_s); }


// decode_float64
Val decode_float64(Val x, Val* out_e, Val* out_s)
    { return decode_float_<DoubleFloatImpl>(x, out_e, out_s); }

// encode_float32
Val encode_float32(Val x, Val e, Val s)
    { return encode_float_<SingleFloatImpl, float32>(x, e, s); }


// encode_float64
Val encode_float64(Val x, Val e, Val s)
    { return encode_float_<DoubleFloatImpl, float64>(x, e, s); }


#if 0
#define define_div(mp_name, mp_round) \
    Val f##mp_name##_float32_1(Val x) \
    { \
        float a = SingleFloatImpl::Get(x); \
        return SingleFloatImpl::Make(mp_round##f(a)); \
    } \
    Val f##mp_name##_float64_1(Val x) \
    { \
        double a = DoubleFloatImpl::Get(x); \
        return DoubleFloatImpl::Make(mp_round(a)); \
    } // define_div

define_div(ceiling,  ceil)
define_div(floor,    floor)
define_div(round,    rint)
define_div(truncate, truncate)
#endif

// float32_truncate -- convert float32 to integer. fraction should be zero.
Val float32_truncate(Val x)
{ 
    check_type(x, single_float);
    return SingleFloatImpl::ToInt(x->Decode<SingleFloatImpl>()->GetLayout());
} // float32_truncate

// float64_truncate -- convert float64 to integer. fraction should be zero.
Val float64_truncate(Val x)
{ 
    check_type(x, double_float);
    return DoubleFloatImpl::ToInt(x->Decode<DoubleFloatImpl>()->GetLayout());
} // float64_truncate

} // MiniLisp


namespace CommonLisp
{
Val rational(double d)
{
    return DoubleFloatImpl::Rational(
        reinterpret_cast<DoubleFloatImpl::Layout*>(&d) );
} // rational


Val rational(float f)
{
    return SingleFloatImpl::Rational(
        reinterpret_cast<SingleFloatImpl::Layout*>(&f) );
} // SingleFloatImpl::Rational

} // CommonLisp
