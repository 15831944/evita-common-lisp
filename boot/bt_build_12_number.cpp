#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - build - 12 Numbers
// boot/bt_build_12_number.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/boot/bt_build_12_number.cpp 13 2006-10-05 15:03:07 yosi $
//
#include "../mini/mini_lisp.h"

namespace Boot
{

extern Val defconstant(LPCWSTR, Val);

// make_bignum
Val make_bignum(const Bigit* prgBigit, uint cBigits)
{
    // Normalization
    //  o skip MSB=0  if non-negative
    //  o skil MSB=-1 if negative
    if (static_cast<SignedBigit>(prgBigit[cBigits-1]) >= 0)
    {
        while (cBigits >= 2)
        {
            if (0 != prgBigit[cBigits-1]) break;
            if (static_cast<SignedBigit>(prgBigit[cBigits-2]) < 0) break;
            --cBigits;
        } // while
    }
    else
    {
        while (cBigits >= 2)
        {
            if (static_cast<Bigit>(-1) != prgBigit[cBigits-1]) break;
            if (static_cast<SignedBigit>(prgBigit[cBigits-2]) >= 0) break;
            --cBigits;
        } // while
    } // if

    // Is fixnum?
    if (1 == cBigits)
    {
        Int ival = static_cast<SignedBigit>(prgBigit[cBigits-1]);
        if (ival >= Fixnum::MostNegative && ival <= Fixnum::MostPositive)
        {
            return Fixnum::Encode(ival);
        }
    } // if

    Val big = MiniThread::Get()->AllocBinVec(
        CLASSD_bignum,
        Fixnum::Encode(cBigits) );

    ::memcpy(
        big->Decode<Bignum>()->m_rgBigit,
        prgBigit,
        sizeof(Bigit) * cBigits );

    return big;
} // make_bignum


// make_float64
Val make_float64(uint nSign, uint nExp, uint64 nSgfnd)
{
    ASSERT(0 == nSign || 1 == nSign);
    ASSERT(nExp  <= DoubleFloat::ExponentMax);
    ASSERT(nSgfnd < 1ull << DoubleFloat::SignificandBits);

    Val x = MiniThread::Get()->AllocBinObj(CLASSD_double_float);

    DoubleFloat::Layout* p = reinterpret_cast<DoubleFloat::Layout*>(
            &x->Decode<DoubleFloat>()->m_dbl );
        p->m_nSign      = nSign;
        p->m_nExponent  = nExp;
        p->m_nSignificandH = static_cast<uint32>(nSgfnd >> 32);
        p->m_nSignificandL = static_cast<uint32>(nSgfnd);

    return x;
} // make_float64


// make_float64
Val make_float64(uint nHigh, uint nLow)
{
    Val x = MiniThread::Get()->AllocBinObj(CLASSD_double_float);

    Float64_LayoutHL* p = reinterpret_cast<Float64_LayoutHL*>(
            &x->Decode<DoubleFloat>()->m_dbl );

    p->m_nHigh = nHigh;
    p->m_nLow  = nLow;

    return x;
} // make_float64


// make_float64
Val make_float64(double dbl)
{
    const DoubleFloat::Layout* p =
        reinterpret_cast<DoubleFloat::Layout*>(&dbl);

    unsigned __int64 nSignificand = p->m_nSignificandH;
        nSignificand <<= 32;
        nSignificand |= p->m_nSignificandL;

    return make_float64(p->m_nSign, p->m_nExponent, nSignificand);
} // make_float64


// make_float32
Val make_float32(uint nSign, uint nExp, uint32 nSgfnd)
{
    ASSERT(0 == nSign || 1 == nSign);
    ASSERT(nExp <= SingleFloat::ExponentMax);
    ASSERT(nSgfnd < 1u << SingleFloat::SignificandBits);

    Val x = MiniThread::Get()->AllocBinObj(CLASSD_single_float);

    SingleFloat::Layout* p = reinterpret_cast<SingleFloat::Layout*>(
            &x->Decode<SingleFloat>()->m_flt );
        p->m_nSign     = nSign;
        p->m_nExponent = nExp;
        p->m_nSignificand = nSgfnd;

    return x;
} // make_float32

static const LPCWSTR
k_rgpwszSign[2] =
{
    L"POSITIVE",
    L"NEGATIVE",
}; // k_rgpwszSign


static const LPCWSTR
k_rgpwszSize[4] =
{
    L"SHORT",
    L"SINGLE",
    L"DOUBLE",
    L"LONG",
}; // k_rgpwszSize


void define_float(
    uint nSign,
    uint nSizeBase,
    LPCWSTR pwsz,
    LPCWSTR pwsz2,
    Val x )
{
    WCHAR wsz[200];

    for (uint nSize = nSizeBase; nSize <= nSizeBase + 1; nSize++)
    {
        if ('-' != *pwsz)
        {
            // least-positive-normalized-short-float
            ::wsprintf(wsz, L"%ls-%ls%ls-%ls-FLOAT",
                pwsz, k_rgpwszSign[nSign], pwsz2, k_rgpwszSize[nSize] );
        }
        else if (0 == nSign)
        {
            // double-float-epsilon
            ::wsprintf(wsz, L"%ls-FLOAT%ls",
                k_rgpwszSize[nSize], pwsz );
        }
        else
        {
            // double-float-negative-epsilon
            ::wsprintf(wsz, L"%ls-FLOAT-%ls%ls",
                k_rgpwszSize[nSize], k_rgpwszSign[1], pwsz );
        }
        defconstant(wsz, x);
    } // nSize
} // define_float


void define_float64(
    uint    nExpt,
    uint64  nSgfnd,
    LPCWSTR pwsz,
    LPCWSTR pwsz2 = L"" )
{
    for (uint nSign = 0; nSign <= 1; nSign++)
    {
        Val x = make_float64(nSign, nExpt, nSgfnd);
        define_float(nSign, 2, pwsz, pwsz2, x);
    } // for nSign
} // define_float64


void define_float32(
    uint    nExpt,
    uint32  nSgfnd,
    LPCWSTR pwsz,
    LPCWSTR pwsz2 = L"" )
{
    for (uint nSign = 0; nSign <= 1; nSign++)
    {
        Val x = make_float32(nSign, nExpt, nSgfnd);
        define_float(nSign, 0, pwsz, pwsz2, x);
    } // for nSign
} // define_float32


// build_12_Numbers
void build_12_Numbers()
{
    ////////////////////////////////////////////////////////////
    //
    // Float
    //
    define_float64(0, 1, L"LEAST");
    define_float64(1, 0, L"LEAST", L"-NORMALIZED");

    define_float64(
        DoubleFloat::ExponentMax - 1,
        (1ull << DoubleFloat::SignificandBits) - 1,
        L"MOST" );

    // double-float-epsilon 1.1102230246251568d-16
    //  (4503599627370497 -105 1)
    defconstant(L"DOUBLE-FLOAT-EPSILON",
        make_float64(
            0,
            DoubleFloat::ExponentBias - DoubleFloat::NormalPrecision,
            1 ) );

    // double-float-epsilon 5.551115123125784d-17
    //  (4503599627370497 -106 1)
    defconstant(L"DOUBLE-FLOAT-NEGATIVE-EPSILON",
        make_float64(
            0,
            DoubleFloat::ExponentBias - DoubleFloat::NormalPrecision - 1,
            1 ) );

    define_float32(1, 0, L"LEAST", L"-NORMALIZED");
    define_float32(0, 1, L"LEAST");

    define_float32(
        SingleFloat::ExponentMax - 1,
        (1 << SingleFloat::SignificandBits) - 1,
        L"MOST" );

    // single-float-epsilon 5.960465E-8 (8388609 -47 1)
    defconstant(L"SINGLE-FLOAT-EPSILON",
        make_float32(
            0,
            SingleFloat::ExponentBias - SingleFloat::NormalPrecision,
            1 ) );

    // single-float-negative-epsilon 2.9802326E-8 (8388609 -48 1)
    defconstant(L"SINGLE-FLOAT-NEGATIVE-EPSILON",
        make_float32(
            0,
            SingleFloat::ExponentBias - SingleFloat::NormalPrecision - 1,
            1 ) );

    defconstant(L"SHORT-FLOAT-EPSILON",
        symbol_value(Q("SINGLE-FLOAT-EPSILON")) );

    defconstant(L"SHORT-FLOAT-NEGATIVE-EPSILON",
        symbol_value(Q("SINGLE-FLOAT-NEGATIVE-EPSILON")) );

    defconstant(L"LONG-FLOAT-EPSILON",
        symbol_value(Q("DOUBLE-FLOAT-EPSILON")) );

    defconstant(L"LONG-FLOAT-NEGATIVE-EPSILON",
        symbol_value(Q("DOUBLE-FLOAT-NEGATIVE-EPSILON")) );

    // We don't need infinity and NaN as constant. We should generate
    // these non-number by expression to signal exception if exception
    // isn't masked. See libm implementation arch/generic/lisp/math/.
    #if 0
    defconstant(L"SINGLE-FLOAT-INFINITY", make_float32(0,  255, 0));
    defconstant(L"DOUBLE-FLOAT-INFINITY", make_float64(0, 2047, 0));

    defconstant(L"SINGLE-FLOAT-NEGATIVE-INFINITY", make_float32(1,  255, 0));
    defconstant(L"DOUBLE-FLOAT-NEGATIVE-INFINITY", make_float64(1, 2047, 0));

    defconstant(L"SINGLE-FLOAT-NAN", make_float32(0,  255, 1));
    defconstant(L"DOUBLE-FLOAT-NAN", make_float64(0, 2047, 1));

    defconstant(L"SINGLE-FLOAT-SNAN", make_float32(0,  255, 1u<<21));
    defconstant(L"DOUBLE-FLOAT-SNAN", make_float64(0, 2047, 1ull<<50));
    #endif

    // PI = 3.14159265358979323846
    defconstant(L"PI", make_float64(0x400921FB, 0x54442D18));

    // (ash (1- (ash 1 24)) 104)
    {
        const uint k = 128 / (sizeof(Bigit) * 8) + 1;
        Bigit rgBigit[k];
            ::ZeroMemory(rgBigit, sizeof(rgBigit));
        #if SIZEOF_VAL == 4
            rgBigit[k-2] = static_cast<Bigit>(0xffffff) << (104-96);
        #elif SIZEOF_VAL == 8
            rgBigit[k-2] = static_cast<Bigit>(0xffffff) << (104-64);
        #else
            #error "Unsupported"
        #endif

        defobject(
            val_most_positive_float32_bignum,
            make_bignum(rgBigit, k) );
    }

    // (ash (1- (ash 1 53)) 971)
    {
        const uint k = 1024 / (sizeof(Bigit) * 8) + 1;
        Bigit rgBigit[k];
            ::ZeroMemory(rgBigit, sizeof(rgBigit));
        #if SIZEOF_VAL == 4
            rgBigit[k-3] = 0xfffff800u;
            rgBigit[k-2] = 0xffffffffu;
        #elif SIZEOF_VAL == 8
            rgBigit[k-2] = 0xfffffffffffff800ull;
        #else
            #error "Unsupported"
        #endif

        defobject(
            val_most_positive_float64_bignum,
            make_bignum(rgBigit, k) );
    }

} // build_12_Numbers

} // Boot
