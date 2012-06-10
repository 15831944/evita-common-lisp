//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Floating-Point Number
// kernel/ke_fp.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_float.h#1 $
//
#if !defined(INCLUDE_kernel_ke_fp_h)
#define INCLUDE_kernel_ke_fp_h

#if FLOAT_WORD_ORDER == BIG_ENDIAN

    struct Float32_Layout
    {
        unsigned int    m_nSign      :  1;
        unsigned int    m_nExponent     : 8;
        unsigned int    m_nSignificand  : 23;
    }; // Float32_Layout

    struct Float64_Layout
    {
        unsigned int    m_nSign         : 1;
        unsigned int    m_nExponent     : 11;
        unsigned int    m_nSignificandH : 20;
        unsigned int    m_nSignificandL : 32;
    }; // Float64_Layout

    struct Float64_LayoutHL
    {
        uint32  m_nHigh;
        uint32  m_nLow;
    }; // Float64_LayoutHL

#elif FLOAT_WORD_ORDER == LITTLE_ENDIAN

    struct Float32_Layout
    {
        unsigned int    m_nSignificand  : 23;
        unsigned int    m_nExponent     : 8;
        unsigned int    m_nSign         : 1;
    }; // Float32_Layout

    struct Float64_Layout
    {
        unsigned int    m_nSignificandL : 32;
        unsigned int    m_nSignificandH : 20;
        unsigned int    m_nExponent     : 11;
        unsigned int    m_nSign         : 1;
    }; // Float64_Layout

    struct Float64_LayoutHL
    {
        uint32  m_nLow;
        uint32  m_nHigh;
    }; // Float64_LayoutHL
#else // FLOAT_WORD_ORDER == LITTLE_ENDIAN
    #error Unsupported FLOAT_WORD_ORDER
#endif // FLOAT_WORD_ORDER == LITTLE_ENDIAN

#endif //!defined(INCLUDE_kernel_ke_fp_h)
