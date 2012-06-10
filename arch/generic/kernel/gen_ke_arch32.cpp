//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - arch 32
// kernel/ke_arch32.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/generic/kernel/gen_ke_arch32.cpp#3 $
//
#if SIZEOF_VAL == 4

#include "../../../kernel/ke_arch.h"

namespace Kernel
{

const uint32
Kernel::Arch32::k_rgfBit[32] =
{
    1u << 0,  1u << 1,  1u << 2,  1u << 3,  1u << 4, 
    1u << 5,  1u << 6,  1u << 7,  1u << 8,  1u << 9, 
    1u << 10, 1u << 11, 1u << 12, 1u << 13, 1u << 14,
    1u << 15, 1u << 16, 1u << 17, 1u << 18, 1u << 19,
    1u << 20, 1u << 21, 1u << 22, 1u << 23, 1u << 24,
    1u << 25, 1u << 26, 1u << 27, 1u << 28, 1u << 29,
    1u << 30, 1u << 31,
}; // Kernel::Arch32::k_rgfBit


//////////////////////////////////////////////////////////////////////
//
// Mask For Bit Stream Leader and Trailer
//
// Bit Stream:
//  LLLL LLLL xxxx xxxx ... xxxx xxxx TTTT TTTT
//
//  x = bits for processing
//  L = leader bits  (not affect)
//  T = trailer bits (not affect)
//
// Note: Bit stream is stored in each 32bit word from LSB to MSB.
//
const Arch32::BitEltT
Arch32::k_rgnLeaderMask[32] =
{
    0x00000000u,    // 0
    0x00000001u,    // 1
    0x00000003u,    // 2
    0x00000007u,    // 3
    0x0000000Fu,    // 4
    0x0000001Fu,    // 5
    0x0000003Fu,    // 6
    0x0000007Fu,    // 7
    0x000000FFu,    // 8
    0x000001FFu,    // 9
    0x000003FFu,    // 10
    0x000007FFu,    // 11
    0x00000FFFu,    // 12
    0x00001FFFu,    // 13
    0x00003FFFu,    // 14
    0x00007FFFu,    // 15
    0x0000FFFFu,    // 16
    0x0001FFFFu,    // 17
    0x0003FFFFu,    // 18
    0x0007FFFFu,    // 19
    0x000FFFFFu,    // 20
    0x001FFFFFu,    // 21
    0x003FFFFFu,    // 22
    0x007FFFFFu,    // 23
    0x00FFFFFFu,    // 24
    0x01FFFFFFu,    // 25
    0x03FFFFFFu,    // 26
    0x07FFFFFFu,    // 27
    0x0FFFFFFFu,    // 28
    0x1FFFFFFFu,    // 29
    0x3FFFFFFFu,    // 30
    0x7FFFFFFFu     // 31
}; // k_rgnLeaderMask


const Arch32::BitEltT
Arch32::k_rgnTrailerMask[32] =
{
    0xFFFFFFFFu,    // 0
    0xFFFFFFFEu,    // 1
    0xFFFFFFFCu,    // 2
    0xFFFFFFF8u,    // 3
    0xFFFFFFF0u,    // 4
    0xFFFFFFE0u,    // 5
    0xFFFFFFC0u,    // 6
    0xFFFFFF80u,    // 7
    0xFFFFFF00u,    // 8
    0xFFFFFE00u,    // 9
    0xFFFFFC00u,    // 10
    0xFFFFF800u,    // 11
    0xFFFFF000u,    // 12
    0xFFFFE000u,    // 13
    0xFFFFC000u,    // 14
    0xFFFF8000u,    // 15
    0xFFFF0000u,    // 16
    0xFFFE0000u,    // 17
    0xFFFC0000u,    // 18
    0xFFF80000u,    // 19
    0xFFF00000u,    // 20
    0xFFE00000u,    // 21
    0xFFC00000u,    // 22
    0xFF800000u,    // 23
    0xFF000000u,    // 24
    0xFE000000u,    // 25
    0xFC000000u,    // 26
    0xF8000000u,    // 27
    0xF0000000u,    // 28
    0xE0000000u,    // 29
    0xC0000000u,    // 30
    0x80000000u     // 31
}; // k_rgnTrailerMask

CASSERT(sizeof(Arch32::k_rgnLeaderMask) == sizeof(Arch32::k_rgnTrailerMask));

} // Kernel

#endif // SIZEOF_VAL == 4
