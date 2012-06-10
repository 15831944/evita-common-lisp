//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - arch 64
// kernel/ke_arch64.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/generic/kernel/gen_ke_arch64.cpp#3 $
//
#if SIZEOF_VAL == 8

#include "../../../kernel/ke_arch.h"

namespace Kernel
{

const uint64
Kernel::Arch64::k_rgfBit[64] =
{
    1ull << 0,  1ull << 1,  1ull << 2,  1ull << 3,  1ull << 4,
    1ull << 5,  1ull << 6,  1ull << 7,  1ull << 8,  1ull << 9,
    1ull << 10, 1ull << 11, 1ull << 12, 1ull << 13, 1ull << 14,
    1ull << 15, 1ull << 16, 1ull << 17, 1ull << 18, 1ull << 19,
    1ull << 20, 1ull << 21, 1ull << 22, 1ull << 23, 1ull << 24,
    1ull << 25, 1ull << 26, 1ull << 27, 1ull << 28, 1ull << 29,
    1ull << 30, 1ull << 31, 1ull << 32, 1ull << 33, 1ull << 34,
    1ull << 35, 1ull << 36, 1ull << 37, 1ull << 38, 1ull << 39,
    1ull << 40, 1ull << 41, 1ull << 42, 1ull << 43, 1ull << 44,
    1ull << 45, 1ull << 46, 1ull << 47, 1ull << 48, 1ull << 49,
    1ull << 50, 1ull << 51, 1ull << 52, 1ull << 53, 1ull << 54,
    1ull << 55, 1ull << 56, 1ull << 57, 1ull << 58, 1ull << 59,
    1ull << 60, 1ull << 61, 1ull << 62, 1ull << 63
}; // Kernel::Arch64::k_rgfBit


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
//
// (loop for i below 64 do (format t "0x~16,'0Xull,    // ~D~%" (1- (ash 1 i)) i))
const Arch64::BitEltT
Arch64::k_rgnLeaderMask[64] =
{
    0x0000000000000000ull,    // 0
    0x0000000000000001ull,    // 1
    0x0000000000000003ull,    // 2
    0x0000000000000007ull,    // 3
    0x000000000000000Full,    // 4
    0x000000000000001Full,    // 5
    0x000000000000003Full,    // 6
    0x000000000000007Full,    // 7
    0x00000000000000FFull,    // 8
    0x00000000000001FFull,    // 9
    0x00000000000003FFull,    // 10
    0x00000000000007FFull,    // 11
    0x0000000000000FFFull,    // 12
    0x0000000000001FFFull,    // 13
    0x0000000000003FFFull,    // 14
    0x0000000000007FFFull,    // 15
    0x000000000000FFFFull,    // 16
    0x000000000001FFFFull,    // 17
    0x000000000003FFFFull,    // 18
    0x000000000007FFFFull,    // 19
    0x00000000000FFFFFull,    // 20
    0x00000000001FFFFFull,    // 21
    0x00000000003FFFFFull,    // 22
    0x00000000007FFFFFull,    // 23
    0x0000000000FFFFFFull,    // 24
    0x0000000001FFFFFFull,    // 25
    0x0000000003FFFFFFull,    // 26
    0x0000000007FFFFFFull,    // 27
    0x000000000FFFFFFFull,    // 28
    0x000000001FFFFFFFull,    // 29
    0x000000003FFFFFFFull,    // 30
    0x000000007FFFFFFFull,    // 31
    0x00000000FFFFFFFFull,    // 32
    0x00000001FFFFFFFFull,    // 33
    0x00000003FFFFFFFFull,    // 34
    0x00000007FFFFFFFFull,    // 35
    0x0000000FFFFFFFFFull,    // 36
    0x0000001FFFFFFFFFull,    // 37
    0x0000003FFFFFFFFFull,    // 38
    0x0000007FFFFFFFFFull,    // 39
    0x000000FFFFFFFFFFull,    // 40
    0x000001FFFFFFFFFFull,    // 41
    0x000003FFFFFFFFFFull,    // 42
    0x000007FFFFFFFFFFull,    // 43
    0x00000FFFFFFFFFFFull,    // 44
    0x00001FFFFFFFFFFFull,    // 45
    0x00003FFFFFFFFFFFull,    // 46
    0x00007FFFFFFFFFFFull,    // 47
    0x0000FFFFFFFFFFFFull,    // 48
    0x0001FFFFFFFFFFFFull,    // 49
    0x0003FFFFFFFFFFFFull,    // 50
    0x0007FFFFFFFFFFFFull,    // 51
    0x000FFFFFFFFFFFFFull,    // 52
    0x001FFFFFFFFFFFFFull,    // 53
    0x003FFFFFFFFFFFFFull,    // 54
    0x007FFFFFFFFFFFFFull,    // 55
    0x00FFFFFFFFFFFFFFull,    // 56
    0x01FFFFFFFFFFFFFFull,    // 57
    0x03FFFFFFFFFFFFFFull,    // 58
    0x07FFFFFFFFFFFFFFull,    // 59
    0x0FFFFFFFFFFFFFFFull,    // 60
    0x1FFFFFFFFFFFFFFFull,    // 61
    0x3FFFFFFFFFFFFFFFull,    // 62
    0x7FFFFFFFFFFFFFFFull,    // 63
}; // k_rgnLeaderMask


// (loop  with x = (1- (ash 1 64)) for i below 64 for m = (1- (ash 1 i))
//    do (format t "    0x~16,'0Xull,    // ~D~%" (- x m) i) )
const Arch64::BitEltT
Arch64::k_rgnTrailerMask[64] =
{
    0xFFFFFFFFFFFFFFFFull,    // 0
    0xFFFFFFFFFFFFFFFEull,    // 1
    0xFFFFFFFFFFFFFFFCull,    // 2
    0xFFFFFFFFFFFFFFF8ull,    // 3
    0xFFFFFFFFFFFFFFF0ull,    // 4
    0xFFFFFFFFFFFFFFE0ull,    // 5
    0xFFFFFFFFFFFFFFC0ull,    // 6
    0xFFFFFFFFFFFFFF80ull,    // 7
    0xFFFFFFFFFFFFFF00ull,    // 8
    0xFFFFFFFFFFFFFE00ull,    // 9
    0xFFFFFFFFFFFFFC00ull,    // 10
    0xFFFFFFFFFFFFF800ull,    // 11
    0xFFFFFFFFFFFFF000ull,    // 12
    0xFFFFFFFFFFFFE000ull,    // 13
    0xFFFFFFFFFFFFC000ull,    // 14
    0xFFFFFFFFFFFF8000ull,    // 15
    0xFFFFFFFFFFFF0000ull,    // 16
    0xFFFFFFFFFFFE0000ull,    // 17
    0xFFFFFFFFFFFC0000ull,    // 18
    0xFFFFFFFFFFF80000ull,    // 19
    0xFFFFFFFFFFF00000ull,    // 20
    0xFFFFFFFFFFE00000ull,    // 21
    0xFFFFFFFFFFC00000ull,    // 22
    0xFFFFFFFFFF800000ull,    // 23
    0xFFFFFFFFFF000000ull,    // 24
    0xFFFFFFFFFE000000ull,    // 25
    0xFFFFFFFFFC000000ull,    // 26
    0xFFFFFFFFF8000000ull,    // 27
    0xFFFFFFFFF0000000ull,    // 28
    0xFFFFFFFFE0000000ull,    // 29
    0xFFFFFFFFC0000000ull,    // 30
    0xFFFFFFFF80000000ull,    // 31
    0xFFFFFFFF00000000ull,    // 32
    0xFFFFFFFE00000000ull,    // 33
    0xFFFFFFFC00000000ull,    // 34
    0xFFFFFFF800000000ull,    // 35
    0xFFFFFFF000000000ull,    // 36
    0xFFFFFFE000000000ull,    // 37
    0xFFFFFFC000000000ull,    // 38
    0xFFFFFF8000000000ull,    // 39
    0xFFFFFF0000000000ull,    // 40
    0xFFFFFE0000000000ull,    // 41
    0xFFFFFC0000000000ull,    // 42
    0xFFFFF80000000000ull,    // 43
    0xFFFFF00000000000ull,    // 44
    0xFFFFE00000000000ull,    // 45
    0xFFFFC00000000000ull,    // 46
    0xFFFF800000000000ull,    // 47
    0xFFFF000000000000ull,    // 48
    0xFFFE000000000000ull,    // 49
    0xFFFC000000000000ull,    // 50
    0xFFF8000000000000ull,    // 51
    0xFFF0000000000000ull,    // 52
    0xFFE0000000000000ull,    // 53
    0xFFC0000000000000ull,    // 54
    0xFF80000000000000ull,    // 55
    0xFF00000000000000ull,    // 56
    0xFE00000000000000ull,    // 57
    0xFC00000000000000ull,    // 58
    0xF800000000000000ull,    // 59
    0xF000000000000000ull,    // 60
    0xE000000000000000ull,    // 61
    0xC000000000000000ull,    // 62
    0x8000000000000000ull,    // 63
}; // k_rgnTrailerMask

CASSERT(sizeof(Arch64::k_rgnLeaderMask) == sizeof(Arch64::k_rgnTrailerMask));

} // Kernel

#endif // SIZEOF_VAL == 8
