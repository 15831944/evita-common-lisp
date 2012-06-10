//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - Code Generator
// x86_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/x86_arch.h#5 $
//
#if !defined(INCLUDE_arch_x86_arch_h)
#define INCLUDE_arch_x86_arch_h

namespace X86
{

enum Reg
{
    Gpr_EAX = 0x20, Gpr_ECX = 0x21, Gpr_EDX = 0x22, Gpr_EBX = 0x23,
    Gpr_ESP = 0x24, Gpr_EBP = 0x25, Gpr_ESI = 0x26, Gpr_EDI = 0x27,

    Gpr_None = Gpr_EBP,
    Gpr_SIB  = Gpr_ESP,

    Gpr_AX = 0x10, Gpr_CX = 0x11, Gpr_DX = 0x12, Gpr_BX = 0x13,
    Gpr_SP = 0x14, Gpr_BP = 0x15, Gpr_SI = 0x16, Gpr_DI = 0x17,

    Gpr_AL = 0x00, Gpr_CL = 0x01, Gpr_DL = 0x02, Gpr_BL = 0x03,
    Gpr_AH = 0x04, Gpr_CH = 0x05, Gpr_DH = 0x06, Gpr_BH = 0x07,

    Fpr_XMM0 = 0x08, Fpr_XMM1 = 0x09, Fpr_XMM2 = 0x0A, Fpr_XMM3 = 0x0B,
    Fpr_XMM4 = 0x0C, Fpr_XMM5 = 0x0D, Fpr_XMM6 = 0x0E, Fpr_XMM7 = 0x0F,

    ////////////////////////////////////////////////////////////
    //
    // Aliases
    //
    $rn   = Gpr_ECX,
    $rtcb = Gpr_EBP,
    $sp   = Gpr_ESP,

    $r0 = Gpr_EAX, $r1 = Gpr_EDX, $r2 = Gpr_EBX, $r3 = Gpr_ESI,
    $r4 = Gpr_EDI,

    $f0 = Fpr_XMM0, $f1 = Fpr_XMM1, $f2 = Fpr_XMM2, $f3 = Fpr_XMM3,
    $f4 = Fpr_XMM4, $f5 = Fpr_XMM5, $f6 = Fpr_XMM6, $f7 = Fpr_XMM7,
}; // Reg


//////////////////////////////////////////////////////////////////////
//
//  ModRm + SIB
//
//    7 6  5 4 3  2 1 0    7 6 5 4 3 2 1 0
//   +----+------+------+ +---+-----+------+ +----------------+
//   |mod | reg  |  r/m | | S | idx | base | |  disp8/disp32  |
//   +----+------+------+ +----------------+ +----------------+
//     00    disp0  [base]
//     01    disp8  [base+disp8]
//     10    disp32 [base+disp32]
//     11    reg    reg
//
//   When mod != 11 and r/m=100(esp):
//     mod=00 reg=xxx r/m=100   [base+index]
//     mod=01 reg=xxx r/m=100   [base+index+disp8]
//     mod=10 reg=xxx r/m=100   [base+index+disp32]
//
//   When mod=00 and r/m=101:
//     mod=00 reg=xxx r/m=101   [disp32]
//
//   When base=101(ebp)
//      mod=00 reg=xxx r/m=100 ss=xx idx=xxx base=101   [idx+disp32]
//      mod=01 reg=xxx r/m=100 ss=xx idx=xxx base=101   [idx+disp8+ebp]
//      mod=10 reg=xxx r/m=100 ss=xx idx=xxx base=101   [idx+disp32+ebp]
//
//   When idx=100, there is no index, so [esp+disp] are:
//      mod=00 reg=xxx r/m=100 ss=xx idx=100 base=100   [esp]
//      mod=01 reg=xxx r/m=100 ss=xx idx=100 base=100   [esp+disp8]
//      mod=10 reg=xxx r/m=100 ss=xx idx=100 base=100   [esp+disp32]
//
enum Mod
{
    Mod_Disp0   = 0x00,
    Mod_Disp8   = 0x40,
    Mod_Disp32  = 0x80,
    Mod_Reg     = 0xC0,
}; // Mod


enum Rm
{
    Rm_SIB      = 4,        // we can't use ESP as index.
    Rm_Disp32   = 5,        // we can't use disp0 with EBP.
}; // Rm


enum Scale
{
    Scale_1 = 0,
    Scale_2 = 0x40,
    Scale_4 = 0x80,
    Scale_8 = 0xC0,
}; // Scale


__forceinline uint8
ModRm(Mod eMod, Reg eR1, Reg eR2)
    { return static_cast<uint8>(eMod | ((eR1 & 7) << 3) | (eR2 & 7)); }


__forceinline uint8
ModRm(Mod eMod, Reg eR1, Rm eRm)
    { return static_cast<uint8>(eMod | ((eR1 & 7) << 3) | eRm); }


////////////////////////////////////////////////////////////
//
// Condition Code
//
enum Tttn
{
   tttn_O     = 0,          // overflow
   tttn_NO    = 1,          // no overflow
   tttn_B     = 2,          // below (ULT)
     tttn_NAE  = tttn_B,    // not above or equal
   tttn_NB    = 3,          // not below (UGE)
     tttn_AE = tttn_NB,     // above or equal
     tttn_NC = tttn_NB,     // no carry
   tttn_E     = 4,          // equal
     tttn_Z = tttn_E,       // zero
   tttn_NE    = 5,          // not equal
     tttn_NZ = tttn_NE,     // not zero
   tttn_BE    = 6,          // below or equal (ULE)
     tttn_NA = tttn_BE,     // not above
   tttn_NBE   = 7,          // not below or equal (UGT)
     tttn_A = tttn_NBE,     // above
   tttn_S     = 8,          // sign
   tttn_NS    = 9,          // not sign
   tttn_P     = 10,         // parity
     tttn_PE = tttn_P,      // parity even
   tttn_NP    = 11,         // not parity
     ttttn_PO = tttn_NP,    // parity odd
   tttn_L     = 12,         // less than
     tttn_NGE = tttn_L,     // not greater than or equal to
   tttn_GE    = 13,         // greater than or equal to
     tttn_NL = tttn_GE,     // not less than
   tttn_LE    = 14,         // less than or equal to
     tttn_NG = tttn_LE,     // not greater than
   tttn_G     = 15,         // greater than
     tttn_NLE = tttn_G,     // not less than
}; // Tttn


__forceinline Tttn FlipTttn(Tttn eTttn)
    { return static_cast<Tttn>(eTttn ^ 1); }

} // X86

#endif //!defined(INCLUDE_arch_x86_arch_h)
