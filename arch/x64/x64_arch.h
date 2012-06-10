//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - Code Generator
// x64_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/x64_arch.h#8 $
//
#if !defined(INCLUDE_arch_x64_arch_h)
#define INCLUDE_arch_x64_arch_h

namespace X64
{

enum Reg
{
    // 64bit registers
    Gpr_RAX = 0x300, Gpr_RCX = 0x301, Gpr_RDX = 0x302, Gpr_RBX = 0x303,
    Gpr_RSP = 0x304, Gpr_RBP = 0x305, Gpr_RSI = 0x306, Gpr_RDI = 0x307,
    Gpr_R8  = 0x308, Gpr_R9  = 0x309, Gpr_R10 = 0x30A, Gpr_R11 = 0x30B,
    Gpr_R12 = 0x30C, Gpr_R13 = 0x30D, Gpr_R14 = 0x30E, Gpr_R15 = 0x30F,

    // 32bit registers
    Gpr_EAX  = 0x200, Gpr_ECX  = 0x201, Gpr_EDX  = 0x202, Gpr_EBX  = 0x203,
    Gpr_ESP  = 0x204, Gpr_EBP  = 0x205, Gpr_ESI  = 0x206, Gpr_EDI  = 0x207,
    Gpr_R8D  = 0x208, Gpr_R9D  = 0x209, Gpr_R10D = 0x20A, Gpr_R11D = 0x20B,
    Gpr_R12D = 0x20C, Gpr_R13D = 0x20D, Gpr_R14D = 0x20E, Gpr_R15D = 0x20F,

    // 16bit registers
    Gpr_AX   = 0x100, Gpr_CX   = 0x101, Gpr_DX   = 0x102, Gpr_BX   = 0x103,
    Gpr_SP   = 0x104, Gpr_BP   = 0x105, Gpr_SI   = 0x106, Gpr_DI   = 0x107,
    Gpr_R8W  = 0x108, Gpr_R9W  = 0x109, Gpr_R10W = 0x10A, Gpr_R11W = 0x10B,
    Gpr_R12W = 0x10C, Gpr_R13W = 0x10D, Gpr_R14W = 0x10E, Gpr_R15W = 0x10F,

    // 8bit registers
    Gpr_AL   = 0x000, Gpr_CL   = 0x001, Gpr_DL   = 0x002, Gpr_BL   = 0x003,
    Gpr_SPL  = 0x004, Gpr_BPL  = 0x005, Gpr_SIL  = 0x006, Gpr_DIL  = 0x007,
    Gpr_R8L  = 0x008, Gpr_R9L  = 0x009, Gpr_R10L = 0x00A, Gpr_R11L = 0x00B,
    Gpr_R12L = 0x00C, Gpr_R13L = 0x00D, Gpr_R14L = 0x00E, Gpr_R15L = 0x00F,

    Fpr_XMM0  = 0x10, Fpr_XMM1  = 0x11, Fpr_XMM2  = 0x12, Fpr_XMM3  = 0x13,
    Fpr_XMM4  = 0x14, Fpr_XMM5  = 0x15, Fpr_XMM6  = 0x16, Fpr_XMM7  = 0x17,
    Fpr_XMM8  = 0x18, Fpr_XMM9  = 0x19, Fpr_XMM10 = 0x1A, Fpr_XMM11 = 0x1B,
    Fpr_XMM12 = 0x1C, Fpr_XMM13 = 0x1D, Fpr_XMM14 = 0x1E, Fpr_XMM15 = 0x1F,


    $r0  = Gpr_RAX, $r1  = Gpr_RDX, $r2  = Gpr_RBX, $r3  = Gpr_RSI,
    $r4  = Gpr_RDI, $r5  = Gpr_R8,  $r6  = Gpr_R9,  $r7  = Gpr_R10,
    $r8  = Gpr_R11, $r9  = Gpr_R14,

    $sp    = Gpr_RSP,
    $rtcb  = Gpr_RBP,
    $rfn   = Gpr_R12,
    $rn    = Gpr_RCX,
    $rnil  = Gpr_R13,

    $f0  = Fpr_XMM0,  $f1  = Fpr_XMM1,  $f2  = Fpr_XMM2,  $f3  = Fpr_XMM3,
    $f4  = Fpr_XMM4,  $f5  = Fpr_XMM5,  $f6  = Fpr_XMM6,  $f7  = Fpr_XMM7,
    $f8  = Fpr_XMM8,  $f9  = Fpr_XMM9,  $f10 = Fpr_XMM10, $f11 = Fpr_XMM11,
    $f12 = Fpr_XMM12, $f13 = Fpr_XMM13, $f14 = Fpr_XMM14, $f15 = Fpr_XMM15,
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
//   When mod != 11 and r/m=100(rsp):
//     mod=00 reg=xxx r/m=100   [base+index]
//     mod=01 reg=xxx r/m=100   [base+index+disp8]
//     mod=10 reg=xxx r/m=100   [base+index+disp32]
//
//   When mod=00 and r/m=101:
//     mod=00 reg=xxx r/m=101   [rip+disp32]    * different from x86
//
//   For [ebp+disp] and [r13+disp], we can't use disp0.
//
//   When base=101(rbp or r13)
//      mod=00 reg=xxx r/m=100 ss=xx idx=xxx base=101   [idx+disp32]
//      mod=01 reg=xxx r/m=100 ss=xx idx=xxx base=101   [idx+disp8+rbp]
//      mod=10 reg=xxx r/m=100 ss=xx idx=xxx base=101   [idx+disp32+rbp]
//
//   When idx=100, there is no index, so [rsp+disp] and [r12+disp] are:
//      mod=00 reg=xxx r/m=100 ss=xx idx=100 base=100   [rsp]
//      mod=01 reg=xxx r/m=100 ss=xx idx=100 base=100   [rsp+disp8]
//      mod=10 reg=xxx r/m=100 ss=xx idx=100 base=100   [rsp+disp32]
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
    Rm_SIB      = 4,    // we can't use RSP as index.
    Rm_Disp32   = 5,    // we can't use disp0 with RBP.
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
     tttn_NC = tttn_NB,     // not carry
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

enum Rex
{
    Rex_WRX     = 0x4E,
    Rex_WRXB    = 0x4F,
    Rex_WRB     = 0x4D,
    Rex_WR      = 0x4C,
    Rex_WXB     = 0x4B,
    Rex_WX      = 0x4A,
    Rex_WB      = 0x49,
    Rex_W       = 0x48,
    Rex_RXB     = 0x47,
    Rex_RX      = 0x46,
    Rex_RB      = 0x45,
    Rex_R       = 0x44,
    Rex_X       = 0x42,
    Rex_XB      = 0x43,
    Rex_B       = 0x41,
    Rex_None    = 0x40,
}; // Rex


__forceinline Tttn FlipTttn(Tttn eTttn)
    { return static_cast<Tttn>(eTttn ^ 1); }

} // X64

#endif //!defined(INCLUDE_arch_x64_arch_h)
