//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - Code Generator
// x86_opcode_h.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/x86_opcode.h#2 $
//
#if !defined(INCLUDE_arch_x86_opcode_h)
#define INCLUDE_arch_x86_opcode_h

#include "./x86_arch.h" // Mod

namespace X86
{

//////////////////////////////////////////////////////////////////////
//
// Operation Code
//
enum Opcode
{
    #define DEFFORMAT_0(mp_opcode, mp_mnemonic) \
        op_##mp_mnemonic = mp_opcode,

    #define DEFFORMAT_1(mp_opcode, mp_mnemonic, mp_1) \
        op_##mp_mnemonic##_##mp_1 = mp_opcode,

    #define DEFFORMAT_EXT_1(mp_opcode, mp_opext, mp_mnemonic, mp_1) \
        op_##mp_mnemonic##_##mp_1 = mp_opcode,

    #define DEFFORMAT_2(mp_opcode, mp_mnemonic, mp_1, mp_2) \
        op_##mp_mnemonic##_##mp_1##_##mp_2 = mp_opcode,

    #define DEFFORMAT_EXT_2(mp_opcode, mp_opext, mp_mnemonic, mp_1, mp_2) \
        op_##mp_mnemonic##_##mp_1##_##mp_2 = mp_opcode,

    #include "./x86_opcode.inc"
}; // OpCode

enum OpExt
{
    #define DEFFORMAT_EXT_1(mp_opcode, mp_opext, mp_mnemonic, mp_1) \
        opext_##mp_mnemonic##_##mp_1 = mp_opext,

    #define DEFFORMAT_EXT_2(mp_opcode, mp_opext, mp_mnemonic, mp_1, mp_2) \
        opext_##mp_mnemonic##_##mp_1##_##mp_2 = mp_opext,

    #include "./x86_opcode.inc"
}; // OpExt

__forceinline uint8
ModRm(Mod eMod, OpExt eExt, Reg eRx)
    { return static_cast<uint8>(eMod | (eExt << 3) | (eRx & 7)); }

__forceinline uint8
ModRm(Mod eMod, OpExt eExt, Rm eRm)
    { return static_cast<uint8>(eMod | (eExt << 3) | eRm); }

} // X86

#endif //!defined(INCLUDE_arch_x86_opcode_h)
