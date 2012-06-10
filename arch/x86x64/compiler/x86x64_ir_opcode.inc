//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - x86x64 - IR Operation Code
// arch/x86x64/compiler/x86x64_ir_opcode.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_ir_opcode.inc#3 $
//
DEFIROP(X86X64_CMP)         // x86x64-CMP      int32       %ef <= %rx %sy
DEFIROP(X86X64_CONVERT)     // x86x64-CONVERT  ty          %rd <= %sx
DEFIROP(X86X64_DEF)         // x86x64-DEF      ty          %rd <=
DEFIROP(X86X64_ENCODE)      // x86x64-ENCODE   ty          %rd <= %sx
DEFIROP(X86X64_LEA2)        // x86x64-LEA2     int32       %rd <= %rx ofs
DEFIROP(X86X64_LEA3)        // x86x64-LEA3     int32       %rd <= %rx ofs %ry
DEFIROP(X86X64_NEG)         // x86x64-NEG      ty          %rd <= %rx %ry
DEFIROP(X86X64_SERVICE)     // x86x64-SERVICE                     code %vy
DEFIROP(X86X64_SETCC)       // x86x64-SETCC    bool        %bd <= tttn %ef
DEFIROP(X86X64_TCB)         // x86x64-TCB      (ptr ty)    %rd <= ofs
DEFIROP(X86X64_TEST)        // x86x64-TEST     int32       %ef <= %rx %sy
