//////////////////////////////////////////////////////////////////////////////
//
// evcl - x86 Assembler
// arch/x86/x86_asm.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/boot/x86_bt_builder.h#2 $
//
#if !defined(INCLUDE_arch_x86_x86_bt_builder)
#define INCLUDE_arch_x86_x86_bt_builder

#include "../x86_asm.h"
#include "../kernel/x86_ke_thread.h"

namespace Boot
{

using namespace X86;

class X86Builder : public X86Assembler
{
    void build_03_Eval();
    void build_04_Type();
    void build_05_Control();
    void build_07_Object();
    void build_08_Struct();
    void build_10_Symbol();
    void build_12_Number();
    void build_13_Character();
    void build_14_Cons();
    void build_15_Array();
    void build_16_String();
    void build_49_Internal();

    public: void Run()
    {
        build_03_Eval();
        build_04_Type();
        build_05_Control();
        build_07_Object();
        build_08_Struct();
        build_10_Symbol();
        build_12_Number();
        build_13_Character();
        build_14_Cons();
        build_15_Array();
        build_16_String();
        build_49_Internal();
    } // Run
}; // X86Builder

} // Boot

#endif //!defined(INCLUDE_arch_x86_x86_bt_builder)
