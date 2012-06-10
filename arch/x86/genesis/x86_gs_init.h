//////////////////////////////////////////////////////////////////////////////
//
// evcl - x86 Assembler
// arch/x86/x86_asm.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/genesis/x86_gs_init.h#2 $
//
#if !defined(INCLUDE_arch_x86_x86_gs_init_h)
#define INCLUDE_arch_x86_x86_gs_init_h

#if MACH == MACH_x86

#include "../../../kernel/ke_thread.h"
#include "../../../mini/mini_lisp.h"
#include "../../../kernel/ke_mach.h"
#include "../../../genesis/gs_init.h"

#include "../x86_arch.h"
#include "../x86_opcode.h"
#include "../x86_asm.h"

namespace Genesis
{

using namespace MiniLisp;
using namespace X86;

//////////////////////////////////////////////////////////////////////
//
//  X86Initializer
//
class X86Initializer : public X86Assembler
{
    public: void Run();

    //protected: void install_service(int, Val);
    //protected: void install_service_aux(int, void*);

    protected: void x86_init_03_Evaluation_and_Compilation();
    protected: void x86_init_04_Types_and_Classes();
    protected: void x86_init_05_Data_and_Control_Flow();
    protected: void x86_init_07_Objects();
    protected: void x86_init_09_Conditions();
    protected: void x86_init_10_Symbols();
    protected: void x86_init_11_Packages() {}
    protected: void x86_init_12_Numbers();
    protected: void x86_init_13_Characters();
    protected: void x86_init_14_Conses();
    protected: void x86_init_15_Arrays() {}
    protected: void x86_init_16_Strings() {}
    protected: void x86_init_17_Sequences() {}
    protected: void x86_init_18_Hash_Tables() {}
    protected: void x86_init_19_Filenames() {}
    protected: void x86_init_20_Files() {}
    protected: void x86_init_21_Streams() {}
    protected: void x86_init_22_Printer();
    protected: void x86_init_23_Reader() {}
    protected: void x86_init_24_System_Construction() {}
    protected: void x86_init_25_Environment();

    protected: void x86_init_49_Internals() {}
    protected: void x86_init_50_Extensions() {}
}; // X86Initializer

} // Genesis

#endif // MACH == MACH_x86
#endif //!defined(INCLUDE_arch_x86_x86_gs_init_h)
