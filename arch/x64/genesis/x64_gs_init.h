//////////////////////////////////////////////////////////////////////////////
//
// evcl - x64 Assembler
// arch/x64/x64_asm.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/genesis/x64_gs_init.h#2 $
//
#if !defined(INCLUDE_arch_x64_x64_gs_init_h)
#define INCLUDE_arch_x64_x64_gs_init_h

#include "../../../kernel/ke_thread.h"
#include "../../../mini/mini_lisp.h"
#include "../../../kernel/ke_mach.h"
#include "../../../genesis/gs_init.h"

#include "../x64_arch.h"
#include "../x64_opcode.h"
#include "../x64_asm.h"

namespace Genesis
{

using namespace MiniLisp;
using namespace X64;

//////////////////////////////////////////////////////////////////////
//
//  X64Initializer
//
class X64Initializer : public X64Assembler
{
    public: void Run();

    //protected: void install_service(int, Val);
    //protected: void install_service_aux(int, void*);

    protected: void x64_init_03_Evaluation_and_Compilation();
    protected: void x64_init_04_Types_and_Classes();
    protected: void x64_init_05_Data_and_Control_Flow();
    protected: void x64_init_07_Objects();
    protected: void x64_init_09_Conditions();
    protected: void x64_init_10_Symbols();
    protected: void x64_init_11_Packages() {}
    protected: void x64_init_12_Numbers();
    protected: void x64_init_13_Characters();
    protected: void x64_init_14_Conses();
    protected: void x64_init_15_Arrays() {}
    protected: void x64_init_16_Strings() {}
    protected: void x64_init_17_Sequences() {}
    protected: void x64_init_18_Hash_Tables() {}
    protected: void x64_init_19_Filenames() {}
    protected: void x64_init_20_Files() {}
    protected: void x64_init_21_Streams() {}
    protected: void x64_init_22_Printer();
    protected: void x64_init_23_Reader() {}
    protected: void x64_init_24_System_Construction() {}
    protected: void x64_init_25_Environment();

    protected: void x64_init_49_Internals() {}
    protected: void x64_init_50_Extensions() {}
}; // X64Initializer

} // Genesis

#endif //!defined(INCLUDE_arch_x64_x64_gs_init_h)
