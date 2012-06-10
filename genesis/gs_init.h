//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - pre-compiled header
// genesis_lisp.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_init.h#5 $
//
#if !defined(INCLUDE_genesis_gs_init_h)
#define INCLUDE_genesis_gs_init_h

#include "./gs_lisp.h"

namespace Genesis
{

// implement in arch/[arch]/[arch]_gs_fns.cpp
Val make_wrapper(int, Val, int, int, const char16*, int);
Val copy_function(Val);

//////////////////////////////////////////////////////////////////////
//
// Initializer
//
class Initializer
{
    public: void Run();

    protected: void init_all();
    protected: void prepare();

    protected: void install_alias(Val, Val);

    protected: Val install_function(
        Val             fname,
        int             iMin,
        int             iMax,
        const char16*   pwszProc,
        int             cVals = 1)
    {
        return install_function(
            0,
            fname, iMin, iMax, pwszProc,
            cVals );
    } // install_function

    protected: Val install_function(
        int, Val, int, int, const char16*, int = 1 );

    protected: Val install_macro_expander(Val, const char16*);

    private: void init_03_Evaluation_and_Compilation();
    private: void init_04_Types_and_Classes();
    private: void init_05_Data_and_Control_Flow();
    private: void init_06_Iteration();
    private: void init_07_Objects();
    private: void init_08_Structures();
    private: void init_09_Conditions();
    private: void init_10_Symbols();
    private: void init_11_Packages();
    private: void init_12_Numbers();
    private: void init_13_Characters();
    private: void init_14_Conses();
    private: void init_15_Arrays();
    private: void init_16_Strings();
    private: void init_17_Sequences();
    private: void init_18_Hash_Tables();
    private: void init_19_Filenames();
    private: void init_20_Files();
    private: void init_21_Streams();
    private: void init_22_Printer();
    private: void init_23_Reader();
    private: void init_24_System_Construction();
    private: void init_25_Environment();

    private: void init_49_Internals();
    private: void init_50_Extensions();

    private: void init_Platform();
    private: void init_Platform_Target();
}; // Initializer


//////////////////////////////////////////////////////////////////////
//
// Helper macros
//
#define INSTALL_MACRO_EXPANDER(mp_name) \
    install_macro_expander(Q##mp_name, L"expand_" L## #mp_name)

#define INSTALL_ALIAS(mp_alias, mp_real) \
    install_alias(Q##mp_alias, Q##mp_real)

#define INSTALL_FUNCTION(mp_name, mp_min, mp_max) \
    install_function(Q##mp_name, mp_min, mp_max, L## #mp_name)


} // Genesis

#endif //!defined(INCLUDE_genesis_gs_init_h)
