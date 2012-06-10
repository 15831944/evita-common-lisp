//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - Compilation Module
// compiler/cm/cm_module.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_module.h#3 $
//
#if !defined(INCLUDE_compiler_cm_module_h)
#define INCLUDE_compiler_cm_module_h

#include "../ir/ir_function.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Module
//
class Module :
    public Graph_<Module, Function>
{
    public: Module() {}
    public: ~Module() {}

    ////////////////////////////////////////////////////////////
    //
    // Call Graph related
    //
    public: bool PrepareTraversal();
    public: bool PrepareSccTraversal();

    Class_Enum_(Function, Layout, Module)
    Class_Enum_(Function, Preorder, Module)
    Class_Enum_(Function, Postorder, Module)

    #define EnumFunction EnumFunction_Layout
}; // Module

} // Compiler

#endif //!defined(INCLUDE_compiler_cm_module_h)
