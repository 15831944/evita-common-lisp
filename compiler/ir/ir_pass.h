//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR - Data Flow Analysis
// ir/ir_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_pass.h#2 $
//
#if !defined(INCLUDE_compiler_ir_pass_h)
#define INCLUDE_compiler_ir_pass_h

#include "../cm/cm_pass.h"

namespace Compiler
{

class IrPass : public Pass
{
    public: IrPass(LPCWSTR pwsz);
    public: ~IrPass();

    void uninit_function(Function*);

    ////////////////////////////////////////////////////////////
    //
    // Instruction Dispatch Table
    //
    #define DEFIROP(mp_name) \
        void process_##mp_name(Instruction*) {}

    #include "./ir_opcode.inc"
}; // IrPass


//////////////////////////////////////////////////////////////////////
//
// FunctionPass
//
class FunctionPass : public IrPass
{
    public: FunctionPass(LPCWSTR);
    public: void Run();
    protected: virtual void process_function(Function*) = 0;
}; // FuncitonPass


//////////////////////////////////////////////////////////////////////
//
// ModulePass
//
class ModulePass : public IrPass
{
    public: ModulePass(LPCWSTR);
    public: void Run();
    protected: virtual void process_module(Module*) = 0;
}; // FuncitonPass

} // Compiler

#endif // !defined(INCLUDE_compiler_ir_pass_h)
