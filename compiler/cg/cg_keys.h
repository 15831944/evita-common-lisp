//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - keys
// comcg/cg_keys.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_keys.h#2 $
//
#if !defined(INCLUDE_compiler_cg_keys_h)
#define INCLUDE_compiler_cg_keys_h

#include "./cg_defs.h"

#include "../ir/ir_pass.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// KeysExpander
//
class KeysExpander : public FunctionPass
{
    public: KeysExpander(LPCWSTR pwszName) :
        FunctionPass(pwszName) {}

    public: virtual void process_function(Function*);

    protected: virtual Register* insert_car(BBlock*, Register*) = 0;
    protected: virtual void      insert_cdr(BBlock*, Register*) = 0;
    protected: virtual Bool*     insert_consp(BBlock*, Register*) = 0;
    protected: virtual Bool*     insert_eq(BBlock*, Register*, Val) = 0;
    protected: virtual Register* insert_unsafe_car(BBlock*, Register*) = 0;
    protected: virtual void      insert_unsafe_cdr(BBlock*, Register*) = 0;

    // Dispatch table
    typedef void (Assembler::*InsnProcT)(Instruction*);
    static const InsnProcT k_rgpInsnProc[IrOp_MAX_1 + 1];

    void process_CHECKKEYS(Instruction*);
    void process_KEY(Instruction*);
    void process_PARSEKEYS(Instruction*);
}; // KeysExpander

} // Compiler

#endif //!defined(INCLUDE_compiler_cg_keys_h)
