//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x86 - ra-ls
// cg/cg_ra.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_ra.h#2 $
//
#if !defined(INCLUDE_compiler_cg_ra_h)
#define INCLUDE_compiler_cg_ra_h

#include "../cm/cm_defs.h"
#include "../cm/cm_base.h"
#include "../cm/cm_fns.h"

#include "../ir/ir_defs.h"
#include "../ir/ir_operand.h"
#include "../ir/ir_pass.h"

#include "./cg_defs.h"

namespace Compiler
{

class BaseRA : public FunctionPass
{
    public: BaseRA(LPCWSTR pwszName) :
        FunctionPass(pwszName) {}

    public: void AssignFrameOffset(Function*);
}; // RegisterAllocator

} // Compiler

#endif //!defined(INCLUDE_compiler_cg_ra_h)
