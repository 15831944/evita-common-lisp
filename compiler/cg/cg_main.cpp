#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x86 - operand
// cg/cg_operand.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_main.cpp#4 $
//
#include "./cg_defs.h"
#include "./cg_target.h"

namespace Compiler
{

void generate()
{
    // See arch/[arch]/compiler/cg/[arch]/[arch]_cg_target.cpp
    cm_get_target()->Generate();
} // Compiler::generate

} // Compiler
