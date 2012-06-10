//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR - Functions
// compiler/ir/ir_fns.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/opt/opt_fns.h#1 $
//
#if !defined(INCLUDE_compiler_opt_fns_h)
#define INCLUDE_compiler_opt_fns_h

namespace Compiler
{

bool opt_eliminate_BOUND(Instruction*, Register*, Register*);

} // Compiler

#endif // !defined(INCLUDE_compiler_opt_fns_h)
