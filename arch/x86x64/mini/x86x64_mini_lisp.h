//////////////////////////////////////////////////////////////////////////////
//
// evcl - x86x64 mini - mini lisp
// arch/x86x64/mini/mini_lisp.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/mini/x86x64_mini_lisp.h#2 $
//
#if !defined(INCLUDE_arch_x86x64_mini_x86x64_mini_lisp_h)
#define INCLUDE_arch_x86x64_mini_x86x64_mini_lisp_h

#include "../../../mini/mini_lisp.h"

#include "../kernel/x86x64_ke_layout.h"

namespace MiniLisp
{

Val allocate_funobj(Val, uint, uint, uint, FunObj::FrameType, uint);

} // MiniLisp

#endif //!defined(INCLUDE_arch_x86x64_mini_x86x64_mini_lisp_h)
