//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 49 Internals - Direct Call
// mini/mini_03_direct_call.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_03_direct_call.h#2 $
//
#if !defined(INCLUDE_mini_03_direct_call_h)
#define INCLUDE_mini_03_direct_call_h

namespace MiniLisp
{
    Val intern_callee(Val);
    Val register_caller(Val, Val);
    Val update_callers(Val, Val);
    Val make_undefined_function_function(Val);
} // MiniLisp

#endif //!defined(INCLUDE_mini_03_direct_call_h)
