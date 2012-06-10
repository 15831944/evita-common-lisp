//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - pre-compiled header
// mini_09_cond.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_09_cond.h#2 $
//
#if !defined(INCLUDE_mini_09_cond_h)
#define INCLUDE_mini_09_cond_h

namespace CommonLisp
{
    // 9 Conditions
    void __declspec(noreturn) error(Val);
    void __declspec(noreturn) error(Val, Val, Val);
    void __declspec(noreturn) error(Val, Val, Val, Val, Val);
    void __declspec(noreturn) error(Val, Val, Val, Val, Val, Val);
    void __declspec(noreturn) error(Val, Val, Val, Val, Val, Val, Val);
    void __declspec(noreturn) invoke_debugger(Val);

} // CommonLisp

namespace MiniLisp
{
    Val make_simple_error(LPCWSTR, Val);
    Val make_type_error(Val datum, Val expected_type);
} // MiniLisp

#endif //!defined(INCLUDE_mini_09_cond_h)
