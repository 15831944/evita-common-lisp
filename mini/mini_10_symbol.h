//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - pre-compiled header
// mini_10_symbol.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_10_symbol.h#2 $
//
#if !defined(INCLUDE_mini_10_symbol_h)
#define INCLUDE_mini_10_symbol_h

namespace CommonLisp
{
    bool boundp(Val);
    bool keywordp(Val x);
    Val make_symbol(Val);
    Val make_symbol(LPCWSTR);
    Val symbol_value(Val);
    Val setf_symbol_value(Val, Val);
    Val symbol_function(Val);
    Val setf_symbol_function(Val, Val);
    bool symbolp(Val x);
    Val symbol_name(Val sym);
    Val symbol_package(Val sym);
} // CommonLisp

namespace MiniLisp
{
    Val symbol_hash_code(Val sym);

} // MiniLisp

#endif //!defined(INCLUDE_mini_10_symbol_h)
