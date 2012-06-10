//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - pre-compiled header
// mini_11_package.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_11_package.h#2 $
//
#if !defined(INCLUDE_mini_11_package_h)
#define INCLUDE_mini_11_package_h

namespace CommonLisp
{
    bool packagep(Val x);

    Val find_package(Val);
    Val find_symbol(Val, Val, Val*);
    Val intern(Val, Val, Val*);
    Val intern(Val name, Val package);
    Val make_package(Val, Val, Val);
    Val package_name(Val x);
    Val use_package(Val, Val);

    Val intern(LPCWSTR, Val);;
} // CommonLisp

namespace MiniLisp
{
    Val internal_make_package(Val, Val, Val);
    Val package_names(Val x);
    Val package_pretty_name(Val);
    Val package_put(Val, Val);

    Val export_intern(LPCWSTR, Val);
} // MiniLisp

#endif //!defined(INCLUDE_mini_11_package_h)
