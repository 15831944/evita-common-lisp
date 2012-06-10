//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - pre-compiled header
// mini_04_type.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_04_type.h#3 $
//
#if !defined(INCLUDE_mini_04_type_h)
#define INCLUDE_mini_04_type_h

namespace CommonLisp
{
    //bool subtypep(Val, Val, bool* = NULL);
    //bool typep(Val, Val);
    Val type_of(Val);
} // CommonLisp

#endif //!defined(INCLUDE_mini_04_type_h)
