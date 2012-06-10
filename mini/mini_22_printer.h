//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - pre-compiled header
// mini_22_printer.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_22_printer.h#3 $
//
#if !defined(INCLUDE_mini_22_printer_h)
#define INCLUDE_mini_22_printer_h

namespace CommonLisp
{
    Val format(Val, const char16*);
    Val format(Val, const char16*, Val);
    Val format(Val, const char16*, Val, Val);
    Val format(Val, const char16*, Val, Val, Val);
    Val format(Val, const char16*, Val, Val, Val, Val);
    Val format(Val, const char16*, Val, Val, Val, Val, Val);
    Val format(Val, const char16*, Val, Val, Val, Val, Val, Val);
    Val format(Val, const char16*, Val, Val, Val, Val, Val, Val, Val);
    Val format(Val, const char16*, Val, Val, Val, Val, Val, Val, Val, Val);
    Val print_object(Val, Val = nil);
} // CommonLisp

#endif //!defined(INCLUDE_mini_22_printer_h)
