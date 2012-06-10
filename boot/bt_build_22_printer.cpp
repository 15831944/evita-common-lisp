#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - build - 23 Reader
// boot/bt_build_23_reader.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/boot/bt_build_22_printer.cpp#1 $
//
#include "../mini/mini_lisp.h"

namespace Boot
{

Int deftlv(LPCWSTR, Val);
Val defvar(LPCWSTR, Val);

void build_22_Printers()
{
    deftlv(L"*PRINTER-LEVEL*", Fixnum::Encode(0));
    deftlv(L"*PRINTER-LABEL-TABLE*", nil);
    deftlv(L"*PRINTER-LABEL*", nil);
    deftlv(L"*PRINTER-STREAM*", nil);

    defvar(L"*BIGNUM-DIVISOR-VECTOR*", make_vector(37));
    defvar(L"*FIXNUM-NDIGITS-VECTOR*", make_vector(37));
} // Build_22_Printer

} // Boot
