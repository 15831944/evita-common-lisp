#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 12 Numbers - Float
// big/big_12_float.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/big/x86_big_12_float.cpp#3 $
//

#include <math.h>   // modf and modff

extern "C"
{

float __cdecl truncatef(float x)
    { float i; ::modff(x, &i); return i; }

double __cdecl truncate(double x)
    { double i; ::modf(x, &i); return i; }

} // extern "C"
