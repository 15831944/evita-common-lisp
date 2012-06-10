#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - spinlock
// ke_spinlock.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_spinlock.cpp#3 $
//
#include "./ke_spinlock.h"

namespace Kernel
{

UINT SpinLock::sm_nSpinCount;

} // Kernel
