//
// evcl - kernel - Memory Manager
// kernel/ke_finalize.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_finalize.h#2 $
//
#if !defined(INCLUDE_kernel_finalize_h)
#define INCLUDE_kernel_finalize_h

#include "./ke_layout.h"

namespace Kernel
{

class Finalization : public Record_<Layout::C_finalization>
    {};

} // Kernel

#endif //!defined(INCLUDE_kernel_finalize_h)
