//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86 Machine Dependent Gcmap
// arch/kernel/x86_ke_gcmap.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/kernel/x86_ke_gcmap.h#3 $
//
#if !defined(INCLUDE_arch_x86_kernel_x86_ke_gcmap_h)
#define INCLUDE_arch_x86_kernel_x86_ke_gcmap_h

#include "../../generic/kernel/gen_ke_gcmap.h"

namespace Kernel
{

class GcMap : public Generic::GcMap_<1, 5>
{
    public: GcMap() {}
    public: GcMap(const uint* p, uint cb) :
        GcMap_<1, 5>(p, cb) {}

    public: static uint Skip(uint32);
}; // GcMap

}; // Kernel

#endif //!defined(INCLUDE_arch_x86_kernel_x86_ke_gcmap_h)
