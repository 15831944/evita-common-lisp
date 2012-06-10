//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x64 Machine Dependent Frame
// arch/kernel/x64_ke_frame.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/kernel/x64_ke_frame.h#2 $
//
#if !defined(INCLUDE_arch_x64_kernel_x64_ke_frame_h)
#define INCLUDE_arch_x64_kernel_x64_ke_frame_h

#if MACH == MACH_x64

#include "../../generic/kernel/gen_ke_frame.h"

namespace X64
{

using namespace Kernel;

typedef Generic::BlockFrame   BlockFrame;
typedef Generic::CatchFrame   CatchFrame;
typedef Generic::FinallyFrame FinallyFrame;
typedef Generic::TagbodyFrame TagbodyFrame;
typedef Generic::XferFrame    XferFrame;

}; // X64

#endif // MACH == MACH_x64

#endif //!defined(INCLUDE_arch_x64_kernel_x64_ke_frame_h)
