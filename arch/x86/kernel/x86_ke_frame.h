//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86 Machine Dependent Frame
// arch/kernel/x86_ke_frame.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/kernel/x86_ke_frame.h#2 $
//
#if !defined(INCLUDE_arch_x86_kernel_x86_ke_frame_h)
#define INCLUDE_arch_x86_kernel_x86_ke_frame_h

#include "../../generic/kernel/gen_ke_frame.h"

namespace X86
{

using namespace Kernel;

typedef Generic::BlockFrame   BlockFrame;
typedef Generic::CatchFrame   CatchFrame;
typedef Generic::FinallyFrame FinallyFrame;
typedef Generic::TagbodyFrame TagbodyFrame;
typedef Generic::XferFrame    XferFrame;

}; // X86

#endif //!defined(INCLUDE_arch_x86_kernel_x86_ke_frame_h)
