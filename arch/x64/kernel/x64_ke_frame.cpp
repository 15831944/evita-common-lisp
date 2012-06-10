#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x64 Frame
// arch/x64/kernel/gen_ke_frame.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/kernel/x64_ke_frame.cpp#4 $
//
// Description:
//  This file contains implementaiton of following Generic frame methods:
//      FinallyFrame::Unwind
//      XferFrame::Transfer
//
#include "./x64_ke_frame.h"

#include "./x64_ke_layout.h"
#include "./x64_ke_mach.h"

#include "../../../kernel/ke_thread.h"


namespace Generic
{

using namespace Kernel;

enum
{
    Tag_Function = FunObj::Tag,
}; // enum


typedef X64::FinallyFrame x64FinallyFrame;

extern "C" void FinallyFrame_Unwind(FinallyFrame*, Thread*);
extern "C" void TagbodyFrame_Transfer(TagbodyFrame*, Thread*, TagbodyFrame::Tag*);
extern "C" void XferFrame_Transfer(XferFrame*, Thread*);


//////////////////////////////////////////////////////////////////////
//
// FinallyFrame::Unwind
//
void
FinallyFrame::Unwind(Thread* pThread)
{
    FinallyFrame_Unwind(this, pThread);
} // FinallyFrame::Unwind


//////////////////////////////////////////////////////////////////////
//
// TagbodyFrame::Transfer
//
void __declspec(noreturn)
TagbodyFrame::Transfer(Thread* pThread, Tag* pTag)
{
    TagbodyFrame_Transfer(this, pThread, pTag);
} // XferFrame::Transfer

//////////////////////////////////////////////////////////////////////
//
// XferFrame::Transfer
//
void __declspec(noreturn)
XferFrame::Transfer(Thread* pThread)
{
    XferFrame_Transfer(this, pThread);
} // XferFrmae::Transfer

} // Generic

namespace Kernel
{
void unwind_finally_frame(Frame* pFrame)
{
    pFrame->StaticCast<X64::FinallyFrame>()->Unwind(Kernel::Thread::Get());
} // unwind_finally_frame

} // Kernel
