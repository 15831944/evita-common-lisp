//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - thread
// mini/mini_thread.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_thread.h#5 $
//
#if !defined(INCLUDE_mini_thread_h)
#define INCLUDE_mini_thread_h

#include "../kernel/ke_arch.h"
#include "../kernel/ke_frame.h"
#include "../kernel/ke_thread.h"

namespace MiniLisp
{

using namespace Kernel;

//////////////////////////////////////////////////////////////////////
//
// Mini Thread
//
class MiniThread : public Kernel::Thread
{
    public: Val AllocBinObj(Val);
    public: Val AllocBinVec(Val, Val);
    public: Val AllocCons();
    public: Val AllocFunction(Val, size_t);
    public: Val AllocInstance(Val);
    public: Val AllocRecord(Val);
    public: Val AllocVector(Val, Val);

    public: Val GcFence(Val);

    public: Val StackAllocBinVec(Val, Val);
    public: Val StackAllocVector(Val, Val);

    public: Val StackAllocBinVec(Val classd, Int iLength)
        { return StackAllocBinVec(classd, Fixnum::Encode(iLength)); }

    public: Val StackAllocVector(Val classd, Int iLength)
        { return StackAllocVector(classd, Fixnum::Encode(iLength)); }

    public: static MiniThread* Get()
        { return Kernel::Thread::Get()->Extend<MiniThread>(); }

    public: void PopFrame();

    public: void PushFrame(Frame* pFrame)
    {
        pFrame->m_pOuter = m_fp->StaticCast<Frame>();
        m_fp = Fixnum::Encode(pFrame);
    } // PushFrame
}; // Thread


//////////////////////////////////////////////////////////////////////
//
// BindFrameScope
//
class BindFrameScope
{
    protected: MiniThread* m_pThread;
    protected: BindFrame*  m_pBindFrame;
    protected: UINT m_cBinds;
    public: BindFrameScope(UINT);
    public: ~BindFrameScope();

    public: void Bind(Val);
    public: void Bind(Val, Val);
    public: void Bind(Int);
    public: void Bind(Int, Val);
}; // BindFrameScope


//////////////////////////////////////////////////////////////////////
//
// DisableGc
//
class DisableGc
{
    GcDisableFrame m_oFrame;
    MiniThread* m_pThread;

    public: DisableGc() :
        m_pThread(MiniThread::Get())
    {
        m_pThread->PushFrame(&m_oFrame);
    } // DisableGc

    public: ~DisableGc()
        { m_pThread->PopFrame(); }
}; // DisableGc


//////////////////////////////////////////////////////////////////////
//
// GcRoot
//
class GcRoot
{
    GcRootFrame m_oFrame;
    MiniThread* m_pThread;

    public: GcRoot(Val val) :
        m_pThread(MiniThread::Get())
    {
        m_oFrame.Set(val);
        m_pThread->PushFrame(&m_oFrame);
    } // GcRoot

    public: ~GcRoot()
        { m_pThread->PopFrame(); }

    public: Val Get() { return m_oFrame.Get(); }
}; // GcRoot


//////////////////////////////////////////////////////////////////////
//
// ObStackScope
//
class ObStackScope : public ObStackFrame
{
    public: ObStackScope();
    public: ~ObStackScope();

    public: Val AllocBinObj(Val);
    public: Val AllocBinVec(Val, Val);
    public: Val AllocCons(Val);
    public: Val AllocRecord(Val);
    public: Val AllocInstance(Val);
    public: Val AllocVector(Val, Val);
}; // ObStackScope

} // MiniLisp

#endif //!defined(INCLUDE_mini_thread_h)
