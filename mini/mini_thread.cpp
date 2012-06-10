#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - thread
// ke_thread.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_thread.cpp#8 $
//

#include "../kernel/ke_executive.h"
#include "../kernel/ke_host.h"
#include "../kernel/ke_memory.h"
#include "./mini_thread.h"
#include "./mini_lisp.h"

namespace MiniLisp
{

namespace
{
    class DataVector : public Kernel::DataVector
    {
        public: DataVector(Val classd, Val length)
        {
            m_classd = classd;
            m_length = length;
        } // DataVector
    }; // DataVector

    class Instance : public Kernel::Instance
    {
        public: Instance(Val classd)
        {
            m_classd  = classd;

            Storage* p = FromInt<Storage>(ToInt() + sizeof(Instance));

            m_storage = p->Encode();
                p->m_classd   = CLASSD_storage;
                p->m_storaged = classd;
        } // Instance
    }; // Instance

    class Function : public Kernel::NativeCodeFunction
    {
        public: Function(Val classd, size_t cbFunction)
        {
            m_classd = classd;
            m_cbFunction = cbFunction;
        } // Function
    }; // Function

    class RecordObj : public Kernel::Record
    {
        public: RecordObj(Val classd)
            { m_classd = classd; }
    };
} // namespace


//////////////////////////////////////////////////////////////////////
//
// Allocate BinObj
//
Val
MiniThread::AllocBinObj(Val classd)
{
    ClassD* pClassD = classd->Decode<ClassD>();

    size_t cbObject = pClassD->ComputeSize();

    for (;;)
    {
        void* pvBinObj = m_pBinObjArea->Alloc(cbObject);

        if (NULL != pvBinObj)
        {
            RecordObj* pBinObj = new(pvBinObj) RecordObj(classd);
            Val binobj = pBinObj->Encode();
                ASSERT(binobj->GetSize() == cbObject);
            return binobj;
        }

        m_pBinObjArea = Memory::AllocDataArea(
            this,
            Area::ScanType_BinObj,
            cbObject );
    } // forever
} // AllocBinObj


//////////////////////////////////////////////////////////////////////
//
// Allocate BinObj
//
Value
MiniThread::AllocBinVec(Value classd, Value length)
{
    ClassD* pClassD = classd->Decode<ClassD>();

    size_t cbObject = pClassD->ComputeSize(length);

    for (;;)
    {
        void* pvBinObj = m_pBinObjArea->Alloc(cbObject);

        if (NULL != pvBinObj)
        {
            DataVector* pBinObj = new(pvBinObj)
                DataVector(classd, length);

            Val binobj = pBinObj->Encode();
                ASSERT(binobj->GetSize() == cbObject);
            return binobj;
        }

        m_pBinObjArea = Memory::AllocDataArea(
            this,
            Area::ScanType_BinObj,
            cbObject );
    } // forever
} // AllocBinVec


//////////////////////////////////////////////////////////////////////
//
// Allocate Cons
//
Value
MiniThread::AllocCons()
{
    for (;;)
    {
        void* pvCons = m_pConsArea->Alloc(sizeof(Cons));

        if (NULL != pvCons)
        {
            Cons* pCons = new(pvCons) Cons();
            return pCons->Encode();
        }

        m_pConsArea = Memory::AllocDataArea(
            this,
            Area::ScanType_Cons,  
            sizeof(Cons) );
    } // forever
} // MiniThread::AllocCons


//////////////////////////////////////////////////////////////////////
//
// Allocate Function
//
Value
MiniThread::AllocFunction(Val classd, size_t cbFunction)
{
    ASSERT(0 == cbFunction % Host::FunObj_Align);

    for (;;)
    {
        void* pvFunction = m_pFunObjArea->Alloc(cbFunction);

        if (NULL != pvFunction)
        {
            Function* pFunction = new(pvFunction)
                Function(classd, cbFunction);

            Val fn = pFunction->Encode();
                ASSERT(fn->GetSize() == cbFunction);
            return fn;
        }

        m_pFunObjArea = Memory::AllocCodeArea(
            this, 
            Area::ScanType_Function, 
            cbFunction );
    } // forever
} // AllocFunction


//////////////////////////////////////////////////////////////////////
//
// Allocate Instance
//
Value
MiniThread::AllocInstance(Value classd)
{
    Kernel::ClassD* pClassD = classd->Decode<Kernel::ClassD>();

    size_t cbObject = pClassD->ComputeSize();

    if (cbObject < sizeof(Instance))
    {
        error(L"Invalid class description: ~S", classd);
    }

    for (;;)
    {
        void* pvInstance = m_pRecordArea->Alloc(cbObject);

        if (NULL != pvInstance)
        {
            Instance* pInstance = new(pvInstance) Instance(classd);

            Val storage = pInstance->m_storage;

            // pClassD->m_format_misc has unaligned size.
                ASSERT(
                    storage->GetSize() - offsetof(Storage, mv_element) ==
                    ROUNDUP(pClassD->m_format_misc->ToInt(),
                            Arch::Record_Align) );

            Int n = Fixnum::Decode_(pClassD->m_format_misc);
            for (Int i = 0; i < n; i++)
            {
                storage->Decode<Storage>()->mv_element[i] =
                    QQunbound_marker;
            } // for i

            Val obj = pInstance->Encode();
                ASSERT(obj->GetSize() == sizeof(Instance));
            return obj;
        }

        m_pRecordArea = Memory::AllocDataArea(
            this,
            Area::ScanType_Record,
            cbObject );
    } // forever
} // MiniThread::AllocInstance


//////////////////////////////////////////////////////////////////////
//
// Allocate Record
//
Value
MiniThread::AllocRecord(Value classd)
{
    Kernel::ClassD* pClassD = classd->Decode<Kernel::ClassD>();

    size_t cbObject = pClassD->ComputeSize();

    if (cbObject < sizeof(Val) * 2)
    {
        error(L"Broken class description ~S.", classd);
    }

    for (;;)
    {
        void* pvRecord = m_pRecordArea->Alloc(cbObject);

        if (NULL != pvRecord)
        {
            RecordObj* pRecord = new(pvRecord) RecordObj(classd);
            Val obj = pRecord->Encode();
                ASSERT(obj->GetSize() == cbObject);
            return obj;
        }

        m_pRecordArea = Memory::AllocDataArea(
            this,
            Area::ScanType_Record,
            cbObject );
    } // forever
} // MiniThread::AllocRecord


//////////////////////////////////////////////////////////////////////
//
// Allocate Vector
//
Val
MiniThread::AllocVector(Val classd, Val length)
{
    Kernel::ClassD* pClassD = classd->Decode<Kernel::ClassD>();

    size_t cbObject = pClassD->ComputeSize(length);

    for (;;)
    {
        void* pvVector = m_pRecordArea->Alloc(cbObject);

        if (NULL != pvVector)
        {
            DataVector* pVector = new(pvVector)
                DataVector(classd, length);

            return pVector->Encode();
        }

        m_pRecordArea = Memory::AllocDataArea(
            this,
            Area::ScanType_Record,
            cbObject );
    } // forever
} // MiniThread::AllocVector


//////////////////////////////////////////////////////////////////////
//
// MiniThread::StackAllocBinVec
//
// Description:
//  Allocates BinVec on object stack and returns it. Allocated BinVec
//  is NOT initialized. Caller must initialize it.
//
Val
MiniThread::StackAllocBinVec(Val classd, Val length)
{
    Kernel::ClassD* pClassD = classd->Decode<Kernel::ClassD>();

    size_t cbObject = pClassD->ComputeSize(length);
        cbObject = ROUNDUP(cbObject, Arch::ObStack_Align);

    DataVector* pBinVec = new(SVC_C_stack_alloc(this, cbObject))
        DataVector(classd, length);

    return pBinVec->Encode();
} // MiniThread::AllocBinVec


//////////////////////////////////////////////////////////////////////
//
// MiniThread::StackAllocVector
//
// Description:
//  Allocates vector on object stack and returns it. Allocated vector
//  is NOT initialized. Caller must initialize it.
//
Val
MiniThread::StackAllocVector(Val classd, Val length)
{
    Kernel::ClassD* pClassD = classd->Decode<Kernel::ClassD>();

    size_t cbObject = pClassD->ComputeSize(length);
        cbObject = ROUNDUP(cbObject, Arch::ObStack_Align);

    DataVector* pVector = new(SVC_C_stack_alloc(this, cbObject))
        DataVector(classd, length);

    return pVector->Encode();
} // MiniThread::AllocVector


///////////////////////////////////////////////////////////////////////////////
//
// BindFrameScope constructor
//
BindFrameScope::BindFrameScope(UINT cBinds) :
    m_pThread(MiniThread::Get()),
    m_cBinds(cBinds)
{
    ObStackArea* pArea   = m_pThread->m_pObStackArea;
    OffsetT      ofsFree = pArea->m_ofsFree;

    Val frob;
    {
        size_t cbFrame = sizeof(ObStackFrame);
            cbFrame += BindFrame::ComputeSize(cBinds);

        Val length = Fixnum::Encode(cbFrame / sizeof(Val));

        frob = m_pThread->StackAllocVector(
            CLASSD_simple_vector,
            length );
    } // frob

    ObStackFrame* pObjFrame =
        new (frob->Decode<SimpleVector>()->mv_element)
            ObStackFrame(pArea, ofsFree);

    m_pThread->PushFrame(pObjFrame);

    m_pBindFrame = new (pObjFrame + 1) BindFrame(cBinds);

    m_pThread->PushFrame(m_pBindFrame);
} // BindFrameScope::BindFrameScope


BindFrameScope::~BindFrameScope()
{
    while (Fixnum::Encode(m_pBindFrame) != m_pThread->m_fp)
    {
        // We are during C++ stack unwinding by C++ throw statement.
        m_pThread->PopFrame();
    }

    m_pThread->PopFrame();  // pop bind frame
    m_pThread->PopFrame();  // pop obstack frame
} // BindFrame::~BindFrame


void BindFrameScope::Bind(Val var)
{
    ASSERT(value_cell_p(var));
    ASSERT(m_cBinds >= 1);

    m_cBinds -= 1;
    m_pBindFrame->Bind(m_cBinds, var, value_cell_value(var));
} // BindFrameScope::Bind


void BindFrameScope::Bind(Val var, Val val)
{
    Bind(var);
    setf_value_cell_value(val, var);
} // BindFrameScope::Bind


void BindFrameScope::Bind(Int ofs)
{
    ASSERT(m_cBinds >= 1);

    m_cBinds -= 1;
    m_pBindFrame->Bind(
        m_cBinds,
        FromInt<Val_>(ofs),
        MiniThread::Get()->GetTlv(ofs) );
} // BindFrameScope::Bind


void BindFrameScope::Bind(Int ofs, Val val)
{
    Bind(ofs);
    MiniThread::Get()->SetTlv(ofs, val);
} // BindFrameScope::Bind


//////////////////////////////////////////////////////////////////////
//
// Pop Control Frame
//
void
MiniThread::PopFrame()
{
    Frame* pFrame = GetFP();

    if (NULL == pFrame)
    {
        error(L"control stack underflow");
    }

    switch (pFrame->GetType())
    {
    case BindFrame::Type_Bind:
        pFrame->StaticCast<BindFrame>()->Unwind(this);
        break;

    case ObStackFrame::Type_ObStack:
        pFrame->StaticCast<ObStackFrame>()->Unwind(this);
        break;
    } // switch

    SetFP(pFrame->GetOuter());
} // MiniThread::PopFrame


//////////////////////////////////////////////////////////////////////
//
// ObStackScope constructor
//
ObStackScope::ObStackScope() :
    ObStackFrame(
        MiniThread::Get()->m_pObStackArea,
        MiniThread::Get()->m_pObStackArea->m_ofsFree )
{
    MiniThread::Get()->PushFrame(this);
} // ObStackScope::ObStackScope


//////////////////////////////////////////////////////////////////////
//
// ObStackScope destructor
//
ObStackScope::~ObStackScope()
{
    ASSERT(MiniThread::Get()->m_fp == Fixnum::Encode(this));
    MiniThread::Get()->PopFrame();
} // ObStackScope::~ObStackScope

} // MiniLisp

namespace Kernel
{

void
BindFrame::Bind(UINT nIndex, Val name, Val curval)
{
    Entry* pEntry = reinterpret_cast<Entry*>(this + 1);
        pEntry += nIndex;
        pEntry->m_name  = name;
        pEntry->m_value = curval;
} // BindFrame::Bind

} // Kernel
