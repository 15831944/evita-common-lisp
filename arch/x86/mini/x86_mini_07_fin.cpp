#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 7 Objects
// arch/x86/genesis/gs_07_object.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/mini/x86_mini_07_fin.cpp#6 $
//
#include "./x86_mini_lisp.h"

#include "../kernel/x86_ke_thread.h"    // SVC_error
#include "../x86_asm.h"

namespace MiniLisp
{

using namespace X86;

typedef X86::Function FunObj;

// allocate_funcallable_instance
Val allocate_funcallable_instance(Val classd)
{
    check_type(classd, class_description);

    const uint cbCodeVec = 6;   // CALL disp32 + RET
    const uint cbGcMap   = 4;
    const uint cbAnnon   = 4;

    Val fin = allocate_funobj(
        classd,
        cbCodeVec,
        cbAnnon,
        cbGcMap,
        FunObj::FrameType_Fixed,
        4 );

    FunObj* pFunObj = fin->Decode<FunObj>();

    // Populate code
    {
        uint8* pbCode = pFunObj->GetCodeVec();
        pbCode[0] = X86Assembler::op_CALL_Jv;
        pbCode[5] = X86Assembler::op_RET;

        fin->Decode<FunObj>()->PatchCallee(1,
            Quninitialized_funcallable_instance->Decode<Symbol>()->
                m_function );
    } // code

    // Initialize annotations
    {
        FunObj::Annon* pnAnnon = pFunObj->GetAnnon();
        pnAnnon->m_eType = FunObj::Annon::Type_Callee;
        pnAnnon->m_ofs   = 1;
    }

    // Populate GC Map
    {
        pFunObj->GetGcMap()[0] = 0;
    }

    ClassD* pClassD = classd->Decode<ClassD>();
    ASSERT(pClassD->m_format->ToInt() == ClassD::Format_FuncallableInstance);

    Val n = pClassD->m_format_misc;
    ASSERT(n->ToInt() >= sizeof(StandardGenericFunction) - sizeof(Storage));

    Val storage = MiniThread::Get()->AllocVector(
        CLASSD_storage,
        n );

    storage->Decode<Storage>()->m_storaged = classd;

    Int cSlots = Fixnum::Decode_(n);
    for (Int i = 0; i < cSlots; i++)
    {
        storage->Decode<Storage>()->mv_element[i] = QQunbound_marker;
    } // for i

    fin->Decode<FuncallableInstance>()->m_storage = storage;

    fin->Decode<FuncallableInstance>()->m_nCookie  = FunObj::Cookie;

    return fin;
} // allocate_funcallable_instance


// funcallable_instance_function (CLOS/MOP)
Val funcallable_instance_function(Val fin)
{
    check_type(fin, funcallable_standard_object);
    return fin->Decode<FunObj>()->FetchCallee(1);
} // funcallable_instance_function


// set_funcallable_instance_function (CLOS/MOP)
Val set_funcallable_instance_function(Val fin, Val fun)
{
    check_type(fin, funcallable_standard_object);
    check_type(fun, function);
    fin->Decode<FunObj>()->PatchCallee(1, fun);
    return fun;
} // set_funcallable_instance_function

} // MiniLisp
