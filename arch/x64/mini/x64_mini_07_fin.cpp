#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 7 Objects
// arch/x86x64/genesis/x86x64_mini_07_fin.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/mini/x64_mini_07_fin.cpp#6 $
//
#include "./x64_mini_lisp.h"

#include "../x64_asm.h"
#include "../kernel/x64_ke_thread.h"    // SVC_error

namespace MiniLisp
{

using namespace X64;

// allocate_funcallable_instance
//          sub rsp, 8
//          call discriminator
//          add rsp, 8
//          ret
Val allocate_funcallable_instance(Val classd)
{
    check_type(classd, class_description);

    const uint cbCodeVec = 15;   // SUB+CALL+LEA+RET
    const uint cbAnnon   = 4;
    const uint cbGcMap   = 4;

    Val fin = allocate_funobj(
        classd,
        cbCodeVec,
        cbAnnon,
        cbGcMap,
        FunObj::FrameType_Fixed,
        16 );

    FunObj* pFunObj = fin->Decode<FunObj>();

    // Populate code
    {
        class Asm : public X64Assembler
        {
            public: static void Run(uint8* pbCode)
            {
                // sub rsp, 8
                pbCode[0] = REX_W;
                pbCode[1] = op_SUB_Ev_Ib;
                pbCode[2] = modrm(mod_reg, opext_SUB_Ev_Ib, rsp);
                pbCode[3] = 8;

                // call disciminator
                pbCode[4] = op_CALL_Jv;

                // add rsp, 8
                pbCode[ 9] = REX_W;
                pbCode[10] = op_LEA_Gv_M;
                pbCode[11] = modrm(mod_disp8, rsp, rsp);
                pbCode[12] = 0x24;
                pbCode[13] = 8;

                // ret
                pbCode[14] = op_RET;
            } // Run
        }; // Asm
        Asm::Run(pFunObj->GetCodeVec());

        fin->Decode<FunObj>()->PatchCallee(5,
            Quninitialized_funcallable_instance->Decode<Symbol>()->
                m_function );
    } // code

    // Initialize annotations
    {
        FunObj::Annon* pnAnnon = pFunObj->GetAnnon();
        pnAnnon->m_eType = FunObj::Annon::Type_Callee;
        pnAnnon->m_ofs   = 5;
    }

    // Populate GC Map
    {
        pFunObj->GetGcMap()[0] = 0;
    }

    Int n = Fixnum::Decode_(classd->Decode<ClassD>()->m_format_misc);

    Val storage = MiniThread::Get()->AllocVector(
        CLASSD_storage,
        Fixnum::Encode(n) );

    storage->Decode<Storage>()->m_storaged = classd;

    for (Int i = 0; i < n; i++)
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
    return fin->Decode<FunObj>()->FetchCallee(5);
} // funcallable_instance_function


// set_funcallable_instance_function (CLOS/MOP)
Val set_funcallable_instance_function(Val fin, Val fun)
{
    check_type(fin, funcallable_standard_object);
    check_type(fun, function);
    fin->Decode<FunObj>()->PatchCallee(5, fun);
    return fun;
} // set_funcallable_instance_function

} // MiniLisp
