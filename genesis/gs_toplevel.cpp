#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - toplevel
// gs_toplevel.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_toplevel.cpp#12 $
//
#include "../build.h"
#include "./gs_init.h"

namespace Genesis
{
    extern Int TLV_Acmdl_conditionA;
    extern Int TLV_Acmdl_levelA;
    void __declspec(noreturn) command_loop();
} // Genesis

namespace
{

//////////////////////////////////////////////////////////////////////
//
// Initialize lisp runtime
//
static void initRuntime()
{
    #if _DEBUG
    {
        Val cell = find_value_cell(QAdefault_featuresA);
        Val Kdebug_kernel = Q(":DEBUG-KERNEL");
        if (nil == memq(Kdebug_kernel, cell->Decode<ValueCell>()->m_value))
        {
            push(Kdebug_kernel, cell->Decode<ValueCell>()->m_value);
        }
    }
    #endif // _DEBUG

    set_tlv(TLV_AenvironmentA,
        value_cell_value(VAR_Aruntime_environmentA) );

    // Note: genesis starts in system package rather than cl-user.
    set_tlv(TLV_ApackageA, PACKAGE_si);

    set_tlv(TLV_AreadtableA, VAR(Astandard_readtableA));
    set_tlv(TLV_AfeaturesA, VAR(Adefault_featuresA));

    set_tlv(TLV_c6_AtargetA, intern(Host::GetName(), PACKAGE_keyword));

    set_tlv(TLV_Aobject_poolA, make_hash_table(Qeq));

    #if defined(_DEBUG)
    {
        if (nil == TLV(c6_AsettingsA))
        {
            set_tlv(TLV_c6_AsettingsA,
                list(list(QA, cons(Kdebug, Fixnum::Encode(9)))) );
        }
    }
    #endif // defined(_DEBUG)
} // InitRuntime

} // namespace

namespace Genesis
{

void __declspec(noreturn)
genesis_toplevel(Val)
{
    initRuntime();

    ASSERT(readtablep(value_cell_value(VAR_Astandard_readtableA)));
    ASSERT(hash_table_p(value_cell_value(VAR_Asetf_tableA)));
    ASSERT(hash_table_p(value_cell_value(VAR_Avalue_tableA)));
    ASSERT(packagep(TLV(ApackageA)));
    ASSERT(readtablep(TLV(AreadtableA)));
    ASSERT(Fixnum::Encode(10) == TLV(Aprint_baseA));
    ASSERT(Fixnum::Encode(10) == TLV(Aread_baseA));

    TLV_Acmdl_conditionA = deftlv(Q("SI:*CMDL-CONDITION*"), nil);
    TLV_Acmdl_levelA     = deftlv(Q("SI:*CMDL-LEVEL*"), Fixnum::Encode(0));

    // Pop "ToForeign" Frame.
    MiniThread::Get()->m_fp = Fixnum::Encode(
        MiniThread::Get()->GetFP()->m_pOuter );

    format(t,
        L";;;; Evita Common Lisp for Windows~%" );

    format(t,
        L";;;; Version ~A ~A (Genesis)~%",
        make_string(MY_FileVersionW),
        VAR(c6_Peval_targetP) );
 
    format(t,
        L";;;; Copyright (C) 1996-2007 Project Vogue. All rights reserved.~%" );

    format(t, L";;;~%");

    Val time = VAR(Aimage_save_timeA);

    if (Fixnum::Encode(0) != time)
    {
        format(t, L";;; Image is saved at ~D.~%", time );
    }
    else
    {
        format(t, L";;; Boot~%");
    } // if

    {
        // See WinNT.h for NT_TIB. (fs:[18h])
        NT_TIB* pTib = (NT_TIB*) NtCurrentTeb();

        format(t, L";;;          tib = ~X~X~%",
            Fixnum::Encode((UInt) pTib->Self >> 4),
            Fixnum::Encode((UInt) pTib->Self & 15) );

        format(t, L";;; user pointer = ~X~X~%",
            Fixnum::Encode((UInt) pTib->ArbitraryUserPointer >> 4),
            Fixnum::Encode((UInt) pTib->ArbitraryUserPointer & 15) );

        format(t, L";;;   fiber data = ~X~X~%",
            Fixnum::Encode((UInt) pTib->FiberData >> 4),
            Fixnum::Encode((UInt) pTib->FiberData & 15) );

        format(t, L";;;   stack base = ~X~X~%",
            Fixnum::Encode((UInt) pTib->StackBase >> 4),
            Fixnum::Encode((UInt) pTib->StackBase & 15) );

        format(t, L";;;  stack limit = ~X~X~%",
            Fixnum::Encode((UInt) pTib->StackLimit >> 4),
            Fixnum::Encode((UInt) pTib->StackLimit & 15) );
    }

    format(t, L";~%");

    command_loop();
} // genesis_toplevel


//////////////////////////////////////////////////////////////////////
//
// Toplevel
//
int Toplevel()
{
    bind_standard_streams();

    if (Fixnum::Encode(0) == VAR(Aimage_save_timeA))
    {
        initRuntime();

        Initializer oIniter;
        oIniter.Run();
    }

    Val fn = Qstart_application->Decode<Symbol>()->m_function;
    if (nil == fn)
    {
        format(t, L"~S must be defined.", Qstart_application);
        return 1;
    }

    Thread* p = Thread::Get();
    p->m_fn = fn;
    p->m_n = Fixnum::Encode(1);
    p->mv_value[0] = nil;
    p->Start();
} // Toplevel

} // Genesis
