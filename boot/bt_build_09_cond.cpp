#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 09 Conditions
// boot/bt_09_cond.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/boot/bt_build_09_cond.cpp 24 2006-10-08 01:14:25 yosi $
//

#include "../mini/mini_lisp.h"

namespace Boot
{

Val compute_class_precedence_list();
Val make_class(Val, Val, Val);

static Val make_std_class(Val, Val);
static Val make_std_dslotd(Val);

static const char16 s_wszCond[] =
    #include "./bt_cond.inc"
;

// Scanner
class Scanner
{
    const char16* m_pwsz;
    char16  m_wsz[100];

    public: Scanner(const char16* pwsz) :
        m_pwsz(pwsz) { if (! AtEnd()) Next(); }

    public: bool AtEnd() const { return 0 == *m_pwsz; }

    public: const char16* Get() const
        { ASSERT(! AtEnd()); return m_wsz; }

    public: void Next()
    {
        ASSERT(! AtEnd());

        while (isspace(*m_pwsz)) m_pwsz++;
        char16* dst = m_wsz;
        while (0 != *m_pwsz && ! isspace(*m_pwsz)) *dst++ = *m_pwsz++;
        *dst = 0;
    } // Next

    bool isspace(char16 ch)
        { return ' ' == ch || 0x09 == ch || 0x0A == ch; }
}; // Scanner


// make_std_class
static Val make_std_class(Val name, Val instanced)
{
    Val klass = make_class(CLASSD_standard_class, name, instanced);
    return klass;
} // make_std_class


// make_std_classd
static Val make_std_classd()
{
    Val classd = MiniThread::Get()->AllocRecord(CLASSD_class_description);

    ClassD* pClassD = classd->Decode<ClassD>();
        pClassD->m_format       = FromInt<Val_>(ClassD::Format_Instance);
        pClassD->m_format_param = Fixnum::Encode(0);
        pClassD->m_format_misc  = Fixnum::Encode(0);
        pClassD->m_tag_code     = Fixnum::Encode(Instance::Tag);

    return classd;
} // make_std_classd


// make_std_dslotd
static Val make_std_dslotd(Val name)
{
    Val slotd = MiniThread::Get()->AllocInstance(
        CLASSD_standard_direct_slot_definition );

    DirectSlotD* pSlotD = slotd->Decode<DirectSlotD>();
        pSlotD->m_name         = name;
        pSlotD->m_allocation   = Q(":INSTANCE");
        pSlotD->m_type         = t;
        pSlotD->m_initform     = nil;
        pSlotD->m_initfunction = nil;
        pSlotD->m_readers      = nil;
        pSlotD->m_writers      = nil;

        pSlotD->m_initargs =
            list(intern(symbol_name(name), PACKAGE_keyword));

    return slotd;
} // make_std_dslotd


// build_09_Conditions
void build_09_Conditions()
{
    enum
    {
        State_Start,
        State_Name,
        State_Super,
        State_SlotName,
        State_SlotType,
    } eState = State_Start;

    Val  instanced = nil;
    Val  klass  = nil;
    Val* plast  = NULL;
    Val  dslotd = nil;

    foreach (Scanner, oScanner, s_wszCond)
    {
        const char16* token = oScanner.Get();
        switch (eState)
        {
        case State_Start:
            if (0 == ::lstrcmpW(token, L"define-condition"))
            {
                instanced = make_std_classd();
            }
            else if (0 == ::lstrcmpW(token, L"defclass"))
            {
                instanced = nil;
            }
            else
            {
                error(L"Unknown keytoken ~S", make_string(token));
            }

            eState = State_Name;
            break;

        case State_Name:
            klass = make_std_class(parse_symbol(token), instanced);
            plast = &(klass->Decode<Class>()->m_direct_slots);
            eState = State_Super;
            break;

        case State_Super:
        {
            Val super = find_class(parse_symbol(token));

            klass->Decode<Class>()->m_direct_superclasses =
                nconc(
                    klass->Decode<Class>()->m_direct_superclasses,
                    list(super) );

            push(klass, super->Decode<Class>()->m_direct_subclasses);

            eState = State_SlotName;
            break;
        } // State_Super

        case State_SlotName:
            if (0 == ::lstrcmpW(token, L"+"))
            {
                eState = State_Super;
            }
            else if (0 == ::lstrcmpW(token, L"define-condition"))
            {
                instanced = make_std_classd();
                klass = nil;
                eState = State_Name;
            }
            else if (0 == ::lstrcmpW(token, L"defclass"))
            {
                instanced = nil;
                klass = nil;
                eState = State_Name;
            }
            else
            {
                dslotd = make_std_dslotd(parse_symbol(token));
                Val kons = list(dslotd);
                *plast = kons;
                plast = &(kons->Decode<Cons>()->m_cdr);
                eState = State_SlotType;
            }
            break;

        case State_SlotType:
            dslotd->Decode<DirectSlotD>()->m_type = parse_symbol(token);
            eState = State_SlotName;
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch state
    } // for each work

    ASSERT(State_SlotName == eState);
} // build_09_Conditions

} // Boot
