#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - 3 Evaluation and Compilation
// compiler/cl/cl_03_call.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_49_internal.cpp#17 $
//
//  Description:
//   This file contains following functions:
//      ref
//
#include "./cl_defs.h"

namespace Compiler
{

namespace
{

// find_slotd
static Val find_slotd(Val klass, Val slot_name)
{
    Val instanced = klass->Decode<Class>()->m_instanced;
    if (nil == instanced)
    {
        if (CLASS_built_in_class == class_of(klass))
        {
            foreach (
                EnumList,
                oEnum,
                klass->Decode<BuiltInClass>()->m_direct_slots )
            {
                Val dslotd = oEnum.Get();
                if (dslotd->Decode<DirectSlotD>()->m_name == slot_name)
                {
                    return dslotd;
                }
            } // for each dslotd
            return nil;
        } // if

        warn(L"Class ~S must be finalized.",
            klass->Decode<Class>()->m_name );
        return nil;
    }

    foreach (EnumList, oEnum, klass->Decode<Class>()->m_slots)
    {
        Val slotd = oEnum.Get();
        EffectiveSlotD* pSlotD = slotd->Decode<EffectiveSlotD>();
        if (pSlotD->m_name == slot_name)
        {
            return slotd;
        }
    } // for each slotd

    return nil;
} // find_slotd


// checK_class_and_slot
static bool check_class_and_slot(Val klass_name, Val slot_name, Ty* out_ty)
{
    bool fFound = false;

    *out_ty = t;

    {
        Val klass = find_class(klass_name, nil, TLV(AenvironmentA));
        if (nil == klass)
        {
            warn(L"Class ~S isn't defined.", klass_name);
        }
        else
        {
            Val eslotd = find_slotd(klass, slot_name);
            if (nil != eslotd)
            {
                fFound = true;
                Ty ty = eslotd->Decode<EffectiveSlotD>()->m_type;
                *out_ty = ty_expand(ty);
            }
        }
    }

    return fFound;
} // check_class_and_lost

// quote
Val quote(Val x)
{
    return list(Qquote, x);
} // quote


// quote_form_p
bool quote_form_p(Val form)
{
    return safe_list_length(form) == Fixnum::Encode(2) &&
           Qquote == car(form);
} // quote_form_p


static Ty ty_native(Ty ty)
{
    if (ty == ty_float64) return ty_double_float;
    if (ty == ty_float32) return ty_single_float;

    if (ty == ty_int)     return ty_integer;
    if (ty == ty_uint)    return ty_unsigned_byte_;

    if (ty == ty_int64)   return ty_signed_byte_64_;
    if (ty == ty_int32)   return ty_signed_byte_32_;
    if (ty == ty_int16)   return ty_signed_byte_16_;
    if (ty == ty_int8)    return ty_signed_byte_8_;

    if (ty == ty_uint64)  return ty_unsigned_byte_64_;
    if (ty == ty_uint32)  return ty_unsigned_byte_32_;
    if (ty == ty_uint16)  return ty_unsigned_byte_16_;
    if (ty == ty_uint8)   return ty_unsigned_byte_8_;

    return ty;
} // ty_native


} // namespace

//////////////////////////////////////////////////////////////////////
//
//  ClParser::parse__elt
//
define_parser(_elt)
{
    CHECK_SYNTAX(4, 4, "(!elt ty obj idx)");

    Val tyform = second(form);
    Val objform = third(form);
    Val idxform = fourth(form);

    unless (quote_form_p(tyform)) goto general_case;

    Ty elty = ty_expand(second(tyform));
    Ty outy = ty_native(elty);

    Operand* pSVector = parseOpd(Q_elt, ty_t, 2, objform);
        if (Obj_Unreachable == pSVector) return useless_form(form);

    Operand* pSIndex = parseOpd(Q_elt, ty_sequence_index, 3, idxform);
        if (Obj_Unreachable == pSIndex) return useless_form(form);

    Register* pRElt = new Register();
        emitInsn(new EltInsn(ty_make_ptr(elty), pRElt, pSVector, pSIndex));
    Register* pRValue = NewOutput(elty)->StaticCast<Register>();
        emitInsn(new LoadInsn(pRValue, pRElt));

    if (elty != outy)
    {
        Register* pRx = pRValue;
        pRValue = new Register();
        emitInsn(new BoxInsn(outy, pRValue, pRx));
    }

    return emitLinkage(emitCast(pExpect, form, pRValue));

  general_case:
    style_warn(L"~S isn't inlined.", form);
    Callee oCallee;
        parseCallee(quote(first(form)), &oCallee);
    return parseCall(pExpect, form, &oCallee, cdr(form));
} // _elt


//////////////////////////////////////////////////////////////////////
//
//  ClParser::parse__elt
//
define_parser_setf(_elt)
{
    CHECK_SYNTAX(6, 6, "(funcall #'(setf !elt) val ty vector index)");

    Val tyform =  fourth(form);
    Val objform = fifth(form);
    Val idxform = sixth(form);

    unless (quote_form_p(tyform)) goto general_case;

    Ty elty  = ty_expand(second(tyform));
    Ty valty = ty_native(elty);

    Operand* pSValue = parseOpd(SETF_elt, valty, 3, third(form));
        if (Obj_Unreachable == pSValue) return useless_form(form);

    Operand* pSVector = parseOpd(SETF_elt, ty_t, 4, objform);
        if (Obj_Unreachable == pSVector) return useless_form(form);

    Operand* pSIndex = parseOpd(SETF_elt, ty_sequence_index, 5, idxform);
        if (Obj_Unreachable == pSIndex) return useless_form(form);

    Operand* pSstore = pSValue;
    if (elty != valty)
    {
        Register* pRd = NewOutput(elty)->StaticCast<Register>();
        emitInsn(new UnboxInsn(elty, pRd, pSValue));
        pSstore = pRd;
    }

    Register* pRElt = new Register();
        emitInsn(new EltInsn(ty_make_ptr(elty), pRElt, pSVector, pSIndex));
        emitInsn(new StoreInsn(pRElt, pSstore));
    return emitLinkage(emitCast(pExpect, form, pSValue));

  general_case:
    style_warn(L"~S isn't inlined.", form);
    Callee oCallee;
        parseCallee(quote(second(form)), &oCallee);
    return parseCall(pExpect, form, &oCallee, cddr(form));
} // setf _elt


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_Zref
//
define_parser(Zref)
{
    CHECK_SYNTAX(4, 4, "(%ref class slot obj)")

    Val klass_name = second(form);
    Val slot_name  = third(form);

    unless (quote_form_p(klass_name)) goto general_case;
        klass_name = second(klass_name);

    unless (quote_form_p(slot_name)) goto general_case;
        slot_name = second(slot_name);

    if (Qclassd == klass_name) klass_name = Qclass_description;

    Operand* pSobject;
    {
        ExpectOperand oExpect(QZref, 2, klass_name);

        if (Qinstance == klass_name)
        {
            oExpect.Type = Qstandard_object;
        }
        else
        {
            Val klass = find_class(klass_name, nil, TLV(AenvironmentA));
            if (nil == klass)
            {
                warn(L"Class ~S isn't defined.", klass_name);
                return Obj_Unreachable;
            }

            if (ty_typep(klass, Qstandard_class))
            {
                oExpect.Type = Qstorage;
            }
            else if (ty_typep(klass, Qfuncallable_standard_class))
            {
                oExpect.Type = Qstorage;
            }
        } // if

        pSobject = parseForm1(&oExpect, fourth(form));
        if (Obj_Unreachable == pSobject)
        {
            return useless_form(form);
        }
    } // pSobject

    Ty sloty;
    {
        Val klass_name2 = klass_name;
            if (klass_name2 == Qlist) klass_name2 = Qcons;
        if (! check_class_and_slot(klass_name2, slot_name, &sloty))
        {
            warn(L"Class ~S doesn't have slot ~S.",
                klass_name2,
                slot_name );
        }
    } // sloty

    Ty outy = ty_native(sloty);

    Register* pRslot = new Register();
    emitInsn(new SlotInsn(ty_make_ptr(sloty), pRslot,
        NewLiteral(klass_name),
        NewLiteral(slot_name),
        pSobject ) );

    Register* pRd = new Register();
    emitInsn(new LoadInsn(pRd, pRslot));

    if (sloty != outy)
    {
        Register* pRx = pRd;
        pRd = new Register();
        emitInsn(new BoxInsn(outy, pRd, pRx));
    }

    return emitLinkage(emitCast(pExpect, form, pRd));

  general_case:
    style_warn(L"~S isn't inlined.", form);
    Callee oCallee;
        parseCallee(quote(first(form)), &oCallee);
    return parseCall(pExpect, form, &oCallee, cdr(form));
} // Zref


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_setf_Zref
//
define_parser_setf(Zref)
{
    CHECK_SYNTAX(6, 6, "(funcall #'(setf %ref) val class slot obj)")

    Val klass_name = fourth(form);
    Val slot_name  = fifth(form);

    if (safe_list_length(klass_name) == Fixnum::Encode(2) &&
        Qquote == car(klass_name) )
    {
        klass_name = second(klass_name);
    }
    else
    {
        goto general_case;
    }

    if (safe_list_length(slot_name) == Fixnum::Encode(2) &&
        Qquote == car(slot_name) )
    {
        slot_name = second(slot_name);
    }
    else
    {
        goto general_case;
    }

    if (Qclassd == klass_name) klass_name = Qclass_description;

    Val sloty;
    if (! check_class_and_slot(klass_name, slot_name, &sloty))
    {
        warn(L"Class ~S doesn't have slot ~S.",
            klass_name,
            slot_name );
        return emitLinkage(NewLiteral(nil));
    } // sloty

    Ty valty = ty_native(sloty);

    Operand* pSvalue;
    {
        ExpectOperand oExpect(SETF_Zref, 3, valty); 
        pSvalue = parseForm1(&oExpect, third(form));
        if (Obj_Unreachable == pSvalue)
        {
            return useless_form(form);
        }
    } // pSvalue

    Operand* pSobject;
    {
        ExpectOperand oExpect(SETF_Zref, 3, klass_name);

        if (Qinstance == klass_name)
        {
            oExpect.Type = Qstandard_object;
        }
        else
        {
            Val klass = find_class(klass_name, nil, TLV(AenvironmentA));
            if (nil == klass)
            {
                warn(L"Class ~S isn't defined.", klass_name);
                return Obj_Unreachable;
            }

            if (ty_typep(klass, Qstandard_class))
            {
                oExpect.Type = Qstorage;
            }
            else if (ty_typep(klass, Qfuncallable_standard_class))
            {
                oExpect.Type = Qstorage;
            }
        } // if

        pSobject = parseForm1(&oExpect, sixth(form));
        if (Obj_Unreachable == pSobject)
        {
            return useless_form(form);
        }
    } // pSobject

    Operand* pSstore = pSvalue;
    if (valty != sloty)
    {
        Register* pRd = NewOutput(sloty)->StaticCast<Register>();
        emitInsn(new UnboxInsn(sloty, pRd, pSvalue));
        pSstore = pRd;
    }

    Register* pRslot = new Register();
    emitInsn(new SlotInsn(ty_make_ptr(sloty), pRslot,
        NewLiteral(klass_name),
        NewLiteral(slot_name),
        pSobject ) );

    emitInsn(new StoreInsn(pRslot, pSstore));
    return emitLinkage(emitCast(pExpect, third(form), pSvalue));

  general_case:
    style_warn(L"~S isn't inlined.", form);
    Callee oCallee;
        parseCallee(quote(second(form)), &oCallee);
    return parseCall(pExpect, form, &oCallee, cddr(form));
} // setf Zref

define_parser(without_garbage_collection)
{
    CHECK_SYNTAX(1, MaxFormLength,
        "(without-garbage-collection decl* form*)" );

    SimpleFrame* pFrame = new SimpleFrame(GetFunction());

    emitInsn(new OpenSimpleI(pFrame, Kernel::Frame::Type_GcDisable));

    Operand* pSx;

    pushFrame(pFrame);

    Context::BindingScope oLocally(
            LexEnv::Kind_locally, GetContext(), pExpect->Type );

    Val forms = parse_declarations(cdr(form));

    BBlock* pSucc = GetContext()->SetContinue();

    Expect oExpect = *pExpect;
    oExpect.Type = GetLexEnv()->GetTy();

    pSx = parseForms(&oExpect, forms);

    popFrame();

    if (Obj_Unreachable != pSx)
    {
        Operand* pSave = emitWeakSaveValues(pSx);
        emitUnwind(pFrame);
        pSx = emitWeakRestoreValues(pSave);
    }

    GetContext()->RestoreSucc(pSucc);
    return emitLinkage(pSx);
} // without_garbage_collection

} // Compiler
