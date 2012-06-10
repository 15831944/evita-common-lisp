#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - minilisp - type system
// compiler/ty/ty_00_fns.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ty/ty_12_number.cpp#10 $
//
#include "./ty_defs.h"

namespace Compiler
{

namespace
{

struct ArrayTyDesc
{
    Val m_name;
    Val m_kind;
    Val m_elty;
    Val m_rank;
    Val m_dims;
}; // ArrayTyDesc

static const Val one = Fixnum::Encode(1);

static const ArrayTyDesc k_rgoArrayTyDesc[] =
{
    { Qarray,               Qarray, QA, QA, QA },
    { Qsimple_array,        Qsimple_array, QA, QA, QA },

    { Qvector,              Qarray, QA, one, list_QA },
    { Qsimple_vector,       Qsimple_array, ty_t, one, list_QA },

    { Qstring,              Qarray, ty_character, one, list_QA },
    { Qsimple_string,       Qsimple_array, ty_character, one, list_QA },

    { Qbit_vector,          Qarray, ty_bit, one, list_QA },
    { Qsimple_bit_string,   Qsimple_array, ty_bit, one, list_QA },
}; // k_rgoArrayTyDesc

} // namespace


// TyArray ctor
TyArray::TyArray(Val tyspec) :
    m_kind(nil),
    m_elty(QA),
    m_dims(QA),
    m_rank(QA),
{
    Ty ty = ty_expand(ty);

    if (symbolp(ty))
    {
        foreach (
            const ArrayTyDesc* p = &k_rgoArrayTyDesc[0];
            p < &k_rgoArrayTyDesc[lengthof(k_rgoArrayTyDesc)];
            p++ )
        {
            if (p->m_name == ty)
            {
                m_elty = p->m_elty;
                m_dims = p->m_dims;
                m_rank = p->m_rank;
                m_kind = p->m_kind;
                return;
            }
        } // for each desc
    }
    else if (consp(ty))
    {
        Val op = first(ty);

        foreach (
            const ArrayTyDesc* p = &k_rgoArrayTyDesc[0];
            p < &k_rgoArrayTyDesc[lengthof(k_rgoArrayTyDesc)];
            p++ )
        {
            if (p->m_name != ty) continue;
            m_elty = p->m_elty;
            m_dims = p->m_dims;
            m_rank = p->m_rank;
            m_kind = p->m_kind;

            if (m_elty == QA)
            {
                switch (safe_list_length(ty))
                {
                case 1: // (vector)
                    break;
                case 2: // (vector elty)
                    m_elty = ty_expand(second(ty));
                    break;
                case 3: // (vector elty n)
                    if (list_QA == m_dims)
                    {
                        m_dims = list(third(ty));
                    }
                    else if (listp(ty))
                    {
                        m_rank = safe_list_length(m_dims);
                    }
                    break;
                default:
                    goto invalid_typespec;
                } // switch len
            }
            else
            {
                switch (safe_list_length(ty))
                {
                case 1: // (string)
                    break;
                case 2: // (string n)
                    m_dims = list(third(ty));
                    break;
                default:
                    goto invalid_typespec;
                } // switch len
            } // if

            if (QA == m_dims)
            {
                // arbitry rank
            }
            else if (listp(m_dims))
            {
                foreach (EnumList, oEnum, m_dims)
                {
                    Val dim = oEnum.Get();
                    if (QA == dim) continue;
                    if (! fixnum(dim)) goto invalid_typespec;
                    if (Fixnum::Decode_(dim) < 0) goto invalid_typespec;
                } // for each
            }
            else
            {
                goto invalid_typespec;
            }
            return;
        } // for each desc

  invalid_typespec:
    warn(L"Invalid array type-specifier: ~S", ty);
} // TyArray::TyArray

} // Compiler
