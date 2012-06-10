#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 17 Sequences
// gs_17_sequences.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_17_sequence.cpp#3 $
//
//
//      cl:nreverse
//
#include "./gs_lisp.h"

using namespace Genesis;

namespace Genesis
{

// nreverse_vector
Val nreverse_vector(Val vector)
{
    if (vector->Is<Record>())
    {
        switch (CLASSD_INDEX_OF(vector->Decode<Record>()->m_classd))
        {
        case CLASSD_(simple_vector):
        {
            Val jndex = vector->Decode<DataVector>()->m_length;
            Val index = Fixnum::Encode(0);
            for (;;)
            {
                if (cmp_xx(index, jndex) >= 0)
                {
                    return vector;
                }
                jndex = sub_xx(jndex, 1);
                Val temp = svref(vector, index);
                setf_svref(svref(vector, jndex), vector, index);
                setf_svref(temp, vector, jndex);
                index = add_xx(index, 1);
            } // while
        } // simple_vector

        case CLASSD_(simple_string):
        {
            Val jndex = vector->Decode<DataVector>()->m_length;
            Val index = Fixnum::Encode(0);
            for (;;)
            {
                if (cmp_xx(index, jndex) >= 0)
                {
                    return vector;
                }
                jndex = sub_xx(jndex, 1);
                Val temp = schar(vector, index);
                setf_schar(schar(vector, jndex), vector, index);
                setf_schar(temp, vector, jndex);
                index = add_xx(index, 1);
            } // while
        } // simple_string

        //case CLASSD_(vector)

        //case CLASSD_(simple_bit_vector):

        //case CLASSD_(signed_byte_8_vector):
        //case CLASSD_(signed_byte_16_vector):
        //case CLASSD_(signed_byte_32_vector):
        //case CLASSD_(signed_byte_64_vector):

        //case CLASSD_(unsigned_byte_8_vector):
        //case CLASSD_(unsigned_byte_16_vector):
        //case CLASSD_(unsigned_byte_32_vector):
        //case CLASSD_(unsigned_byte_64_vector):

        //case CLASSD_(double_float_vector):
        //case CLASSD_(single_float_vector):

        //case CLASSD_(double_float_complex_vector):
        //case CLASSD_(single_float_complex_vector):
        } // switch classd
    } // if

    error(make_type_error(vector, Qvector));
} // nreverse_vector

} // Genesis

namespace CommonLisp
{

// nreverse
Val nreverse(Val x)
{
    if (listp(x))
    {
        return nreverse_list(x);
    }
    else if (vectorp(x))
    {
        return nreverse_vector(x);
    }

    error(make_type_error(x, Qsequence));
} // nreverse

} // CommonLisp
