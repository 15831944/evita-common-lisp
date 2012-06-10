//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - pre-compiled header
// mini_lisp.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_lisp.h#3 $
//
#if !defined(INCLUDE_mini_lisp_h)
#define INCLUDE_mini_lisp_h


#include "../kernel/ke_support.h"
#include "./mini_thread.h"

#include "./mini_03_direct_call.h"
#include "./mini_03_eval.h"
#include "./mini_04_type.h"
#include "./mini_05_ctrl.h"
#include "./mini_07_object.h"
#include "./mini_09_cond.h"
#include "./mini_10_symbol.h"
#include "./mini_11_package.h"
#include "./mini_12_number.h"
#include "./mini_13_char.h"
#include "./mini_14_cons.h"
#include "./mini_15_array.h"
#include "./mini_17_sequence.h"
#include "./mini_16_string.h"
#include "./mini_18_hash_table.h"
#include "./mini_21_stream.h"
#include "./mini_22_printer.h"
#include "./mini_23_reader.h"
#include "./mini_49_cell.h"
#include "./mini_49_tlv.h"
#include "./mini_50_lock.h"
#include "./mini_50_weak.h"

#include "./mini_inline.h"

namespace MiniLisp
{
    Val get_tlv(Int ofs);
    Val set_tlv(Int ofs, Val);

    inline Val get_tlv(Val index)
    {
        Int iOffset = MiniThread::ToTlvOffset(Fixnum::Decode_(index));
        return get_tlv(iOffset);
    } // set_tlv

    inline Val set_tlv(Val index, Val val)
    {
        Int iOffset = MiniThread::ToTlvOffset(Fixnum::Decode_(index));
        return set_tlv(iOffset, val);
    } // set_tlv

    ////////////////////////////////////////////////////////////
    //
    // Check Type
    //
    #define check_type(mp_var, mp_type) \
        if (! mp_type ## p(mp_var)) \
            error(make_type_error(mp_var, Q ## mp_type))

    inline bool hash_tablep(Val x)     { return hash_table_p(x); }
    inline bool class_descriptionp(Val x) { return class_description_p(x); }
    inline bool si_environmentp(Val x) { return environmentp(x); }
    inline bool si_value_cellp(Val x)  { return value_cell_p(x); }
    inline bool simple_bit_vectorp(Val x) { return simple_bit_vector_p(x); }
    inline bool simple_stringp(Val x) { return simple_string_p(x); }
    inline bool simple_vectorp(Val x) { return simple_vector_p(x); }
    inline bool tlv_recordp(Val x)    { return tlv_record_p(x); }

    ////////////////////////////////////////////////////////////
    //
    // For accessing lisp object from C
    //
    Val parse_symbol(LPCWSTR, LPCWSTR);

    inline Val parse_symbol(LPCWSTR pwsz)
        { return parse_symbol(pwsz, pwsz + ::lstrlenW(pwsz)); }

    ////////////////////////////////////////////////////////////
    //
    // For compiler and macro expander
    //
    Val safe_list_length(Val);


    #define Q(name) MiniLisp::parse_symbol(L##name)
    #define TLV(name) get_tlv(TLV_##name)

    #if SIZEOF_VAL == 8
        inline bool is_32bit(Int i)
            { return i < (1ll << 31) && i >= (-1ll << 31); }

        inline bool is_32bit(Val x)
        {
            return (fixnump(x) || characterp(x)) && is_32bit(x->ToInt());
        } // is_32bit
    #endif // SIZEOF_VAL == 8
} // MiniLisp

#endif //!defined(INCLUDE_mini_lisp_h)
