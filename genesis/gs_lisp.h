//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - Genesis Lisp
// genesis/gs_lisp.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_lisp.h#4 $
//
#if !defined(INCLUDE_genesis_gs_lisp_h)
#define INCLUDE_genesis_gs_lisp_h

#define DEFEXPORT extern "C" Val __fastcall

#include "../big/big_lisp.h"

namespace CommonLisp
{
    // 3 Evaluation and Compilation
    Val compiler_macro_function(Val, Val = nil);
    Val macroexpand_1(Val, Val = nil);

    // 5 Data and Control Flows
    Val apply(Val, Val);
    bool equal(Val, Val);
    Val values_list(Val);

    // 11 Package
    Val find_package(Val);

    // 14 Conses
    Val last(Val, Val = Fixnum::Encode(1));

    // 17 Sequences
    Val nreverse(Val);

    // 23 Reader
    Val read(Val = t, Val = nil, Val = nil, Val = nil);

    // 25 Environment
    Val disassemble(Val);
    Val encode_universal_time(Val, Val, Val, Val, Val, Val, Val);
    Val get_decoded_time();
    Val get_universal_time();
    Val inspect(Val);
    Val room(Val);
} // CommonLisp


namespace Genesis
{

    enum { MaxFormLength = 1 << 30 };

    // 3 Evaluation and Compilation
    Val expand_lambda(Val, Val);

    // 3.1 Global Environment
    Val setf_function_information(Val, Val, Val, Val = nil);
    Val setf_variable_information(Val, Val, Val, Val = nil);
    Val function_information(Val, Val, Val*, Val = nil);
    Val variable_information(Val, Val, Val*, Val = nil);
    Val toplevel_environment(Val);

    inline Val function_information(Val name, Val key, Val env = nil)
    {
        Val present_p;
        return function_information(name, key, &present_p, env);
    } // function_information

    inline Val variable_information(Val name, Val key, Val env = nil)
    {
        Val present_p;
        return variable_information(name, key, &present_p, env);
    } // variable_information

    // 5 Data and Control Flows
    Val expand_and(Val, Val);
    Val expand_cond(Val, Val);
    Val expand_defconstant(Val, Val);
    Val expand_defun(Val, Val);
    Val expand_defvar(Val, Val);
    Val expand_multiple_value_bind(Val, Val);
    Val expand_multiple_value_list(Val, Val);
    Val expand_multiple_value_setq(Val, Val);
    Val expand_nth_value(Val, Val);
    Val expand_or(Val, Val);
    Val expand_prog1(Val, Val);
    Val expand_psetq(Val, Val);
    Val expand_return(Val, Val);
    Val expand_setf(Val, Val);
    Val expand_unless(Val, Val);
    Val expand_when(Val, Val);

    Val Pdefconstant(Val, Val, Val);
    Val Pdefun(Val, Val, Val);
    Val Pdefvar(Val, Val, Val, Val);

    Val C_funcall(MiniThread*);

    // 6 Iteration
    Val expand_do(Val, Val);
    Val expand_doA(Val, Val);
    Val expand_dolist(Val, Val);
    Val expand_dotimes(Val, Val);
    Val expand_loop(Val, Val);

    // 9 Conditions
    void __declspec(noreturn) C_error(MiniThread*);
    Val expand_assert(Val, Val);
    Val expand_check_type(Val, Val);

    // 11 Packages
    Val ensure_package(Val);
    Val expand_in_package(Val, Val);

    Val Zin_package(Val);

    // 12 Numbers
    Val expand_decf(Val, Val);
    Val expand_incf(Val, Val);

    Val op_1P(Val); // 1+
    Val op_1_(Val); // 1-
    Val C_op_GE(MiniThread*);

    // 14 Constes
    Val expand_pop(Val, Val);
    Val expand_push(Val, Val);

    Val C_list(MiniThread*);
    Val C_listA(MiniThread*);
    Val C_nconc(MiniThread*);

    // 17 Sequences
    bool sequencep(Val);
    Val nreverse_vector(Val);

    // 23 Reader
    Val get_char_attr(Val, Val);

    // 24 System Construction
    Val C_load(MiniThread*);

    // 25 Environment
    Val expand_time(Val, Val);
    Val get_function(Val);
    Val time_it(Val);

    // 49 Internals
    Val expand_ext_ref(Val, Val);

    ////////////////////////////////////////////////////////////
    //
    // Utility functions for macro expander
    //
    Val check_syntax(Val, int, int, LPCWSTR);

    #define CHECK_SYNTAX(mp_form, mp_min, mp_max, mp_syntax) \
        { \
            Val mvar_x = check_syntax(mp_form, mp_min, mp_max, L##mp_syntax); \
            if (nil != mvar_x) return mvar_x; \
        }

    inline bool sequence(Val x)
        { return listp(x) || vectorp(x); }
} // Genesis

#endif //!defined(INCLUDE_genesis_gs_lisp_h)
