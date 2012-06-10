//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 3 Evaluation and Compilation
// mini_03_eval.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_03_eval.h#2 $
//
#if !defined(INCLUDE_mini_03_eval_h)
#define INCLUDE_mini_03_eval_h

namespace CommonLisp
{
    bool constantp(Val, Val = nil);
    bool special_operator_p(Val);

    Val eval(Val);
    Val macro_function(Val, Val = nil);

    Val setf_macro_function(Val, Val, Val = nil);
    //Val setf_macro_function_aux(Val, Val, Val);

} // CommonLisp

namespace MiniLisp
{
    bool environmentp(Val);
    bool toplevel_environment_p(Val);

    Val make_environment(Val name, Val = nil);
    Val make_not_function_function(Val);

    Val intern_dll_entry(Val, Val);

    Val var_info(Val);
} // MiniLisp

#endif //!defined(INCLUDE_mini_03_eval_h)
