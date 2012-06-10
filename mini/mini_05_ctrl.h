//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 5 Data and Control Flow
// mini_05_ctrl.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_05_ctrl.h#4 $
//
#if !defined(INCLUDE_mini_05_ctrl_h)
#define INCLUDE_mini_05_ctrl_h

namespace MiniLisp
{
    bool function_name_p(Val);
    Val make_undefined_function(Val);

    Val find_setf_cell(Val);
    Val find_value_cell(Val);
    Val intern_setf_cell(Val);
    Val intern_value_cell(Val);

    Val ensure_funcallable(Val);
} // MiniLisp

namespace CommonLisp
{
    bool eq(Val, Val);
    bool eql(Val, Val);
    bool equal(Val, Val);
    bool equalp(Val, Val);

    bool fboundp(Val);

    Val fdefinition(Val);
    Val setf_fdefinition(Val, Val);

    Val funcall(Val);
    Val funcall(Val, Val);
    Val funcall(Val, Val, Val);
    Val funcall(Val, Val, Val, Val);
    Val funcall(Val, Val, Val, Val, Val);
    Val funcall(Val, Val, Val, Val, Val, Val);
    Val funcall(Val, Val, Val, Val, Val, Val, Val);
    Val funcall(Val, Val, Val, Val, Val, Val, Val, Val);
    Val funcall(Val, Val, Val, Val, Val, Val, Val, Val, Val);
    Val funcall(Val, Val, Val, Val, Val, Val, Val, Val, Val, Val);
    Val funcall(Val, Val, Val, Val, Val, Val, Val, Val, Val, Val, Val);

    bool functionp(Val x);

    Val multiple_value_list(Thread*);

    Val values();
    Val values(Val);
    Val values(Val, Val);
    Val values(Val, Val, Val);
    Val values(Val, Val, Val, Val);
    Val values(Val, Val, Val, Val, Val);
    Val values(Val, Val, Val, Val, Val, Val);
    Val values(Val, Val, Val, Val, Val, Val, Val);
    Val values(Val, Val, Val, Val, Val, Val, Val, Val);
    Val values(Val, Val, Val, Val, Val, Val, Val, Val, Val);
    Val values(Val, Val, Val, Val, Val, Val, Val, Val, Val, Val);
} // CommonLisp

#endif //!defined(INCLUDE_mini_05_ctrl_h)
