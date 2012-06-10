//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 49 Internals - Cell
// mini/mini_49_cell.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_49_cell.h#3 $
//
#if !defined(INCLUDE_mini_49_cell_h)
#define INCLUDE_mini_49_cell_h

namespace MiniLisp
{
    // 49 Image
    bool heap_object_p(Val);

    bool class_description_p(Val);

    // 49 Value Cell
    bool value_cell_p(Val);
    Val make_value_cell(Val, Val);
    Val value_cell_name(Val);
    Val value_cell_type(Val);
    Val value_cell_value(Val);
    Val setf_value_cell_value(Val, Val);

    bool setf_cell_p(Val);
    Val make_setf_cell(Val, Val);
    Val setf_cell_name(Val);
    Val setf_cell_type(Val);
    Val setf_cell_value(Val);
    Val setf_setf_cell_value(Val, Val);

    // 49 Value Cell
    bool closed_cell_p(Val);
    Val closed_cell_value(Val);
    Val setf_closed_cell_value(Val);
} // MiniLisp

#endif //!defined(INCLUDE_mini_49_cell_h)
