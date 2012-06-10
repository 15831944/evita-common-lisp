//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - Common Lisp Parser
// compiler/cl/cl_parser.inc
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_parser.inc#21 $
//
#if ! defined(declare_parser)
    #error You must define declare_parser macro
#endif // ! defined(declare_parser)


    // Special operators
    declare_parser(block)                  //  1
    declare_parser(catch)                  //  2
    declare_parser(eval_when)              //  3
    declare_parser(flet)                   //  4
    declare_parser(function)               //  5
    declare_parser(go)                     //  6
    declare_parser(if)                     //  7
    declare_parser(labels)                 //  8
    declare_parser(let)                    //  9
    declare_parser(letA)                   // 10
    declare_parser(load_time_value)        // 11
    declare_parser(locally)                // 12
    declare_parser(macrolet)               // 13
    declare_parser(multiple_value_call)    // 14
    declare_parser(multiple_value_prog1)   // 15
    declare_parser(progn)                  // 16
    //declare_parser(progv)                  // 17
    declare_parser(quote)                  // 18
    declare_parser(return_from)            // 19
    declare_parser(setq)                   // 20
    declare_parser(symbol_macrolet)        // 21
    declare_parser(tagbody)                // 22
    declare_parser(the)                    // 23
    declare_parser(throw)                  // 24
    declare_parser(unwind_protect)         // 25

    // 4 Types
    declare_parser(coerce)
    declare_parser(typep)

    // 5 Data and Control Flow
    declare_parser(apply) 
    declare_parser(eq) 
    declare_parser(eql) 
    declare_parser(funcall) 
    declare_parser(multiple_value_bind)
    declare_parser(not)
    declare_parser(nth_value)
    declare_parser(return)
    declare_parser(values)

    // 12 Numbers
    declare_parser(Eq);     // 12.2.15 =
    declare_parser(Ne);     // 12.2.15 /
    declare_parser(Ge);     // 12.2.15 >=
    declare_parser(Gt);     // 12.2.15 >
    declare_parser(Le);     // 12.2.15 <=
    declare_parser(Lt);     // 12.2.15 <

    declare_parser(Add);        // 12.2.25 +
    declare_parser(Div);        // 12.2.27 /
    declare_parser(Mul);        // 12.2.24 *
    declare_parser(Sub);        // 12.2.26 -
    declare_parser(1Minus);     // 12.2.28 1-
    declare_parser(1Plus);      // 12.2.28 1+

    // [L]
    declare_parser(logand)      // 12.2.68
    declare_parser(logandc1)    // 12.2.68
    declare_parser(logandc2)    // 12.2.68
    declare_parser(logeqv)      // 12.2.68
    declare_parser(logior)      // 12.2.68
    declare_parser(lognand)     // 12.2.68
    declare_parser(lognor)      // 12.2.68
    declare_parser(lognot)      // 12.2.68
    declare_parser(logorc1)     // 12.2.68
    declare_parser(logorc2)     // 12.2.68
    declare_parser(logxor)      // 12.2.68

    // [M]
    declare_parser(max);        // 12.2.16 max
    declare_parser(min);        // 12.2.16 min
    declare_parser(minusp)      // 12.2.17 minusp

    // [P]
    declare_parser(plusp)       // 12.2.17 plusp

    // [Z]
    declare_parser(zerop)       // 12.2.18 zerop

    // 13 Characters
    declare_parser(CharEq) declare_parser(CharCiEq)
    declare_parser(CharGe) declare_parser(CharCiGe)
    declare_parser(CharGt) declare_parser(CharCiGt)
    declare_parser(CharLe) declare_parser(CharCiLe)
    declare_parser(CharLt) declare_parser(CharCiLt)
    declare_parser(CharNe) declare_parser(CharCiNe)

    // 14 Conses
    declare_parser(atom)
    declare_parser(endp)
    declare_parser(list)
    declare_parser(null)

    // 15 Arrays
    declare_parser(svref)           // 15.2.28
    declare_parser_setf(svref)      // 15.2.28

    // 49 Internals
    declare_parser(_elt)
    declare_parser_setf(_elt)

    declare_parser(Zref)
    declare_parser_setf(Zref)

    declare_parser(without_garbage_collection)

#undef declare_parser
#undef declare_parser_setf
