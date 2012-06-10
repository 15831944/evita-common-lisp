//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - pre-compiled header
// mini_07_object.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_07_object.h#3 $
//
#if !defined(INCLUDE_mini_07_object_h)
#define INCLUDE_mini_07_object_h

namespace MiniLisp
{
    bool classp(Val);
    bool subclassp(Val, Val);

    bool funcallable_standard_object_p(Val);

    inline bool funcallable_standard_objectp(Val x)
        { return funcallable_standard_object_p(x); }

    Val allocate_funcallable_instance(Val);
    Val funcallable_instance_function(Val);
    Val set_funcallable_instance_function(Val, Val);
    Val subst_in_function(Val, Val, Val, Val);
} // MiniLisp

namespace CommonLisp
{
    Val class_name(Val);
    Val class_of(Val);
    Val find_class(Val, Val = t, Val = nil);
} // CommonLisp

#endif //!defined(INCLUDE_mini_07_object_h)
