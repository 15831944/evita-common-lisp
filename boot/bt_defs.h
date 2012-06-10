//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - pre-compiled header
// boot_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/boot/bt_defs.h 16 2006-10-02 11:14:12 yosi $
//
#if !defined(INCLUDE_boot_defs_h)
#define INCLUDE_boot_defs_h

#include "../mini/mini_defs.h"

#include "./bt_object.h"

using namespace Image;

namespace Boot
{
    void augment_function(Val, Val, Val);
    Val defobject_(LPCWSTR, Val);

    #define defobject(mp_name, mp_val) \
        defobject_(L ## #mp_name, mp_val)

    #define Defobject(mp_name, mp_val) \
        mp_name = defobject_(L ## #mp_name, mp_val)
} // Boot

enum
{
    PACKAGE_cl_externals = 978,
    PACKAGE_clos_externals = 87,
}; // enum

#endif //!defined(INCLUDE_boot_defs_h)
