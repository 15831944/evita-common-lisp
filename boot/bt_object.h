//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - Boot Objects
// boot/bt_objet.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/boot/bt_object.h 19 2006-10-09 02:26:17 yosi $
//
#if !defined(INCLUDE_boot_objet_h)
#define INCLUDE_boot_objet_h

namespace Kernel
{
    class Value_;
    typedef Value_* Value;
} // Kernel

namespace Image
{

using namespace Kernel;

#define DEFCLASS(mp_cname, mp_NAME, mp_super) \
    extern Val CLASS_##mp_cname; \
    extern Val CLASSD_##mp_cname; \
    extern Val Q##mp_cname; \
    extern Val ty_##mp_cname;

#include "../kernel/ke_layout.inc"


#define DEFKEY(mp_cname, mp_name) \
    extern Val K##mp_cname;

#define DEFPACKAGE(mp_cname, mp_name, mp_alias, mp_ext, mp_int, mp_uses) \
    extern Val PACKAGE_##mp_cname;

#define DEFOBJECT(mp_cname, mp_expr) \
    extern Val mp_cname;

#define DEFSYM(mp_pkg, mp_cname, mp_name) \
    extern Val Q##mp_cname;

#define DEFVAR(mp_cname, mp_name) \
    extern Val VAR_##mp_cname;

#define DEFTLV(mp_cname, mp_name, mp_init) \
    extern Int TLV_##mp_cname;

#include "./bt_object.inc"


extern Val QQchar_min;
extern Val QQself_file_info;

extern Val PACKAGE_target;

extern Val ty_complex_single_float;
extern Val ty_complex_double_float;

} // Image

using namespace Image;

#endif //!defined(INCLUDE_boot_objet_h)
