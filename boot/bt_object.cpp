#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - thread
// bt_thread.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/boot/bt_object.cpp 25 2006-10-09 02:26:42 yosi $
//
// Description:
//  Defines objects that used by builder and writer.
//

namespace Image
{

//////////////////////////////////////////////////////////////////////
//
// Defines C-Variable for Lisp Object
//

#define DEFCLASS(mp_cname, mp_NAME, mp_super) \
    Val CLASS_##mp_cname; \
    Val CLASSD_##mp_cname; \
    Val Q##mp_cname; \
    Val ty_##mp_cname;

#include "../kernel/ke_layout.inc"


#define DEFKEY(mp_cname, mp_name) \
    Val K##mp_cname;

#define DEFPACKAGE(mp_cname, mp_name, mp_alias, mp_ext, mp_int, mp_uses) \
    Val PACKAGE_##mp_cname;

#define DEFOBJECT(mp_cname, mp_expr) \
    Val mp_cname;

#define DEFSYM(mp_pkg, mp_cname, mp_name) \
    Val Q##mp_cname;

#define DEFVAR(mp_cname, mp_name) \
    Val VAR_##mp_cname;

#define DEFTLV(mp_cname, mp_name, mp_init) \
    Int TLV_##mp_cname;

#include "./bt_object.inc"


Val QQchar_min;

Val QQself_file_info;

Val ty_complex_single_float;
Val ty_complex_double_float;

// Compilation target.
Val PACKAGE_target;

} // Image
