//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - pre-compiled header
// precomp.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/boot/precomp.h 13 2006-07-29 01:55:00 yosi $
//
#if !defined(INCLUDE_boot_precomp_h)
#define INCLUDE_boot_precomp_h

#define EVCL_BOOT 1

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

// Requires at least Windows XP
#define _WIN32_WINNT    0x501

#define STRICT
#define INC_OLE2
#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include <stddef.h> // ptrdiff_t
#include "./bt_defs.h"

#pragma warning(disable: 4291)

// warning C4291: 'void *Kernel::Area::operator new(size_t,void *)' :
// no matching operator delete found; memory will not be freed if
// initialization throws an exception

#define evcl_memcmp     memcmp
#define evcl_memcpy     memcpy
#define evcl_memmove    memmove
#define evcl_memset     memset

#endif //!defined(INCLUDE_boot_precomp_h)
