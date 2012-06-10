//////////////////////////////////////////////////////////////////////////////
//
// evcl - console - pre-compiled header
// precomp.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/precomp.h#2 $
//
#if !defined(INCLUDE_kernel_precomp_h)
#define INCLUDE_kernel_precomp_h

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
#include "../mini/mini_defs.h"

#pragma warning(disable: 4291)

// warning C4291: 'void *Kernel::Area::operator new(size_t,void *)' :
// no matching operator delete found; memory will not be freed if
// initialization throws an exception

namespace Image
{
    using namespace Kernel;
    #include "_img_object.h"
} // Image

using namespace Image;

#endif //!defined(INCLUDE_kernel_precomp_h)
