//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Configuration
// ke_config.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_config.h#3 $
//
#if !defined(INCLUDE_kernel_config_h)
#define INCLUDE_kernel_config_h


typedef char int8;
typedef unsigned char uint8;

typedef short int16;
typedef unsigned short uint16;

typedef int int32;
typedef unsigned int uint32;

typedef __int64 int64;
typedef unsigned __int64 uint64;

typedef unsigned int uint;

typedef wchar_t char16;

typedef float float32;
typedef double float64;

//////////////////////////////////////////////////////////////////////
//
// Pointer Compatible Integer
//
typedef INT_PTR  Int;
typedef UINT_PTR UInt;

#if ! defined(_MSC_VER)
    #define __declspec(x)
#endif  // defined(_MSC_VER)

#if ! defined(__GNUC__)
    #define __attribute__(x)
#endif // defined(__GNUC__)

#endif //!defined(INCLUDE_kernel_config_h)
