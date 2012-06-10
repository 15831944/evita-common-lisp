//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - pre-compiled header
// kernel_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_util.h#2 $
//
#if !defined(INCLUDE_kernel_util_h)
#define INCLUDE_kernel_util_h

//////////////////////////////////////////////////////////////////////
//
// Swap
//
template<typename T> void swap(T& rX, T& rY)
{
    T oTemp = rX;
    rX = rY;
    rY = oTemp;
} // swap


//////////////////////////////////////////////////////////////////////
//
// Pointer Compatible Integer
//
template<typename T> __forceinline T* FromInt(Int iValue)
{
    return reinterpret_cast<T*>(iValue);
} // FromInt


//////////////////////////////////////////////////////////////////////
//
// Length of C-array
#define lengthof(mp_array) \
    ( sizeof(mp_array) / sizeof(*mp_array) )


__forceinline size_t CEILING(size_t m, size_t n) { return (m + n - 1) / n; }
__forceinline size_t ROUNDUP(size_t m, size_t n) { return CEILING(m, n) * n; }

//////////////////////////////////////////////////////////////////////
//
// Strings
//
LPCWSTR lstrchrW(LPCWSTR, wchar_t);

#endif //!defined(INCLUDE_kernel_util_h)
