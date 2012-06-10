//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Definitions for Lisp Kernel
// kernel/ke_defs.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_defs.h#6 $
//
#if !defined(INCLUDE_kernel_ke_defs_h)
#define INCLUDE_kernel_ke_defs_h

#define BIG_ENDIAN      0x42
#define LITTLE_ENDIAN   0x4C

#include "./ke_config.h"

#define nil Qnil
#define t   Qt

#define foreach(mp_enum, mp_var, mp_arg) \
    for (mp_enum mp_var(mp_arg); ! (mp_var).AtEnd(); (mp_var).Next())

#define unless(mp_bool) if (! (mp_bool) )
#define when(mp_bool)   if (mp_bool)

//////////////////////////////////////////////////////////////////////
//
// Pointer Compatible Integer
//
class AsInt
{
    public: void* operator new(size_t, void* pv) { return pv; }

    public: Int ToInt() const
    {
        return reinterpret_cast<Int>(this);
    } // ToInt
}; // AsInt


namespace Kernel
{
    class Thread;
    class Memory;
    class Value_;
    typedef Value_* Value;
    typedef Value Val;
    typedef Value_ Val_;
} // Kernel

#include "./ke_arch.h"
#include "./ke_debugger.h"
#include "./ke_float.h"

// Enum_
template<class Element_>
class Enum_
{
    private: Element_* m_pRunner;

    public: Enum_(Element_* p) : m_pRunner(p) {}

    public: bool AtEnd() const
        { return NULL == m_pRunner; }

    public: Element_* Get()  const
        { ASSERT(! AtEnd()); return m_pRunner; }

    public: void Next()
        { ASSERT(! AtEnd()); m_pRunner = m_pRunner->GetNext(); }
}; // Enum_

#endif //!defined(INCLUDE_kernel_ke_defs_h)
