//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Memory Manager
// kernel/ke_weak.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_weak.h#3 $
//
#if !defined(INCLUDE_kernel_weak_h)
#define INCLUDE_kernel_weak_h

#include "./ke_layout.h"

namespace Kernel
{

// WeakVector
class WeakVector : public Kernel::SimpleVector
{
    public: WeakVector(Val length)
    {
        m_classd = CLASSD_simple_vector;
        m_length = length;
    } // WeakVector
}; // WeakVector

typedef WeakVector CallerSet;

// WeakPointer
class WeakPointer : public Record_<Layout::C_weak_pointer>
{
    // Val m_classd;    [0]
    // Val m_object;    [1]
    public: WeakPointer(Val x)
        { m_classd = CLASSD_weak_pointer; m_value = x;}
}; // WeakPointer


// WeakVectorLeader
class WeakVectorLeader : public Record_<Layout::C_weak_vector_leader>
{
    // Val  m_classd;   [0]
    // Val  m_kind;     [1]
    public: WeakVectorLeader(Val kind)
        { m_classd = CLASSD_weak_vector_leader; m_kind = kind;}
}; // WeakVectorLeader


class Weak
{
    public: static void* Alloc(size_t);
}; // Weak

} // Kernel

#endif //!defined(INCLUDE_kernel_weak_h)
