//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - pre-compiled header
// kernel/ke_sxhash.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_sxhash.h#2 $
//
#if !defined(INCLUDE_kernel_sxhash_h)
#define INCLUDE_kernel_sxhash_h

#include "./ke_layout.h"
#include "./ke_memory.h"

namespace Kernel
{

class Memory;
class Thread;

class ObjTab : public Area
{
    static ObjTab* sm_pObjTab;
    static Val     sm_random;

    public: struct Slot
    {
        Val m_obj;
        Val m_hash_code;
    }; // Slot

    public: static ObjTab* Get()          { return sm_pObjTab; }
    public: static ObjTab* Set(ObjTab* p) { return sm_pObjTab = p; }
    public: static void Init(ObjTab*);

    public: Slot* Intern(Thread*, Val);
    public: Slot* Intern(Val, Val);

    public: static void     Lock(Thread*);
    public: static ObjTab*  Rehash(Thread*, size_t);
    public: static void     Unlock();

    static Val random();
}; // ObjTab

} // Kernel

#endif //!defined(INCLUDE_kernel_sxhash_h)
