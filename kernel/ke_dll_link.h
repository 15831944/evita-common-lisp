//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - pre-compiled header
// kernel_thread.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_dll_link.h#2 $
//
#if !defined(INCLUDE_kernel_dll_link_h)
#define INCLUDE_kernel_dll_link_h

#include "./ke_layout.h"
#include "./ke_memory.h"

namespace Kernel
{

struct DllFileInfo;
struct DllProcInfo;
struct DllEntry;

//////////////////////////////////////////////////////////////////////
//
// DllEntry
//
struct DllEntry
{
    void*   m_pfn;
    Val     m_proc_info;
}; // DllEntry


//////////////////////////////////////////////////////////////////////
//
// DllProcInfo
//
struct DllProcInfo : 
    public Record_<Layout::C_dll_proc_info>
{
    // Val  m_classd;       // [0]
    //Val m_file_info;        // [1]
    //Val m_proc_name;        // [2]
    //Val m_entry;            // [3]
}; // DllProcInfo


//////////////////////////////////////////////////////////////////////
//
// DllLibraryInfo
//
struct DllFileInfo : 
    public Record_<Layout::C_dll_file_info>
{
    // Val  m_classd;       // [0]
    //Val m_handle;           // [1]
    //Val m_filename;         // [2]
    //Val m_proc_table;       // [3] Mapping procedure name to DllProcInfo
}; // DllFileInfo


//////////////////////////////////////////////////////////////////////
//
// Dll Link Error
//
struct DllLinkError : 
    public Instance_<Layout::C_dll_link_error>
{
    //Val m_arguments;
    //Val m_filename;
    //Val m_name;
    //Val m_code;
}; // DllLinkError


//////////////////////////////////////////////////////////////////////
//
// DllLink Area
//
class DllLinkArea : public Area
{
    public: static ScanType getType_() { return ScanType_DllLink; }

    public: static Val  AddEntry(Val);
    public: void Reinitialize();

    public: class EnumEntry
    {
        protected: DllEntry* m_pRunner;
        protected: DllEntry* m_pEnd;

        public: EnumEntry(DllLinkArea* p) :
            m_pRunner(p->GetTop<DllEntry>()),
            m_pEnd(p->GetFree<DllEntry>()) {}

        public: bool AtEnd() const { return m_pRunner >= m_pEnd; }
        public: DllEntry* Get() const { ASSERT(! AtEnd()); return m_pRunner; }
        public: void Next() { ASSERT(! AtEnd()); m_pRunner++; }
    }; // EnumEntry
}; // DllLinkArea


void DllRestart();

// DllLinkStab is defined in arch/[arch]/[arch]_ke_mach.cpp
extern "C" Val __fastcall   DllLinkStab();

// DllResolve
//  Resolve DLL link. This function is called by DllLinkStab.
extern "C" void* __fastcall DllResolve(Thread*, DllEntry*);


extern HMODULE g_hSelf;
 
} // Kernel

#endif //!defined(INCLUDE_kernel_dll_link_h)
