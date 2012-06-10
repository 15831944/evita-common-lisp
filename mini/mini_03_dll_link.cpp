#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - Dll Link
// genesis/gs_03_dll_link.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_03_dll_link.cpp#3 $
//
#include "./mini_lisp.h"

#include "../kernel/ke_dll_link.h"

namespace Kernel
{

// DllLinkArea::AddEntry
Val DllLinkArea::AddEntry(Val procinfo)
{
    assert_latch_locked(VAR(Adll_link_latchA));

    DllLinkArea* pArea = VAR(Adll_link_areaA)->
        StaticCast<DllLinkArea>();

    for (;;)
    {
        if (pArea->m_ofsFree + sizeof(DllEntry) < pArea->m_cbArea)
        {
            DllEntry* pEntry = reinterpret_cast<DllEntry*>(
                pArea->ToInt() + pArea->m_ofsFree );

            pEntry->m_proc_info = procinfo;
            pEntry->m_pfn = DllLinkStab;

            pArea->m_ofsFree += sizeof(DllEntry);

            return Fixnum::Encode(pEntry);
        } // if

        DllLinkArea* pPrev = pArea;

        pArea = Memory::AllocDataArea(
            Kernel::Thread::Get(),
            Area::ScanType_DllLink,
            sizeof(DllEntry) )->StaticCast<DllLinkArea>();

        pArea->m_pPrev = pPrev;

        VAR(Adll_link_areaA) = Fixnum::Encode(pArea);
    } // for
} // DllLinkArea::AddEntry

} // Kernel

namespace
{

// make_dll_file_info
Val make_dll_file_info(Val filename)
{
    assert_latch_locked(VAR(Adll_link_latchA));

    Val fileinfo = MiniThread::Get()->AllocRecord(CLASSD_dll_file_info);
    DllFileInfo* pFileInfo = fileinfo->Decode<DllFileInfo>();
        pFileInfo->m_handle     = Fixnum::Encode(0);
        pFileInfo->m_filename   = filename;
        pFileInfo->m_proc_table = make_hash_table(Qequal);
    return fileinfo;
} // make_dll_file_info


// /Fo $(OutDir)\$(InputName).obj 
Val make_dll_proc_info(Val fileinfo, Val procname)
{
    assert_latch_locked(VAR(Adll_link_latchA));

    Val procinfo = MiniThread::Get()->AllocRecord(CLASSD_dll_proc_info);
    DllProcInfo* pProcInfo = procinfo->Decode<DllProcInfo>();
        pProcInfo->m_file_info = fileinfo;
        pProcInfo->m_proc_name = procname;

        pProcInfo->m_entry = DllLinkArea::AddEntry(procinfo);

    return procinfo;
} // make_dll_proc_info

} // namespace

namespace MiniLisp
{
// intern_dll_entry
Val intern_dll_entry(Val filename, Val procname)
{
    Val fileinfo;
    {
        with_exclusive_latch(VAR(Adll_link_latchA));

        Val htb = VAR(Adll_file_tableA);

        fileinfo = gethash(filename, htb);
        if (nil == fileinfo)
        {
            fileinfo = make_dll_file_info(filename);
            setf_gethash(fileinfo, filename, htb);
        }
    } // fileinfo

    Val procinfo;
    {
        with_exclusive_latch(VAR(Adll_link_latchA));

        Val htb = fileinfo->Decode<DllFileInfo>()->m_proc_table;

        procinfo = gethash(procname, htb);
        if (nil == procinfo)
        {
            procinfo = make_dll_proc_info(fileinfo, procname);
            setf_gethash(procinfo, procname, htb);
        }
    } // procinfo

    return procinfo->Decode<DllProcInfo>()->m_entry;
} // intern_dll_entry

} // MiniLisp
