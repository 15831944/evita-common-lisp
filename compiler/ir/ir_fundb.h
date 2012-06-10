//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR instruction
// compiler/ir/ir_instruction.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_fundb.h#2 $
//
#if !defined(INCLUDE_compiler_ir_fundb_h)
#define INCLUDE_compiler_ir_fundb_h

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// FunDb
//
template<class Entry>
class FunDb_
{
    
    struct Slot
    {
        const Entry*  m_pEntry;
    }; // Slot

    public: static const uint km_cEntries = lengthof(Entry::sm_rgoEntry);

    static FunDb_<Entry> sm_oFunDb;

    uint m_cEntries;
    Slot m_rgoSlot[km_cEntries * 400 / 300];

    public: static const Entry* Get(Val fname)
        { return sm_oFunDb.get(fname); }

    const Entry* get(Val fname)
    {
        if (0 == m_cEntries) init();

        uint nHash = hash(fname);
        const Slot* pTop = &m_rgoSlot[0];
        const Slot* pBtm = &m_rgoSlot[lengthof(m_rgoSlot)];
        const Slot* pStart = pTop + static_cast<uint>(nHash % (pBtm - pTop));
        const Slot* pRunner = pStart;
        do
        {
            if (pRunner->m_pEntry == NULL)  return NULL;
            if (pRunner->m_pEntry->m_fname == fname) return pRunner->m_pEntry;
            pRunner++;
            if (pRunner == pBtm) pRunner = pTop;
        } while (pRunner != pStart);
        CAN_NOT_HAPPEN();
    } // Get

    static uint hash(Val fname)
    {
        if (symbolp(fname))
        {
            return static_cast<uint>(Fixnum::Decode_(
                fname->Decode<Symbol>()->m_hash_code ) );
        }
        else if (setf_cell_p(fname))
        {
            return hash(fname->Decode<SetfCell>()->m_name) << 1;
        }
        else
        {
            // May be function object.
            return 0;
        }
    } // hash

    // init
    void init()
    {
        for (
            const Entry* pRunner = &Entry::sm_rgoEntry[0];
            pRunner < &Entry::sm_rgoEntry[lengthof(Entry::sm_rgoEntry)];
            pRunner++ )
        {
            put(pRunner);
        } // for each entry
    } // init

    void put(const Entry* pEntry)
    {
        uint nHash = hash(pEntry->m_fname);
        Slot* pTop = &m_rgoSlot[0];
        Slot* pBtm = &m_rgoSlot[lengthof(m_rgoSlot)];
        Slot* pStart = pTop + static_cast<uint>(nHash % (pBtm - pTop));
        Slot* pRunner = pStart;
        do
        {
            if (pRunner->m_pEntry == NULL)
            {
                pRunner->m_pEntry = pEntry;
                m_cEntries += 1;
                return;
            }
            pRunner++;
            if (pRunner == pBtm) pRunner = pTop;
        } while (pRunner != pStart);
        CAN_NOT_HAPPEN();
    } // put
}; // FunDb_

} // Compiler

#endif //!defined(INCLUDE_compiler_ir_fundb_h)
