#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 25 Environment - room
// genesis/gs_25_room.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_25_room.cpp#9 $
//
// Description:
//  This file contains C implementation of lisp functions.
//      room
//
#include "./gs_lisp.h"

#include "../kernel/ke_memory.h"

namespace Kernel
{
    extern size_t g_cbFree;
} // Kernel

namespace CommonLisp
{

namespace
{

// AreaStat
class AreaStat
{
    public: AreaStat() {}

    public: void Show()
    {
        char16 wsz[100];

        format(t, L"; Area:~%");
        foreach (Memory::EnumAreaAll, oEnum, Memory::GetStart())
        {
            Area* pArea = oEnum.Get();

            ::wsprintf(wsz,
                L";    ~X~X"
                L" ~4,'0X ~2D %8s ~10:D/~10:D~%%",
                pArea->GetTypeName() );

            format(t, wsz,
                Fixnum::Encode(pArea->ToInt() >> 4),
                Fixnum::Encode(pArea->ToInt() & 15),
                Fixnum::Encode(pArea->m_nFlags >> 16),
                Fixnum::Encode(pArea->m_nFlags & 0xffff),
                Fixnum::Encode(pArea->m_ofsFree),
                Fixnum::Encode(pArea->m_cbArea) );
        } // for each area

        format(t, L";~%");
    } // Show
}; // AreaStat


// ObjStat
//  Shows number of objects and total byte size of objects in lisp heap.
class ObjStat
{
    Val m_htb;

    public: ObjStat() :
        m_htb(make_hash_table())
    {
        foreach (Memory::EnumAreaAll, oEnum, Memory::GetStart())
        {
            Area* pArea = oEnum.Get();
            switch (pArea->GetType())
            {
            case Area::ScanType_Cons:
            {
                addObject(
                    Qcons,
                    pArea->m_ofsFree / sizeof(Cons),
                    pArea->m_ofsFree );
                break;
            } // cons

            case Area::ScanType_Function:
            case Area::ScanType_Record:
            case Area::ScanType_BinObj:
                foreach (Area::EnumObject, oEnum, pArea)
                {
                    Val obj = oEnum.Get();

                    // To check size of each function.
                    #if 0
                    {
                        if (functionp(obj))
                        {
                            format(t, L"; ~S ~:D~%",
                                obj,
                                Fixnum::Encode(obj->GetSize()) );
                        }
                    }
                    #endif

                    addObject(
                        class_of(obj)->Decode<Class>()->m_name,
                        1,
                        obj->GetSize() );
                } // for each object
                break;
            } // switch area type
        } // for each area
    } // Run

    void addObject(Val name, size_t c, size_t cb)
    {
        Val count_size = CommonLisp::gethash(name, m_htb);
        if (nil == count_size)
        {
            count_size = cons(Fixnum::Encode(0), Fixnum::Encode(0));
            setf_gethash(count_size, name, m_htb);
        }
        setf_car(add(car(count_size), c), count_size );
        setf_cdr(add(cdr(count_size), cb), count_size);
    } // add

    public: void Show()
    {
        format(t, L"; Objects:~%");
        size_t c = 0;
        size_t cb = 0;
        foreach (EnumHashTable, oEnum, m_htb)
        {
            Val name = oEnum.GetKey();
            Val count_size = oEnum.GetVal();
            format(t, L";  ~12:D  ~10:D    ~S~%",
                Fixnum::Encode(cdr(count_size)),
                Fixnum::Encode(car(count_size)),
                name );

            c  += Fixnum::Decode_(car(count_size));
            cb += Fixnum::Decode_(cdr(count_size));
        } // for each entry

        format(t, L";  ------------+-----------+----------------------~%");
        format(t, L";  ~12:D  ~10:D    Total~%;~%",
            Fixnum::Encode(cb),
            Fixnum::Encode(c) );
    } // Show
}; // ObjStat


// Age statistics
class AgeStat
{
    struct Stat
    {
        size_t  m_cbObUse;
        size_t  m_cbObAlloc;
        size_t  m_cbRsUse;
        size_t  m_cbRsAlloc;
    }; // Stat

    Stat rgoStat[Area::Age_Max + 1];

    public: AgeStat()
    {
        ::ZeroMemory(rgoStat, sizeof(rgoStat));

        foreach (Memory::EnumAreaAll, oEnum, Memory::GetStart())
        {
            Area* pArea = oEnum.Get();
                if (pArea->GetType() == Area::ScanType_None) continue;

            Stat* pStat = &rgoStat[pArea->GetAge()];
            if (pArea->GetType() == Area::ScanType_RS)
            {
                pStat->m_cbRsAlloc += pArea->m_cbArea;
                pStat->m_cbRsUse   += pArea->m_ofsFree;
            }
            else
            {
                pStat->m_cbObAlloc += pArea->m_cbArea;
                pStat->m_cbObUse   += pArea->m_ofsFree - sizeof(Area);
            }
        } // for each area
    } // AgeStat

    public: void Show()
    {
        //                | 0123456789AB/0123456789AB | 01234567/01234567
        format(t, L"; Age | Object                    | RS~%");
        format(t, L"; ====+===========================+===================~%");

        Stat oTotal;
            ::ZeroMemory(&oTotal, sizeof(oTotal));

        Stat* pStat = &rgoStat[0];
        for (uint nAge = 0; nAge <= Area::Age_Max; nAge++)
        {
            if (0 != pStat->m_cbObAlloc)
            {
                format(t, L";  ~2D | ~12:D/~12:D | ~8:D/~8:D~%",
                    Fixnum::Encode(nAge),
                    Fixnum::Encode(pStat->m_cbObUse),
                    Fixnum::Encode(pStat->m_cbObAlloc),
                    Fixnum::Encode(pStat->m_cbRsUse),
                    Fixnum::Encode(pStat->m_cbRsAlloc) );

                oTotal.m_cbObUse   += pStat->m_cbObUse;
                oTotal.m_cbObAlloc += pStat->m_cbObAlloc;
                oTotal.m_cbRsUse   += pStat->m_cbRsUse;
                oTotal.m_cbRsAlloc += pStat->m_cbRsAlloc;
            } // if

            pStat++;
        } // for n

        format(t, L"; ----+---------------------------+-------------------~%");
        format(t, L";       ~12:D/~12:D   ~8:D/~8:D~%",
            Fixnum::Encode(oTotal.m_cbObUse),
            Fixnum::Encode(oTotal.m_cbObAlloc),
            Fixnum::Encode(oTotal.m_cbRsUse),
            Fixnum::Encode(oTotal.m_cbRsAlloc) );

        format(t, L";~%");
    } // Show
}; // AreaStat


// FunStat
class FunStat
{
    Val m_callees;

    public: FunStat() :
        m_callees(nil)
    {
        with_shared_latch(VAR(Acaller_table_latchA));

        foreach (EnumHashTable, oEnum, VAR(Acaller_tableA))
        {
            Val fn_vec = oEnum.GetVal();
            Val fn = car(fn_vec);

            if (fn->Decode<Funcallable>()->m_classd ==
                    CLASSD_undefined_function_function )
            {
                Val callee_callers = list(oEnum.GetKey());
                Val last = callee_callers;

                push(callee_callers, m_callees);

                foreach (EnumVector, oEnum, cdr(fn_vec))
                {
                    Val caller = oEnum.Get();
                    if (functionp(caller))
                    {
                        last = setf_cdr(list(caller), last);
                    }
                } // for each elt
            }
        } // for each key
    } // FunStat

    public: void Show()
    {
        format(t, L"; Undefined Functions:~%");

        if (nil == m_callees)
        {
            format(t, L";  No undefined function.~%");
            return;
        }
        Val count = Fixnum::Encode(0);
        foreach (EnumList, oEnum, m_callees)
        {
            Val callee_callers = oEnum.Get();

            format(t, L";    ~S~%", car(callee_callers));

            count = add_xx(count, 1);

            foreach (EnumList, oEnum, cdr(callee_callers))
            {
                Val caller = oEnum.Get();

                // BUGBUG: GF doesn't have m_name.
                format(t, L";        ~S~%",
                    //caller
                    caller->Decode<NativeCodeFunction>()->m_name );
            } // for each caller
        } // for each callee
        format(t, L";  ~:D undefined functions.~%", count);
    } // Show
}; // FunStat

} // namespace

//extern "C" Val __declspec(dllexport) __fastcall room(Val)
Val room(Val detail)
{
    format(t, L"; Heap:  ~X~X...~X~X...~X~X ~:D/~:D KB~%;~%",
        Fixnum::Encode(reinterpret_cast<Int>(Memory::GetStart()) >> 4),
        Fixnum::Encode(reinterpret_cast<Int>(Memory::GetStart()) & 15),
        Fixnum::Encode(reinterpret_cast<Int>(Memory::GetCommit()) >> 4),
        Fixnum::Encode(reinterpret_cast<Int>(Memory::GetCommit()) & 15),
        Fixnum::Encode(reinterpret_cast<Int>(Memory::GetEnd()) >> 4),
        Fixnum::Encode(reinterpret_cast<Int>(Memory::GetEnd()) & 15),
        Fixnum::Encode(
            (Memory::GetCommit()->ToInt() - Memory::GetStart()->ToInt()) /
            1024 ),
        Fixnum::Encode(
            (Memory::GetEnd()->ToInt() - Memory::GetStart()->ToInt()) /
            1024 ) );

    ObjStat oObjStat;
    AgeStat oAgeStat;

    oAgeStat.Show();

    if (nil != detail) oObjStat.Show();

    if (t == detail)
    {
        AreaStat oAreaStat;
        oAreaStat.Show();
    }

    if (t == detail)
    {
        FunStat oFunStat;
        oFunStat.Show();
    }

    return nil;
} // room

} // CommonLisp

