#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - Image Writer
// boot/bt_writer.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id$
//
#include "../build.h"

#include "../mini/mini_lisp.h"
#include "../kernel/ke_executive.h"
#include "../kernel/ke_memory.h"

namespace Boot
{

// platform/{platform}/boot/{platform}_bt_stream.cpp
void save_image(const char16*);

namespace
{

using namespace MiniLisp;

static void generate_object_h(const char16*);

static Val begin_header(const char16*);
static Val encode_file_name(const char16*);
static Val encode_symbol(Val);
static void encode_symbol_aux(Val, Val);
static void end_header(Val, const char16*);

static void write_class(Val, Val);
static void write_classes(Val);

static void write_symbols(Val, Val);
static void write_packages(Val);

static void write_val(Val, const char16*, Val, Val);

static void write_setfs(Val);
static void write_vars(Val);


// defimage
void defimage(Val stream, const char16* pwszPrefix, Val cname, Val val)
{
    if (fixnump(val))
    {
        write_string(L"  #define ", stream);
        write_string(pwszPrefix, stream);
        format(stream, L"~A Fixnum::Encode(~Dll)~%", cname, val);
        return;
    }

    #ifdef HAS_BUG_64BIT_PTR_CONSTANT
    {
        write_string(L"  #define ", stream);
        write_string(pwszPrefix, stream);

        // Note:
        //  Since MSC14 produces 32bit value for reinterpret_cast, we use
        //  C-stype cast instead of reinterpret_cast.
        format(stream, L"~A k_(~X~X)~%",
            cname,
            Fixnum::Encode(val->ToInt() >> 4),
            Fixnum::Encode(val->ToInt() & 15) );
    }
    #else // HAS_BUG_64BIT_PTR_CONSTANT
    {
        write_string(L"defimage(", stream);
        write_string(pwszPrefix, stream);

        format(stream, L"~A, ~X~X)~%",
            cname,
            Fixnum::Encode(val->ToInt() >> 4),
            Fixnum::Encode(val->ToInt() & 15) );
    }
    #endif // HAS_BUG_64BIT_PTR_CONSTANT
} // defimage


// defimage
void defimage(Val stream, const char16* pwszPrefix, Val val)
{
    ASSERT(NULL != QQnull_string);
    defimage(stream, pwszPrefix, QQnull_string, val);
} // defimage


//////////////////////////////////////////////////////////////////////
//
// ObjectEntry
//
class ObjectEntry
{
    static ObjectEntry* sm_pLast;

    ObjectEntry*    m_pPrev;
    const char16*         m_pwszCName;
    Val             m_obj;

    public: ObjectEntry(const char16* pwszCName, Val obj) :
        m_pwszCName(pwszCName),
        m_obj(obj),
        m_pPrev(sm_pLast)
    {
        sm_pLast = this;
    } // ObjectEntry

    public: static void Write(Val stream)
    {
        format(stream, L"~2%// Objects~%");
        for (
            const ObjectEntry* pRunner = sm_pLast;
            NULL != pRunner;
            pRunner = pRunner->m_pPrev )
        {
            defimage(stream, pRunner->m_pwszCName, pRunner->m_obj);
        } // for runner
        format(stream, L"// End Objects~%");
    } // Write
}; // ObjectEntry

ObjectEntry* ObjectEntry::sm_pLast;

//////////////////////////////////////////////////////////////////////
//
// Generate img_object.h
//
void
generate_object_h(const char16* pwszFileName)
{
    Val stream = begin_header(pwszFileName);

    #ifdef HAS_BUG_64BIT_PTR_CONSTANT
        format(stream, L"#define k_(mp) ( (Val)(0x##mp##ull) )");
    #else // HAS_BUG_64BIT_PTR_CONSTANT
        format(stream,
            L"~%#define defimage(mp_name, mp_addr) \\~%"
            L"    Val const mp_name = reinterpret_cast<Val>(0x##mp_addr);~%" );
    #endif // HAS_BUG_64BIT_PTR_CONSTANT

    write_classes(stream);
    write_vars(stream);
    write_setfs(stream);
    write_packages(stream);

    ObjectEntry::Write(stream);

    #ifndef HAS_BUG_64BIT_PTR_CONSTANT
        format(stream, L"~%#undef defimage~%");
    #endif // HAS_BUG_64BIT_PTR_CONSTANT

    end_header(stream, pwszFileName);
} // generate_object_h


// create_header
Val begin_header(const char16* pwszFileName)
{
    Val filename = make_string(pwszFileName);

    format(Qt, L"Generating ~A~%", filename);

    Val stream = make_file_stream_char_out(filename);

    Val cname = encode_file_name(pwszFileName);

    format(stream, L"// ~A ~A~%",
        filename,
        make_string(MY_FileVersionW) );

    {
        SYSTEMTIME oNow;
            ::GetLocalTime(&oNow);
        format(stream,
            L"// Generated at ~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D.~%",
            Fixnum::Encode(oNow.wYear),
            Fixnum::Encode(oNow.wMonth),
            Fixnum::Encode(oNow.wDay),
            Fixnum::Encode(oNow.wHour),
            Fixnum::Encode(oNow.wMinute),
            Fixnum::Encode(oNow.wSecond) );
    }

    format(stream, L"#if !defined(~A)~%", cname);
    format(stream, L"#define ~A~%", cname);

    return stream;
} // create_header


// encode_file_name
Val encode_file_name(const char16* pwszFileName)
{
    Val stream = make_string_output_stream();
    write_string(L"INCLUDE_", stream);
    for (const char16* pwszRunner = pwszFileName; 0 != *pwszRunner; pwszRunner++)
    {
        Val ch = Character::Encode(*pwszRunner);
        if (Character::Encode('.') == ch ||
            Character::Encode('.') == ch )
        {
            ch = Character::Encode('_');
        }
        write_char(ch, stream);
    } // for each char
    return get_output_stream_string(stream);
} // encode_file_name


// encode_symbol
Val encode_symbol(Val symbol)
{
    Val stream = make_string_output_stream();

    Val package = symbol_package(symbol);
    if (PACKAGE_cl == package ||
        PACKAGE_si == package ||
        PACKAGE_ext == package ||
        PACKAGE_clos == package ||
        PACKAGE_keyword == package )
    {
        // no package name
    }
    else
    {
        encode_symbol_aux(stream, package_pretty_name(package));
        write_char('_', stream);
    }
    encode_symbol_aux(stream, symbol_name(symbol));
    return get_output_stream_string(stream);
} // encode_symbol


// encode_symbol_aux
void encode_symbol_aux(Val stream, Val str)
{
    static char k_wszMap[] =
        "________________"  // #x00
        "________________"  // #x10
      //  !"#$%&'()*+,-./
        "_____ZA___APC_DS"  // #x20
      // 0123456789:;<=>?
        "0123456789CSLEGQ"  // #x30
        "_abcdefghijklmno"
        "pqrstuvwxyz[\\]^_"
        "_abcdefghijklmno"
        "pqrstuvwxyz{|}~_";

    Val len = length(str);
    for (Val i = Fixnum::Encode(0); cmp_xx(i, len) < 0; i = add_xx(i, 1))
    {
        Int ch = Character::ToCode(schar(str, i));
        if (ch >= 0 && ch < lengthof(k_wszMap))
        {
            write_char(k_wszMap[ch], stream);
        }
        else
        {
            write_char('_', stream);
        }
    } // for
} // encode_symbol_aux


// end_header
void end_header(Val stream, const char16* pwszFileName)
{
    format(stream, L"~%#endif // ~A~%", encode_file_name(pwszFileName));
    close(stream);
} // end_stream

// write_class
//  Writes CLASS_xxx and CLASSD_xxx
void write_class(Val stream, Val klass)
{
    Class* pClass = klass->Decode<Class>();

    format(stream, L"// ~S~%", pClass->m_name);
    write_val(stream, L"CLASS_", encode_symbol(pClass->m_name), klass);
    write_val(stream, L"ty_", encode_symbol(pClass->m_name), pClass->m_name);

    Val classd = pClass->m_instanced;

    if (nil != classd)
    {
        write_val(stream, L"CLASSD_",
            encode_symbol(pClass->m_name), pClass->m_instanced );
    }
} // write_class


// write_classes
void write_classes(Val stream)
{
    Val classes =  VAR(Aruntime_environmentA)->
            Decode<Environment>()->m_classes;

    Val count  = hash_table_count(classes);

    format(Qt, L"  write ~D classes.~%", count);
    format(stream, L"~%// ~D classes~%", count);

    foreach (EnumHashTable, oEnum, classes)
    {
        Val klass = oEnum.GetVal();
        if (klass->Is<Instance>())
        {
            write_class(stream, klass);
        }
    } // for each entry
} // write_classes


// write_packages
void write_packages(Val stream)
{
    for (
        Val runner = value_cell_value(VAR_Aall_packagesA);
        ! endp(runner);
        runner = cdr(runner) )
    {
        Val package = car(runner);

        Package* pPackage = package->Decode<Package>();

        format(stream, L"~%// package ~A,", car(pPackage->m_names));

        format(stream, L" external=~D,",
            svref(pPackage->m_external_table, Fixnum::Encode(0) ) );

        format(stream, L" internal=~D, ",
            svref(pPackage->m_internal_table, Fixnum::Encode(0)) );

        format(stream, L" nicknames=~:S~%", cdr(pPackage->m_names));

        Val name;
        {
            Val sstream = make_string_output_stream();
            encode_symbol_aux(sstream, package_pretty_name(package));
            name = get_output_stream_string(sstream);
        }; // name;

        write_val(stream, L"PACKAGE_", name, package);

        if (PACKAGE_cl_user != package)
        {
            format(Qt, L"  write ~S~%", package);

            write_symbols(stream, pPackage->m_external_table);
            write_symbols(stream, pPackage->m_internal_table);
        }
    } // for each package
} // write_packages


// write_setf
void write_setfs(Val stream)
{
    Val table =  value_cell_value(VAR_Asetf_tableA);
    Val count  = hash_table_count(table);

    format(Qt, L"  write ~D setf's.~%", count);
    format(stream, L"~%// ~D setf's~%", count);

    foreach (EnumHashTable, oEnum, table)
    {
        Val name = oEnum.GetKey();
        Val cell = oEnum.GetVal();

        format(stream, L"// ~S~%", name);

        write_val(
            stream,
            L"SETF_",
            encode_symbol(value_cell_name(cell)),
            cell );
    } // for each entry
} // write_setfs


// Ignore symbol that name contains downcase character.
static bool need_write(Val symbol)
{
    for (
        const char16* pwsz = symbol_name(symbol)->
                        Decode<SimpleString>()->GetElements();
        0 != *pwsz;
        pwsz++ )
    {
        if (*pwsz >= 'a' && *pwsz <= 'z') return false;
    } // for name
    return true;
} // need_write


// write package aux
void write_symbols(Val stream, Val vector)
{
    foreach (PackageImpl::Enum, oEnum, vector)
    {
        Val symbol = oEnum.Get();

        if (! need_write(symbol)) continue;

        format(stream, L"// ~S ~S~%",
            car(symbol_package(symbol)->Decode<Package>()->m_names),
            symbol );

        if (PACKAGE_keyword == symbol_package(symbol))
        {
            write_val(stream, L"K", encode_symbol(symbol), symbol);
        }
        else
        {
            write_val(stream, L"Q", encode_symbol(symbol), symbol);
        }
    } // for each symbol
} // write_symbols


// write_val
// Note: We assume width of fixnum tag is less than four.
void write_val(Val stream, const char16* pwszPrefix, Val cname, Val val)
{
    defimage(stream, pwszPrefix, cname, val);
} // write_val


// write_vars
void write_vars(Val stream)
{
    Val table =  value_cell_value(VAR_Avalue_tableA);
    Val count  = hash_table_count(table);

    format(Qt, L"  write ~D variables.~%", count);
    format(stream, L"~%// ~D variables~%", count);

    foreach (EnumHashTable, oEnum, table)
    {
        Val name = oEnum.GetKey();
        Val cell = oEnum.GetVal();

        format(stream, L"// ~S~%", name);

        if (value_cell_p(cell))
        {
            write_val(
                stream,
                L"VAR_",
                encode_symbol(value_cell_name(cell)),
                cell );
        }
        else if (tlv_record_p(cell))
        {
            TlvRecord* pTlvRecord = cell->Decode<TlvRecord>();

            Int iIndex = Fixnum::Decode_(pTlvRecord->m_index);

            format(stream, L"  Int const TLV_~A = ~D;~%",
                encode_symbol(name),
                Fixnum::Encode(Thread::ToTlvOffset(iIndex)) );
        }
        else
        {
            error(L"Broken *value-table*");
        }
    } // for each entry
} // write_vars

} // namespace

//////////////////////////////////////////////////////////////////////
//
// defobject
Val defobject_(const char16* pwszCName, Val obj)
{
    new ObjectEntry(pwszCName, obj);
    return obj;
} // defobject

//////////////////////////////////////////////////////////////////////
//
// Write Boot Image
//
void Write(Thread* pThread)
{
    ASSERT(NULL != pThread);

    // finalize
    {
        foreach (Memory::EnumArea, oEnum, Memory::sm_rgpAreaByAge[0])
        {
            Area* pArea = oEnum.Get();
            pArea->m_nFlags &= ~Area::Flags_AgeMask;
            pArea->m_nFlags |= Area::Age_Static;
        } // for each area

        VAR(Aweak_areaA) = Fixnum::Encode(NULL);

        Memory::Verify();
    } // finalize

    Memory::AllocDataArea(
        pThread,
        Area::ScanType_HashTable,
        sizeof(Val),
        Area::Age_System );

    save_image(L"_boot.image");

    // finalize
    {
        foreach (Memory::EnumArea, oEnum, Memory::sm_rgpAreaByAge[0])
        {
            Area* pArea = oEnum.Get();
            pArea->m_nFlags &= ~Area::Flags_AgeMask;
        } // for each area
    } // finalize

    generate_object_h(L"_img_object.h");
} // Write

} // Boot
