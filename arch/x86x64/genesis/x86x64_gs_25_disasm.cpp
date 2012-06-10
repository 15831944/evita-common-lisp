#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 25 Environment - Disassembler
// arch/x64/x64_gs_25_disasm.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/genesis/x86x64_gs_25_disasm.cpp#6 $
//
#include "../../../genesis/gs_lisp.h"
#include "../kernel/x86x64_ke_layout.h"
#include "./x86x64_gs_25_disasm.h"

namespace Genesis
{

//////////////////////////////////////////////////////////////////////
//
// get-function fspec
//
// Description:
//  Returns function object specified by fspec.
//
Val get_function(Val fspec)
{
    class Parser
    {
        Val m_visits;

        public: Val Run(Val fspec)
        {
            m_visits = nil;

            if (functionp(fspec))
            {
                return fspec;
            }

            if (symbolp(fspec))
            {
                return symbol_function(fspec);
            }

            if (! consp(fspec))
            {
                error(L"Invalid fspec: ~S", fspec);
            }

            if (car(fspec) == Qlabels || car(fspec) == Qflet)
            {
                Val fn = get_toplevel_function(second(fspec));
                return parse_labels(fn, fspec);
            } // labels

            if (car(fspec) == Kinternal)
            {
                Val fn = get_toplevel_function(second(fspec));
                return parse_internal(fn, fspec, cddr(fspec));
            } // internal

            return get_toplevel_function(fspec);
        } // Run

        static Val get_toplevel_function(Val fname)
        {
            if (symbolp(fname))
            {
                return symbol_function(fname);
            }

            if (safe_list_length(fname) != Fixnum::Encode(2))
            {
                invalid_fspec(fname);
            }

            if (car(fname) == Qsetf)
            {
                return fdefinition(fname);
            }

            if (car(fname) == Qmacro_function)
            {
                return macro_function(second(fname));
            }

            // compiler-macro-function
            // method

            invalid_fspec(fname);
        } // get_top_level_function

        // invalid_fspec
        static void __declspec(noreturn) invalid_fspec(Val fname)
        {
            error(L"Invalid function name: ~S", fname);
        } // invalid_fspec

        // parse_internal
        Val parse_internal(Val fn, Val fspec, Val runner)
        {
            if (nil == runner)
            {
                return fn;
            }

            Val nth = car(runner);
            runner = cdr(runner);

            if (! fixnump(nth))
            {
                return nth;
            }

            FunObj* pFunObj = fn->Decode<FunObj>();

            foreach (FunObj::EnumAnnon, oEnum, pFunObj)
            {
                Val inner;
                {
                    FunObj::Annon oAnnon = oEnum.Get();
                    switch (oAnnon.m_eType)
                    {
                    case FunObj::Annon::Type_LispVal:
                        inner = pFunObj->FetchVal(oAnnon.m_ofs);
                        break;

                    case FunObj::Annon::Type_LocalCallee:
                        inner = pFunObj->FetchCallee(oAnnon.m_ofs);
                        break;

                    default:
                        inner = nil;
                        break;
                    }
                } // inner

                if (functionp(inner))
                {
                    if (nth == Fixnum::Encode(0))
                    {
                        return parse_internal(inner, fspec, runner);
                    }

                    nth = sub_xx(nth, 1);
                } // if inner
            } // for each annon

            return nil;
        } // parse_internal

        // parse_labels
        Val parse_labels(Val fn, Val fspec)
        {
            if (nil != memq(fn, m_visits))
            {
                return nil;
            }

            m_visits = cons(fn, m_visits);

            FunObj* pFunObj = fn->Decode<FunObj>();

            foreach (FunObj::EnumAnnon, oEnum, pFunObj)
            {
                Val inner;
                {
                    FunObj::Annon oAnnon = oEnum.Get();
                    switch (oAnnon.m_eType)
                    {
                    case FunObj::Annon::Type_LispVal:
                        inner = pFunObj->FetchVal(oAnnon.m_ofs);
                        break;

                    case FunObj::Annon::Type_LocalCallee:
                        inner = pFunObj->FetchCallee(oAnnon.m_ofs);
                        break;

                    default:
                        inner = nil;
                        break;
                    }
                } // inner

                unless (functionp(inner)) continue;

                if (equal(inner->Decode<FunObj>()->m_name, fspec))
                {
                    return inner;
                }

                Val found = parse_labels(inner, fspec);
                if (nil != found)
                {
                    return found;
                }
            } // for each annon

            return nil;
        } // parse_labels
    }; // Parser

    Parser oParser;
    return oParser.Run(fspec);
} // get_function


// OpcodeTable::Get
const Format* OpcodeTable::Get(uint nOpcode)
{
    if (0 == m_cEntries)
    {
        populate();
    }

    int iStart = nOpcode % lengthof(m_rgpVector);
    int iRunner = iStart;
    for (;;)
    {
        const Format* pFormat = m_rgpVector[iRunner];

        if (NULL == pFormat)
        {
            return NULL;
        }

        if (pFormat->m_nOpcode == nOpcode)
        {
            return pFormat;
        }

        iRunner += 1;

        if (iRunner >= lengthof(m_rgpVector))
        {
            iRunner = 0;
        }

        ASSERT(iRunner != iStart);
    } // for
} // OpcodeTable::Get


// OpcodeTable::populate
void OpcodeTable::populate()
{
    static Format const k_rgoFormat[] =
    {
        #define DEFFORMAT_0(mp_opcode, mp_mnemonic) \
            { \
                L ## #mp_mnemonic, \
                mp_opcode, \
                0, \
                Format::Extend_None, \
                Format::Operand_None, \
                Format::Operand_None, \
                Format::Operand_None \
            },

        #define DEFFORMAT_1(mp_opcode, mp_mnemonic, mp_1) \
            { \
                L ## #mp_mnemonic, \
                mp_opcode, \
                1, \
                Format::Extend_None, \
                Format::Operand_ ## mp_1, \
                Format::Operand_None, \
                Format::Operand_None \
            },

        #define DEFFORMAT_2(mp_opcode, mp_mnemonic, mp_1, mp_2) \
            { \
                L ## #mp_mnemonic, \
                mp_opcode, \
                2, \
                Format::Extend_None, \
                Format::Operand_ ## mp_1, \
                Format::Operand_ ## mp_2, \
                Format::Operand_None \
            },

        #define DEFFORMAT_3(mp_opcode, mp_mnemonic, mp_1, mp_2, mp_3) \
            { \
                L ## #mp_mnemonic, \
                mp_opcode, \
                3, \
                Format::Extend_None, \
                Format::Operand_ ## mp_1, \
                Format::Operand_ ## mp_2, \
                Format::Operand_ ## mp_3 \
            },

        #define DEFFORMAT_EXT_1(mp_opcode, mp_opext, mp_mnemonic, mp_1) \
            { \
                L ## #mp_mnemonic, \
                (mp_opcode << 8) | mp_opext, \
                1, \
                Format::Extend_None, \
                Format::Operand_ ## mp_1, \
                Format::Operand_None, \
                Format::Operand_None \
            },

        #define DEFFORMAT_EXT_2(mp_opcode, mp_opext, mp_mnemonic, \
                                    mp_1, mp_2 ) \
            { \
                L ## #mp_mnemonic, \
                (mp_opcode << 8) | mp_opext, \
                2, \
                Format::Extend_None, \
                Format::Operand_ ## mp_1, \
                Format::Operand_ ## mp_2, \
                Format::Operand_None \
            },

        #define DEFFORMAT_X(mp_opcode, mp_extend) \
            { \
                NULL, \
                mp_opcode, \
                0, \
                Format::Extend_ ## mp_extend, \
                Format::Operand_None, \
                Format::Operand_None, \
                Format::Operand_None \
            },

        #include "../x86x64_opcode.inc"
    }; // k_rgoFormat

    for (
        const Format* p = k_rgoFormat;
        p < &k_rgoFormat[lengthof(k_rgoFormat)];
        p++ )
    {
        put(p);
    } // for
} // OpcodeTable::populate

// OpcodeTable::put
void OpcodeTable::put(const Format* p)
{
    int iStart = p->m_nOpcode % lengthof(m_rgpVector);
    int iRunner = iStart;
    for (;;)
    {
        if (NULL == m_rgpVector[iRunner])
        {
            m_rgpVector[iRunner] = p;
            m_cEntries += 1;
            return;
        }

        iRunner += 1;

        if (iRunner >= lengthof(m_rgpVector))
        {
            iRunner = 0;
        }

        ASSERT(iRunner != iStart);
    } // for
} // OpcodeTable::put


OpcodeTable X86X64Disassembler::sm_oOpcodeTable;

Format X86X64Disassembler::sm_oFormat_DB =
{
    L"DB",
    0,      // opcode
    1,      // #operands
    Format::Extend_None,
    { Format::Operand_Ib }
}; // sm_oFormat_DB

Format X86X64Disassembler::sm_oFormat_DD =
{
    L"DD",
    0,      // opcode
    1,      // #operands
    Format::Extend_None,
    { Format::Operand_Iz }  // operands
}; // sm_oFormat_DB


// ensure_modrm
uint
X86X64Disassembler::ensure_modrm(Context* pContext)
{
    if (! hasModRm())
    {
        m_nModRm = pContext->ReadU8();
    }
    return m_nModRm;
} // ensure_modrm


// get_format
const Format*
X86X64Disassembler::get_format(Context* pContext, uint nOpcode)
{
    const Format* pFormat = sm_oOpcodeTable.Get(nOpcode);
    if (NULL == pFormat)
    {
        return &sm_oFormat_DB;
    }

    switch (pFormat->m_eExtend)
    {
    case Format::Extend_TwoByte:
        nOpcode <<= 8;
        nOpcode |= pContext->ReadU8();
        return get_format(pContext, nOpcode);

    case Format::Extend_ModRm:
        nOpcode <<= 8;
        nOpcode |= ldb(3, 3, ensure_modrm(pContext));
        return get_format(pContext, nOpcode);

    default:
        return pFormat;
    } // switch
} // X86X64Disassembler::get_format


// Run - entry point
void X86X64Disassembler::Run()
{
    start();

    Context oContext(m_fun);

    setup_labels(&oContext);

    do
    {
        uint ofsInsn = oContext.GetOffset();

        disasm_gcmap(ofsInsn);

        Val labeled = get_labeled(&oContext);

        const Format* pFormat = decode_insn(&oContext);

        Val operands = disasm_operands(&oContext, pFormat);

        uint ofsNext = oContext.GetOffset();

        format(m_stream, L"; ~C~4,'0X", labeled, Fixnum::Encode(ofsInsn));

        uint ofs;
        for (ofs = ofsInsn; ofs < ofsInsn + 6; ofs++)
        {
            if (ofs >= ofsNext)
            {
                write_string(L"   ", m_stream);
            }
            else
            {
                format(m_stream, L" ~2,'0X",
                    Fixnum::Encode(oContext.FetchU8(ofs)) );
            }
        } // for

        StackString_<16> oMnemonic;
            oMnemonic.Init(pFormat->m_pwszMnemonic);

        format(m_stream, L"  ~A", oMnemonic.Encode());

        for (
            int iLen = ::lstrlenW(pFormat->m_pwszMnemonic);
            iLen < 10;
            iLen += 1)
        {
            write_char(' ', m_stream);
        } // for

        LPCWSTR pwszComma = L"";
        for (Val runner = operands; ! endp(runner); runner = cdr(runner))
        {
            write_string(pwszComma, m_stream);

            Val operand = car(runner);
            if (consp(operand) && Qquote == car(operand))
            {
                format(m_stream, L"'~S", cadr(operand));
            }
            else
            {
                print_object(operand, m_stream);
            }
            pwszComma = L", ";
        } // dolist

        write_char(0x0A, m_stream);

        if (ofs < ofsNext)
        {
            //             ;  1234
            write_string(L";      ");
            while (ofs < ofsNext)
            {
                format(m_stream, L" ~2,'0X",
                    Fixnum::Encode(oContext.FetchU8(ofs)) );
                ofs += 1;
            } // while

            write_char(0x0A, m_stream);
        } // if
    } while (! oContext.AtEnd());

    format(m_stream, L";~%");

    if (nil != m_cvars)
    {
        format(m_stream, L"; Closed variables:~%");
        foreach (EnumList, oEnum, m_cvars)
        {
            Val cell = oEnum.Get();
            format(m_stream, L";  ~S: ~S~%",
                cell,
                closed_cell_value(cell) );
        } // for
        format(m_stream, L";~%");
    } // if
} // X86X64Disassembler::Run


// X86X64Disassembler constructor
X86X64Disassembler::X86X64Disassembler(Val thing)
{
    m_fun = get_function(thing);

    if (! functionp(m_fun))
    {
        error(make_type_error(
            thing,
            list(Qor, Qfunction, Qfunction_name) ) );
    }

    m_stream  = TLV(Astandard_outputA);

    m_pFunObj = m_fun->Decode<FunObj>();

    m_oGcMap.Init(m_pFunObj->GetGcMap(), m_pFunObj->GetCodeSize());
    m_oGcMapEnum.Init(&m_oGcMap);
} // X86X64Disassembler::X86X64Disassembler


// X86X64Disassembler::start
void X86X64Disassembler::start()
{
    FunObj::Desc* pDesc = m_pFunObj->GetDesc();

    format(m_stream, L"; ~S~%", m_fun);

    format(m_stream, L";  code size = #x~X (~D)~%",
        Fixnum::Encode(pDesc->m_cbCodeVec),
        Fixnum::Encode(pDesc->m_cbCodeVec) );

    format(m_stream, L";  frame = #x~X ~A size=~D~%",
        Fixnum::Encode(pDesc->m_nFrame),
        m_pFunObj->GetFrameType() == FunObj::FrameType_Fixed ?
            Q(":FIXED") :
        m_pFunObj->GetFrameType() == FunObj::FrameType_Restify ?
            Q(":RESTIFY") :
            Q(":UNKNOWN"),
        Fixnum::Encode(m_pFunObj->GetFrameSize()) );

    format(m_stream, L";  annon = @ #x~X, size=~D~%",
        Fixnum::Encode(pDesc->m_ofsAnnon),
        Fixnum::Encode(m_pFunObj->GetAnnonSize()) );

    format(m_stream, L";  gcmap = @ #x~X, size=~D~%",
        Fixnum::Encode(pDesc->m_ofsGcMap),
        Fixnum::Encode(m_pFunObj->GetGcMapSize()) );

    format(m_stream, L";~%");
} // X86X64Disassembler::start

} // Genesis
