#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - x64 - expand TYPEP.
// cg/x64/x64_cg_typep.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/compiler/x64_cg_typep.cpp#10 $
//
//
// BUGBUG: We should expand OPENxxx and CLOSE in X64TypepPass instead of
// assembler. Post RA is another place for expanding OPENxxx and CLOSE.
//
#include "../../x86x64/compiler/x86x64_cg_typep.h"

namespace Compiler
{

using namespace X64;


//////////////////////////////////////////////////////////////////////
//
// Instruction X64TypepPass
//
class X64TypepPass : public X86X64TypepPass
{
    public: X64TypepPass() : X86X64TypepPass(L"X64-TYPEP") {}

    protected: virtual void process_instruction(Instruction* pInsn)
    {
        InsnProcT pfn = k_rgpInsnProc[pInsn->GetOpcode()];
        (this->*pfn)(pInsn);
    } // process_instruction

    typedef void (X64TypepPass::*InsnProcT)(Instruction*);
    static const InsnProcT k_rgpInsnProc[IrOp_MAX_1 + 1];
}; // X64TypepPass

const X64TypepPass::InsnProcT
X64TypepPass::k_rgpInsnProc[IrOp_MAX_1 + 1] =
{
    #define DEFIROP(mp_name) &X64TypepPass::process_##mp_name,
    #include "../../../compiler/ir/ir_opcode.inc"
}; // k_rgpInsnProc


//////////////////////////////////////////////////////////////////////
//
// X64TypepPass::process_TYPEP
//

static const TypepDesc
k_rgoTypepDesc[] =
{
    TYPEP_SUBMASK(record,       Record),
    TYPEP_SUBMASK(function,     Funcallable),
    TYPEP_SUBMASK(native_code_object, Funcallable),

    // 10 Symbols
    {
        Qsymbol,
        {
            { TypepCmd::Op_Eq, nil },
            { TypepCmd::Op_BranchTrue },
            { TypepCmd::Op_IsRecord },
            { TypepCmd::Op_BranchFalse},
            { TypepCmd::Op_EqClassD, CLASSD_symbol },
            { TypepCmd::Op_End }
        }
    }, // symbol

    // 11 Packages
    TYPEP_SINGLE(package),

    // 12 Numbers
    TYPEP_SUBMASK(fixnum, Fixnum),
    TYPEP_SINGLE(bignum),
    TYPEP_SINGLE(ratio),
    TYPEP_SINGLE(double_float),
    TYPEP_SINGLE(single_float),
    TYPEP_SINGLE(double_float_complex),
    TYPEP_SINGLE(single_float_complex),
    TYPEP_SINGLE(rational_complex),

    TYPEP_RANGE(complex),
    TYPEP_OR(float,   float_min,   float_max),

    {
        Qinteger,
        {
            { TypepCmd::Op_EqTag, TYPEP_TAG_OPERAND(Fixnum) },
            { TypepCmd::Op_BranchTrue },

            { TypepCmd::Op_IsRecord },
            { TypepCmd::Op_BranchFalse},

            { TypepCmd::Op_EqClassD, CLASSD_bignum },

            { TypepCmd::Op_End },
        }
    }, // integer

    {
        Qnumber,
        {
            { TypepCmd::Op_EqTag, TYPEP_TAG_OPERAND(Fixnum) },
            { TypepCmd::Op_BranchTrue },

            { TypepCmd::Op_IsRecord },
            { TypepCmd::Op_BranchFalse},

            { TypepCmd::Op_GeClassD, CLASSD_number_min },
            { TypepCmd::Op_BranchFalse },

            { TypepCmd::Op_LeClassD, CLASSD_number_max} ,

            { TypepCmd::Op_End },
        }
    }, // number

    {
        Qrational,
        {
            { TypepCmd::Op_EqTag, TYPEP_TAG_OPERAND(Fixnum) },
            { TypepCmd::Op_BranchTrue },

            { TypepCmd::Op_IsRecord },
            { TypepCmd::Op_BranchFalse },

            { TypepCmd::Op_EqClassD, CLASSD_bignum },
            { TypepCmd::Op_BranchTrue },

            { TypepCmd::Op_EqClassD, CLASSD_ratio},

            { TypepCmd::Op_End },
        }
    }, // rational

    {
        Qreal,
        {
            { TypepCmd::Op_EqTag, TYPEP_TAG_OPERAND(Fixnum) },
            { TypepCmd::Op_BranchTrue },

            { TypepCmd::Op_IsRecord },
            { TypepCmd::Op_BranchFalse},

            { TypepCmd::Op_GeClassD, CLASSD_real_min },
            { TypepCmd::Op_BranchFalse },

            { TypepCmd::Op_LeClassD, CLASSD_real_max },

            { TypepCmd::Op_End },
        }
    }, // real

    // 13 Character
    TYPEP_SINGLE(character),

    // 14 Conses
    TYPEP_SUBMASK(cons, Cons),
    TYPEP_SUBMASK(list, List),

    {
        Qnull,
        {
            { TypepCmd::Op_Eq, nil },
            { TypepCmd::Op_End }
        }
    }, // null

    ////////////////////////////////////////////////////////////
    //
    // 15 Arrays
    //

    // int{8, 16, 32, 64}
    TYPEP_SINGLE(signed_byte_8_vector),
    TYPEP_SINGLE(signed_byte_16_vector),
    TYPEP_SINGLE(signed_byte_32_vector),
    TYPEP_SINGLE(signed_byte_64_vector),

    // uint{8, 16, 32, 64}
    TYPEP_SINGLE(unsigned_byte_8_vector),
    TYPEP_SINGLE(unsigned_byte_16_vector),
    TYPEP_SINGLE(unsigned_byte_32_vector),
    TYPEP_SINGLE(unsigned_byte_64_vector),

    // float32
    TYPEP_SINGLE(single_float_vector),
    TYPEP_SINGLE(single_float_complex_vector),

    // float64
    TYPEP_SINGLE(double_float_vector),
    TYPEP_SINGLE(double_float_complex_vector),

    // bit-vector and bit-array
    TYPEP_SINGLE(simple_bit_vector),
    TYPEP_SINGLE(bit_array_object),
    TYPEP_SINGLE(bit_vector_object),
    TYPEP_OR(bit_vector, simple_bit_vector, bit_vector_object),

    // general array
    TYPEP_SINGLE(simple_vector),
    TYPEP_SINGLE(general_vector_object),
    TYPEP_SINGLE(general_array_object),

    // abstract arrays and vectors
    TYPEP_RANGE(array),
    TYPEP_RANGE(array_object),
    TYPEP_RANGE(data_vector),
    TYPEP_RANGE(simple_array),
    TYPEP_RANGE(vector),
    TYPEP_RANGE(vector_object),

    // 16 Strings
    TYPEP_SINGLE(simple_string),
    TYPEP_OR(string, simple_string, string_object),

    // 18 Hash Tables
    TYPEP_SINGLE(hash_table),

    // 22 Printer
    // pprint-dispatch-table

    // 23 Readers
    TYPEP_SINGLE(readtable),

    // 49 Internal
    TYPEP_SINGLE(environment),
    TYPEP_SINGLE(setf_cell),
    TYPEP_SINGLE(tlv_record),
    TYPEP_SINGLE(value_cell),

    // 50 Extensions
    TYPEP_SINGLE(latch),
    TYPEP_SINGLE(mutex),

    TYPEP_SINGLE(finalization),
    TYPEP_SINGLE(weak_pointer),
}; // k_rgoTypeDesc


//////////////////////////////////////////////////////////////////////
//
// Get Type Description from TYPEP instruction.
//
const TypepCmd*
X86X64TypepPass::get_typep_desc(Instruction* pTypep)
{
    Val name = pTypep->GetLy();
        if (classp(name)) name = name->Decode<Class>()->m_name;

    for (
        const TypepDesc* pRunner = &k_rgoTypepDesc[0];
        pRunner < &k_rgoTypepDesc[lengthof(k_rgoTypepDesc) - 1];
        pRunner++ )
    {
        if (pRunner->m_name == name)
        {
            return pRunner->m_rgoCmd;
        }
    } // for each entry

    // There is no description in table. We use general method.
    return NULL;
} // X86X64TypepPass::get_typep_desc


//////////////////////////////////////////////////////////////////////
//
// Code Generator Entry Point
//
void
x64_pass_expand_typep()
{
    X64TypepPass oPass;
    oPass.Run();
} // x64_pass_typeping

} // Compiler
