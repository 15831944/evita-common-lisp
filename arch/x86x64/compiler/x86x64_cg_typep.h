//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - x86x64 - Typep Pass
// arch/x86x64/compiler/x86x64_cg_typep.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_typep.h#7 $
//
#if !defined(INCLUDE_arch_x86x64_compiler_cg_typep_h)
#define INCLUDE_arch_x86x64_compiler_cg_typep_h

#include "../../../compiler/ir/ir_pass.h"

#include "./x86x64_cg_instruction.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Typep Description
//
struct TypepCmd
{
    enum Op
    {
        Op_End,         // terminator
        Op_Generic,     // terminator

        Op_Branch,
        Op_BranchFalse,
        Op_BranchTrue,

        Op_Eq,          // for null
        Op_EqTag,
        Op_EqClassD,
        Op_GeClassD,
        Op_IsRecord,
        Op_LeClassD,

        Op_FixnumRange,

        Op_Subclassp,
        Op_SlowSubclassp,
    }; // Op

    Op  m_eOpcode;
    Val m_operand;
}; // TypepCmd

struct TypepDesc
{
    Val         m_name;
    TypepCmd    m_rgoCmd[15];
}; // TypepDesc


//////////////////////////////////////////////////////////////////////
//
// X86X64TypepPass
//
class X86X64TypepPass : public FunctionPass
{
    protected: X86X64TypepPass(const char16* pwsz) :
        FunctionPass(pwsz) {}

    protected: virtual void process_function(Function*);

    protected: virtual void process_instruction(Instruction*) = 0;

    protected: static const TypepCmd* get_typep_desc(Instruction*);

    protected: void process_TYPEP(Instruction*);
}; // X86X64TypepPass

#define TYPEP_TAG_OPERAND(mp_name) \
    reinterpret_cast<Val>( ((mp_name::Tag)<<8) | (mp_name::TagMask) )


#define TYPEP_OR(mp_name, mp_1, mp_2) \
{ Q##mp_name, \
  { \
    { TypepCmd::Op_IsRecord }, \
    { TypepCmd::Op_BranchFalse }, \
    { TypepCmd::Op_EqClassD, CLASSD_##mp_1}, \
    { TypepCmd::Op_BranchTrue }, \
    { TypepCmd::Op_EqClassD, CLASSD_##mp_2}, \
    { TypepCmd::Op_End }, \
  } \
} // TYPEP_OR

#define TYPEP_RANGE(mp_name) \
{ Q##mp_name, \
  { \
    { TypepCmd::Op_IsRecord }, \
    { TypepCmd::Op_BranchFalse }, \
    { TypepCmd::Op_GeClassD, CLASSD_##mp_name##_min }, \
    { TypepCmd::Op_BranchFalse }, \
    { TypepCmd::Op_LeClassD, CLASSD_##mp_name##_max }, \
    { TypepCmd::Op_End }, \
  } \
} // TYPEP_RANGE

#define TYPEP_SINGLE(mp_name) \
{ Q##mp_name, \
  { \
    { TypepCmd::Op_IsRecord }, \
    { TypepCmd::Op_BranchFalse }, \
    { TypepCmd::Op_EqClassD, CLASSD_##mp_name }, \
    { TypepCmd::Op_End }, \
  } \
} // TYPEP_SINGLE

#define TYPEP_SUBMASK(mp_name, mp_class) \
{ Q##mp_name, \
  { \
    { TypepCmd::Op_EqTag, TYPEP_TAG_OPERAND(mp_class) }, \
    { TypepCmd::Op_End }, \
  } \
} // TYPEP_SUBMASK

} // Compiler

#endif //!defined(INCLUDE_arch_x86x64_compiler_cg_typep_h)
