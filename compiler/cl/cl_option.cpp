#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - 3 Evaluation and Compilation
// compiler/cl/cl_03_eval.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_option.cpp#7 $
//
//      cl:eval-when        NYI
//      cl:locally
//      cl:quote
//      cl:symbol-macrolet
//      cl:the
//
#include "./cl_defs.h"

namespace Compiler
{

bool
is_known_package(Val name)
{
    Val pkg = symbol_package(name);
    return PACKAGE_cl == pkg ||
           PACKAGE_clos == pkg ||
           PACKAGE_si == pkg ||
           PACKAGE_ext == pkg;
} // is_known_package

#define DEFINE_QUALITY(mp_Name) \
    static uint get##mp_Name() \
        { return static_cast<uint>( \
            Session::Get()->m_oOptimizeQualities.Get##mp_Name() ); }

//DEFINE_QUALITY(CompilationSpeed)
DEFINE_QUALITY(Debug)
DEFINE_QUALITY(Safety)
//DEFINE_QUALITY(Space)
DEFINE_QUALITY(Speed)


//////////////////////////////////////////////////////////////////////
//
// ClParser::option_check_parameter_count
//
bool ClParser::option_check_parameter_count(Function*)
{
    switch (getSafety())
    {
    case 3:
    case 2:
        return true;
    case 1:
    case 0:
        return false;
    default:
        CAN_NOT_HAPPEN();
    } // switch safety
} // ClParser::option_check_parameter_count


bool ClParser::option_check_index() { return getSafety() >= 1; }
bool ClParser::option_check_type()  { return getSafety() >= getSpeed(); }


// ClParser::option_check_unbound_variable
bool ClParser::option_check_unbound_variable(Val name)
{
    ASSERT(symbolp(name));

    switch (getSafety())
    {
    case 3: return true;
    case 2: return true;
    case 1: return ! is_known_package(name);
    case 0: return false;
    default: CAN_NOT_HAPPEN();
    } // switch safety
} // ClParser::option_check_unbound_variable

// ClParser::option_check_undefined_function
bool ClParser::option_check_undefined_function(FunPcl* pFunPcl)
{
    Val name = pFunPcl->GetName();
        if (setf_cell_p(name)) name = value_cell_name(name);

    if (FunPcl::Kind_Function == pFunPcl->GetKind() &&
        PACKAGE_cl == symbol_package(name) )
    {
        return false;
    }

    ASSERT(symbolp(name));

    switch (getSafety())
    {
    case 3: return true;
    case 2: return true;
    case 1: return ! is_known_package(name);
    case 0: return false;
    default: CAN_NOT_HAPPEN();
    } // switch safety
} // ClParser::option_check_undefined_function


// ClParser::option_no_type_check
bool ClParser::option_no_type_check()
{
    if (getSafety() == 0) return true;
    return false;
} // ClParser::option_no_type_check


// ClParser::option_simple_type_check
bool ClParser::option_simple_type_check()
{
    if (getDebug()  >= 1) return false;
    if (getSafety() >= 2) return false;
    return true;
} // ClParser::option_simple_type_check

} // Compiler
