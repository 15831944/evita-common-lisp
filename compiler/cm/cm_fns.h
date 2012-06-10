//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - pre-compiled header
// cm_lisp_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_fns.h#6 $
//
#if !defined(INCLUDE_compiler_lisp_h)
#define INCLUDE_compiler_lisp_h

#include "./cm_defs.h"
#include "../../mini/mini_lisp.h"

namespace CommonLisp
{
Val make_string(const char*, size_t);

inline Val make_string(const char* s)
    { return make_string(s, ::lstrlenA(s)); }
} // CommonLisp

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
// Phases
//
void initialize_target();
void parse();
void optimize();
void generate();

Target* cm_get_target();


////////////////////////////////////////////////////////////
//
// Warnings
//
void style_warn(const char16*);
void style_warn(const char16*, Val);
void style_warn(const char16*, Val, Val);
void warn(const char16*);
void warn(const char16*, Val);
void warn(const char16*, Val, Val);
void warn(const char16*, Val, Val, Val);
void warn(const char16*, Val, Val, Val, Val);

////////////////////////////////////////////////////////////
//
// Log releated functions
//
void log_html_start(Val, const char16*, const char16* = NULL);
Val time_string(const SYSTEMTIME*);

void text_dump(Val);
void html_dump(Val);

Val log_format(int, const char16*);
Val log_format(int, const char16*, Val);
Val log_format(int, const char16*, Val, Val);
Val log_format(int, const char16*, Val, Val, Val);
Val log_format(int, const char16*, Val, Val, Val, Val);
Val log_format(int, const char16*, Val, Val, Val, Val, Val);
Val log_format(int, const char16*, Val, Val, Val, Val, Val, Val);
Val log_format(int, const char16*, Val, Val, Val, Val, Val, Val, Val);

void html_formatV(Val, const char16*, va_list);
void html_format(Val, const char16*, ...);
void html_log_format(int, const char16*, ...);

void cm_format(Val, const char16*, ...);
void cm_print_object(Val, Val);
bool cm_verify(Module*);

void CompilerInternalError(const char*, int);

#define COMPILER_INTERNAL_ERROR() \
    CompilerInternalError(__FILE__, __LINE__)
} // Compiler

#endif //!defined(INCLUDE_compiler_lisp_h)
