//{{NO_DEPENDENCIES}}
// Microsoft Developer Studio generated include file.
// Used by console.rc
//
#define IDI_EVCL    100
#define IDI_LISP    101

#ifndef APSTUDIO_INVOKED

// Common
#define IDS_E_ALL_BASE_BAD          L"Base number ~D must be in [2, 36]."
#define IDS_E_ALL_FNAME_BAD         L"Bad function designator: ~S"

// 5 Data and Control Flow
#define IDS_E_FUNCTION_UNDEFINED    L"Undefind function: ~S"
#define IDS_E_FUNCALL_TOOFEW        L"Function ~S requires at least ~D arguments instead of ~D arguments."
#define IDS_E_FUNCALL_TOOMANY       L"Function ~S accepts ~D arguments instead of ~D arguments."
#define IDS_E_EXIT_POINT            L"Can't transfer to abandoned exit-point: ~S"

// 10 Symbols
#define IDS_E_SYMBOL_ALTER_CONST    L"Can't alter constant: ~S"

// 13 Characters
#define IDS_E_CHAR_NAME             L"Bad chracter name ~S."

// 15 Arrays
#define IDS_E_ARRAY_TOTAL_SIZE      L"Array total size(~D) must be smaller than ~D."

// 18 Hash Tables
#define IDS_E_HASH_TABLE_TEST       L"Bad hash-table test: ~S"

// 21 Streams
#define IDS_E_STREAM_UNSUPPORTED    L"Unsupported operation ~S for ~S."

// 22 Printer
#define IDS_E_FORMAT_ARG_NOMORE     L"Too few arguments are supplied." 
#define IDS_E_FORMAT_DIRECTIVE_BAD  L"Unknown directive ~C." 
#define IDS_E_FORMAT_PARAM_NUM_BAD  L"Bad numeric parameter." 
#define IDS_E_FORMAT_PARAM_QUOTE    L"Missing chracter after quote(')."
#define IDS_E_FORMAT_PARAM_TOOMANY  L"Too many parameters."

// 23 Reader
#define IDS_E_READER_BROKEN         L"Broken readtable: ~S"
#define IDS_E_READER_CHAR_INVALID   L"Invalid character ~@C is appeared."
#define IDS_E_READER_LIST_DOT_FIRST L"No object before dot."
#define IDS_E_READER_LIST_DOT_DOT   L"Dot after dot."
#define IDS_E_READER_LIST_DOT_NO_OBJECT L"No object after dot."
#define IDS_E_READER_MACRO_COMMA    L"Comma(,) is used out side of backquote."
#define IDS_E_READER_MACRO_RPAREN   L"Mismatched right parenthesis."
#define IDS_E_READER_OBJECT_DOT     L"Dot is appeared out of list."
#define IDS_E_READER_OBJECT_DOTS    L"Repeated(~D) dots are appeared."
#define IDS_E_READER_PARSER_COLON   L"Too many package markers."
#define IDS_E_READER_PARSER_DIGITS  L"Invalid digits for radix ~D: ~S"
#define IDS_E_READER_PARSER_INTERNAL L"Can't access internal symbol ~S."
#define IDS_E_READER_PARSER_NONAME  L"Package specifier isn't implemented yet."
#define IDS_E_READER_PARSER_NOTFOUND L"No such symbol ~S in ~S."
#define IDS_E_READER_PARSER_PACKAGE L"No such package ~S."
#define IDS_E_READER_PARSER_SYMBOL  L"Can't use ~@C as symbol name."
#define IDS_E_READER_SHARP          L"Can't use #~C."
#define IDS_E_READER_SHARP_NYI      L"#~C is not implemented yet."
#define IDS_E_READER_TOKEN_TOOLONG  L"Too long token."
#define IDS_E_READER_TOKEN_SYMBOL   L"Bad symbol name: ~S"

#if USE_VM_COMPILER
//////////////////////////////////////////////////////////////////////
//
// Compiler
//
#define IDS_E_CM_BLOCK_NAME     L"Block name must be a symbol: ~S"
#define IDS_E_CM_DECLARE        L"Illegal placed declaration: ~S"
#define IDS_E_CM_DECLSPEC       L"Invalid declaration: ~S"
#define IDS_E_CM_EVAL_WHEN      L"Malformed situations: ~S"
#define IDS_E_CM_EVAL_WHEN_BAD  L"Invalid situation: ~S"
#define IDS_E_CM_EVAL_WHEN_OLD  L"Situation ~S supercedes: ~S"
#define IDS_E_CM_FLET_DUP       L"Duplicated function-name: ~S"
#define IDS_E_CM_FNAME          L"Invalid function-name: ~S"
#define IDS_E_CM_FUNC_NOT       L"Reference non-function: ~S"
#define IDS_E_CM_GO             L"Undefined tag: ~S"
#define IDS_E_CM_LAMBDA_AUX     L"Malformed &aux specifier: ~S"
#define IDS_E_CM_LAMBDA_KEY     L"Malformed &key specifier: ~S"
#define IDS_E_CM_LAMBDA_KEY_BAD L"Keyword name must be a symbol: ~S"
#define IDS_E_CM_LAMBDA_KEY_DUP L"Duplicate keyword name: ~S"
#define IDS_E_CM_LAMBDA_OPT     L"Malformed &optional specifier: ~S"
#define IDS_E_CM_LAMBDA_LIST    L"Malformed lambda-list: ~S"
#define IDS_E_CM_LET_BINDING    L"Malformed binding: ~S"
#define IDS_E_CM_LET_CONST      L"Can't bind constant: ~S"
#define IDS_E_CM_LET_DUP        L"Duplicated binding: ~S"
#define IDS_E_CM_MALFORMED      L"Malformed form: ~S"
#define IDS_E_CM_NTH_VALUE      L"Value is out of multiple-value bound: ~S"
#define IDS_E_CM_OPERATOR       L"Operator must be a symbol: ~S"
#define IDS_E_CM_PROGN          L"Malformed form sequence: ~S"
#define IDS_E_CM_RETURN         L"No such block named: ~S"
#define IDS_E_CM_SETQ_CONST     L"Can't alter constant: ~S"
#define IDS_E_CM_SYNTAX         L"Syntax error: ~S"
#define IDS_E_CM_TAG            L"Tag name must be a symbol or an integer: ~S"
#define IDS_E_CM_TAGBODY_TAG    L"Duplicated tag name: ~S"
#define IDS_E_CM_VARIABLE       L"Variable name must be a symbol: ~S"

#define IDS_W_CM_FORM_CONSTANT  L"Ignore constant form: ~S"
#define IDS_W_CM_FUNC_FREE      L"Reference undefined function: ~S"
#define IDS_W_CM_TAG_NOREF      L"Unreferenced tag: ~S"
#define IDS_W_CM_VAR_FREE       L"Reference free variable: ~S"
#define IDS_W_CM_VAR_NOREF      L"Unreferenced variable: ~S"
#define IDS_W_CM_VAR_USE_IGNORE L"Use ignored variable: ~S"

#endif // USE_VM_COMPILER

#endif // !defined(APSTUDIO_INVOKED)

// Next default values for new objects
// 
#ifdef APSTUDIO_INVOKED
#ifndef APSTUDIO_READONLY_SYMBOLS
#define _APS_NEXT_RESOURCE_VALUE        202
#define _APS_NEXT_COMMAND_VALUE         32768
#define _APS_NEXT_CONTROL_VALUE         201
#define _APS_NEXT_SYMED_VALUE           101
#endif
#endif
