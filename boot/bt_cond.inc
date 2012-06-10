//  evcl - boot - conditions
//  boot/bt_cond.inc
//
//  This file is part of Evita Common Lisp.
//
//  Copyright (C) 1996-2006 by Project Vogue.
//  Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
//  @(#)$Id: //proj/evcl3/mainline/boot/bt_cond.inc#16 $
//
//  Description:
//   This file contains condition definitions.

// 9.2.2 warning
    L"defclass warning condition\n"

// 9.2.3 style-warning
    L"defclass style-warning warning\n"

// 9.2.9 storage-condition
    L"defclass storage-condition serious-condition\n"

//////////////////////////////////////////////////////////////////////
//
// 03 Evaluation and Comilation
//


//////////////////////////////////////////////////////////////////////
//
// 04 Types and Classes
//
L"define-condition simple-type-error type-error + simple-condition\n"
L"define-condition coerce-error type-error\n"

//////////////////////////////////////////////////////////////////////
//
// 05 Data and Control Flow
//
L"define-condition too-many-values error\n"
L"define-condition c::simple-program-error program-error + simple-condition\n"
L"define-condition c::simple-style-warning style-warning + simple-condition\n"

L"define-condition c::syntax-error program-error"
L"  syntax  cons"
L"  form    t"
L"\n"


//////////////////////////////////////////////////////////////////////
//
// 07 Objects
//
L"define-condition incompatible-superclass program-error"
L"  class       class"
L"  superclass  class"
L"\n"

L"define-condition invalid-class-name type-error"
L"\n"

L"define-condition lambda-list-mismatch program-error"
L"  function                generic-function"
L"  expected-lambda-list    list"
L"  lambda-list             list"
L"\n"

L"define-condition method-not-found program-error"
L"  generic-function    generic-function"
L"  qualifiers          list"
L"  specializers        list"
L"\n"

L"define-condition slot-missing-error program-error"
L"  instance    t"
L"  slot-name   symbol"
L"\n"

L"define-condition unbound-slot cell-error"
L"  instance    t"
L"\n"

L"define-condition unrecognized-initialization-argument program-error"
L"  class   class"
L"  key     symbol"
L"  keys    list"
L"\n"

//////////////////////////////////////////////////////////////////////
//
// 09 Conditions
//

L"define-condition assertion-failure error"
L"  form    t"
L"\n"

// 9.2.21 simple-warning
L"define-condition simple-warning simple-condition + warning\n"
L"define-condition simple-style-warning style-warning + simple-condition\n"

//////////////////////////////////////////////////////////////////////
//
// 11 Packages
//
L"defclass package-error error"
L"  package package-designator"
L"\n"

L"define-condition cannot-export-symbol package-error"
L"  symbol  symbol"
L"  conflict-to symbol"
L"\n"

L"define-condition cannot-import-symbol package-error"
L"  symbol  symbol"
L"  conflict-to symbol"
L"\n"

L"define-condition package-not-found package-error\n"
L"define-condition package-already-exists package-error\n"
L"define-condition simple-package-error package-error + simple-error\n"

//////////////////////////////////////////////////////////////////////
//
// 12 Numbers
//
L"define-condition arithmetic-error error"  // 12.2.78
L"  operation   function-designator"
L"  operands    list"
L"\n"

L"define-condition division-by-zero arithmetic-error\n" // 12.2.80
L"define-condition floating-point-invalid-operation"
L"  arithmetic-error\n" // 12.2.81
L"define-condition floating-point-inexact arithmetic-error\n"   // 12.2.82
L"define-condition floating-point-overflow arithmetic-error\n"  // 12.2.83
L"define-condition floating-point-underflow arithmetic-error\n" // 12.2.84

//////////////////////////////////////////////////////////////////////
//
// 19 Filenames
//
L"defclass file-error error"
L"   pathname pathname-designator"
L"\n"

L"define-condition bad-pathname-character  parse-error"
L"    component t"
L"    datum   chracter"
L"    host    basic-host"
L"\n"

L"define-condition bad-pathname-component  parse-error"
L"    component   t"
L"    datum       t"
L"    host        basic-host"
L"\n"

L"define-condition invalid-logical-pathname-translation    error"
L"    host            logical-host"
L"    translation     t"
L"\n"

L"define-condition no-translation-matches  file-error"
L"    pathname    logical-pathname"
L"\n"

L"defclass pathname-error  error"
L"    pathname    pathname"
L"\n"

L"define-condition unknown-host-name   error"
L"    name    string"
L"\n"

L"define-condition pathname-parse-error    parse-error"
L"    host    basic-host"
L"    string  string"
L"    position    sequence-index"
L"\n"

L"define-condition pathname-not-matched    pathname-error"
L"    wildcard    pathname"
L"\n"

L"define-condition translation-limits-exceeds  pathname-error"
L"\n"


//////////////////////////////////////////////////////////////////////
//
// 20 Files
//
L"define-condition access-error         file-error\n"
L"define-condition file-not-found       file-error\n"
L"define-condition file-already-exists  file-error\n"
L"define-condition path-not-found       file-error\n"
L"define-condition wildcard-not-allowed file-error\n"

L"define-condition platform-error error"
L"  code        integer"
L"  operation   t"
L"  operands    list"
L"\n"


//////////////////////////////////////////////////////////////////////
//
// 21 Streams
//
L"defclass stream-error error stream stream\n"

L"define-condition end-of-file stream-error\n"
L"define-condition closed-stream stream-error\n"

L"define-condition platform-stream-error stream-error"
L"  code    integer"
L"  operation   symbol\n"

L"define-condition unsupported-stream-operation stream-error"
L"  operation   symbol\n"


//////////////////////////////////////////////////////////////////////
//
// 22 Printer
//
L"define-condition format-error program-error"
L"  complaint   string"
L"  arguments   list"
L"  context     format-context\n"

L"define-condition print-not-readable error"
L"  object  t\n"

//////////////////////////////////////////////////////////////////////
//
// 23 Reader
//
L"defclass reader-error error"
L"  stream  stream\n"

L"define-condition simple-reader-error reader-error + simple-error\n"

L"define-condition reader-eof-error reader-error"
L"  start   sequence-index\n"

L"define-condition reader-read-delimiter reader-error"
L"  delimiter character\n"

L"define-condition reader-read-dot reader-error"
L"  where symbol\n" // (member nil first cdr last)

L"define-condition reader-read-invalid-char reader-error"
L"  character character\n"

L"define-condition reader-read-object-after-dot reader-error\n"

L"define-condition not-dispatch-macro-character error"
L"  readtable   readtable"
L"  character   character\n"

L"define-condition invalid-dispatch-subchar error"
L"  character   character\n"

// 49 Internals
L"define-condition keyboard-interrupt condition\n"

L"define-condition access-violation error"
L"  function    t"      // (or function null)
L"  ip          unsigned-byte"
L"  address     unsigned-byte"
L"\n"

L"define-condition platform-exception error"
L"  function    t"      // (or function null)
L"  ip      unsigned-byte"
L"  code    unsigned-byte"
L"\n"
