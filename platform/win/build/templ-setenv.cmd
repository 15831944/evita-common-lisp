@if not defined _echo echo off
:////////////////////////////////////////////////////////////////////////
://
:// Build Script For Evita Common Lisp
:// build.cmd
://
:// @(#)$Id: //proj/evcl3/mainline/platform/win/build/templ-setenv.cmd#2 $
://
@if ""=="%_echo%" echo off
set DSTDIR=d:\bin
set SRCDIR=d:\proj\evcl3
set SOLUTION=evcl3
set Platforms=win32 x64
set Configs=debug release
