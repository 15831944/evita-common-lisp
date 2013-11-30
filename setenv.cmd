@if not defined _echo echo off
:////////////////////////////////////////////////////////////////////////
://
:// Build Script For Evita Common Lisp
:// build.cmd
://
:// @(#)$Id: //proj/evcl3/mainline/setenv.cmd#1 $
://
@if ""=="%_echo%" echo off
set DSTDIR=c:\bin
set SRCDIR=d:\proj\evcl3
set SOLUTION=evcl3
set Platforms=win32 x64
set Configs=debug release
