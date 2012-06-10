@if not defined _echo echo off
:////////////////////////////////////////////////////////////////////////
://
:// Initialize developer environment.
:// platform/win/dev-initenv.cmd
://
:// @(#)$Id: //proj/evcl3/mainline/platform/win/build/dev-initenv.cmd#2 $
://
setlocal

set srcdir=platform\win

copy %srcdir%\build\templ-setenv.cmd setenv.cmd
copy %srcdir%\build\build.txt
copy %srcdir%\build\setbuild.cs

copy %srcdir%\build\dev-build.cmd         build.cmd
copy %srcdir%\build\dev-make_image.cmd    make_image.cmd

copy %srcdir%\config.h
touch build.h

endlocal

