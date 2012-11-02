@if not defined _echo echo off
setlocal
if not "%1"=="" set dstdir=%1
echo.
echo Drop to %dstdir%
echo.
set src=%srcdir%\release\win32
copy %src%\console.exe %dstdir%\evcl3.com
copy %src%\vanilla.dll %dstdir%\evcl3.dll
copy %src%\evcl3.image %dstdir%\evcl3.image
copy %src%\winapp.exe %dstdir%\evcl3.exe
