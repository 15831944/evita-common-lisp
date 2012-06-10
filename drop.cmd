@if not defined _echo echo off
set dst=c:\bin
if not "%1"=="" set dst=%1
echo.
echo Drop to %dst%
echo.
set src=%srcdir%\release\win32
copy %src%\console.exe %dst%\evcl3.com
copy %src%\vanilla.dll %dst%\evcl3.dll
copy %src%\evcl3.image %dst%\evcl3.image
copy %src%\winapp.exe %dst%\evcl3.exe
