@if not defined _echo echo off
for %%p in (win32 x64) do (
    for %%f in (evcl3 genesis _stop _untested) do (
      if exist %SRCDIR%\release\%%p\%%f.image (
          copy %SRCDIR%\release\%%p\%%f.image %SRCDIR%\debug\%%p
      )
    )
)
