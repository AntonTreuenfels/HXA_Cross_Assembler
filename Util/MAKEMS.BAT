@echo off
echo I am in charge here (call me Al).
echo.

rem - make sure we know where TAWK compiler is

if !%AWKPATH%==! set AWKPATH=c:\util\tawk
set TMP=%TEMP%

rem - make sure we have clean place to put our MS-DOS executables

if exist e:\msdos\nul deltree /Y e:\msdos
md e:\msdos

rem - make our executables

del *.exe > NUL
%AWKPATH%\awkc -xe -o hxa65 -s @make65.rsp
%AWKPATH%\awkc -xe -o hxa_t -s @make_t.rsp

rem - move our executables

move *.exe e:\msdos

rem - all finished

set TMP=
