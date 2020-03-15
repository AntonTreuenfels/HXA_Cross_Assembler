@echo off
echo I am in charge here (call me Al).
echo.

rem  This batch file creates the HTML documentation for HXA
rem - it is left in the %TEMP% directory

rem - make sure we know where TAWK compiler is

if !%AWKPATH%==! set AWKPATH=c:\util\tawk
set TMP=%TEMP%

rem - source directory of current version

if !%SRCDIR%==! set SRCDIR=c:\hxa

rem - make sure "test", "demo", "proc" and "hello" are up-to-date

if not exist testlist.awk copy %SRCDIR%\testlist.awk
%AWKPATH%\awk.exe -f testlist.awk

rem - make sure "mesglist" is up-to-date

if not exist mesglist.awk copy %SRCDIR%\mesglist.awk
%AWKPATH%\awk.exe -f mesglist.awk

rem - create HTML documentation

if not exist mut2html.awk copy c:\util\html\mut\mut2html.awk 
%AWKPATH%\awk.exe -f mut2html.awk hxa.doc

rem - all finished

set TMP=
