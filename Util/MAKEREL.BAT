@echo off
echo I am in charge here (call me Al).
echo.

rem  This batch file creates a directory tree of HXA-related files,
rem  zips them, and then creates a website directory with the new
rem  zip file and a few others

rem - make sure we know where TAWK compiler is

set AWKPATH=c:\util\tawk
set TMP=%TEMP%

rem - useful directories

set SRCDIR=c:\hxa
set RELDIR=c:\hxa\release

rem - version number (low three digits)

set VER=201

rem - get rid of possible clutter in work directory

del *.lst > NUL
del *.err > NUL
del *.obj > NUL
del *.hex > NUL
del *.s?? > NUL

rem - make upload directory

if exist e:\upload\nul deltree /Y e:\upload
md e:\upload

rem - create release directory tree and begin populating

if exist %RELDIR%\nul deltree /Y %RELDIR%

rem - main directory
md %RELDIR%
copy c:\misctext\gpl\gpl.txt %RELDIR% 
ren %RELDIR%\gpl.txt license.gpl

rem - doc directory
md %RELDIR%\docs
copy %RELDIR%\license.gpl %RELDIR%\docs 

rem - source directory
md %RELDIR%\source
xcopy a*.awk %RELDIR%\source 

rem - general test directory
md %RELDIR%\test
xcopy test*.a %RELDIR%\test
if exist %RELDIR%\test\test.a del /Y %RELDIR%\test\test.a
copy iabsend.a %RELDIR%\test
copy ifilenam.a %RELDIR%\test
copy ihexdata.a %RELDIR%\test
copy ihexseg.a %RELDIR%\test
copy ilisting.a %RELDIR%\test
copy inarrow.a %RELDIR%\test
copy ioverlap.a %RELDIR%\test
copy ipagsize.a %RELDIR%\test
copy ixrefglb.a %RELDIR%\test
copy ixrefmac.a %RELDIR%\test

rem - general demo directory
md %RELDIR%\demo
xcopy demo*.a %RELDIR%\demo 
copy i6502.a %RELDIR%\demo
copy i65c02.a %RELDIR%\demo
copy ir65c02.a %RELDIR%\demo
copy i8080.a %RELDIR%\demo
copy iz80.a %RELDIR%\demo

rem - processor directory
md %RELDIR%\proc
xcopy proc*.a %RELDIR%\proc 

rem - hello world directory
md %RELDIR%\hello
xcopy %SRCDIR%\hello*.a %RELDIR%\hello

rem - utilities directory
md %RELDIR%\util
xcopy %SRCDIR%\make*.* %RELDIR%\util
xcopy %SRCDIR%\m*.awk %RELDIR%\util
xcopy %SRCDIR%\reg*.* %RELDIR%\util
xcopy %SRCDIR%\test*.bat %RELDIR%\util
xcopy %SRCDIR%\test*.awk %RELDIR%\util
xcopy %SRCDIR%\*.rsp %RELDIR%\util

rem - create HTML documentation

call makehtm.bat

move e:\temp\index.htm e:\upload > NUL
move e:\temp\*.htm %RELDIR%\docs > NUL

rem - create MS-DOS executables
rem - result in e:\msdos

call makems.bat

rem - zip MS-DOS version

del *.zip

xcopy e:\msdos\*.exe %RELDIR%
call zip.bat -pr hxa0%VER%.zip %RELDIR%\*.*
del %RELDIR%\*.exe

rem - Win32 executables created elsewhere and brought over manually
rem - assumed to be in Win32 directory

rem - zip Win32 version

xcopy e:\win32\*.exe %RELDIR%
call zip.bat -pr hxa0%VER%w.zip e:\win32\*.exe %RELDIR%\*.*
del %RELDIR%\*.exe

rem - zip HXA_T ala cartes

call zip.bat hxa_tw.zip e:\win32\hxa_tw.exe
call zip.bat hxa_t.zip e:\msdos\hxa_t.exe
call zip.bat doc.zip %RELDIR%\docs\hxa*.htm
call zip.bat test.zip %RELDIR%\test\*.a
call zip.bat demo.zip %RELDIR%\demo\*.a
call zip.bat source.zip %RELDIR%\source\a*.awk

rem - zip HXA65 ala cartes

call zip.bat hxa65w.zip e:\win32\hxa65w.exe
call zip.bat hxa65.zip e:\msdos\hxa65.exe
call zip.bat doc65.zip %RELDIR%\docs\hxa65.htm
call zip.bat proc.zip %RELDIR%\proc\*.a

move *.zip e:\upload > NUL

rem - all finished

set VER=
set SRCDIR=
set TMP=

