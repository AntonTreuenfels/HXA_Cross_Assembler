@echo off
echo I am in charge here (call me Al).

rem - run all versions of all tests and demos

if !%1==!/? goto help
if !%1==!? goto help

rem - erase any previous results

del *.lst > NUL
del *.err > NUL
del *.obj > NUL
del *.hex > NUL
del *.s?? > NUL
del testchk.txt > NUL

rem - do HXA executables exist ?

if not exist hxa_t.exe call make_t.bat
if not exist hxa65.exe call make.bat

rem - make sure test files in other directories exist

copy test253h.a ..\temp

rem - make sure any data-creating tests run first

hxa_t test260b.a

rem - run the test series

for %%f in (test*.a) do call testone.bat hxa_t.exe %%f
for %%f in (demo0*.a) do call testone.bat hxa_t.exe %%f
for %%f in (proc*.a) do call testone.bat hxa65.exe %%f
for %%f in (demo1*.a) do call testone.bat hxa65.exe %%f

rem - examine results

edit testchk.txt

goto done

:help

echo Run all variants of all HXA tests and demos
echo.
echo Useage: TESTALL
echo.

:done
