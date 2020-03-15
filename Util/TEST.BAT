@echo off
echo I am in charge here (call me Al).

rem - run all versions of one test

if "%1==" goto help
if !%1==!/? goto help

rem - does test exist ?

if exist test%1?.a goto begin
echo Test%1 not found!
goto done

:begin

rem - does HXA executable exist ?

if not exist hxa_t.exe call make_t.bat

rem - erase any/all previous results

if exist *.lst del *.lst > NUL
if exist *.err del *.err > NUL
if exist *.obj del *.obj > NUL
if exist *.hex del *.hex > NUL
if exist *.s19 del *.s19 > NUL
if exist *.s28 del *.s28 > NUL
if exist *.s37 del *.s37 > NUL
if exist *.001 del *.001 > NUL
if exist *.002 del *.002 > NUL
if exist *.005 del *.005 > NUL
if exist *.006 del *.006 > NUL

rem - run the test series

for %%f in (test%1?.a) do hxa_t.exe %%f

rem - check the test series

if exist testchk.txt del testchk.txt > NUL
for %%f in (test%1?.a) do %AWKPATH%\awk -f testchk.awk %%f

edit testchk.txt

goto done

:help

echo Run all variants of one HXA test
echo.
echo Useage: TEST num
echo.
echo  num - three digit test number or "*" (to run all tests)

:done
