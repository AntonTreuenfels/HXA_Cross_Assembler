@echo off
echo I am in charge here (call me Al).

rem - run all versions of one test

if "%1==" goto help
if !%1==!/? goto help

rem - does test exist ?

if exist proc%1?.a goto begin
echo proc%1 not found!
goto done

:begin

rem - does HXA executable exist ?

if not exist hxa65.exe call make.bat

rem - erase results of any previous runs of this test

for %%f in (proc%1?.lst) do del %%f
for %%f in (proc%1?.err) do del %%f

rem - run the test series

for %%f in (proc%1?.a) do hxa65.exe %%f

rem - check the test series

if exist testchk.txt del testchk.txt
for %%f in (proc%1?.a) do call awk.bat testchk.awk %%f

edit testchk.txt

goto done

:help

echo Run all variants of one HXA65 test
echo.
echo Useage: TEST65 num
echo.
echo  num - three digit test number or "*" (to run all tests)

:done
