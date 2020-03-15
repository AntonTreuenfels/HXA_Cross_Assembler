@echo off
echo I am in charge here (call me Al).

rem - run all versions of one test

if "%1==" goto help
if !%1==!/? goto help

rem - does test exist ?

if exist demo%1?.a goto begin
echo demo%1 not found!
goto done

:begin

rem - erase any old results

if exist testchk.txt del testchk.txt

rem - run the test(s)

if exist demo%1.a call regress1.bat demo%1

if exist demo%1e.a call regress1.bat demo%1e

edit testchk.txt

goto done

:help

echo Run all variants of one test in both old and new HXA versions
echo.
echo Useage: REGDEMO num
echo.
echo  num - three digit demo number

:done
