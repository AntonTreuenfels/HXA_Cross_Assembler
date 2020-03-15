rem - check the test series

for %%f in (test*.a) do call awk.bat testchk.awk %%f
for %%f in (demo*.a) do call awk.bat testchk.awk %%f
for %%f in (proc*.a) do call awk.bat testchk.awk %%f

