@echo off
set files=cpp_err_symbols cpp_sig_symbols
set dstdir=%1

if not exist %dstdir%/libguile (
    echo "create directory %dstdir%/libguile"
    md %dstdir%/libguile
)
echo %files%
for %%I in (%files%) do (
echo "generate %%I.c"
C:\msys64\usr\bin\awk.exe -f ./libguile/cpp_cnvt.awk < ./libguile/%%I.in > %dstdir%/libguile/%%I.c
)
