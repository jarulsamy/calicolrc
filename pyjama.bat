@echo off
echo Loading Pyjama...
cd %HOMEDRIVE%\%HOMEPATH%\Pyjama\
"c:\Program Files (x86)\Mono-2.8.1\bin\mono" bin\ipy.exe src\pyjama.py %*
REM "c:\Program Files (x86)\Mono-2.8\bin\mono.exe" bin\ipy.exe src\pyjama.py %*
REM pause