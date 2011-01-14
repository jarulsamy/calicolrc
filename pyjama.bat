@echo off
echo Loading Pyjama...
cd %HOMEDRIVE%\%HOMEPATH%\Pyjama\
"c:\Program Files (x86)\Mono-2.8.1\bin\mono" bin\pyjama.exe %*
if errorlevel 1 (
   echo Failure Reason Given is %errorlevel%
   pause
)
