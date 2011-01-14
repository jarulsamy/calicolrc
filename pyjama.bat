@echo off
echo Loading Pyjama...
cd %HOMEDRIVE%\%HOMEPATH%\Pyjama\
set MONO_PATH=bin
"c:\Program Files (x86)\Mono-2.8.1\bin\mono" pyjama.exe %*
if errorlevel 1 (
   echo Failure Reason Given is %errorlevel%
   pause
   REM exit /b %errorlevel%
)
