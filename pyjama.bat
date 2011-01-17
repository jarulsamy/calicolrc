@echo off
REM Batch file for Windows to Start the Pyjama Project
REM --------------------------------------------------
echo Loading Pyjama...
REM Change to the directory holding this batch file:
cd /d %~dp0
REM Run pyjama.exe with mono:
"c:\Program Files (x86)\Mono-2.6.4\bin\mono" bin\pyjama.exe %*
REM "c:\Program Files (x86)\Mono-2.8.2\bin\mono" bin\pyjama.exe %*
REM "c:\Program Files (x86)\Mono-2.6.7\bin\mono" bin\pyjama.exe %*
REM Report an error, if one, and pause to let user see it:
if errorlevel 1 (
   echo Failure Reason Given is %errorlevel%
   pause
)
