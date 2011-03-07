@echo off
REM Batch file for Windows to Start the Pyjama Project
REM --------------------------------------------------
echo Loading Pyjama...
REM Change to the directory holding this batch file:
cd /d "%~dp0"
REM Run pyjama.exe with mono in a particular language:
REM SET LANGUAGE=es_ES.UTF8
REM SET LANG=es_ES.UTF8
"C:\Program Files (x86)\Mono-2.6.7\bin\mono" bin\pyjama.exe %*
REM Report an error, if one, and pause to let user see it:
if errorlevel 1 (
   echo Failure Reason Given is %errorlevel%
   pause
)
