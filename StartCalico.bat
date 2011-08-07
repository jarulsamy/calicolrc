@echo off
REM Batch file for Windows to Start the Calico Project
REM --------------------------------------------------
echo Loading Calico...
REM Change to the directory holding this batch file:
cd /d "%~dp0"
REM Run calico.exe with mono in a particular language:
REM SET LANGUAGE=es_ES.UTF8
REM SET LANG=es_ES.UTF8
SET PATH=%PATH%;%CD%\bin
REM SET MONO_LOG_LEVEL=debug
"C:\Program Files (x86)\Mono-2.6.7\bin\mono" bin\calico.exe %*
REM "C:\Program Files (x86)\Mono-2.8.2\bin\mono" bin\calico.exe %*
REM "C:\Program Files (x86)\Mono-2.10.3\bin\mono" bin\calico.exe %*
REM Report an error, if one, and pause to let user see it:
if errorlevel 1 (
   echo Failure Reason Given is %errorlevel%
   pause
)
