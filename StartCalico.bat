@echo off
REM Batch file for Windows to Start the Calico Project
REM --------------------------------------------------
REM Change to the directory holding this batch file:
cd /d "%~dp0"

REM Run Calico.exe with mono in a particular language:
REM SET LANGUAGE=es_ES.UTF8
REM SET LANG=es_ES.UTF8

REM Setup paths to find DLLs, and other env settings:
SET PATH=%PATH%;%CD%\bin;%CD%\modules
REM SET MONO_LOG_LEVEL=debug

REM Start up Calico in the background with no terminal:
start /MIN bin\Calico.exe %*

REM Change back to where we were:
cd %PD%