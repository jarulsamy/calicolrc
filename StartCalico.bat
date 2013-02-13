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

REM Set the MONO_PATH as we no longer use the GAC:
SET MONO_PATH=%CD%\mono\lib\4.0;%CD%\mono\lib\gtk-sharp-2.0;%CD%\bin;%CD%\mono\lib\2.0;%CD%\mono\lib\3.5;

REM Start up Calico in the background with no terminal:
REM "mono\bin\mono.exe" bin\Calico.exe %*
C:\PROGRA~2\MONO-2~1.8\bin\mono bin\Calico.exe %*
REM /cygdrive/c/Program\ Files\ \(x86\)/Mono-2.10.8/bin/mono.exe bin/Calico.exe %*

PAUSE

REM Change back to where we were:
cd %PD%