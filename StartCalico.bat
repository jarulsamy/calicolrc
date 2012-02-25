@echo off
REM Batch file for Windows to Start the Calico Project
REM --------------------------------------------------
echo Loading Calico...
REM Change to the directory holding this batch file:
cd /d "%~dp0"
REM Run Calico.exe with mono in a particular language:
REM SET LANGUAGE=es_ES.UTF8
REM SET LANG=es_ES.UTF8
SET PATH=%PATH%;%CD%\bin;%CD%\modules
REM SET MONO_LOG_LEVEL=debug

start /MIN bin\Calico.exe %*

REM Report an error, if one, and pause to let user see it:
if errorlevel 1 (
   echo Failure Reason Given is %errorlevel%
   echo                                              .
   echo *** NEED HELP?? READ THE FOLLOWING ***
   echo                                              .
   echo If you have run Calico successfully before
   echo then, please let your instructor know what 
   echo happened, and we'll try to fix it.
   echo 
   pause
)
