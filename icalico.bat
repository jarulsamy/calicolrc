@echo off
REM Batch file for Windows to Start the ICalico Project
REM ---------------------------------------------------
REM Change to the directory holding this batch file:
cd /d "%~dp0"

SET PATH=%PATH%;%CD%\bin;%CD%\modules
SET MONO_PATH=%CD%\mono\lib\4.0;%CD%\mono\lib\gtk-sharp-2.0;%CD%\bin;%CD%\mono\lib\2.0;%CD%\mono\lib\3.5;%CD%\bin\windows

"mono\bin\mono.exe" bin\Calico.exe --check-profile
ipython notebook --profile calico %*

REM Change back to where we were:
cd %PD%
