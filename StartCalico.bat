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

REM **********************************************
REM **********************************************
REM **     Having problems?? Read this Info     **
REM **     Having problems?? Read this Info     **
REM **     Having problems?? Read this Info     **
REM **********************************************
REM ** If you get an error when you run Calico, **
REM ** this may fix it.                         **
REM **                                          **
REM ** First, put the letters REM in front      **
REM ** of the line that starts with             **
REM **      C:\Program Files                    **
REM **                                          **
REM ** Then, look at the other 5 similar lines  **
REM ** that start with REM. Go to my computer   **
REM ** and follow the path to see which of the  **
REM ** paths from C:\Program Files              **
REM ** down to the \bin\mono actually exists    **
REM ** on your machine. Remove the REM from in  **
REM ** front of that line. Then save this file  **
REM ** and try running it again                 **

REM **********************************************
REM **       ADD REM TO ONE LINE BELOW &        **
REM **   REMOVE REM FROM ANOTHER LINE BELOW     **
REM **********************************************

REM "C:\Program Files (x86)\Mono-2.6.7\bin\mono" bin\Calico.exe %*
REM "C:\Program Files (x86)\Mono-2.8.2\bin\mono" bin\Calico.exe %*
REM "C:\Program Files (x86)\Mono-2.10.3\bin\mono" bin\Calico.exe %*
REM "C:\Program Files\Mono-2.6.7\bin\mono" bin\Calico.exe %*
REM "C:\Program Files\Mono-2.8.2\bin\mono" bin\Calico.exe %*
REM "C:\Program Files (x86)\Mono-2.10.3\bin\mono" bin\Calico.exe %*

bin\Calico.exe %*

REM **********************************************
REM **       ADD REM TO ONE LINE ABOVE &        **
REM **   REMOVE REM FROM ANOTHER LINE ABOVE     **
REM **********************************************
REM **********************************************

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
   echo If you have never run Calico successfully yet
   echo then you may be able to fix this problem quickly and
   echo easily - even if you don't think you're a computer geek.
   echo                                               .
   echo You need to right click on the StartCalico.bat file in the 
   echo Calico folder and select "edit" - once you do that it will
   echo open up in notepad and there are instructions that can help
   echo you in there!
   echo                                              .
   pause
)
