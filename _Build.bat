::@echo off
::
:: set vars
::
set drive=d:
if exist c:\data\dropbox set drive=c:

set buildDate=%date:~9,4%%date:~6,2%%date:~3,2%
set baseDir=%drive%\Data\DropBox\DataDigger
set buildDir=%baseDir%\BetaDigger\BetaDigger.%buildDate%
set srcDir=%baseDir%
set zip="%drive%\Data\DropBox\Apps2\Tools\7Zip\7z.exe"

:: Save build number
::
echo %buildDate% > %baseDir%\build.i

:: Remove old build dir
::
if exist %buildDir% rmdir /S /Q %buildDir%

:: Create a dir for the new build
::
md %buildDir%

:: Copy sources
::
cd /d %buildDir%

copy %srcDir%\*.w .
copy %srcDir%\*.p .
copy %srcDir%\*.i .
copy %srcDir%\*.wrx .
copy %srcDir%\readme.txt .
copy %srcDir%\DataDigger.txt .
copy %srcDir%\myDataDigger.txt .
copy %srcDir%\DataDigger.pf .
copy %srcDir%\Sports.pf .
copy %srcDir%\DataDiggerAbout.txt .
copy %srcDir%\DataDiggerHelp.ini .

:: Create a dummy file to avoid compiling errors
:: Needed in v19
echo /* dummy */ > frLoadMapping.w

:: Files that should NOT be in the package
if exist myDataDigger.p del myDataDigger.p

:: Images in their own dir
::
md image
xcopy %srcDir%\image image

:: Zip it all
::
if exist %buildDir%.zip del %buildDir%.zip
%zip% a -r %buildDir%.zip *.*

:: Remove dir
:: 
cd ..
rmdir /S /Q %buildDir%

:: Generate version file
::
set versionFile=%baseDir%\BetaDigger\DataDiggerVersion.txt

echo [DataDigger]         > %versionFile%
echo version=20          >> %versionFile%
echo build=20130531      >> %versionFile%
echo download=http://www/oehive.org/files/DataDigger-20.zip >> %versionFile%
echo.                    >> %versionFile%

echo [BetaDigger]        >> %versionFile%
echo version=20          >> %versionFile%
echo build=%buildDate%   >> %versionFile%
echo download=http://www/oehive.org/files/BetaDigger.%buildDate%.zip >> %versionFile%

:: Done
::
pause. 