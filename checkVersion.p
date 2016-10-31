/*------------------------------------------------------------------------
  File : checkVersion.p
  Desc : Check if there is a new version on GitHub
    
  Notes:
    The version nr is increased when it is ready for production, the
    build nr is increaded when something is ready for beta testing.
    
  Parameters:
    piChannel : 0=no check, 1=check stable, 2=check beta
    plSilent  : if NO, then report local and remote versions
  ----------------------------------------------------------------------*/

DEFINE INPUT PARAMETER piChannel     AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER plManualCheck AS LOGICAL NO-UNDO.

/* Constant values for update channels */
&GLOBAL-DEFINE CHECK-MANUAL 0
&GLOBAL-DEFINE CHECK-STABLE 1
&GLOBAL-DEFINE CHECK-BETA   2

DEFINE VARIABLE cLocalVersion  AS CHARACTER   NO-UNDO INITIAL '{version.i}'.
DEFINE VARIABLE cLocalBuildNr  AS CHARACTER   NO-UNDO INITIAL '{build.i}'.
DEFINE VARIABLE cRemoteVersion AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRemoteBuildNr AS CHARACTER   NO-UNDO.

RUN getVersionInfo.p(OUTPUT cRemoteVersion, OUTPUT cRemoteBuildNr).

message
  'RemoteVersion' cRemoteVersion skip
  'LocalVersion' cLocalVersion skip
  'RemoteBuildNr' cRemoteBuildNr skip
  'LocalBuildNr' cLocalBuildNr skip
  'ManualCheck' plManualCheck skip
  'Channel' piChannel skip
  'msg:' cRemoteVersion = cLocalVersion cRemoteBuildNr = cLocalBuildNr
  
  view-as alert-box.
  output to c:\temp\ccc.txt.
  put unformatted
  'RemoteVersion' cRemoteVersion skip
  'LocalVersion' cLocalVersion skip
  'RemoteBuildNr' cRemoteBuildNr skip
  'LocalBuildNr' cLocalBuildNr skip
  'ManualCheck' plManualCheck skip
  'Channel' piChannel skip
. output close.

IF (cRemoteVersion > cLocalVersion)
  AND (plManualCheck OR piChannel = {&CHECK-STABLE}) THEN
DO:
message 1 view-as alert-box.
  OS-COMMAND NO-WAIT START VALUE('https://datadigger.wordpress.com/category/status').
  MESSAGE 'A new version is available on the DataDigger website' VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
    
ELSE
IF (cRemoteBuildNr > cLocalBuildNr)
  AND (plManualCheck OR piChannel = {&CHECK-BETA}) THEN
DO:
message 2 view-as alert-box.
  OS-COMMAND NO-WAIT START VALUE('https://datadigger.wordpress.com/category/beta').
  MESSAGE 'A new BETA version is available on the DataDigger website' VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
  
/* In case of a manual check, report what is found */
ELSE
IF plManualCheck
  AND cRemoteVersion = cLocalVersion
  AND cRemoteBuildNr = cLocalBuildNr THEN
  MESSAGE 'No new version available, you are up to date' VIEW-AS ALERT-BOX INFO BUTTONS OK.