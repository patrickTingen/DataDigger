/*------------------------------------------------------------------------

  Name : checkVersion.p
  Desc : Check if there is a new version 

  Notes:
    The version nr is increased when it is ready for production, the
    build nr is increased when something is ready for beta testing.

  Parameters:
    piChannel     : 0=no check, 1=check stable, 2=check beta
    plManualCheck : TRUE when user presses 'Check Now' button
  ----------------------------------------------------------------------*/

DEFINE INPUT PARAMETER piChannel     AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER plManualCheck AS LOGICAL NO-UNDO.

{ DataDigger.i }

DEFINE VARIABLE cLocalVersion  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLocalBuildNr  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRemoteVersion AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRemoteBuildNr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNewVersionUrl AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lVisit         AS LOGICAL     NO-UNDO INITIAL TRUE.

/* Might be spaces in the include file */
cLocalVersion = TRIM('{version.i}').
cLocalBuildNr = TRIM('{build.i}').

/* If channel is set to manual, but this is not a manual check then return. */
IF piChannel = {&CHECK-MANUAL} AND NOT plManualCheck THEN RETURN.

/* Get proper version info, depending on channel */
CASE piChannel:
  WHEN {&CHECK-MANUAL} THEN RUN getVersionInfo.p(INPUT 'master' , OUTPUT cRemoteVersion, OUTPUT cRemoteBuildNr).
  WHEN {&CHECK-STABLE} THEN RUN getVersionInfo.p(INPUT 'master' , OUTPUT cRemoteVersion, OUTPUT cRemoteBuildNr).
  WHEN {&CHECK-BETA}   THEN RUN getVersionInfo.p(INPUT 'develop', OUTPUT cRemoteVersion, OUTPUT cRemoteBuildNr).
END CASE.

/* If version cannot be determined then don't bother. Unless this is a manual check */
IF cRemoteBuildNr = '' THEN
DO:
  IF plManualCheck THEN MESSAGE 'Cannot reach version the DataDigger website' VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  RETURN.
END.

/* Save remote version / build */
setRegistry('DataDigger:Update', 'RemoteBuildNr', cRemoteBuildNr).
setRegistry('DataDigger:Update', 'RemoteVersion', cRemoteVersion).

/* If you are using a build that is newer than the production version, it means you are running a beta version.
 * Then force the update channel to 'beta'
 */
IF (piChannel = {&CHECK-MANUAL} OR piChannel = {&CHECK-STABLE})
  AND '{build.i}' > cRemoteBuildNr THEN setRegistry("DataDigger:Update","UpdateChannel", "{&CHECK-BETA}").

/* New version? */
IF cRemoteVersion > cLocalVersion THEN
DO:
  cNewVersionUrl = 'https://github.com/patrickTingen/DataDigger/releases/latest'.
  
  IF plManualCheck THEN
  DO:
    MESSAGE 'A new version is available on the DataDigger website~n~nDo you want to check it?' VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO-CANCEL UPDATE lVisit.
    IF lVisit = TRUE THEN OS-COMMAND NO-WAIT START VALUE(cNewVersionUrl).
  END.
  ELSE 
    setRegistry('DataDigger:Update', 'NewVersionURL', cNewVersionUrl).

END.

ELSE
/* New BETA? */
IF cRemoteBuildNr > cLocalBuildNr THEN
DO:
  cNewVersionUrl = 'https://github.com/patrickTingen/DataDigger/releases/'.
  
  IF plManualCheck THEN
  DO:
    MESSAGE 'A new BETA version is available on the DataDigger website~n~nDo you want to check it?' VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO-CANCEL UPDATE lVisit.
    IF lVisit = TRUE THEN OS-COMMAND NO-WAIT START VALUE(cNewVersionUrl).
  END.
  ELSE 
    setRegistry('DataDigger:Update', 'NewVersionURL', cNewVersionUrl).

END.

ELSE
/* Up to date */
DO:
  IF plManualCheck THEN
    MESSAGE 'No new version available, you are up to date.' VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  ELSE 
    setRegistry('DataDigger:Update', 'NewVersionURL', '').
END.

