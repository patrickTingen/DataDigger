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

DEFINE VARIABLE cLocalBuild    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRemoteBuild   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNewVersionUrl AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lVisit         AS LOGICAL     NO-UNDO INITIAL TRUE.
DEFINE VARIABLE cStableBuild   AS CHARACTER   NO-UNDO.

/* Might be spaces in the include file */
cLocalBuild = TRIM('{build.i}').

/* If channel is set to manual, but this is not a manual check then return. */
IF piChannel = {&CHECK-MANUAL} AND NOT plManualCheck THEN RETURN.

/* Get current stable build */
RUN getVersionInfo.p(INPUT 'master', OUTPUT cStableBuild).

/* Get proper version info, depending on channel */
IF piChannel = {&CHECK-MANUAL} OR piChannel = {&CHECK-STABLE} THEN 
DO:
  /* If local build is newer than stable, set update channel to BETA */
  IF cLocalBuild > cStableBuild THEN 
  DO:
    setRegistry("DataDigger:Update","UpdateChannel", "{&CHECK-BETA}").
    piChannel = {&CHECK-BETA}.
  END.
  ELSE 
    cRemoteBuild = cStableBuild.
END.
   
IF piChannel = {&CHECK-BETA} THEN 
  RUN getVersionInfo.p(INPUT 'develop', OUTPUT cRemoteBuild).

/* If version cannot be determined then don't bother. Unless this is a manual check */
IF cRemoteBuild = '' OR cRemoteBuild = ? THEN
DO:
  IF plManualCheck THEN MESSAGE 'Cannot reach the DataDigger website' VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  RETURN.
END.

/* Save remote version / build */
setRegistry('DataDigger:Update', 'RemoteBuildNr', cRemoteBuild).

/* Check build to detect new versions */
IF cRemoteBuild > cLocalBuild THEN
DO:
  IF piChannel = {&CHECK-MANUAL} OR piChannel = {&CHECK-STABLE} THEN 
    cNewVersionUrl = 'https://github.com/patrickTingen/DataDigger/releases/latest'.
  ELSE 
    cNewVersionUrl = 'https://github.com/patrickTingen/DataDigger/releases/'.
  
  IF plManualCheck THEN
  DO:
    MESSAGE 'A new version is available on the DataDigger website~n~nDo you want to check it?' VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO-CANCEL UPDATE lVisit.
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

