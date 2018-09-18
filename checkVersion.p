/*------------------------------------------------------------------------

  Name : checkVersion.p
  Desc : Check if there is a new version on GitHub

  Notes:
    The version nr is increased when it is ready for production, the
    build nr is increaded when something is ready for beta testing.

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
DEFINE VARIABLE lAutoCheck     AS LOGICAL     NO-UNDO.
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

/* If remote build is different than local, but we have already noticed this before, 
 * then do not report new version, unless this is a manual check
 */
IF NOT plManualCheck
  AND cRemoteBuildNr <> ?
  AND cRemoteBuildNr = getRegistry('DataDigger:Update', 'RemoteBuildNr') THEN RETURN.

/* Save remote build */
IF cRemoteBuildNr <> ? THEN setRegistry('DataDigger:Update', 'RemoteBuildNr', cRemoteBuildNr).

/* New version available */
IF cRemoteVersion > cLocalVersion
  AND (plManualCheck = TRUE OR piChannel <> {&CHECK-MANUAL}) THEN
DO:
  MESSAGE 'A new version is available on the DataDigger website~nDo you want to check it?' VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO-CANCEL UPDATE lVisit.
  IF lVisit = TRUE THEN OS-COMMAND NO-WAIT START VALUE('https://datadigger.wordpress.com/category/status').
END.

ELSE
/* New BETA version available */
IF cRemoteBuildNr > cLocalBuildNr
  AND (plManualCheck = TRUE OR piChannel <> {&CHECK-MANUAL}) THEN
DO:
  MESSAGE 'A new BETA version is available on the DataDigger website~nDo you want to check it?' VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO-CANCEL UPDATE lVisit.
  IF lVisit = TRUE THEN OS-COMMAND NO-WAIT START VALUE('https://datadigger.wordpress.com/category/beta').
END.

ELSE
/* In case of a manual check, report what is found */
IF plManualCheck THEN
  MESSAGE 'No new version available, you are up to date.' VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.