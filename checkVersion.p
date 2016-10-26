/* Get version information from GitHub */

PROCEDURE URLDownloadToFileA EXTERNAL "URLMON.DLL" :
   DEFINE INPUT PARAMETER pCaller    AS LONG.
   DEFINE INPUT PARAMETER szURL      AS CHARACTER.
   DEFINE INPUT PARAMETER szFilename AS CHARACTER.
   DEFINE INPUT PARAMETER dwReserved AS LONG.
   DEFINE INPUT PARAMETER lpfnCB     AS LONG.
   DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.

PROCEDURE DeleteUrlCacheEntry EXTERNAL "WININET.DLL" :
   DEFINE INPUT PARAMETER lbszUrlName AS CHARACTER.
END PROCEDURE.

FUNCTION getRemoteFile RETURNS CHARACTER (pcRemoteFile AS CHARACTER): 
  DEFINE VARIABLE cLocalFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cContents  AS LONGCHAR  NO-UNDO.
  DEFINE VARIABLE iResult    AS INTEGER   NO-UNDO.

  cLocalFile = SESSION:TEMP-DIR + 'VersionInfo.txt'.
  OS-DELETE cLocalFile.

  RUN DeleteURLCacheEntry (INPUT pcRemoteFile). 
  RUN urlDownloadToFileA (0, pcRemoteFile, cLocalFile, 0, 0, OUTPUT iResult).

  COPY-LOB FILE cLocalFile TO cContents.
  RETURN STRING(cContents).
END FUNCTION. 

/* Versioning strategy:
 * 
 * Version nr is increased when it is ready for production
 * Build nr is increaded when something is ready for beta
 */
DEFINE INPUT PARAMETER pcChannel AS CHARACTER NO-UNDO.

DEFINE VARIABLE cCurrent AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRemote  AS CHARACTER   NO-UNDO.

CASE pcChannel:
  WHEN 'prod' THEN 
    ASSIGN cCurrent = '{version.i'}
           cRemote  = getRemoteFile('https://raw.githubusercontent.com/patrickTingen/DataDigger/master/version.i').

  WHEN 'beta' THEN 
    ASSIGN cCurrent = '{build.i'}
           cRemote  = getRemoteFile('https://raw.githubusercontent.com/patrickTingen/DataDigger/master/build.i').

  END.
END CASE. /* pcChannel */

IF cRemote <> '' AND cRemote > cCurrent THEN
DO:
  MESSAGE SUBSTITUTE('A new version (&1) is available at github', cRemote)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

END.

